{-# LANGUAGE Arrows, NoMonomorphismRestriction, TemplateHaskell #-}
-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009-2010 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable

-}
-- --------------------------------------------------------------------------
module Hawk.View.Template.DataType
    ( viewDataType
    , viewDataTypeWithPrefix
    , Bindable(..)
    , showData)
    where

import Text.XML.HXT.Arrow hiding (mkName)
import Language.Haskell.TH
import Hawk.Controller.Util.Text (firstLower, firstUpper, toCamelCase)
import Hawk.View.Template.Interpreter
import Hawk.View.Template.ToXhtml

import Control.Monad ((>=>), liftM, filterM)
import Data.Char (isAlpha)
import Text.Regex.Posix (getAllTextSubmatches, (=~))
import Data.EitherMapTree

-- | A Class which allow to write a custom function to create a BindTree
class Bindable a where
    bindable :: a -> BindTree

-- | The path of the templates
baseDir :: String
baseDir = "App/template/" --TODO make use of config

-- | Check if the node is for Hawk
isHawk :: (ArrowXml a) => a XmlTree XmlTree
isHawk = hasNamePrefix hawkPrefix -- hasNamespaceUri "http://fh-wedel.de/hawk"

-- | Calculate the function name of bind function for this File
functionName :: File -> Name
functionName = mkName . (++) "bind" . toTypeName . name2

-- | Calculate the typeNam name of bind function for this File
typeName :: File -> Name
typeName = mkName . toTypeName . name2

-- | Convert a String to type name (all . are converted to CamelCase and the first char is convertet
-- to upper)
toTypeName :: String -> String
toTypeName = firstUpper . toCamelCase '.'

-- | Calculate the Type of field of the data
dataConstructor :: Bind -> TypeQ
dataConstructor (Bind _ _ Nothing _) = [t| [XmlTree] |]
dataConstructor (Bind _ _ (Just t) _) = conT t
dataConstructor (Embed _ m n _) = appT listT $ conT $ mkName $ (++) m $ firstUpper $ toCamelCase '.' n

-- | Calculate the Name of field of the data
constructorName :: Bind -> Name
constructorName (Bind p n _ _) = mkName $ firstLower (p ++ firstUpper (toCamelCase '.' n))
constructorName (Embed p m n _) = mkName $ firstLower (p ++ firstUpper (toCamelCase '.' (m ++ firstUpper n)))

-- | Calculate the Name from a String
toTypeContructor :: String -> Name
toTypeContructor = mkName . firstLower . toCamelCase '.'

-- | Drop all not alpha (isAlpha) chars of a String
firstToAlpha :: String -> String
firstToAlpha = dropWhile $ not . isAlpha

-- | A data structure that represenst a Bind or a embed Tag
data Bind = Bind {
    prefix :: String,
    name :: String, -- ^ the name
    typeInfo :: Maybe Name, -- ^ the type info
    formatFunc :: Maybe Name -- ^ the format function
    } |
    Embed {
    prefix :: String,
    moduleName :: String,
    name :: String,
    what :: String
    }
    deriving (Show)

-- | A data structure that represents a File with a list of binds and a name
data File = File {
    name2 :: String,
    binds :: [Bind]
    }
    deriving (Show)

-- | Create a embed Data from the strings of a embed tag
embed :: String -- ^ the Prefix
    -> String -- ^ the current Module
    -> String -- ^ the value of the what attribut
    -> Bind
embed p m w = Embed p m' n w
    where
    (m', n) = parseWhat m w

-- | Parse the what attribut of a embed tag
parseWhat :: String -> String -> (String, String)
parseWhat modu s = if null withMod then (modu, fileWithout) else (withMod !! 1, withMod !! 2)
    where
    withMod = getAllTextSubmatches $ s =~ "../(.*)/(.*).xhtml" :: [String]
    [_, fileWithout] = getAllTextSubmatches $ s =~ "(.*).xhtml"

-- | Create the Bin data
bindAttr :: String -- ^ The Prefix
    -> String -- ^ The Name
    -> String -- ^ The value of the type attribute
    -> String -- ^ The value of the format attribute
    -> Bind
bindAttr p nameAtr typeAtr format = Bind p nameAtr
    (toMaybe' typeAtr >>= Just . mkName . firstUpper)
    (toMaybe' format >>= Just . mkName)

-- | Creates a DataType for a Xhtml Template
viewDataType :: String -- ^ The module of the Template
    -> String -- ^ The Name of the Template without the file extension
    -> Q [Dec]
viewDataType = viewDataTypeWithPrefix ""

-- | Creates a DataType for a Xhtml Template with a Prefix. This could be used to prevent
-- name clashes
viewDataTypeWithPrefix :: String -> String -> String -> Q [Dec]
viewDataTypeWithPrefix pre modu = runIO . readTree pre modu >=> (sequence . buildPair)

-- | Create the data and the instacne declaration for a File
buildPair :: File -> [DecQ]
buildPair f = [buildInstance f, buildData f]

-- | create the Data declaration for a File
buildData :: File -> DecQ
buildData s@(File _ l) =
    makeData name' [(name', map (constructorName &&& dataConstructor) l)]
    where
        name' = typeName s

-- | Create the instance declaration for the Bindable class
buildInstance :: File -> DecQ
buildInstance f = do
    x <- newName "x"
    instanceD (cxt [])
        (appT
            (conT ''Bindable)
            (conT $ typeName f))
        [funD 'bindable
            [clause
                [varP x]
                (normalB (appE
                    (varE 'Data.EitherMapTree.fromList)
                    (listE (map (makeTerm x) (binds f)))))
                []
             ]
         ]

-- | Create the Term for converting the Data into the BindTree
makeTerm :: Name -> Bind -> ExpQ
makeTerm x b@(Bind _ n _ f) = singleTerm f
    where
        singleTerm Nothing = [| ($(litE $ stringL n), Right $ toXhtml ($(varE $ constructorName b) $(varE x))) |]
        -- Use $(litE $ stringL n) insteed of n for readablety in pprint output
        singleTerm (Just f') = [| ($(litE $ stringL n), Right $ $(varE f') ($(varE $ constructorName b) $(varE x))) |]
makeTerm x e@(Embed _ m n w) = deeper
    where
        deeper = [| ($(litE $ stringL w), Left $
            map $(varE 'bindable)
                $(appE
                    (varE $ constructorName e)
                    (varE x))) |]

-- | Create a Data declaration
makeData :: Name                       -- ^ the name of the new Data
          -> [(Name, [(Name, TypeQ)])] -- ^ the Constructors and the containing names and types
          -> DecQ                      -- ^ the data type
makeData name' members = dataD (cxt []) name' [] (map construct members) [] -- derivingShow
    where
        cons = map (\(x,y) -> varStrictType x (strictType notStrict y))
        construct (n, m) = recC n (cons m)

-- | Convert a list maybe list which is not empty. So [] gives Nothing and [a] gives Just [a]
toMaybe' :: [a] -> Maybe [a]
toMaybe' x = toMaybe (not $ null x) x

-- * Build Tree

-- | Read a File and return the tree of a relevant Tags
readTree :: String -> String -> String -> IO File
readTree pre modu name' = liftM head (
    runX $
        constA (baseDir ++ modu ++ "/" ++ name' ++ ".xhtml")
        >>> prepareDoc' >>> makeTree pre modu name') >>= filterEmbed

-- | filter the embed for empty files. If the embeded file contain no defniton of embed or bind
-- it will remved from the file.
filterEmbed :: File -> IO File
filterEmbed s@(File _ b) = do
    b' <- filterM isNotEmtpy b
    return s {binds = b'}

-- | Check if the Embeded file contain a embed or bind tag. For a Bind alway True is returned.
isNotEmtpy :: Bind -> IO Bool
isNotEmtpy (Embed _ m n _) =
  liftM (not . null) $ runX $
    constA (baseDir ++ m ++ "/" ++ n ++ ".xhtml")
    >>> prepareDoc'
    >>> deep
        (isHawk >>>
            (hasLocalPart "bind" `orElse` hasLocalPart "embed"))
isNotEmtpy (Bind _ _ _ _) = return True


-- | Arrow for converting the XmlTree into the File data
makeTree :: (ArrowXml a, ArrowChoice a) => String -> String -> String -> a XmlTree File
makeTree pre modu name' = listA (makeHawk pre modu) >>> arr (File (modu ++ firstUpper name'))

-- | Arrow that filter the hawk Tags bind and embed and put them into buildFile
makeHawk :: (ArrowXml a, ArrowChoice a) => String -> String -> a XmlTree Bind
makeHawk pre modu = proc s -> do
    c <- getChildren -< s
    b <- deep (isHawk >>> (hasLocalPart "bind" `orElse` hasLocalPart "embed")) -< c
    buildFile pre modu -< b

-- | run buildBind or buildEmbed for the Xml tag
buildFile :: (ArrowXml a) => String -> String -> a XmlTree Bind
buildFile pre modu = choiceA
    [ hasLocalPart "bind"  :-> buildBind pre
    , hasLocalPart "embed" :-> buildEmbed pre modu]

-- | Convert a hawk bind tag into the Bind Data
buildBind :: (ArrowXml a) => String -> a XmlTree Bind
buildBind pre = (getAttrValue0 "name"
        &&& getAttrValue "type")
        &&& getAttrValue "format" >>> arr (uncurry (uncurry (bindAttr pre)))

-- | Convert a hawk embed tag into the Embed Data
buildEmbed :: (ArrowXml a) => String -> String -> a XmlTree Bind
buildEmbed pre modu = getAttrValue0 "what" >>> arr (embed pre modu)

-- * Testing Helpe

-- | Function to show a DataType
showData :: String -- ^ The Prefix
    -> String -- ^ The module name
    -> String -- ^ The template name
    -> IO ()
showData p m n = runQ (viewDataTypeWithPrefix p m n) >>= putStrLn . pprint
