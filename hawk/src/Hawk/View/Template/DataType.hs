{-# LANGUAGE Arrows, NoMonomorphismRestriction, TemplateHaskell #-}
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
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.Template.ToXhtml

import Control.Monad ((>=>), liftM, filterM)
import Data.Char (isAlpha)
import Text.Regex.Posix (getAllTextSubmatches, (=~))
import Data.EitherMapTree

class Bindable a where
    bindable :: a -> BindTree

baseDir :: String
baseDir = "App/template/" --TODO make use of config

isHawk :: (ArrowXml a) => a XmlTree XmlTree
isHawk = hasNamePrefix hawkPrefix -- hasNamespaceUri "http://fh-wedel.de/hawk"

-- | Calculate the function name of bind function for this File
functionName :: File -> Name
functionName = mkName . (++) "bind" . toTypeName . name2

-- | Calculate the typeNam name of bind function for this File
typeName :: File -> Name
typeName = mkName . toTypeName . name2

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

firstToAlpha :: String -> String
firstToAlpha = dropWhile $ not . isAlpha

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

data File = File {
    name2 :: String,
    binds :: [Bind]
    }
    deriving (Show)

embed :: String -> String -> String -> Bind
embed p m w = Embed p m' n w
    where
    (m', n) = parseWhat m w

parseWhat :: String -> String -> (String, String)
parseWhat modu s = if null withMod then (modu, fileWithout) else (withMod !! 1, withMod !! 2)
    where
    withMod = getAllTextSubmatches $ s =~ "../(.*)/(.*).xhtml" :: [String]
    [_, fileWithout] = getAllTextSubmatches $ s =~ "(.*).xhtml"

bindAttr :: String -> String -> String -> String -> Bind
bindAttr p nameAtr typeAtr format = Bind p nameAtr
    (toMaybe' typeAtr >>= Just . mkName . firstUpper)
    (toMaybe' format >>= Just . mkName)

viewDataType :: String -> String -> Q [Dec]
viewDataType = viewDataTypeWithPrefix ""

viewDataTypeWithPrefix :: String -> String -> String -> Q [Dec]
viewDataTypeWithPrefix pre modu = runIO . readTree pre modu >=> (sequence . buildPair)


filterEmbed :: File -> IO File
filterEmbed s@(File _ b) = do
    b' <- filterM isNotEmtpy b
    return s {binds = b'}

isNotEmtpy :: Bind -> IO Bool
isNotEmtpy (Embed _ m n _) =
  liftM (not . null) $ runX $
    constA (baseDir ++ m ++ "/" ++ n ++ ".xhtml")
    >>> prepareDoc'
    >>> deep
        (isHawk >>>
            (hasLocalPart "bind" `orElse` hasLocalPart "embed"))
isNotEmtpy (Bind _ _ _ _) = return True

buildPair :: File -> [DecQ]
buildPair f = [buildInstance f, buildData f]

buildData :: File -> DecQ
buildData s@(File _ l) =
    makeData name' [(name', map (constructorName &&& dataConstructor) l)]
    where
        name' = typeName s

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

readTree :: String -> String -> String -> IO File
readTree pre modu name' = liftM head (
    runX $
        constA (baseDir ++ modu ++ "/" ++ name' ++ ".xhtml")
        >>> prepareDoc' >>> makeTree pre modu name') >>= filterEmbed


makeData :: Name                       -- ^ the name of the new Data
          -> [(Name, [(Name, TypeQ)])] -- ^ the Constructors and the containing names and types
          -> DecQ                      -- ^ the data type
makeData name' members = dataD (cxt []) name' [] (map construct members) [] -- derivingShow
    where
        cons = map (\(x,y) -> varStrictType x (strictType notStrict y))
        construct (n, m) = recC n (cons m)

toMaybe' :: [a] -> Maybe [a]
toMaybe' x = toMaybe (not $ null x) x

-- Build Tree

makeTree :: (ArrowXml a, ArrowChoice a) => String -> String -> String -> a XmlTree File
makeTree pre modu name' = listA (makeHawk pre modu) >>> arr (File (modu ++ firstUpper name'))

makeHawk :: (ArrowXml a, ArrowChoice a) => String -> String -> a XmlTree Bind
makeHawk pre modu = proc s -> do
    c <- getChildren -< s
    b <- deep (isHawk >>> (hasLocalPart "bind" `orElse` hasLocalPart "embed")) -< c
    buildFile pre modu -< b

buildFile :: (ArrowXml a) => String -> String -> a XmlTree Bind
buildFile pre modu = choiceA
    [ hasLocalPart "bind"  :-> buildBind pre
    , hasLocalPart "embed" :-> buildEmbed pre modu]

buildBind :: (ArrowXml a) => String -> a XmlTree Bind
buildBind pre = (getAttrValue0 "name"
        &&& getAttrValue "type")
        &&& getAttrValue "format" >>> arr (uncurry (uncurry (bindAttr pre)))

buildEmbed :: (ArrowXml a) => String -> String -> a XmlTree Bind
buildEmbed pre modu = getAttrValue0 "what" >>> arr (embed pre modu)

-- Testing Helpers

showData :: String -> String -> String -> IO ()
showData p m n = runQ (viewDataTypeWithPrefix p m n) >>= putStrLn . pprint

test :: IO ()
test = showData "" "Step" "show"

test' :: IO File
test' = readTree "" "Step" "show"

