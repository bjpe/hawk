{-# LANGUAGE Arrows, TemplateHaskell #-}
module Hawk.View.Template.Interpreter
  ( evalTemplate, bind, bindAttribute, prepareDoc, bindTyped, BindTree, prepareDoc', hawkPrefix
  ) where

-- Hawk
import Hawk.Controller.Types
  ( ResponseState (..)
  , RequestEnv (..)
  , EnvController
  , runController
  , StateController
  )
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.EitherMapTree as E
import Hack

-- Xml processing
import Text.XML.HXT.Arrow as Arrow
import qualified Text.XML.HXT.DOM.ShowXml as ShowXml
import qualified Text.XML.HXT.DOM.QualifiedName as QN

-- other stuff
import qualified Data.Map as M
import Control.Monad.State (StateT, runStateT, liftIO, get, put)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Either (runEitherT, returnLeft)
import Data.Maybe (fromMaybe)
import Hawk.Controller.Responses

-- logging
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)

$(deriveLoggers "Logger" [Logger.DEBUG, Logger.WARNING])


type HawkArrow = IOSLA (XIOState (RequestEnv, ResponseState))

debugA :: ArrowIO a => a String ()
debugA = arrIO debugM
{-
debugA' :: (ArrowIO a, Show b) => a b b
debugA' = arr id &&& arrIO (debugM . show) >>> arr (\(x,_) -> x)
-}
hawkPrefix :: String
hawkPrefix = "hawk"

hawkQName :: String -> QN.QName
hawkQName l = QN.mkQName hawkPrefix l ""
-- TODO use url not prefix

-- TODO catch errors
evalTemplate :: (XmlTree -> StateController [XmlTree]) -> FilePath -> StateController ByteString
evalTemplate f fp = do
  env <- ask
  state <- get
  (state', res) <- liftIO $ runIOSLA (mainA f) (Arrow.initialState (env, state)) fp
  put $ snd $ xio_userState state'
  case res of
    (Left resp   : _) -> returnLeft resp
    (Right body' : _) -> return $ fromString body'
    _                 -> returnLeft $ errorResponse $ fromString "error in template"

--  liftM fromString $ EitherT $ return $ head res
  -- TODO handel result with not one result

-- --------------------------------------------------------------------------
-- General template processing
-- --------------------------------------------------------------------------

mainA :: (XmlTree -> StateController [XmlTree]) -> HawkArrow String (Either Response String)
mainA rw = prepareDoc
     >>> invokeController rw
     >>> Arrow.right (unlistA
                     >>> processTD interpreteLast
                     >>> headMerge
                     >>> clearNamespace
                     -- TODO this should be read from template ...
                     >>> addDoctypeDecl "html" "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
                     >>> writeDocumentToString [(a_no_empty_elements,v_1), (a_indent, v_1), (a_output_html,v_1), (a_output_encoding, utf8)]
                     )


prepareDoc :: IOStateArrow s String XmlTree
prepareDoc = readDoc >>> processTD interpreteTag

prepareDoc' :: IOStateArrow s String XmlTree
prepareDoc' = readDoc >>> processTD interpreteTag'

isInterpretableTag :: ArrowXml a => a XmlTree XmlTree
isInterpretableTag = isElem >>> hasNamePrefix hawkPrefix

processTD :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
processTD = processTopDown . (`when` isInterpretableTag)

interpreteTag :: IOStateArrow s XmlTree XmlTree
interpreteTag = choiceA
    [ isEmbed    :-> embed
    , isIgnore   :-> (ignore >>> processTD interpreteTag)
    , isSurround :-> surround
    , this       :-> this
    ]

interpreteTag' :: IOStateArrow s XmlTree XmlTree
interpreteTag' = choiceA
    [ isIgnore   :-> (ignore >>> processTD interpreteTag')
    , isSurround :-> surround'
    , this       :-> this
    ]


interpreteLast :: HawkArrow XmlTree XmlTree
interpreteLast = choiceA
    [ isEmbed        :-> (removeEmbed >>> processTD interpreteLast)
    , isMessage      :-> message
    , isErrorMessage :-> errorMessage
    , this           :-> (remaining >>> processTD interpreteLast)
    ]

-- --------------------------------------------------------------------------
-- Arrows for specific tags
-- --------------------------------------------------------------------------

removeEmbed :: ArrowXml a => a XmlTree XmlTree
removeEmbed = getChildren -- TODO handle tag with no children

-- | Check whether the current element is an embed tag
isEmbed :: ArrowXml a => a XmlTree XmlTree
isEmbed = hasLocalPart "embed" >>> hasAttr "what"

-- | Embed another template into the current template
embed :: IOStateArrow s XmlTree XmlTree
embed = applyA $  getAttrValue "what" >>>  listA loadTemplateA >>> arr setChildren
{- proc t -> do
  what  <- getAttrValue "what"  -< t
  debugA -< "embedding template '" ++ what ++ "'"
  loadTemplateA                 -< what -}

isIgnore :: ArrowXml a => a XmlTree XmlTree
isIgnore = hasLocalPart "ignore"

ignore :: ArrowXml a => a XmlTree XmlTree
ignore = getChildren

isMessage :: ArrowXml a => a XmlTree XmlTree
isMessage = hasLocalPart "message" >>> hasAttr "type"

message :: HawkArrow XmlTree XmlTree
message = proc t -> do
  msgtype <- getAttrValue "type" -< t
  (_,us)  <- getUserState        -< ()
  case M.lookup msgtype (flash us) of
    Nothing  -> none   -< ()
    Just msg -> do
       text  <- mkText -< msg
       replace <-  arr replaceContent -< text
       (getChildren <<< replace) -<< t

isErrorMessage :: ArrowXml a => a XmlTree XmlTree
isErrorMessage = hasLocalPart "error" >>> hasAttr "for"

errorMessage :: HawkArrow XmlTree XmlTree
errorMessage = proc t -> do
  msgFor <- getAttrValue "for" -< t
  (_,us) <- getUserState       -< ()
  case M.findWithDefault [] msgFor (errors us) of
    [] -> none -< ()
    es -> do
       let errs = map (\(a,e) -> if null a then e else a ++ " : " ++ e) es
       texts <- (selem "ul" [unlistA >>> selem "li" [mkText]]) -< errs
       replace <- arr replaceContent -< texts
       (getChildren <<< replace) -<< t

isContent :: ArrowXml a => a XmlTree XmlTree
isContent = hasNamePrefix hawkPrefix >>> hasLocalPart "content"

replaceContent :: ArrowXml a => XmlTree -> a XmlTree XmlTree
replaceContent content = processTopDown (constA content `when` isContent)

isSurround :: ArrowXml a => a XmlTree XmlTree
isSurround = hasLocalPart "surround" >>> hasAttr "with" >>> hasAttr "at"

-- Surround template with another template.
surround :: IOStateArrow s XmlTree XmlTree
surround = proc t -> do
  children <- listA getChildren    -< t
  at       <- getAttrValue "at"    -< t
  with     <- getAttrValue "with"  -< t
  -- Load the surrounding template
  outer    <- loadTemplateA         -< with
  -- Insert the original template into the surrounding template
  debugA -< "surrounding with '" ++ with ++ "' at '" ++ at ++ "'"
  bound    <- bindA                -< ([(at, children)], outer)
  processTD interpreteTag          -< bound

surround' :: IOStateArrow s XmlTree XmlTree
surround' = proc t -> do
  children <- listA getChildren    -< t
  at       <- getAttrValue "at"    -< t
  with     <- getAttrValue "with"  -< t
  -- Load the surrounding template
  outer    <- loadTemplateA         -< with
  -- Insert the original template into the surrounding template
  debugA -< "surrounding with '" ++ with ++ "' at '" ++ at ++ "'"
  bound    <- bindA                -< ([(at, children)], outer)
  processTD interpreteTag'          -< bound

remaining :: HawkArrow XmlTree XmlTree
remaining = proc t -> do
    n  <- arr (ShowXml.xshow . (:[])) -< t
    arrIO warningM -< "There are remaining not replaced hawk-Tag: " ++ n
    s <- returnA -< "True" --arr (fromMaybe "" . lookup "hide_hol" .  environmentOptions) <<< getUserState -< t
    case s of
        "True" -> ignore -< t
        _      -> this   -< t


-- --------------------------------------------------------------------------
-- Helper arrows
-- --------------------------------------------------------------------------

loadTemplateA :: IOStateArrow s String XmlTree
loadTemplateA =  runInLocalURIContext readDoc -- [(a_validate, v_0)]
                 >>>
                 getChildren -- discard root node

readDoc :: IOStateArrow s String XmlTree
readDoc = readFromDocument [(a_parse_html, v_1)] -- >>> propagateNamespaces

-- | lift a IOState Monad in a IOState Arrow
arrM :: (t -> StateT s EnvController a) -> IOSLA (XIOState (RequestEnv, s)) t a
arrM f = proc a -> do
  (env,state) <- getUserState -< ()
  (b,state') <- arrIO (\(a,(e,s)) -> runReaderT (runController (runStateT (f a) s)) e) -< (a, (env,state))
  setUserState -< (env,state')
  returnA -< b


invokeController :: (XmlTree -> StateController [XmlTree]) -> HawkArrow XmlTree (Either Response [XmlTree])
invokeController c = arrM $ runEitherT . c

-- --------------------------------------------------------------------------
-- Head merge
-- --------------------------------------------------------------------------

-- | merge all head Elements in a XmlTree
-- TODO only to the first head, need to delete the others
headMerge :: ArrowXml a => a XmlTree XmlTree
headMerge = headList &&& removeHeadsFromBody
    >>>
    applyA (arr (\(heads,tree) -> constA tree >>> insertHeads heads))

-- | get the content of all head Elements
headList ::  ArrowXml a => a XmlTree [XmlTree]
headList = listA (heads >>> getChildren)
    where
    heads = deep $ isElem >>> hasName "head"

removeHeadsFromBody :: ArrowXml a => a XmlTree XmlTree
removeHeadsFromBody = processTopDown
    $ removeHeads `when` isBody
    where
    isBody = isElem >>> hasName "body"

removeHeads :: ArrowXml a => a XmlTree XmlTree
removeHeads = processTopDown
    $ none `when` isHead
    where
    isHead = isElem >>> hasName "head"

insertHeads :: ArrowXml a => [XmlTree] -> a XmlTree XmlTree
insertHeads h = processTopDown
        $ mkelem "head" [] (map constA h) `when` isHead
        where
        isHead = isElem >>> hasName "head"

-- --------------------------------------------------------------------------
-- Binding
-- --------------------------------------------------------------------------

type BindTree = E.EitherMapTree String [XmlTree]

bindTyped :: XmlTree
    -> BindTree
    -> [(String, [(String, String)])]
    -> [XmlTree]
bindTyped x t _ = runLA (bindTypedA t) x

bindTypedA :: (ArrowChoice a, ArrowXml a)  => BindTree -> a XmlTree XmlTree
bindTypedA t = processTopDown (bindNode `when` (isBind `orElse` isEmbed))
    where
    bindNode = applyA (getBindName >>> arr (helper t))
    helper :: (ArrowChoice a, ArrowXml a) => BindTree -> String -> a XmlTree XmlTree
    helper t' n = case E.lookup n t' of
        Just v -> case v of
            Left  t'' -> applyA (constA t'' >>> unlistA >>> arr bindTypedA)
            Right x  -> constA x >>> unlistA
        Nothing -> this

isBind :: ArrowXml a => a XmlTree XmlTree
isBind = isElem >>> hasQName (hawkQName "bind")

getBindName :: ArrowXml a => a XmlTree String
getBindName = choiceA [isBind :-> getAttrValue "name"
                  ,isEmbed :-> getAttrValue "what"
                  ]
                  
-- Arrow version of bind
bindA :: ArrowXml cat => cat ([(String, [XmlTree])], XmlTree) XmlTree
bindA = applyA $ arr $ \(m, tree) -> constA tree >>> bind m

-- |Inserts the values of a list of (name, value) pairs into a template at the
--  positions defined by bind-Tags.
bind :: (ArrowXml cat) => [(String, [XmlTree])] -> cat XmlTree XmlTree
bind m = processTopDown (replaceFrom m `when` isBind')
  where
  isBind' = isElem >>> hasQName (hawkQName "bind") >>> hasAttrValue "name" (flip elem keys)
  keys = map fst m

lookupA :: Arrow a  => a (String, [(String, [XmlTree])]) [XmlTree]
lookupA = arr $ fromMaybe [] . uncurry lookup

replaceFrom :: (ArrowXml cat) => [(String, [XmlTree])] -> cat XmlTree XmlTree
replaceFrom m = proc t -> do
  name       <- getAttrValue "name" -< t
  unlistA <<< lookupA               -< (name, m)

bindAttribute :: ArrowXml cat => [(String, [XmlTree])] -> cat XmlTree XmlTree
bindAttribute m = processTopDown $ replaceAttribute m `when` hasBindAttr
    where
    hasBindAttr = isElem >>> getQAttrValue0 (hawkQName "bind")

replaceAttribute :: ArrowXml cat => [(String, [XmlTree])] -> cat XmlTree XmlTree
replaceAttribute m = addAttrl (getQAttrValue0 (hawkQName "bind") &&& constA m >>> lookupA >>> unlistA)
    >>>
    removeQAttr (hawkQName "bind")

-- --------------------------------------------------------------------------
-- clearNamespace
-- --------------------------------------------------------------------------

-- | Remove the hawk namespace
clearNamespace :: (ArrowXml a) => a XmlTree XmlTree
clearNamespace = processTopDown (removeAttr ("xmlns:" ++ hawkPrefix) `when` isHtml) -- TODO do not use hawk prefix, use full Namespace
    where isHtml = isElem >>> hasName "html"
