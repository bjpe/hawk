{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hawk.View.TemplateView
  ( TemplateView (..)
  , XmlTree
  , bind
  , xhtml
  , xhtmlTypedView
  , xhtmlTemplateView
  , subTemplate
  , getAttribute
  , lookupAttribute
  , typedView
  , typedViewWithAttributes
  ) where

import Hawk.Controller.Types
    (StateController, Route(Route), View(..), HasState
    , templatePath, configuration, request, Routing)
import qualified Hawk.View.Template.Interpreter as Interpreter
import Hawk.View.Template.DataType
import Hawk.Controller.Util.Text
import Hawk.Controller.Util.Uri
import Hawk.Controller.Routes
import qualified Text.XML.HXT.DOM.XmlNode as XN

import Hack
import Text.XML.HXT.Arrow
import System.FilePath (joinPath)
import System.Directory (doesFileExist)
import Data.ByteString.Lazy (empty)
import Control.Monad.Either
import Control.Monad.Reader (asks)
import Hawk.View.Template.Interpreter (hawkPrefix)

data TemplateView a = TemplateView
  { templateName :: String
  , toXhtml      :: XmlTree -> a -> StateController [XmlTree]
  }

instance View (TemplateView a) where
  type Target (TemplateView a) = a
  render a b = do
    (Route c _ ) <- asks $ dispatch . unescapeUri . pathInfo . request
    tp <- findTemplateM c $ templateName a
    case tp of
      Nothing -> return empty
      Just f  -> Interpreter.evalTemplate (flip (toXhtml a) b) f

typedView :: Bindable b => String
    -> (a -> StateController b)
    -> TemplateView a
typedView n t = TemplateView
  { templateName = n
  , toXhtml      = \x a -> do
      t' <- t a
      return $ Interpreter.bindTyped x (bindable t') []
  }

typedViewWithAttributes :: Bindable b => String
    -> (a -> StateController (b, [(String, [(String, String)])]))
    -> TemplateView a
typedViewWithAttributes n t = TemplateView
  { templateName = n
  , toXhtml      = \x a -> do
      (t', l) <- t a
      return $ Interpreter.bindTyped x (bindable t') l
  }

xhtml :: (View a) => String -> StateController (Target a) -> a -> [Routing]
xhtml s a v = [(s,c),(s++".xhtml",c)]
  where c = a >>= render v

xhtmlTypedView :: (Bindable b, View (TemplateView a)) =>
                String -> StateController a -> (a -> StateController b) -> [Routing]
xhtmlTypedView = xhtmlView typedView

xhtmlTemplateView
  :: (View (TemplateView a)) =>  String
     -> StateController a
     -> (XmlTree -> a -> StateController [XmlTree])
     -> [Routing]
xhtmlTemplateView = xhtmlView TemplateView

xhtmlView :: (View (TemplateView a)) => (String -> c -> TemplateView a) -> String -> StateController a -> c -> [Routing]
xhtmlView f s a = xhtml s a . f s

bind :: XmlTree -- The Template
    -> [(String, [XmlTree])] -- The Pairs of bind name and the inserting XML
    -> [(String, [(String, String)])] -- The Pairs for the Attribute bindings
    -> [XmlTree]
bind t m a = runLA (Interpreter.bind m >>> Interpreter.bindAttribute a' >>> getChildren) t
  where
  a' = map (second (map (\(k,v) -> XN.mkAttr (mkName k) $ (:[]) $ XN.mkText v))) a

subTemplate :: XmlTree -> String -> [XmlTree]
subTemplate tree name = runLA (deep arrow) tree
  where arrow = isElem
                >>> hasNamePrefix hawkPrefix
                >>> hasLocalPart "bind"
                >>> hasAttrValue "name" (== name)

getAttribute :: XmlTree -> String -> String
getAttribute t a = head $ runLA (getAttrValue a) t

lookupAttribute :: XmlTree -> String -> Maybe String
lookupAttribute t a = case runLA (getAttrValue0 a) t of
  []    -> Nothing
  (v:_) -> Just v

findTemplateM :: (MonadIO m, HasState m) => String -> String -> m (Maybe FilePath)
findTemplateM c s = do
  tp <- asks $ templatePath . configuration
  let fullPath = joinPath [tp, firstUpper c, s ++ ".xhtml"]
  fileExists <- liftIO $ doesFileExist fullPath
  if fileExists then
    return $ Just fullPath
    else return Nothing

