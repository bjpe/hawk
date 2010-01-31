module App.HolumbusWrapper.Print where

import Holumbus.Query.Result
import Holumbus.Index.Common

import qualified Hawk.View as H

import App.HolumbusWrapper.Types

import qualified Data.Map as M

formatCloud :: Result FunctionInfo -> H.XmlTrees
formatCloud r = [H.showtext $ getDocuments r]

formatList :: Result FunctionInfo -> H.XmlTrees
formatList r = toDivList (getDocuments r)

formatOffsetList :: Result FunctionInfo -> Int -> H.XmlTrees
formatOffsetList r n = toDivList ( drop n (getDocuments r))

formatStatus :: Result FunctionInfo -> String -> H.XmlTrees
formatStatus r q = [H.text ("Found " ++ show (sizeDocHits r) ++ " entries for search term \"" ++ q ++"\".")]

toDivList :: [Document FunctionInfo] -> H.XmlTrees
toDivList []     = [H.tag "div" []]
toDivList (x:[]) = [H.contentTag "div" [] [(H.showtext x)]]
toDivList (x:xs) = (H.contentTag "div" [] [(H.showtext x)]) : toDivList xs
