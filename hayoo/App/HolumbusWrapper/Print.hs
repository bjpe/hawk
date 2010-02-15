module App.HolumbusWrapper.Print where

import Holumbus.Query.Result
import Holumbus.Index.Common

import qualified Hawk.View as H

import App.HolumbusWrapper.Types

import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B

formatCloud :: Result FunctionInfo -> H.XmlTrees
formatCloud r = [H.text "here will be the function name cloud"]--[H.showtext $ getDocuments r]

formatList :: Result FunctionInfo -> H.XmlTrees
formatList r = toDivList (getDocuments r)

formatOffsetList :: Result FunctionInfo -> Int -> H.XmlTrees
formatOffsetList r n = toDivList ( take 10 (drop n (getDocuments r)))

formatStatus :: Result FunctionInfo -> H.XmlTrees
formatStatus r = let wHits = sizeWordHits r
                 in case wHits of
                   0 -> [H.text "No entry found."]
                   1 -> [H.text ("Found " ++ show (sizeDocHits r) ++ " entries.")]
                   _ -> [H.text ("Found " ++ show (sizeDocHits r) ++ " entries and " ++ show wHits ++ " completions.")]

formatPages :: Result FunctionInfo -> Int -> H.XmlTrees
formatPages r i = [H.text ("pages " ++ (show i))]

formatPM :: Result FunctionInfo -> H.XmlTrees
formatPM r = [H.text "packages and modules"]

toDivList :: [Document FunctionInfo] -> H.XmlTrees
toDivList []     = [H.tag "div" []]
toDivList (x:[]) = [H.contentTag "div" [] (formatDocument x)]
toDivList (x:xs) = (H.contentTag "div" [] (formatDocument x)) : toDivList xs

formatDocument :: Document FunctionInfo -> H.XmlTrees
formatDocument d = case custom d of
                     Nothing -> [H.link (uri d) [H.text (title d)]]
                     Just f  -> [ H.link (uri d) [H.text (B.toString (package f))]
                                , H.text " @ "
                                , H.link (uri d) [H.text ((B.toString (moduleName f)) ++ ".")]
                                , H.link (uri d) [H.text (title d)]
                                , H.text " :: "
                                , H.text (B.toString (signature f))
                                ]
