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

formatPages :: Result FunctionInfo -> Int -> String -> H.XmlTrees
formatPages r i q = let num = (sizeDocHits r)
                        n = div num 10
                        m = rem num 10
                        cur = div i 10
                        s = pageStart n cur
                        e = pageEnd n s -- pages end
                    in pages s e cur ("index/search?q=" ++ q ++ "&o=") q 0

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
-- | Number of pages; current offset
pageStart :: Int -> Int -> Int
pageStart n i
  | i <= 5 = 0
  | otherwise = i-5

pageEnd :: Int -> Int -> Int
pageEnd n m
  | (m+10) >= n = n
  | otherwise = m + 10

pages :: Int -> Int -> Int -> String -> String-> Int -> H.XmlTrees
pages s e i t q n 
  | s+n > e = [H.contentTag "a" (attrs (i+1)) [H.text ">"]]
  | s+n == i = (H.contentTag "span" [("class","current")] [H.text (show i)]) : pages s e i t q (n+1)
  | otherwise = (H.contentTag "a" (attrs (s+n)) [H.text (show (s+n))]) : pages s e i t q (n+1)
  where attrs i = [("href",t ++ (toPN i)), ("class","page"), ("onclick","return processQuery(\""++q++"\","++(show i)++")")]
        toPN i = (show i) ++ "0"
        
