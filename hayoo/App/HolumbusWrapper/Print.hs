module App.HolumbusWrapper.Print where

import Holumbus.Query.Result
import Holumbus.Index.Common

import qualified Hawk.View as H

import App.HolumbusWrapper.Types

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as B

formatCloud :: Result FunctionInfo -> H.XmlTrees
formatCloud r = let max = maxScoreWordHits r 
                in cloud max $ toSortedScoreList $ wordHits r

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
formatPages r i q = let n = flip div 10 $ sizeDocHits r
                        cur = div i 10
                        s = pageStart n cur
                        e = pageEnd n s -- pages end
                    in if (cur == 0)
                      then pages s e cur n ("index/search?q=" ++ q ++ "&o=") q 0
                      else pages s e cur n ("index/search?q=" ++ q ++ "&o=") q (-1)

formatPM :: Result FunctionInfo -> H.XmlTrees
formatPM r = [H.contentTag "div" [("id","modules")] 
               (
                 (H.contentTag "div" [("class","headline")] [H.text "Top 15 Modules"])
                 : printPMList (L.take 15 modules) "Module"
                 ++ ((H.contentTag "div" [("class","headline")] [H.text "Top 15 Packages"])
                 : printPMList (L.take 15 packages) "Package"
                 )
               )
              ]
         where modules = toCountedList $ toModuleList $ getDocuments r
               packages = toCountedList $ toPackageList $ getDocuments r
-- -----------------------------------------------------------------------------
-- LOCALS
-- -----------------------------------------------------------------------------
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

cloud :: Float -> [(Word,Score)] -> H.XmlTrees
cloud m [] = []
cloud m ((w,s):xs) = (H.contentTag "span" [("class","clouds")] cloudLink) : (H.text " ") : cloud m xs
     where cloudLink = [H.contentTag "a" [("class","cloud"++cloudScore)] [H.text (w ++ (show s))]]
           cloudScore | m < 0.1 = show 3 -- min value
                      | otherwise = show $ round (9 - ((m - s) / m) * 8) -- max - ((maxScore - curScore) / maxScore) * (max - min)

toSortedScoreList :: WordHits -> [(Word, Score)]
toSortedScoreList wh = --L.sort $ clrWhitespace $ fst $ L.unzip $ (M.toList wh)
                toScoreList $ M.toList wh
                where toScoreList [] = []
                      toScoreList ((w,x):xs) 
                           | hasWS w = toScoreList xs
                           | otherwise = (w,(wordScore (fst x))) : toScoreList xs
                        where hasWS [] = False
                              hasWS (' ':xs) = True
                              hasWS ('>':xs) = True
                              hasWS (_:xs) = hasWS xs

-- | Number of pages; current offset
pageStart :: Int -> Int -> Int
pageStart n i
  | i <= 5 = 0
  | otherwise = i-5

pageEnd :: Int -> Int -> Int
pageEnd n m
  | (m+10) >= n = n
  | otherwise = m + 10

pages :: Int -> Int -> Int -> Int -> String -> String-> Int -> H.XmlTrees
pages s e i m t q n 
  | n<0 = (H.contentTag "a" (attrs (i-1)) [H.text "<"]) : pages s e i m t q (n+1)
  | s+n > e = if (i<m) then [H.contentTag "a" (attrs (i+1)) [H.text ">"]] else []
  | s+n == i = (H.contentTag "span" [("class","current")] [H.text (show i)]) : pages s e i m t q (n+1)
  | otherwise = (H.contentTag "a" (attrs (s+n)) [H.text (show (s+n))]) : pages s e i m t q (n+1)
  where attrs i = [("href",t ++ (toPN i)), ("class","page"), ("onclick","return processQuery(\""++q++"\","++(toPN i)++")")]
        toPN i = (show i) ++ "0"

printPMList :: [(Int, String)] -> String -> H.XmlTrees
printPMList [] n = []
printPMList (x:xs) n = (H.contentTag "div" [("class","root"++n)] (content x)) : printPMList xs n
   where content (i,s) = [ H.contentTag "a" [("class","root"++n++"Name")] [H.text s]
                         , H.contentTag "span" [("class","root"++n++"Count")] [H.text (show i)]]

toCountedList :: [String] -> [(Int, String)]
toCountedList [] = []
toCountedList l = L.sort $ accum l []
  where -- [Sting] -> [(Int, String)] -> [(Int, String)]
    accum [] a = a
    accum (x:xs) a
        | preLook x a = accum xs incr
        | otherwise = accum xs ((1,x):a)-- x does not exist in a, add it
       where preLook s [] = False
             preLook s ((_,as):ass) = if as==s then True else False  -- = maybe False (\_ -> True) (look x a)
             incr = map (\(i,s) -> if s==x then (i+1,s) else (i,s)) a

toPackageList :: [Document FunctionInfo] -> [String]
toPackageList [] = []
toPackageList (x:xs) = case custom x of
                         Nothing -> toPackageList xs
                         Just v -> B.toString (package v) : toPackageList xs

toModuleList :: [Document FunctionInfo] -> [String]
toModuleList [] = []
toModuleList (x:xs) = case custom x of
                        Nothing -> toModuleList xs
                        Just v -> getRootModule (B.toString (moduleName v)) : toModuleList xs

getRootModule :: String -> String
getRootModule l = fst $ break (=='.') l

