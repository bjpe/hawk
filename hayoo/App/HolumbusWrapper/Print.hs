module App.HolumbusWrapper.Print where

import Holumbus.Query.Result
import Holumbus.Index.Common

import qualified Hawk.View as H

import App.HolumbusWrapper.Types

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as B
import qualified Data.IntMap as IM

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)

formatCloud :: Result FunctionInfo -> H.XmlTrees
formatCloud r = let max = maxScoreWordHits r 
                in cloud max $ toSortedScoreList $ wordHits r

{-formatList :: Result FunctionInfo -> H.XmlTrees
formatList r = toDivList (getDocuments r)-}

formatOffsetList :: HolCache c => Result FunctionInfo -> Int -> c -> H.XmlTrees
formatOffsetList r n c = toDivList (offset (sortedList (docHits r)))
  where offset l = take 10 $ drop n l
        toDivList = map (formatDocument c)
        sortedList = IM.toList -- TODO sort it

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
formatPM r = [xDivId "modules" 
               (
                 (xDivClass "headline" [H.text "Top 15 Modules"])
                 : printPMList (L.take 15 modules) "Module"
                 ++ ((xDivClass "headline" [H.text "Top 15 Packages"])
                 : printPMList (L.take 15 packages) "Package"
                 )
               )
              ]
         where modules = toCountedList $ toModuleList $ getDocuments r
               packages = toCountedList $ toPackageList $ getDocuments r
-- -----------------------------------------------------------------------------
-- LOCALS
-- -----------------------------------------------------------------------------
formatDocument :: HolCache c => c -> (DocId, (DocInfo FunctionInfo, DocContextHits)) -> H.XmlTree
formatDocument c (i, (DocInfo d _, _)) = 
  case custom d of
    Nothing -> xDiv [H.link (uri d) [H.text (title d)]]
    Just f  -> xDiv [ xDivClass "function" [getModule, getFunction, getSignature]
                    , xDivClass "details"  [getPackage, getDescription]
                    ]
        where 
        getPackage = xSpanClass "package" [xLinkClass (uri d) "package" [H.text (B.toString (package f))]]
        getModule = xSpanClass "module" [xLinkClass (uri d) "module" [H.text ((B.toString (moduleName f)) ++ ".")]]
        getFunction = xSpanClass "function" [xLinkClass (uri d) "function" [H.text (title d)]]
        getSignature = xSpanClass "signature" [H.text (B.toString (signature f))]
        getDescription = xSpanClass "description" [maybe (H.text "No Description") (\s -> H.text s) (getDescr i)]
          where getDescr = unsafePerformIO . getDocText c "description"

cloud :: Float -> [(Word,Score)] -> H.XmlTrees
cloud m [] = []
cloud m ((w,s):xs) = (xSpanClass "clouds" cloudLink) : (H.text " ") : cloud m xs
     where cloudLink = [H.contentTag "a" [("class","cloud"++cloudScore)] [H.text w]] -- (w ++ (show s))
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
                              hasWS ('-':'>':xs) = True
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
  | m == 0 = [H.text ""]
  | n<0 = (H.contentTag "a" (attrs (i-1)) [H.text "<"]) : pages s e i m t q (n+1)
  | s+n > e = if (i<m) then [H.contentTag "a" (attrs (i+1)) [H.text ">"]] else []
  | s+n == i = (xSpanClass "current" [H.text (show i)]) : pages s e i m t q (n+1)
  | otherwise = (H.contentTag "a" (attrs (s+n)) [H.text (show (s+n))]) : pages s e i m t q (n+1)
  where attrs i = [("href",t ++ (toPN i)), ("class","page"), ("onclick","return processQuery(\""++q++"\","++(toPN i)++")")]
        toPN i = (show i) ++ "0"

printPMList :: [(Int, String)] -> String -> H.XmlTrees
printPMList [] n = []
printPMList (x:xs) n = (xDivClass ("root"++n) (content x)) : printPMList xs n
   where content (i,s) = [ xLinkClass "" ("root"++n++"Name") [H.text s]
                         , xSpanClass ("root"++n++"Count") [H.text (' ':(show i))]]

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


xDiv :: H.XmlTrees -> H.XmlTree
xDiv = H.contentTag "div" []
xDivClass :: String -> H.XmlTrees -> H.XmlTree
xDivClass s = H.contentTag "div" [("class",s)]
xDivId :: String -> H.XmlTrees -> H.XmlTree
xDivId s = H.contentTag "div" [("id",s)]
xSpan :: H.XmlTrees -> H.XmlTree
xSpan = H.contentTag "span" []
xSpanClass :: String -> H.XmlTrees -> H.XmlTree
xSpanClass s = H.contentTag "span" [("class",s)]
xLinkClass :: String -> String -> H.XmlTrees -> H.XmlTree
xLinkClass uri c = H.contentTag "a" [("href",uri),("class",c)]
