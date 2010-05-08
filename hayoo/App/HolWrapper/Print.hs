-- |
-- / (The whole module needs to be customized)
module App.HolWrapper.Print
  ( formatCloud
  , formatOffsetList
  , formatStatus
  , formatPages
  , formatPM
  , formatApiFunctions
  , formatApiCompletitions
  , formatApiModules
  , formatApiPackages
  ) where

import Holumbus.Index.Common
import Holumbus.Query.Result

import qualified Hawk.View as H
import Hawk.Controller.Util.Text (splitWhere)

import App.HolWrapper.Types
import App.HolWrapper.Common
import App.HolWrapper.Parser (replace)
import App.View.HtmlCommon

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as B
import qualified Data.IntMap as IM

import Text.XML.HXT.DOM.Util (stringToLower)

import System.IO.Unsafe (unsafePerformIO)

formatCloud :: ResultTuple -> H.XmlTrees
formatCloud (r, _) = let ms = maxScoreWordHits r 
                in cloud ms $ toSortedScoreList $ wordHits r

formatOffsetList :: {-HolCache c => -}ResultTuple -> H.XmlTrees -- Result FunctionInfo -> Int -> c -> H.XmlTrees
formatOffsetList (r, i) = toDivList (offsetL (sortedList (docHits r)))
  where offsetL l = take 10 $ drop (offset (querySettings i)) l
        toDivList l = [H.contentTag "table" [] (L.concat $ map (formatDocument' $ cache i) l)]
        -- toDivList = map (formatDocument $ cache i)
        sortedList d = reverse $ L.sortBy sortF (IM.toList d)

sortF :: (DocId, (DocInfo FunctionInfo, DocContextHits)) -> (DocId, (DocInfo FunctionInfo, DocContextHits)) -> Ordering
sortF x y = compare (st x) (st y)
  where st = docScore . fst . snd

formatStatus :: ResultTuple -> H.XmlTrees
formatStatus (r, _) = let wHits = sizeWordHits r
                 in case wHits of
                   0 -> [H.text "No entry found."]
                   1 -> [H.text ("Found " ++ show (sizeDocHits r) ++ " entries.")]
                   _ -> [H.text ("Found " ++ show (sizeDocHits r) ++ " entries and " ++ show wHits ++ " completions.")]

formatPages :: ResultTuple -> H.XmlTrees
formatPages (r, i) = let n = flip div 10 $ sizeDocHits r
                         cur = div ((offset . querySettings) i) 10
                         s = pageStart n cur
                         e = pageEnd n s -- pages end
                         q = searchString $ querySettings i
                    in if (cur == 0)
                      then pages s e cur n ("index/search?q=" ++ q ++ "&o=") q 0
                      else pages s e cur n ("index/search?q=" ++ q ++ "&o=") q (-1)

formatPM :: ResultTuple -> H.XmlTrees
formatPM (r, i) = [divId "modules" 
               (
                 (divClass "headline" [H.text "Top 15 Modules"])
                 : (printPMList (L.take 15 $ moduleL r) "Module" $ searchString $ querySettings i)
                 ++ ((divClass "headline" [H.text "Top 15 Packages"])
                 : (printPMList (L.take 15 $ packageL r) "Package" $ searchString $ querySettings i)
                 )
               )
              ]

formatApiFunctions :: ResultTuple -> H.JSON
formatApiFunctions (r, i) = H.jArray $ map docs $ IM.toList $ docHits r
  where docs (_, (DocInfo (Document t u (Just (FunctionInfo m s p _))) _, _)) = 
            H.jObject [ ("name", H.jString t)
                      , ("uri", H.jString u)
                      , ("module", H.jString $ B.toString m)
                      , ("signature", H.jString $ B.toString s)
                      , ("package", H.jString $ B.toString p)
                      ]
        docs _ = H.jObject []

formatApiCompletitions :: ResultTuple -> H.JSON
formatApiCompletitions (r, i) = H.jArray $ map words $ M.toList $ wordHits r
  where words (w, (WordInfo _ s, _)) = H.jObject [ ("word", H.jString w)
                                                 , ("count", H.jFloat s)] 

formatApiModules :: ResultTuple -> H.JSON
formatApiModules (r, i) = H.jArray $ map toPL $ moduleL r

formatApiPackages :: ResultTuple -> H.JSON
formatApiPackages (r, i) = H.jArray $ map toPL $ packageL r
  
-- -----------------------------------------------------------------------------
-- LOCALS
-- -----------------------------------------------------------------------------
{-formatDocument :: HolCache c => c -> (DocId, (DocInfo FunctionInfo, DocContextHits)) -> H.XmlTree
formatDocument c (i, (DocInfo d _, _)) = 
  case custom d of
    Nothing -> divX [H.link (uri d) [H.text (title d)]]
    Just f  -> divX [ divClass "function" [getModule, getFunction, getSignature]
                    , divClass "details"  [getPackage, getDescription]
                    ]
        where 
        getPackage = spanClass "package" [linkClass (uri d) "package" [H.text (B.toString $ package f)]]
        getModule = spanClass "module" [linkClass (uri d) "module" [H.text ((B.toString (moduleName f)) ++ ".")]]
        getFunction = spanClass "function" [linkClass (uri d) "function" [H.text (title d)]]
        getSignature = spanClass "signature" [H.text (":: " ++ (B.toString (signature f)))]
        getDescription = spanClass "description" [ maybe (H.text "No Description") (\s -> H.text s) (getDescr i)]
          where getDescr = unsafePerformIO . getDocText c "description"
-}
formatDocument' :: HolCache c => c -> (DocId, (DocInfo FunctionInfo, DocContextHits)) -> H.XmlTrees
formatDocument' c (i, (DocInfo d _, _)) = 
  case custom d of
    Nothing -> [H.link (uri d) [H.text (title d)]]
    Just f  -> [ trClass "function" [getModule, getFunction, getSignature]
               , trClass "details"  [getPackage, getDescription]
               ]
        where 
        getPackage = tdClass "package" [linkClass (packLink pkg) "package" [H.text pkg]]
          where pkg = B.toString $ package f
                packLink "gtk2hs" = "http://www.haskell.org/gtk2hs"
                packLink p = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ p
        getModule = tdClass "module" [linkClass (modLink $ uri d) "module" [H.text ((B.toString (moduleName f)) ++ ".")]]
          where modLink = takeWhile (/= '#')
        getFunction = tdClass "function" [linkClass (uri d) "function" [H.text (title d)]]
        getSignature = tdClass "signature" [H.text sig]
          where sig = replace' [("->"," -> "),("[ ", "["),(" ]","]")] $ ":: " ++ (B.toString $ signature f)
        getDescription = H.contentTag "td" [("class", "description"), ("colspan", "2")]
                           [ maybe (H.text "No Description") (\s -> divX [H.text s]) (getDescr i)
                           , linkClass (srcUri (uri d)) "source" [H.text " Source"]]
          where getDescr = unsafePerformIO . getDocText c "description"
                srcUri = (\(a,b) -> (reverse b) ++ "/src/" ++ (delType a)) . splitWhere (== '/') . reverse 
                  where delType = (\(a,b) -> a ++ "#" ++ (snd $ splitWhere (== ':') b)) . splitWhere (== '#') . reverse

replace' :: [(String, String)] -> String -> String
replace' [] s = s
replace' ((x,y):xs) s = replace' xs $ replace x y s

cloud :: Float -> [(Word,Score)] -> H.XmlTrees
cloud _ [] = []
cloud m ((w,s):xs) = (spanClass "clouds" cloudLink) : (H.text " ") : cloud m xs
     where cloudLink = [H.contentTag "a" cAttr [H.text w]] -- (w ++ (show s))
             where cAttr = [ ("class","cloud"++cloudScore)
                           , ("href","/index/search?q=" ++ w)
                           , ("onclick","return processQuery(" ++ w ++ ",0)")]
           cloudScore | m < 0.1 = "3" -- min value
                      | otherwise = show $ round (9 - ((m - s) / m) * 8) -- max - ((maxScore - curScore) / maxScore) * (max - min)

toSortedScoreList :: WordHits -> [(Word, Score)]
toSortedScoreList wh = --L.sort $ clrWhitespace $ fst $ L.unzip $ (M.toList wh)
                toScoreList $ M.toList wh
                where toScoreList [] = []
                      toScoreList ((w,x):xs) 
                           | hasWS w = toScoreList xs
                           | otherwise = (w,(wordScore (fst x))) : toScoreList xs
                        where hasWS [] = False
                              hasWS (' ':_) = True
                              hasWS ('-':'>':_) = True
                              hasWS (_:ys) = hasWS ys

-- | Number of pages; current offset
pageStart :: Int -> Int -> Int
pageStart _n i
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
  | s+n == i = (spanClass "current" [H.text (show i)]) : pages s e i m t q (n+1)
  | otherwise = (H.contentTag "a" (attrs (s+n)) [H.text (show (s+n))]) : pages s e i m t q (n+1)
  where attrs j = [("href",t ++ (toPN j)), ("class","page"), ("onclick","return processQuery(\""++q++"\","++(toPN j)++")")]
        toPN j = (show j) ++ "0"

printPMList :: [(Int, String)] -> String -> String -> H.XmlTrees
printPMList [] _n _q = []
printPMList (x:xs) n q = (divClass ("root"++n) (content x)) : printPMList xs n q
   where content (i,s) = [ H.contentTag "a" 
                            [ ("href", "/index/search?q=" ++ q ++ " " ++ (stringToLower n) ++ ":" ++ s)
                            , ("onclick", "return processQuery(" ++ q ++ " " ++ (stringToLower n) ++ ":" ++ s ++ ",0)")
                            , ("class", "root"++n++"Name")
                            ] [H.text s]
                         , spanClass ("root"++n++"Count") [H.text (' ':(show i))]]

moduleL :: Result FunctionInfo -> [(Int, String)]
moduleL r = toCountedList $ toModuleList $ getDocuments r

packageL :: Result FunctionInfo -> [(Int, String)]
packageL r = toCountedList $ toPackageList $ getDocuments r

toPL :: (Int, String) -> H.JSON
toPL (i,s) = H.jObject [ ("name", H.jString s)
                       , ("count", H.jInt i)]

toCountedList :: [String] -> [(Int, String)]
toCountedList [] = []
toCountedList l = L.sort $ accum l []
  where -- [Sting] -> [(Int, String)] -> [(Int, String)]
    accum [] a = a
    accum (x:xs) a
        | preLook x a = accum xs incr
        | otherwise = accum xs ((1,x):a)-- x does not exist in a, add it
       where preLook _s [] = False
             preLook s ((_,as):_ass) = as==s  -- = maybe False (\_ -> True) (look x a)
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
