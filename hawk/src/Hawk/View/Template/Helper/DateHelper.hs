-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Helper functions for creating xml tags for date and time input.
-}
-- --------------------------------------------------------------------------
module Hawk.View.Template.Helper.DateHelper
  ( -- date selection
    selectYear
  , selectYearS
  , selectMonth
  , monthNumbers
  , monthNames
  , selectDay
  , selectDate

  -- time selection
  , selectHour
  , selectMinute
  , selectSecond
  , selectTime

  -- datetime selection
  , selectUTCDateTime
  , selectLocalDateTime
  ) where

import Hawk.View.Template.Helper.TagHelper
import Hawk.View.Template.Helper.FormHelper
import Data.Time

-- --------------------------------------------------------------------------
-- Date Selection
-- --------------------------------------------------------------------------

-- | Create a select combobox for year selection
selectYear :: String -> Integer -> Integer -> Integer -> Attributes -> XmlTree
selectYear name current start end attrs = select name opts attrs
  where
    years = if start <= end then [start..end] else reverse [end..start]
    opts  = optionsWithSelected show show current years

selectYearS :: String -> Integer -> Attributes -> XmlTree
selectYearS name current = selectYear name current (current - 5) (current + 5)

selectMonth ::String -> Int -> (Int -> String) -> Attributes -> XmlTree
selectMonth name current format attrs = select name opts attrs
  where opts = optionsWithSelected show format current [1..12]

monthNumbers :: Int -> String
monthNumbers = show

monthNames :: Int -> String
monthNames i = [ "January"  , "February", "March"   , "April"
               , "May"      , "June"    , "July"    , "August"
               , "September", "October" , "November", "December"
               ] !! (i-1)

selectDay :: String -> Int -> Attributes -> XmlTree
selectDay name current attrs = select name opts attrs
  where opts = optionsWithSelected show show current [1..31]

selectDate :: String -> Day -> (Int -> String) -> Attributes -> XmlTrees
selectDate name day f attrs
  = [ selectDay   (name ++ ".day"  ) d   attrs
    , selectMonth (name ++ ".month") m f attrs
    , selectYearS (name ++ ".year" ) y   attrs
    ]
  where (y,m,d) = toGregorian day

-- --------------------------------------------------------------------------
-- Time Selection
-- --------------------------------------------------------------------------

selectHour :: String -> Int -> Attributes -> XmlTree
selectHour name currentHour attrs = select name opts attrs
  where opts = optionsWithSelected leadingZero leadingZero currentHour [0..23]


selectMinute :: String -> Int -> Attributes -> XmlTree
selectMinute name currentMinute attrs = select name opts attrs
  where opts = optionsWithSelected leadingZero leadingZero currentMinute [0..59]


selectSecond :: String -> Int -> Attributes -> XmlTree
selectSecond name currentSecond attrs = select name opts attrs
  where opts = optionsWithSelected leadingZero leadingZero currentSecond [0..59]

leadingZero :: Int -> String
leadingZero n = if n <= 9 then '0':show n else show n


selectTime :: String -> TimeOfDay -> Attributes -> XmlTrees
selectTime name now attrs
  = [ selectHour   (name ++ ".hour"  ) h attrs
    , selectMinute (name ++ ".minute")   m attrs
    , selectSecond (name ++ ".second") (truncate s) attrs
    ]
  where (TimeOfDay h m s) = now


selectUTCDateTime :: String -> String -> UTCTime -> (Int -> String) -> Attributes -> XmlTrees
selectUTCDateTime name sep now monthFormat attrs
  =  selectDate name (utctDay now) monthFormat attrs
  ++ [mkText sep]
  ++ selectTime name (timeToTimeOfDay $ utctDayTime now) attrs


selectLocalDateTime :: String -> String -> LocalTime -> (Int -> String) -> Attributes -> XmlTrees
selectLocalDateTime name sep now monthFormat attrs
  =  selectDate name (localDay now) monthFormat attrs
  ++ [mkText sep]
  ++ selectTime name (localTimeOfDay now) attrs

