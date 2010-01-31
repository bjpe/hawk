module Hawk.Controller.Util.Read where

import Data.Maybe (listToMaybe)

maybeRead :: Read a => String -> Maybe a
maybeRead = listToMaybe . map fst . filter (null . snd) . reads
