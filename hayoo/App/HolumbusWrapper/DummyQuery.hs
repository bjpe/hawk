module App.HolumbusWrapper.DummyQuery where

import Holumbus.Query.Result
import qualified Holumbus.Index.SmallDocuments as SD
import Holumbus.Index.Common

import App.HolumbusWrapper.Types

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.ByteString as B

-- this dummy ignores settings
procQuery :: QueryInfo -> Result FunctionInfo
procQuery _q = do
  let f = mkFuncInfo
  mkRes f

mkRes :: FunctionInfo -> Result FunctionInfo
mkRes f = Result
  { docHits = IM.singleton 0 (
    DocInfo
    { document = (
      Document
      { title  = "dummy"
      , uri    = "no uri"
      , custom = Just f
      })
    , docScore = 0.9
    }
    ,
    M.singleton "" (M.singleton "dummy" (IS.singleton 0))) 
  , wordHits = M.singleton "dummy" (
    WordInfo 
    { terms = ["dummy"] -- TODO ??
    , wordScore = 0.9
    }
    , M.singleton "" (IM.singleton 0 (IS.singleton 0)))
  }

mkFuncInfo :: FunctionInfo
mkFuncInfo = FunctionInfo 
    { moduleName = B.pack [65,65,65]
    , signature  = B.pack [66,66,66]
    , package    = B.pack [67,67,67]
    , sourceURI  = Nothing
    }
