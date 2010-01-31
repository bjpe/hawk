module Hawk.Model.CriteriaSelect where

import Hawk.Model.Criteria
import Hawk.Model.MonadDB
import Hawk.Model.Types

-- | Select a list of 'SqlValue's by a 'Query'
querySelect :: MonadDB m => Query -> m [[SqlValue]]
querySelect = uncurry sqlSelect . toExprPair

executeManipulation :: MonadDB m => Manipulation -> m Integer
executeManipulation = uncurry sqlExecute . toExprPair
