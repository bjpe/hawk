module App.HolumbusWrapper.HolumbusWrapper where

-- holumbus imports
import Holumbus.Query.Result
import Holumbus.Index.SmallDocuments

import Hawk.Controller
import Hawk.View

-- common wrapper modules
import App.HolumbusWrapper.Types

-- concrete implementations only use dummy or the real one
--import App.HolumbusWrapper.DummyQuery (procQuery)
--import App.HolumbusWrapper.DummyPrint (printResult)
import qualified App.HolumbusWrapper.Query as Q
import qualified App.HolumbusWrapper.Print as P

--import Hawk.Model

-- function to send a query
-- change FunctionInfo to your result type
query :: QueryInfo -> Result FunctionInfo
query = Q.query

-- function to format a query for output as html
formatCloud :: Result FunctionInfo -> XmlTrees
formatCloud = P.formatCloud

formatList :: Result FunctionInfo -> XmlTrees
formatList = P.formatList

formatOffsetList :: Result FunctionInfo -> Int -> XmlTrees
formatOffsetList = P.formatOffsetList

formatStatus :: Result FunctionInfo -> XmlTrees
formatStatus = P.formatStatus

-- extra function for json output? - or parameter for the printing module

-- constructor for query configuration type
getQuerySettings :: QuerySettings
getQuerySettings = NoSettings
-- getQuerySettingsFromUser

-- getQuerySettingsFromSession

-- getQuerySettingsFromRequest

-- more specialized settings overwrite more common ones 
-- i.e. session settings overwrite user settings and 
-- request settings overwrite session settings.
-- no settings mean no restrinction, 
-- all data processed by the request will be returned
