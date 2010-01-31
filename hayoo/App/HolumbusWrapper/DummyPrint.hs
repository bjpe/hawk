module App.HolumbusWrapper.DummyPrint where

import App.HolumbusWrapper.Types

import Holumbus.Query.Result

-- this module is specialized to hayoo, cause its based on the result format/structure

printResult :: Result FunctionInfo -> String
printResult = undefined

-- upper center
printCloud :: Result FunctionInfo -> String
printCloud = undefined

-- main part
printResultList :: Result FunctionInfo -> String
printResultList = undefined

-- to see in the upper right corner
printModuleList :: Result FunctionInfo -> String
printModuleList = undefined

numOfResults :: Result FunctionInfo -> Int
numOfResults = undefined
