module Hawk.Controller.ConfigParser where

import Hawk.Controller.Types

import Text.XML.HXT.Arrow

-- | Creates a BasicConfiguration Type
parseBasic :: String -- ^ Path to configuration file, relative to $projectDir/Config
           -> BasicConfiguration
parseBasic 

-- | Creates an AppEnvironment Type
--parseAppEnv :: String -- ^ Path to configuration file, relative to $projectDir/Config
--            -> AppConfiguration

readConfigDoc = IOStateArrow BasicConfiguration XmlTree XmlTree
readConfigDoc = parseXmlDocument True . readDocument [(a_remove_whitespace, "1")]
