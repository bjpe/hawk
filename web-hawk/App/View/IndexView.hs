{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType
import Hawk.View.Template.HtmlHelper

import App.View.Common (titleT, mainLCR, mainLC)
import App.View.Home (homeL, homeC, homeR)
import App.View.Start (startL, startC)
import App.View.Download (downloadL, downloadC)
import App.View.API (apiL, apiC)
import App.View.FAQ (faqL, faqC)

$(viewDataType "Index" "index")

indexXhtml :: a -> StateController IndexIndex
indexXhtml _ = return IndexIndex
    { title = [text titleT]
    ,  main = mainLCR homeL homeC homeR
    }

startXhtml :: a -> StateController IndexIndex
startXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - Getting Started")]
    ,  main = mainLC startL startC
    }

downloadXhtml :: a -> StateController IndexIndex
downloadXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - Download")]
    ,  main = mainLC downloadL downloadC
    }

apiXhtml :: a -> StateController IndexIndex
apiXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - API Documentation")]
    ,  main = mainLC apiL apiC
    }

faqXhtml :: a -> StateController IndexIndex
faqXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - FAQ")]
    ,  main = mainLC faqL faqC
    }

