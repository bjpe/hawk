module App.View.Download where

import App.View.Common

downloadL :: XmlTrees
downloadL = (lHead "Download")
         ++ (lList [ ("Latest Release", "latest")
                   , ("Release History", "history")
                   , ("Linux", "linux")
                   ] "")
         ++ (lList [ ("Install Haskell", "linuxghc")
                   , ("Install Libraries", "linuxlibs")
                   , ("Install Hawk", "linuxhawk")
                   ] "1")
         ++ (lList [("Mac OSX", "mac")] "")
{-         ++ (lList [ ("Install Haskell", "macghc")
                   , ("Install Libraries", "maclibs")
                   , ("Install Hawk", "machawk")
                   ] "1")-}
         ++ (lList [("Windows", "windows")] "")
{-         ++ (lList [ ("Install Haskell", "winghc")
                   , ("Install Libraries", "winlibs")
                   , ("Install Hawk", "winhawk")
                   ] "1")-}
         ++ (lList [("Test Installation", "test")] "")

downloadC :: XmlTrees
downloadC = [ cHead "Get Haskell Web Application Kit" ""
            , cBody [ text "We recommend using Linux for developing Web Applications with Hawk, because Mac OSX and Windows can cause some problems installing needed development libraries."
                    , linebreak, linebreak
                    , text "The Versions of Hawk, available for download have only Beta status. They are still under developement. There is a lot of work to do, but if you've any inspirations, comments, feedback or you want to join us, please mail us at "
                    , link "mail:mail@hawk-project.org" [text "mail@hawk-project.org"]
                    ]
            , cHead "Latest Release" "latest"
            , cBody [ text "Download the latest release from right "
                    , link "/package/hawk-latest.tar.gz" [text "here"]
                    , text " or an older release from our history section below."
                    , linebreak, linebreak
                    , text "Alternativly you can also get hawk without examples and 'project-creator' from "
                    , linkN "http://hackage.haskell.org/" [text "Hackage"]
                    , text " or get it with "
                    , linkN "http://haskell.org/cabal/" [text "cabal"]
                    , text " by using "
                    , codei "cabal install Hawk"
                    , text " from your console."
                    ]
            , cHead "Release History" "history"
            , cBody [ contentTag "table" [("id","history")]
                       [ contentTag "tr" [] 
                         [ contentTag "th" [] [text "Filename"]
                         , contentTag "th" [] [text "Version"]
                         , contentTag "th" [] [text "Size"]
                         , contentTag "th" [] [text "Download"]
                         ]
                       {-divId "historyhead"
                        [ spanClass "historyName" [text "Filename"]
                        , spanClass "historyVersion" [text "Version"]
                        , spanClass "historySize" [text "Size"]
                        , spanClass "historyLink" [text "Download"]
                        ]-}
                       , history "hawk-latest.tar.gz" "0.0.2" "2.1 MB"
                       ]
                    ]
            , cHead "Install on Linux" "linux"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Haskell" "linuxghc"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Development Libraries" "linuxlibs"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Hawk" "linuxhawk"
            , cBody [ text "if you installed ghc and haskell platform / cabal, you can run 'cabal install hawk'"
                    , text "if you want not to install it directly from hackage, unpack your downloaded file and cd to hawk/hawk/ and run 'cabal install'"
                    , text "it is recommended to have cabal installed, because if not, you will need to install all dependencies on your own"
                    , text "if you got all packages installed and for whatever reason dont want to use cabal, do 'runhaskell Setup.hs configure' 'runhaskell Setup.hs build' 'runhaskell Setup.hs install' from the hawk/hawk/ directory"
                    ]
            , cHead "Install on Mac OSX" "mac"
            , cBody [ text "coming up soon ..."
                    , linebreak, linebreak
                    , text "Please use the instructions from \""
                    , link "#linux" [text "Install on Linux"]
                    , text "\" instead, until this section becomes available."
                    ]
{-            , cHead' "Install Haskell" "macghc"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Development Libraries" "maclibs"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Hawk" "machawk"
            , cBody [ text ""
                    , text ""
                    ]-}
            , cHead "Install on Windows" "windows"
            , cBody [ text "coming up soon ..."
                    , linebreak, linebreak
                    , text "Please use the instructions from \""
                    , link "#linux" [text "Install on Linux"]
                    , text "\" instead, until this section becomes available."
                    ]
{-            , cHead' "Install Haskell" "winghc"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Development Libraries" "winlibs"
            , cBody [ text ""
                    , text ""
                    ]
            , cHead' "Install Hawk" "winhawk"
            , cBody [ text ""
                    , text ""
                    ]-}
            , cHead "Test Your Installation" "test"
            , cBody [ text "run the customer application"
                    , text ""
                    ]
{-         , cHead' "Install GHC and Haskell Platform" "installghc"
         , cBody [ text "If you already installed GHC 6.10 or higher and Haskell-Platform skip to \"Install Header\""
                 , linebreak, linebreak
                 , contentTag "strong" [] [text "Ubuntu 9.04"], linebreak
                 , text "Please follow the Instructions on "
                 , linkN "http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html" [text "Haskell-Platform"]
                 , text " to install GHC 6.10 and Haskell-Platform if it is not installed on your system. The Cabal package will also be installed with the platform."
                 , linebreak, linebreak
                 , contentTag "strong" [] [text "Ubuntu 9.10"], linebreak
                 , text "Only use the standard install procedure "
                 , shell' "> sudo apt-get install ghc"
                 , text "to install GHC 6.10."
                 , linebreak, linebreak
                 , text "Installing Haskell-Platform is a little tricky on Ubuntu 9.10. You can try "
                 , linkN "http://davidsiegel.org/haskell-platform-in-karmic-koala/" [text "this"]
                 , text ", if it not works you have to install cabal on your own."
                 , linebreak
                 , text "If you got cabal installed it will be easy to install the other packages using"
                 , shell' "> cabal install"
                 , text "in each of the package directories."
                 , linebreak, linebreak
                 , text "Maybe there will be a way to easily install Haskell-Platform on "
                 , linkN "http://hackage.haskell.org/platform/" [text "Haskell-Platform"]
                 , text " in future."
                 ]
         , cHead' "Install Libraries" "installlibs"
         , cBody [ text "The following headers are needed to work with Hawk Framework. Okay, they are not needed by Hawk itself but by "
                 , linkN "http://www.fh-wedel.de/~si/HXmlToolbox/index.html" [text "HXT"]
                 , text " and \"HDBC-SQlite3\""
                 , linebreak, linebreak
                 , contentTag "strong" [] [text "Curl"], linebreak
                 , text "Curl is used by \"HXT\" package and we need the header files, lets install them."
                 , shell' "> sudo apt-get install libcurl-dev"
                 , linebreak
                 , contentTag "strong" [] [text "Sqlite3"], linebreak
                 , text "Hawk Framework in its current version runs only with SQlite3 and do not runs without a database, so you need to install the following header package."
                 , shell' "> sudo apt-get install libsqlite3-dev"
                 ]
         , cHead' "Install Hawk" "installhawk"
         , cBody [ text "Now that our system is ready we will install Hawk Framework. First unpack "
                 , link "/index/download?file=hawk" [text "hawk.tar.gz"]
                 , text " to a folder of your choise. For example to "
                 , codei "$HOME/hawk/"
                 , shell' "> tar -xf hawk.tar.gz"
                 , text "This should create the following directory structure:"
                 , pre [ "$HOME/hawk/"
                       , "`- Applications"
                       , "   `- example"
                       , "   `- projectdiary"
                       , "`- Libraries"
                       , "   `- eithert"
                       , "   `- hawk"
                       , "   `- stringable"
                       , "`- Makefile"
                       , "`- README"
                       ]
                 , text "Now do the following steps. If you have installed GHC 6.10 and Haskell-Platform properly, all will work."
                 , linebreak, linebreak
                 , text "Change to the project directory."
                 , shell' "> cd hawk/"
                 , text "Update the package list of cabal."
                 , shell' "> cabal update"
                 , text "Start the Makefile."
                 , shell' "> make"
                 , text "(wait a minute or two, till you get your prompt back ;-)"
                 , linebreak
                 , text "Hawk Framework and its depending haskell packages should now be installed."
                 , linebreak, linebreak
                 , text "Instead of running "
                 , codei "make"
                 , text " you can "
                 , codei "cd"
                 , text " to the Libraries directory and run the following commands:"
                 , shell [ "> cd eithert/"
                         , "> cabal install"
                         , "> cd ../stringable/"
                         , "> cabal install"
                         , "> cd ../hawk/"
                         , "> cabal install"
                         ]
                 , text "This will do the same as the Makefile does. Instead of "
                 , codei "cabal install"
                 , text " you can also run:"
                 , shell [ "> runhaskell Setup.hs configure"
                         , "> runhaskell Setup.hs build"
                         , "> runhaskell Setup.hs install"
                         ]
                 , text "Please try to use one of the above described methods and do not try to install the depending packages by yourself!"
                 , linebreak, linebreak
                 , text "If you've any conflicting dependencies for example a \"time\" package that is not equal to \"time-1.1.3\" then use "
                 , codei "ghc-pgk list"
                 , text " and "
                 , codei "ghc-pgk unregister"
                 , text " to delete these. And try to use "
                 , codei "make"
                 , text " or "
                 , codei "cabal install" 
                 , text " after unregistering it."
                 ]-}
--         , cHead' "Run a test Web-Application" "runexample"
{-           , cBody [ text "The following descriptions will work similar with the projectdiary project."
                   , linebreak
                   , text "First please setup the database. From "
                   , codei "$HOME/hawk/Applications/example/"
                   , text " do:"
                   , shell [ "> cd db/"
                           , "> sqlite3 database.db"
                           , "sqlite3> .read create.sql"
                           , "sqlite3> .read insert.sql"
                           , "sqlite3> .quit"
                           , "> cd ../"
                           ]
                   , text "For installing the web application do either:"
                   , shell' "> make Applications/example/"
                   , text "from "
                   , codei "$HOME/hawk/"
                   , text " or "
                   , codei "cd"
                   , text " to "
                   , codei "$HOME/hawk/Applications/example/"
                   , text " and run:"
                   , shell' "> cabal install"
                   , text "then you are able to start the application by running:"
                   , shell' "> ../../../.cabal/bin/example"
                   , text "or you can also add "
                   , codei "$HOME/.cabal/bin"
                   , text " to your "
                   , codei "$PATH"
                   , text " variable, then you only have to run:"
                   , shell' "> ./example"
                   , text "You can also use ghci from "
                   , codei "$HOME/hawk/Applications/example/"
                   , text " to start the application:"
                   , shell [ "> ghci Main.hs"
                           , "Main.hs> main"
                           ]
                   , text "After you have started the example web application open a browser of your choise and type in the URI "
                   , codei "http://localhost:3000/customer/list"
                   , text " to run the \"customer\" controller with the \"list\" action. This should result in the following."
                   , linebreak
                   , img "/images/example.png"
                   , linebreak
                   , text "It is necessary to start the application from the "
                   , codei "$HOME/hawk/Applications/example/"
                   , text " directory, otherwise the application will not find its public directory or any other files, like templates or database."
                   ]-}
            ]

history :: String -> String -> String -> XmlTree
history f v s = contentTag "tr" [("class","history")]
             [ contentTag "td" [("class","historyName")] [text f]
             , contentTag "td" [("class","historyVersion")] [text v]
             , contentTag "td" [("class","historySize")] [text s]
             , contentTag "td" [("class","historyLink")] [link ("/packages/"++f) [text "Download"]]
             ]
{-history :: String -> String -> String -> XmlTree
history f v s = divClass "history" 
             [ spanClass "historyName" [text f]
             , spanClass "historyVersion" [text v]
             , spanClass "historySize" [text s]
             , spanClass "historyLink" [link ("/packages/"++f) [text "Download"]]
             ]-}
