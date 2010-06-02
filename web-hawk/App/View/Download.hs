module App.View.Download where

import App.View.Common

downloadL :: XmlTrees
downloadL = (lHead "Download")
         ++ (lList [ ("Latest Release", "latest")
                   , ("Release History", "history")
                   , ("Install Haskell", "ghc")
                   , ("Linux Libs", "linuxlibs")
                   , ("Mac OSX Libs", "maclibs")
                   , ("Windows Libs", "windowslibs")
                   , ("Install Hawk", "linuxhawk")
                   , ("Test Installation", "test")] "")

downloadC :: XmlTrees
downloadC = [ cHead "Get Haskell Web Application Kit" ""
            , cBody [ text "We recommend using Linux for developing Web Applications with Hawk, because Mac OSX and Windows can cause some problems installing needed development libraries."
                    , linebreak, linebreak
                    , text "The Versions of Hawk, available for download have only Beta status. It is still under developement. There is a lot of work to do, but if you've any inspirations, comments, feedback or you want to join us, please mail us at "
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
                       , history "hawk-latest.tar.gz" "0.0.2" "50.1 KB"
                       ]
                    ]
            , cHead "Install Haskell" "ghc"
            , cBody [ text "If you've already installed GHC 6.10 or higher as well as Cabal on your system skip this step."
                    , linebreak, linebreak
                    , text "For "
                    , linkN "http://haskell.org/ghc/download_ghc_6_10_4.html" [text "GHC 6.10.4"], linebreak
                    , text " just follow the instructions or use 'Haskell Platform' instead, it contains GHC 6.10.4 and Cabal 1.6."
                    , linebreak, linebreak
                    , text "For "
                    , linkN "http://haskell.org/ghc/download_ghc_6_12_1.html" [text "GHC 6.12.1"], linebreak
                    , text " just follow the instructions for your system and to install Cabal 1.8 and Cabal-Install 0.8 see below."
                    , linebreak, linebreak
                    , text "For "
                    , linkN "http://hackage.haskell.org/platform/" [text "Haskell Platform"], linebreak
                    , text " just follow the instructions for your system. Use this links if your platform is "
                    , linkN "http://sitr.us/2009/07/02/how-to-install-haskell-platform-on-ubuntu-jaunty.html" [text "Ubuntu 9.04"]
                    , text " or "
                    , linkN "http://davidsiegel.org/haskell-platform-in-karmic-koala/" [text "Ubuntu 9.10"]
                    , text "." 
                    , linebreak, linebreak
                    , text "For "
                    , contentTag "strong" [] [text "Cabal"], linebreak
                    , text "you can look right "
                    , linkN "http://hackage.haskell.org/trac/hackage/wiki/CabalInstall" [text "here"]
                    , text " or "
                    , linkN "http://haskell.org/cabal/download.html" [text "here"]
                    , text " how to install it."
                    ]
            , cHead "Install Development Libraries on Linux" "linuxlibs"
            , cBody [ text "Hawk only will need 'HXT' and 'HDBC-SQLite3' Haskell packages to be installed, to do so you will need some C-Header files."
                    , linebreak, linebreak
                    , text "For HXT its "
                    , linkN "" [text "curl"]
                    , text " you will need to install and for HDBC-SQLite3 its "
                    , linkN "" [text "sqlite3"]
                    , text "."
                    , linebreak
                    , text "The easiest way on Ubuntu-Linux is to use "
                    , codei "apt-get install"
                    , text ", just do:"
                    , shell' "> apt-get install libcurl-dev"
                    , text "and"
                    , shell' "> apt-get install libsqlite3-dev"
                    ]
            , cHead "Install Development Libraries on Mac OSX" "maclibs"
            , cBody [ text "coming up soon ..."
                    , linebreak, linebreak
                    , text "Please use the instructions from \""
                    , link "#linuxlibs" [text "Linux Libs"]
                    , text "\" instead, until this section becomes available."
                    ]
            , cHead "Install Development Libraries on Windows" "windowslibs"
            , cBody [ text "coming up soon ..."
                    , linebreak, linebreak
                    , text "Please use the instructions from \""
                    , link "#linuxlibs" [text "Linux Libs"]
                    , text "\" instead, until this section becomes available. We recommend to use 'mingw' or 'Cygwin' for Windows"
                    ]
            , cHead "Install Hawk" "hawk"
            , cBody [ text "When you got Haskell and Cabal running and installed the Libraries, only do:"
                    , shell [ "> cabal update"
                            , "> cabal install Hawk"
                            ]
                    , text "to get the latest release from "
                    , linkN "http://hackage.haskell.org" [text "Hackage"]
                    , text " and to install it."
                    , linebreak, linebreak
                    , text "If you installed all, but downloaded Hawk as gzip just unzip it, "
                    , codei "cd"
                    , text " into the "
                    , codei "hawk"
                    , text " directory and run:"
                    , shell' "> cabal install"
                    , text "this will install Hawk to your system."
                    , linebreak, linebreak
                    , text "Instead of using "
                    , codei "cabal install"
                    , text " you can run:"
                    , shell [ "> runhaskell Setup.hs configure"
                            , "> runhaskell Setup.hs build"
                            , "> runhaskell Setup.hs install"
                            ]
                    , linebreak
                    , text "If you got any trouble installing Hawk, please have a look at our "
                    , link "/index/faq#install" [text "FAQ"]
                    , text " or just "
                    , link "mailto:mail@hawk-project.org" [text "mail us"]
                    , text "."
                    ]
            , cHead "Test Your Installation" "test"
            , cBody [ text "To test if your installation was successful, just download and unzip "
                    , link "/packages/example.tar.gz" [text "this"]
                    , text " file."
                    , linebreak, linebreak
                    , text "Now go to the "
                    , codei "example"
                    , text " directory and either run:"
                    , shell [ "> ghci Main.hs"
                           , ".. loading .."
                           , "*Main> main"
                           ]
                    , text "or add the Cabal binaries directory ("
                    , codei "$HOME/.cabal/bin"
                    , text ") to your environment variable PATH and run:"
                    , shell [ "> cabal install"
                           , ".. installing .."
                           , "./example"
                           ]
                    , linebreak
                    , text "Now open your Browser and go to "
                    , linkN "http://localhost:3000/example/list" [text "http://localhost:3000/example/list"]
                    , text " you should see the same as shown below"
                    , img "/images/example.png"
                    , note "It's necessary to run the binary or 'ghci' from the directory containing the 'public', 'db' and 'App/template' folders. Otherwise your application will not run properly."
                    ]
            ]

history :: String -> String -> String -> XmlTree
history f v s = contentTag "tr" [("class","history")]
             [ contentTag "td" [("class","historyName")] [text f]
             , contentTag "td" [("class","historyVersion")] [text v]
             , contentTag "td" [("class","historySize")] [text s]
             , contentTag "td" [("class","historyLink")] [link ("/packages/"++f) [text "Download"]]
             ]

