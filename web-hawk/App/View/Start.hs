module App.View.Start where

import App.View.Common

-- ------------------------------------------------------------------------------
-- Getting Started
-- ------------------------------------------------------------------------------
startL :: XmlTrees
startL = (lHead "Getting Started") 
      ++ (lList 
          [ ("Installation", "install")
          , ("Project Structure", "structure")
          , ("First Steps", "steps")
          ] "")
      ++ (lList
          [ ("Main Application", "mainapp")
          , ("Cabal Package", "cabal")
          , ("Configuration", "config")
          , ("Routing", "routing")
          , ("Controller", "controller")
          , ("Model", "model")
          , ("View", "view")
          , ("Templates", "templates")
          , ("Start it!", "start")
          ] "1")
      ++ (lList' [("FAQ", "/index/faq")] "")

startC :: XmlTrees
startC = [ cHead "Getting Started" ""
         , cBody [ text "This documentation is for developers which want to use Hawk Framework. It is split into two main parts. The \"Getting Started\" part describes how to setup the framework and guide you through your first simple web application with Hawk Framework. The API documentation will explain all functions you are able to use within your web application."
                 , linebreak
                 , text "It is recommended that you are familiar with Haskell to use Hawk Framework. Haskell is a pure functional programming language. If you are new to functional programming, please have a look at "
                 , linkN "http://book.realworldhaskell.org/read/" [text "Real World Haskell"]
                 , text " or on the "
                 , linkN "http://haskell.org/" [text "Haskell Website"]
                 , text "."
                 , linebreak
                 , text "In this part we will guide you to a setup of the framework and we'll try to spot all possible pitfalls (so if you find any that is not mentioned here, please "
                 , link "mailto:mail@hawk-project.org" [text "mail us"]
                 , text "). Also we will show you how to set up your project structure, to keep track of your project and a step by step introduction to your first web application using Hawk Framework, by writing a guestbook."
                 ]
-- Installation
         , cHead "Install Hawk" "install"
         , cBody [ text "See our "
                 , link "/index/download" [text "Download"]
                 , text " section for how to install Hawk."
                 ]
-- Project Structure
         , cHead "Project Structure" "structure"
         , cBody [ text "The directory stucture described in the following is recommended for keep track of your project, but you can change it as you need."
                 , linebreak
                 , text "Web applications will be created in the "
                 , codei "$HOME/hawk/Applications/"
                 , text " directory. To start with our first own Project we need a new folder in there. Lets call it \"guestbook\" and create the following directory structure:"
                 , pre [ "$HOME/hawk/Applications/guestbook"
                       , "`- App              <- contains MVC-pattern folders, main part of the web application"
                       , "   `- Controller    <- controllers handles all other logic"
                       , "   `- Model         <- model handles db and db logic"
                       , "   `- template      <- xhtml files for page generation"
                       , "   `- View          <- view logic"
                       , "`- Config           <- configuration and routing/dispatcher information"
                       , "`- db               <- database folder and sql files"
                       , "`- public           <- folder the webserver points to"
                       , "   `- images        <- images to refere at"
                       , "   `- javascript    <- javascript files"
                       , "   `- stylesheets   <- stylesheets for the page"
                       ]
                 ]
-- First Steps
         , cHead "First Steps" "steps"
         , cBody [ text "As mentioned in \"Project Structure\" our first project will be named \"guestbook\" and we will now learn how to write a simple guestbook using Hawk Framework."
                 , linebreak, linebreak
                 , text "The first question is: What do we need for a guestbook?"
                 , linebreak
                 , text "We will need ..."
                 , contentTag "ul" [] 
                      [ contentTag "li" [] [text "... a data type that has an id, name, message and time field"]
                      , contentTag "li" [] [text "... the data type can be saved and loaded for that it is persistent"]
                      , contentTag "li" [] [text "... a page to show the guestbook"]
                      , contentTag "li" [] [text "... a form to add entries"]
                      ]
                 , text "Before we start to implement all these stuff lets have a short look at the main application file and the cabal package to install our application easily. For short testing we will use ghci, to show that our application compiles successfully."
                 ]
         , cHead' "Main Application" "mainapp"
         , cBody [ text "The main application is the start of the application you run and does not contain any hard to understand code."
                 , linebreak
                 , text "So lets create a "
                 , codei "Main.hs"
                 , text " file in the \"guestbook\" directory and open it:"
                 , filehead "guestbook/Main.hs"
                 , code [ "module Main (main) where           -- define a module that only exports the \"main\" function"
                        , "import Config.Config (configuration, environment)    -- import the four functions, used below"
                        , "import Hawk.Controller.Initializer (getApplication)"
                        , ""
                        , "import Hack.Handler.SimpleServer as Server (run)"
                        , ""
                        , "main :: IO ()                -- main function, start webserver and initialize our application"
                        , "main = run 3000 $ getApplication environment configuration"
                        ]
                 , text "This will run a simple webserver at port 3000. The server will pass all requests to the application that is returned by "
                 , codei "getApplication"
                 , text ". "
                 , codei "getApplication"
                 , text " will run the two funtions "
                 , codei "configuration"
                 , text " and "
                 , codei "environment"
                 , text " from the "
                 , codei "Config.Config"
                 , text " module we will define later on."
                 ]
         , cHead' "Cabal Package" "cabal"
         , cBody [ text "As said above we'll create a cabal file named "
                 , codei "Guestbook.cabal"
                 , text " to use it (this step is optional, you can also use ghci or ghc):"
                 , filehead "guestbook/Guestbook.cabal"
                 , code [ "Name: Guestbook"
                        , "Version: 0.0.1"
                        , "Description: My first simple guestbook web application"
                        , "License: BSD3"
                        , "Author: <your name>"
                        , "Build-Type: Simple"
                        , "Cabal-Version: >=1.6"
                        , ""
                        , "Executable  guestbook"
                        , "  ghc-options: -Wall"
                        , "  Build-Depends: base == 4.*,"
                        , "    hack-handler-simpleserver >= 0.0.1 && < 1,"
                        , "    HDBC >= 2.1.0 && < 3,"
                        , "    HDBC-sqlite3 >= 2.1.0.0 && < 3,"
                        , "    utf8-string >= 0.3.5 && < 1,"
                        , "    hslogger >= 1.0.7 && < 2,"
                        , "    containers >= 0.2.0.1 && < 1,"
                        , "    time == 1.1.3,"
                        , "    mtl >= 1.1.0.2 && < 2,"
                        , "    SHA >= 1.4.0 && < 2,"
                        , "    Hawk >= 0.0.1 && < 0.1"
                        , "  main-is:  Main.hs"
                        ]
                 , text "In addition to the "
                 , codei "Guestbook.cabal"
                 , text " file you also have to create a "
                 , codei "Setup.hs"
                 , text " file which only contains:"
                 , filehead "guestbook/Setup.hs"
                 , code [ "import Distribution.Simple"
                        , "main = defaultMain"
                        ]
                 , text "For further questions please visit "
                 , linkN "http://www.haskell.org/cabal/" [text "cabal page"]
                 , text "."
                 ]
         , cHead' "Configuration" "config"
         , cBody [ text "Configuration is needed to setup pathes, routing and database usage to work together properly. Please change to the "
                 , codei "Config"
                 , text " directory you created in \"Project Structure\", create a file named "
                 , codei "Config.hs"
                 , text " and open it."
                 , filehead "guestbook/Config/Config.hs"
                 , code [ "module Config.Config (environment, configuration) where  -- export only environment and configuration"
                        , ""
                        , "import qualified Config.Routes as Routes (routing)       -- now we can call \"routing\" by \"Routes.routing\""
                        , ""
                        , "import Hawk.Controller.Initializer (AppEnvironment (..)) -- import needed types and functions from Hawk"
                        , "import Hawk.Controller.Routes (simpleRouting)"
                        , "import Hawk.Controller.Session.CookieSession (cookieSession)"
                        , "import Hawk.Controller.Types (AppConfiguration (..))"
                        , ""
                        , "import Control.Monad (liftM)                             -- import types an functions for database usage"
                        , "import Database.HDBC.Sqlite3"
                        , "import Database.HDBC (ConnWrapper(..))"
                        , "import System.Log.Logger                                 -- import logger functions"
                        , ""
                        , "environment :: AppEnvironment                            -- configure of our application environment"
                        , "environment = AppEnvironment"
                        , "  { connectToDB = ConnWrapper `liftM`                    -- our databasefile loaded with a Sqlite3 connector"
                        , "                  connectSqlite3 \"./db/database.db\"      --   and wrapped in HDBC"
                        , "  , logLevels   = [(rootLoggerName, DEBUG)]              -- realy verbose logging to stdout"
                        , "  , envOptions  = []                                     -- no need for any other environment options now"
                        , "  }"
                        , ""
                        , "configuration :: AppConfiguration                        -- configure our application"
                        , "configuration = AppConfiguration"
                        , "  { sessionStore = cookieSession                         -- only need session handling for flash messages"
                        , "  , sessionOpts  =                                       --   on redirects"
                        , "      [(\"secret\",\"01234567890123456789012345678901\")]    -- 32 byte secret for HMAC"
                        , "  , routing      = simpleRouting Routes.routing          -- \"routing\" will be called in every request"
                        , "  , templatePath = \"./App/template\"                      -- directory for xhtml templates"
                        , "  , publicDir    = \"./public\"                            -- public visible directory"
                        , "  , confOptions  = []                                    -- no need for any other configuration options now"
                        , "  , error404file = \"404.html\"                            -- file to show when there is a 404"
                        , "  , error500file = \"500.html\"                            -- file to show on a 500"
                        , "  }"
                        ]
                 ]
         , cHead' "Routing" "routing"
         , cBody [ text "As shown in the last code sample, we need a routing module and it will be named Routes.hs in the Config directory. Please create that file."
                 , filehead "guestbook/Config/Routes.hs"
                 , code [ "module Config.Routes (routing) where                      -- export the routing function"
                        , ""
                        , "import qualified App.Controller.GuestbookController as GC -- our controller will say how to route to actions"
                        , ""
                        , "import Hawk.Controller.Types (Controller)"
                        , ""
                        , "import qualified Data.Map as M"
                        , ""
                        , "routing :: M.Map String (M.Map String Controller)         -- don't bother about this type"
                        , "routing = M.singleton \"guestbook\" $ M.fromList GC.routes  -- only \"guestbook\" will map to our guestbook controller"
                        , "                                                          --   we don't need another controller for our application"
                        ]
                 , text "As you can see we will use a module called GuestbookController to define the actions. The url to refere to this controller will be "
                 , codei "http://localhost:3000/guestbook"
                 , text " as "
                 , codei "guestbook"
                 , text " is the mapped singleton and our port will be \"3000\", defined in "
                 , codei "Main.hs"
                 , text "."
                 ]
         , cHead' "Controller" "controller"
         , cBody [ text "Lets write our first Controller. Go to the "
                 , codei "guestbook/App/Controller"
                 , text " directory and create a "
                 , codei "GuestbookController.hs"
                 , text " file. Open the file and create the module header which only exports a "
                 , codei "routes"
                 , text " function for the dispatcher."
                 , filehead "guestbook/App/Controller/GuestbookController.hs"
                 , code [ "module App.Controller.GuestbookController (routes) where -- only export \"routes\" it is needed by Routes.hs"
                        , ""
                        , "import App.Model.GuestbookEntry                          -- import our model, to read and write data from/to database"
                        , "import qualified App.View.GuestbookView as GV            -- import our view to call \"showXhtml\" function"
                        , ""
                        , "import Hawk.Controller                                   -- types and functions (Routing, StateController, render, ..)"
                        , "import Hawk.Model                                        -- read write functions (select, updateAndValidate, ..)"
                        , "import Hawk.View.TemplateView                            -- for \"typedView\" function"
                        , ""
                        , "routes :: [Routing]                                      -- create a list of routing for mapping string to action"
                        , "routes = "
                        , "  [ (\"show\",indexAction >>= render (typedView \"show\" V.showXhtml))    -- do indexAction and pass result to showXhtml"
                        , "  , (\"index\",indexAction >>= render (typedView \"show\" V.showXhtml))   --   function from our view module"
                        , "  , (\"insert\",insertAction >> redirectToAction \"guestbook\" \"index\")   -- for insert and delete forget the action result"
                        , "  , (\"delete\",deleteAction >> redirectToAction \"guestbook\" \"index\") ] --   and redirect to the index action"
                        , ""
                        , "indexAction :: StateController [GuestbookEntry]          -- create a new criteria db request and order it by its id"
                        , "indexAction = select (setOrder [desc \"_id\"] newCriteria) --   \"select\" will return the request result"
                        , ""
                        , "insertAction :: StateController ()"
                        , "insertAction = do"
                        , "  method <- getRequestMethod"
                        , "  case method of"
                        , "    POST -> do"
                        , "      n <- new :: StateController GuestbookEntry         -- need to set type explicit here"
                        , "      (ge, errs) <- getParams >>= updateAndValidate n \"\" -- pass the POST params to updateAndValidate"
                        , "                                                         --   validate POST data and update the entry \"object\""
                        , "      if null errs"
                        , "        then do"
                        , "          insert ge                                        -- make the entry persistent if no errors occured"
                        , "          setFlash \"notice\" \"Successfully added your message!\""
                        , "        else"
                        , "          setErrors \"guestbookEntry\" errs                -- set an error message to show"
                        , "      return ()                                          -- we dont care about the result so return unit"
                        , "    _ -> return ()"
                        , ""
                        , "deleteAction :: StateController ()"
                        , "deleteAction = do"
                        , "  guestbookEntry <- readParam \"id\" >>= liftMaybe findMaybe :: StateController (Maybe GuestbookEntry)"
                        , "                                                         -- this will get us a GET param and find it from db"
                        , "  case guestbookEntry of"
                        , "    Nothing ->                                           -- if a entry with this id does not exists"
                        , "      setFlash \"error\" \"The requested customer does not exist\""
                        , "    Just ge -> do                                        -- if entry with the given id exists"
                        , "      delete ge                                          --  delete the entry from database"
                        , "      setFlash \"notice\" \"The guestbook entry has been deleted\""
                        , "  return ()"
                        ]
                 , note "setErrors and setFlash does not work when redirected afterwards."
                 , linebreak
                 , note "explicit typing is only needed if the compiler can't find out the type by itself. If we got a \"insertAction :: StateController GuestbookEntry\" we will only need to return out new created entry."
                 ]
         , cHead' "Model" "model"
         , cBody [ text "As you see above we need a model named \"GuestbookEntry\". So please create a "
                 , codei "GuestbookEntry.hs"
                 , text " in the "
                 , codei "Model"
                 , text " directory."
                 , filehead "guestbook/App/Model/GuestbookEntry.hs"
                 , code [ "module App.Model.GuestbookEntry (GuestbookEntry (..)) where"
                        , ""
                        , "import Hawk.Model"
                        , "import Control.Monad.Trans (liftIO)"
                        , "import Data.Time (UTCTime(..), getCurrentTime)"
                        , ""
                        , "data GuestbookEntry = GuestbookEntry                     -- the data type with constructor \"GuestbookEntry\""
                        , "  { _id         :: PrimaryKey                            -- our auto-increment primary key"
                        , "  , name        :: String                                -- user generated name"
                        , "  , message     :: String                                -- user generated message"
                        , "  , createdAt   :: UTCTime                               -- timestamp"
                        , "  } deriving (Eq, Read, Show)"
                        , ""
                        , "instance Persistent GuestbookEntry where                 -- make it a persistent type"
                        , "  persistentType _ = \"GuestbookEntry\""
                        , "  fromSqlList (l0:l1:l2:l3:[])                           -- sql column names to record names mapping"
                        , "    = GuestbookEntry (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)"
                        , "  fromSqlList _ = error \"wrong list length\""
                        , "  toSqlAL x = [ (\"_id\"       , toSql $ _id       x)      -- record names to sql column names mapping"
                        , "              , (\"name\"      , toSql $ name      x)"
                        , "              , (\"message\"   , toSql $ message   x)"
                        , "              , (\"createdAt\" , toSql $ createdAt x)"
                        , "              ]"
                        , "  tableName = const \"guestbook\"                          -- sql table name definition"
                        , ""
                        , "instance WithPrimaryKey GuestbookEntry where             -- we have a type with a primary key"
                        , "  primaryKey = _id"
                        , "  pkColumn = head . tableColumns"
                        , "  setPrimaryKey pk ge = ge {_id = pk}"
                        , ""
                        , "instance Model GuestbookEntry where"
                        , "  new = do                                               -- new function, used by GuestbookController"
                        , "    t <- liftIO getCurrentTime                        -- get the current time and assign it to t"
                        , "    return $ GuestbookEntry 0 \"\" \"\" t                    -- construct a new empty GuestbookEntry"
                        , "  insert ge = do                                         -- insert function, used by GuestbookController"
                        , "    now <- liftIO getCurrentTime"
                        , "    insertInTransaction $ ge { createdAt = now }         -- insert our record to db with updated \"createAt\" field"
                        , ""
                        , "instance Validatable GuestbookEntry where                -- we have a validation on this type"
                        , "  validator ge = do"
                        , "    validateNotNull \"name\"          $ name    ge         -- assert that the name is not empty"
                        , "    validateLength  5 400 \"message\" $ message ge         -- the message has to be 5 to 400 characters long"
                        , "    return ()"
                        , ""
                        , "instance Updateable GuestbookEntry where                 -- our type should be updateable by parameters (GET, POST)"
                        , "  updater ge s = do"
                        , "    n <- updater (name    ge) $ subParam s \"name\"     -- assign the value of \"name\" to n"
                        , "    m <- updater (message ge) $ subParam s \"message\"  -- assign the value of \"message\" to m"
                        , "    return $ ge { name = n, message = m }                -- assign the values to our record"
                        ]
                 , text "And out of this module we have to create a database with the table. Change to "
                 , codei "guestbook/db/"
                 , text " and create a file "
                 , codei "create.sql"
                 , filehead "guestbook/db/create.sql"
                 , code [ "DROP TABLE IF EXISTS `guestbook`;"
                        , "CREATE TABLE `guestbook` ("
                        , "  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,"
                        , "  `name` VARCHAR(100) NOT NULL,"
                        , "  `message` TEXT NOT NULL,"
                        , "  `createdAt` DATETIME NOT NULL"
                        , ");"
                        ]
                 , text "Now only the view module and templates are left to let us see our first guestbook written with Hawk Framework."
                 ]
         , cHead' "View" "view"
         , cBody [ text "Create a "
                 , codei "guestbook/View/GuestbookView.hs"
                 , text " file. For filling out templates with data processed by "
                 , codei "GuestbookController"
                 , filehead "guestbook/App/View/GuestbookView.hs"
                 , code [ "{-# LANGUAGE TemplateHaskell #-}                         -- we need this language extension to use templates"
                        , "module App.View.GuestbookView (showXhtml) where          -- only export our \"showXhtml\" function"
                        , ""
                        , "import App.Model.GuestbookEntry                          -- use the model for the data type"
                        , ""
                        , "import Hawk.Controller"
                        , "import Hawk.View.TemplateView                            -- only needed for \"XmlTree\" type"
                        , "import Hawk.View.Template.DataType                       -- need for meta-program parts \"$(viewDataType ..)\""
                        , "                                                         --   you can first think of it as a makro"
                        , "import qualified Hawk.View.Template.HtmlHelper as H      -- use helper function to show text in templates"
                        , ""
                        , "import Data.Time                                         -- used for \"formatTi\" function"
                        , "import System.Locale                                     --   also used for it"
                        , ""
                        , "formatTi :: UTCTime -> [XmlTree]                         -- formats the time to H:M M/D/Y"
                        , "formatTi t = [H.text (formatTime defaultTimeLocale \"%R %D\" t)]"
                        , ""
                        , "$(viewDataType \"Guestbook\" \"singleEntry\")                -- create a type GuestbookSingleEntry"
                        , "$(viewDataType \"Guestbook\" \"show\")                       -- create a type GuestbookShow"
                        , "                                                         -- these types also refere to template locations"
                        , "                                                         --   i.e. \"guestbook/App/templates/Guestbook/show.xhtml\""
                        , ""
                        , "showXhtml :: [GuestbookEntry] -> StateController GuestbookShow"
                        , "showXhtml gs = do"
                        , "  ge <- mapM entryXhtml gs                            -- use the entryXhtml for every entry"
                        , "  return GuestbookShow                                   -- construct GuestbookShow type"
                        , "    { guestbookSingleEntry = ge                          -- needed with this name to insert all the entries"
                        , "    , title = [H.text \"My first Guestbook\"]              -- set the page title"
                        , "    , posts = length gs                                  -- set a counter of entries to \"posts\""
                        , "    }"
                        , ""
                        , "entryXhtml :: GuestbookEntry -> StateController GuestbookSingleEntry"
                        , "entryXhtml ge = return GuestbookSingleEntry              -- construct a GuestbookSingleEntry"
                        , "    { username = name ge                                 -- accesses the name and assign it to \"username\""
                        , "    , at = createdAt ge                                  -- set creation date and time"
                        , "    , msg = message ge                                   -- read message from record and set it"
                        , "    , delete = [H.textlink (\"/guestbook/delete?id=\" ++ show (_id ge)) \"[Delete]\"] -- create a delete link"
                        , "    }"
                        ]
                 ]
         , cHead' "Templates" "templates"
         , cBody [ text "Now we will create the places where to insert our record data produced above. First create a header.xhtml in the shown directory."
                 , filehead "guestbook/App/template/Layouts/header.xhtml"
                 , code [ "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
                        , "<html>"
                        , "<head>"
                        , "  <title><hawk:bind name=\"title\"/></title>       <!-- binds the title from \"GuestbookShow\" to this position -->"
                        , "  <link href=\"/stylesheets/style.css\" media=\"screen\" rel=\"Stylesheet\" type=\"text/css\"/>"
                        , "</head>"
                        , "<body>"
                        , "  <!-- error message -->                               <!-- this is something you may always want to insert-->"
                        , "  <hawk:message type=\"error\">"
                        , "    <div class=\"error\">"
                        , "    <image alt=\"Warning\" title=\"Warning\" src=\"/images/warning.png\"/>"
                        , "    <hawk:content/>"
                        , "    </div>"
                        , "  </hawk:message>"
                        , "  <!-- info message -->"
                        , "  <hawk:message type=\"notice\">"
                        , "    <div class=\"notice\">"
                        , "    <image alt=\"info\" title=\"Info\" src=\"/images/info.png\"/>"
                        , "    <hawk:content/>"
                        , "    </div>"
                        , "  </hawk:message>"
                        , "  <!-- content -->"
                        , "  <hawk:bind name=\"content\"/>                          <!-- our content will be bind to here-->"
                        , "</body>"
                        , "</html>"
                        ]
                 , text "What will be the content? - there is another nice xml tag for that, to surround a xhtml file with another."
                 , linebreak
                 , text "Create the "
                 , codei "show.xhtml"
                 , text " in "
                 , codei "/template/Guestbook/"
                 , text "."
                 , filehead "guestbook/App/template/Guestbook/show.xhtml"
                 , code [ "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
                        , "<!-- surround this file with \"../Layouts/header.xhtml\" and insert it at bind \"content\" -->"
                        , "<hawk:surround with=\"../Layouts/header.xhtml\" at=\"content\" xmlns:hawk=\"http://fh-wedel.de/hawk\">"
                        , "  <form method=\"POST\" action=\"/guestbook/insert\">  <!-- set our insert function here -->"
                        , "    <p>"
                        , "      <label for=\"name\">Name</label><br/>"
                        , "      <input type=\"text\" name=\"name\"/>"
                        , "    </p>"
                        , "    <p>"
                        , "      <label for=\"name\">Message</label><br/>"
                        , "      <textarea cols=\"50\" rows=\"5\" name=\"message\"/>"
                        , "    </p>"
                        , "    <input type=\"Submit\" value=\"Post\"/>"
                        , "  </form>"
                        , "  <p>Guestbook Entries: <hawk:bind name=\"posts\" type=\"Int\"/></p> <!-- bind our \"posts\" var here -->"
                        , "  <hawk:embed what=\"singleEntry.xhtml\"/>       <!-- and another nice tag, embed the \"singleEntry.xhtml here -->"
                        , "</hawk:surround>"
                        ]
                 , text "So lets create our last xhtml template for this project and bind the GuestbookEntry record fields to their positions."
                 , filehead "guestbook/App/template/Guestbook/singleEntry.xhtml"
                 , code [ "<div class=\"entry\">"
                        , "  <div class=\"head\">"
                        , "    <span class=\"l\">"
                        , "      <hawk:bind name=\"username\" type=\"String\"/>"
                        , "      <hawk:bind name=\"delete\"/>"
                        , "    </span>"
                        , "    <span class=\"r\">"
                        , "      <hawk:bind name=\"at\" type=\"UTCTime\" format=\"formatTi\"/>"
                        , "    </span>"
                        , "  </div>"
                        , "  <div class=\"body\">"
                        , "    <hawk:bind name=\"msg\" type=\"String\"/>"
                        , "  </div>"
                        , "</div>"
                        ]
                 ]
         , cHead' "Start it!" "start"
         , cBody [ text "There are only some small points to do before we can run our web application."
                 , linebreak, linebreak
                 , text "Copy ("
                 , link "/guestbook-src/info.png" [text "info"]
                 , text " and "
                 , link "/guestbook-src/alert.png" [text "alert"]
                 , text ") to "
                 , codei "guestbook/public/images/"
                 , text ","
                 , linebreak
                 , text "copy ("
                 , link "/guestbook-src/style.css" [text "style"]
                 , text ") to "
                 , codei "guestbook/public/stylesheets/"
                 , linebreak
                 , text "and copy ("
                 , link "/guestbook-src/404.html" [text "404"]
                 , text " and "
                 , link "/guestbook-src/500.html" [text "500"]
                 , text ") to "
                 , codei "guestbook/public/"
                 , text "."
                 , linebreak, linebreak
                 , text "Now go to "
                 , codei "~/hawk/Applications/guestbook/"
                 , text " and run:"
                 , shell [ "> cabal install"
                         , "> ./guestbook"
                         ]
                 , note "This will only work if \"$HOME/.cabal/bin\" is in your $PATH"
                 , text "or" 
                 , shell [ "> ghci Main.hs"
                         , "Main.hs> main"
                         ]
                 , text "Now open your browser and go to "
                 , codei "http://localhost:3000/guestbook/show"
                 , text "."
                 , linebreak
                 , text "There is your first web application written with Hawk Framework."
                 , linebreak
                 , img "/images/guestbook.png"
                 ]
         ]
