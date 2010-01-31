-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $


-}
-- --------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module App.Action.Project where

import App.Model.Project as Project
import Hawk.Controller as Controller
import Hawk.Model
import Hawk.Controller.Util.Read
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Time.Calendar

import Hawk.View.TemplateView as TV
--import Hawk.Template.Types
--import Hawk.Template.Helper
--import Hawk.Controller.Render
--import Hawk.Template.DataType
--import Text.XML.HXT.Arrow
--import Data.Data
--import Control.Monad (liftM)

controllers :: [(String, RenderWrapper)]
controllers = [ ("show"  , RW projectShow  )
           , ("new"   , RW projectNew   )
           , ("create", RW projectCreate)
           , ("list"  , RW projectList  )
           ]

-- --------------------------------------------------------------------------
-- show
-- --------------------------------------------------------------------------
projectShow :: TemplateView (Maybe Project.Project)
projectShow = TemplateView
  { TV.controller = do
      i <- getParam "id"
      case Hawk.Util.Read.maybeRead i of
        Just n ->  findMaybe n
        Nothing -> return Nothing
  , toXhtml = \template project' -> return $
      case project' of
        Just p -> bind template
                  [ ("_id", [XN.mkText $ show $ Project._id p])
                  , ("description", [XN.mkText $ Project.description p])
                  ]
                  []
        Nothing -> [XN.mkText "no project found"]
  }

-- --------------------------------------------------------------------------
-- new
-- --------------------------------------------------------------------------
projectNew :: TemplateView ()
projectNew = emptyView (return ())
{-
  # Formular zum Erstellen eines neuen Projekts
  def new
    @project = Project.new
  end
-}

-- --------------------------------------------------------------------------
-- create
-- --------------------------------------------------------------------------
projectCreate :: TemplateView Project
projectCreate = emptyView (do
            debugM "ProjectCreate aufgerufen"
            pre <- getParam "project.prefix"
            desc' <- getParam "project.description"
            let p = Project 0 pre desc' (fromGregorian 2009 06 23) (fromGregorian 2010 06 23)
            insert p
            commit
            setFlash "notice" "Das Projekt wurde erfolgreich angelegt"
            redirectToAction "project" "list")

{-
  # Erzeugen eines neuen Projekts
  def create
    @project = Project.new(params[:project])
    if @project.save
      flash[:notice] = 'Das Projekt wurde erfolgreich angelegt.'
      redirect_to :action => 'list'
    else
      render :action => 'new'
    end
  end
-}

-- --------------------------------------------------------------------------
-- edit
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- update
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- destroy
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- list
-- --------------------------------------------------------------------------

-- --------------------------------------------------------------------------
-- clear
-- --------------------------------------------------------------------------

projectList :: TemplateView [Project.Project]
projectList = TemplateView
  { TV.controller = select newCriteria -- Project.findMany [0..10] --(undefined::Project.Project) [1..10]
  , toXhtml = \template -> return $ concatMap
    (\p -> bind template [("name",[XN.mkText $ Project.description p])] [])
  }

{- Rails-Code

  # zeigt die Details eines Projekts an
  def show
    @project = Project.find(params[:id])
    @users = User.find(:all) - @project.workers.collect {|w| w.user}
  end





  # Formular zum �ndern von Projekteigenschaften
  def edit
    @project = Project.find(params[:id])
  end

  # Speichert die �nderungen am Projekt
  def update
    @project = Project.find(params[:id])
    if @project.update_attributes(params[:project])
      flash[:notice] = 'Das Projekt wurde erfolgreich aktualisiert.'
      redirect_to :action => 'show', :id => @project
    else
      render :action => 'edit'
    end
  end

  # L�scht ein Projekt
  def destroy
    Project.find(params[:id]).destroy
    redirect_to :action => 'list'
  end

  # F�gt einem Projekt einen neuen Mitarbeiter hinzu
  def add_user
    worker = Worker.new(:user => User.find(params[:user]), :project => Project.find(params[:id]))
    if worker.save!
      flash[:notice] = "Der Benutzer wurde dem Projekt erfolgreich zugewiesen."
      redirect_to :action => "show",  :id => params[:id]
    else
      flash[:error] = "Der Benutzer konnte dem Projekt nicht zugewiesen werden."
    end
  end
  
  # Entfernt einen Mitarbeiter aus einem Projekt
  def delete_user
    project = Project.find(params[:id])
    worker = Worker.find(params[:worker])
    if worker.destroy
      flash[:notice] = "Die Zuordnung des Benutzers zum Projekt wurde erfolgreich aufgehoben."
      redirect_to :action => "show",  :id => params[:id]
    else
      flash[:error] = "Beim Versuch, die Zuordnung des Benutzers zum Projekt aufzuheben, ist ein Fehler aufgetreten."
    end
  end
  
  # Zur�cksetzen eines Projekts
  def clear
    project = Project.find(params[:id])
    project.areas.each {|area| area.destroy}
    project.changes.each {|change| change.destroy}    
    index
  end

-}

{-
-- TODO move to a common place
constText :: (ArrowXml a, Show s) => s -> a b XmlTree
constText x = (constA . show) x >>> mkText

-- TODO better Name an move to common place
snipp :: (Templatable s, Xhtml.XhtmlController s) => EnvController s -> XmlSnippet
snipp s = evalAM (runController s) &&& Text.XML.HXT.Arrow.getChildren
        >>>
        arr2A showC

$(viewDataType "show")
-- data Project_show = Project_show { _id :: Int, des :: Des } deriving (Show, Data, Typeable)
-- data Des = Des { description :: String } deriving (Show, Data, Typeable)

instance Templatable Des where
    getParams = const [("description", constText . description)]

instance Templatable Project_show where
    getParams = const [("_id", constText . _id), ("des", \x -> Text.XML.HXT.Arrow.getChildren >>> (showC . des) x)]

instance Snippet Project_show where
    snippet = do
        project <- find (undefined::Project.Project) 1
        case project of
            Just p  -> return Project_show { _id = (fromInteger $ Project._id p), des = Des {description = Project.description p}}
            Nothing -> fail "no project found :("
    asXhtml = showC

instance ToText Project_show where
    toText = return . show
{-
data Show = Show

instance NewSnippet Show Project_show where
    run _ = do
        project <- find (undefined::Project.Project) 1
        case project of
            Just p  -> return Project_show { _id = (fromInteger $ Project._id p), des = Des {description = Project.description p}}
            Nothing -> fail "no project found :("
-}
showXml :: XmlSnippet
showXml = snipp (snippet::EnvController Project_show)

showJSON :: EnvController String
showJSON = liftM toJSON (snippet::EnvController Project_show)

showJSON' :: HawkArrow a String
showJSON' = evalAM $ runController showJSON

$(viewDataType "list")
-- data Project_list' = Project_list' {project_list :: [Project_list]}
-- data Project_list = Project_list {name :: String} deriving Show
--, startAt :: XmlTree, endAt :: XmlTree, workMin :: XmlTree, workMax :: XmlTree, workers :: Workers} deriving Show
--data Workers = Workers {user_name :: XmlTree} deriving Show

instance Templatable Project_list where
    getParams = const [("name", constText . name)]

instance Templatable Project_list' where
    getParams = undefined
    showC (Project_list' l)  = arrL (zip l . repeat) >>> arr2A showC

instance Snippet Project_list' where
    snippet = do
        projects <- findMany (undefined::Project.Project) [1..10]
        return $ Project_list' $ map (\x -> Project_list {name = Project.description x}) projects
    asXhtml = showC

list' :: XmlSnippet
list' = snipp (snippet::EnvController Project_list')
-}


