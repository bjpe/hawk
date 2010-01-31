module App.Action.ProjectController where

import App.Action.Common
import App.Action.MainController
import App.Model.Project as Project
import qualified App.Model.Worker as Worker
import qualified App.Model.User as User
import qualified App.Model.Area as Area
import Hawk.Controller
import Hawk.Model
import qualified Hawk.View.Template.HtmlHelper as Html
import Hawk.View.TemplateView
import Hawk.View.EmptyView
import Hawk.Controller.Util.Monad
import Data.List ((\\))
import Data.Maybe (isNothing)

controllers :: [Routing]
controllers =  [ ("list"        , adminAuthorize listAction >>= render (TemplateView "list" listXhtml ))
               , ("edit"        , adminAuthorize editAction >>= render (TemplateView "edit" editXhtml ))
               , ("show"        , adminAuthorize showAction >>= render (TemplateView "show" showXhtml ))
               , ("delete"      , adminAuthorize deleteAction >>= render emptyView)
               , ("clear"       , adminAuthorize clearAction >>= render emptyView )
               , ("addworker"   , adminAuthorize addworkerAction >>= render emptyView )
               , ("removeworker", adminAuthorize removeworkerAction >>= render emptyView )
               ]

listAction :: StateController [Project]
listAction = select newCriteria

editAction :: StateController (Project, Bool)
editAction = do
  (project', isNew) <- findOrCreate
  method <- getRequestMethod
  case method of
    POST -> do
      (p, errs) <- getParams >>= updateAndValidate project' ""
      if null errs then do
        if isNew then insert p else update p
        setFlash "notice" "Your changes have been saved"
        redirectToAction "project" "list"
        else do
          setErrors "project" errs
          return (p, isNew)
    _ -> return (project', isNew)

showAction :: StateController Project
showAction = do
  project' <- readParam "id" >>= liftMaybe findMaybe
  case project' of
    Nothing -> do
      setFlash "error" "the requested project could not be found"
      redirectToAction "project" "list"
    Just p  -> return p


listXhtml :: XmlTree -> [Project] -> StateController [XmlTree]
listXhtml template projects = do
  header <- adminHeaderXhtml $ head $ template `subTemplate` "admin.header"
  renderedProjects <- concatMapM (oneXhtml $ head $ subTemplate template "project") projects
  return $ bind template
    [ ("admin.header" , header)
    , ("project", renderedProjects)
    ] []
  where
    oneXhtml :: XmlTree -> Project -> StateController [XmlTree]
    oneXhtml template' p = do
      wMin <- workMin p
      wMax <- workMax p
      comp <- Project.completeness p
      c <- color p
      workers <- getWorkers p >>= mapM Worker.getUser >>= concatMapM (\u -> return $ bind (head $ template `subTemplate` "workers") [("user_name", [Html.text $ User.name u])] [])
      return $ bind template'
        [ ("name"        , [Html.text $ Project.name p])
        , ("startAt"     , [Html.showtext $ startAt p])
        , ("endAt"       , [Html.showtext $ endAt   p])
        , ("workMin"     , [Html.showtext wMin])
        , ("workMax"     , [Html.showtext wMax])
        , ("completeness", [completenessTag c $ round comp])
        , ("workers"     , workers)
        , ("showAction"  , [Html.link ("/project/show?id=" ++ show (_id p)) [Html.image "Show details" "/images/details.png" []]])
        , ("editAction"  , [Html.link ("/project/edit?id=" ++ show (_id p)) [Html.image "Edit project" "/images/edit.png" []]])
        , ("deleteAction", [Html.link ("/project/delete?id=" ++ show (_id p)) [Html.image "Delete project" "/images/delete.png" []]])
        , ("clearAction" , [Html.link ("/project/clear?id=" ++ show (_id p)) [Html.image "Clear project" "/images/setback.png" []]])
        ] []


editXhtml :: XmlTree -> (Project, Bool) -> StateController [XmlTree]
editXhtml template (p, isNew) = do
  header <- adminHeaderXhtml $ head $ template `subTemplate` "admin.header"
  return $ bind template
    [ ("admin.header", header)
    , ("prefix"     , [Html.textfield "prefix"      (prefix p)      [("maxlength", "10")]])
    , ("description", [Html.textfield "description" (description p) [("maxlength", "64")]])
    , ("startAt"    , Html.selectDate "startAt"     (startAt p) Html.monthNames [])
    , ("endAt"      , Html.selectDate "endAt"       (endAt p)   Html.monthNames [])
    , ("id"         , [Html.hidden    "id"          (show $ _id p) [] | not isNew])
    ] []

showXhtml :: Html.XmlTree -> Project -> StateController [Html.XmlTree]
showXhtml template p = do
  header <- adminHeaderXhtml $ head $ template `subTemplate` "admin.header"
  workers <- getWorkers p
  workingUsers' <- mapM Worker.getUser workers
  allUsers <- select newCriteria
  let newUsers = allUsers \\ workingUsers'
  wMin <- workMin p
  wMax <- workMax p
  wNow <- workNow p
  comp <- Project.completeness p
  c <- color p
  renderedWorkers <- concatMapM (\w -> do
    u <- Worker.getUser w
    removeUrl <- actionUrl "project" "removeworker" [("id", show $ Worker._id w)]
    return $ bind (head $ template `subTemplate` "workers")
     [ ("user_name"   , [Html.text $ User.name u])
     , ("removeAction", [Html.link removeUrl [Html.image "Remove worker" "/images/member-remove.png" []]])
     ] []
    ) workers
  return $ bind template
    [ ("admin.header", header)
    , ("name"        , [Html.text $ Project.name p])
    , ("startAt"     , [Html.showtext $ startAt p])
    , ("endAt"       , [Html.showtext $ endAt   p])
    , ("workMin"     , [Html.showtext wMin])
    , ("workMax"     , [Html.showtext wMax])
    , ("workNow"     , [Html.showtext wNow])
    , ("completeness", [completenessTag c $ round comp])
    , ("workers"     , renderedWorkers)
    , ("userselect"  , [Html.select "user.id" (Html.options (show . User._id) User.name newUsers) []])
    , ("id"          , [Html.hidden "id" (show $ _id p) []])
    , ("loginAction" , [Html.textlink ("/admin/switchtoproject?id=" ++ show (_id p)) "Login in to project"])
    , ("editAction"  , [Html.textlink ("/project/edit?id=" ++ show (_id p)) "Edit Project"])
    , ("listAction"  , [Html.textlink "/project/list" "Back"])
    ] []

deleteAction :: StateController ()
deleteAction = do
  (project', isNew) <- findOrCreate :: StateController (Project, Bool)
  if isNew then setFlash "error" "The requested project does not exist"
    else do
      deleteCascading project'
      setFlash "notice" "The project has been deleted"
  redirectToAction "project" "list"

clearAction :: StateController ()
clearAction = do
  (project', isNew) <- findOrCreate
  if isNew then setFlash "error" "The requested project does not exist"
    else do
      getAreas project' >>= mapM Area.deleteCascading
      setFlash "notice" "The project has been cleared"
  redirectToAction "project" "list"

addworkerAction :: StateController ()
addworkerAction = do
  (project', isNew) <- findOrCreate
  user <- readParam "user.id" >>= liftMaybe findMaybe
  if isNew || isNothing user then setFlash "error" "The user could not be assigned"
    else do
      new >>= flip Worker.setUser user >>= flip Worker.setProject (Just project') >>= insert
      setFlash "notice" "The user has been assigned"
  redirectTo "project" "show" [("id", show $ _id project')]

removeworkerAction :: StateController ()
removeworkerAction = do
  (worker, isNew) <- findOrCreate :: StateController (Worker.Worker, Bool)
  if isNew then do
    setFlash "error" "The requested worker does not exist"
    redirectToAction "project" "list"
    else do
      delete worker
      setFlash "notice" "The worker has been deleted"
      p <- Worker.getProject worker
      redirectTo "project" "show" [("id", show $ _id p)]

{-
# =ProjectController
# Dieser Controller enthält Methoden zum Modifizieren und Verwalten von Projekten
# (Administratorbereich)
class ProjectController < ApplicationController

  layout 'admin'
  before_filter :admin_authorize

  # GETs should be safe (see http://www.w3.org/2001/tag/doc/whenToUseGet.html)
  verify :method => :post, :only => [ :destroy, :create, :update ],
         :redirect_to => { :action => :list }

  # zeigt eine Übersicht der Projekte an       
  def index
    list
    render :action => 'list'
  end

  # zeigt eine Liste der Projekte an
  def list
    @project_pages, @projects = paginate :projects, :per_page => 10
  end

  # zeigt die Details eines Projekts an
  def show
    @project = Project.find(params[:id])
    @users = User.find(:all) - @project.workers.collect {|w| w.user}
  end

  # Formular zum Erstellen eines neuen Projekts
  def new
    @project = Project.new
  end

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

  # Formular zum Ändern von Projekteigenschaften
  def edit
    @project = Project.find(params[:id])
  end

  # Speichert die Änderungen am Projekt
  def update
    @project = Project.find(params[:id])
    if @project.update_attributes(params[:project])
      flash[:notice] = 'Das Projekt wurde erfolgreich aktualisiert.'
      redirect_to :action => 'show', :id => @project
    else
      render :action => 'edit'
    end
  end

  # Löscht ein Projekt
  def destroy
    Project.find(params[:id]).destroy
    redirect_to :action => 'list'
  end

  # Fügt einem Projekt einen neuen Mitarbeiter hinzu
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
  
  # Zurücksetzen eines Projekts
  def clear
    project = Project.find(params[:id])
    project.areas.each {|area| area.destroy}
    project.changes.each {|change| change.destroy}    
    index
  end
end
-}
