module App.Controller.GuestbookController (routes) where

import App.Model.GuestbookEntry
import qualified App.View.GuestbookView as GV

import Hawk.Controller
import Hawk.Model
import Hawk.View.TemplateView

routes :: [Routing]
routes = 
  [ ("show",indexAction >>= render (typedView "show" GV.showXhtml))
  , ("index",indexAction >>= render (typedView "show" GV.showXhtml))
  , ("insert",insertAction >> redirectToAction "guestbook" "index")
  , ("delete",deleteAction >> redirectToAction "guestbook" "index") ]

indexAction :: StateController [GuestbookEntry]
indexAction = select (setOrder [desc "_id"] newCriteria)

insertAction :: StateController ()
insertAction = do
  method <- getRequestMethod
  case method of
    POST -> do
      n <- new :: StateController GuestbookEntry
      (ge, errs) <- getParams >>= updateAndValidate n ""
      if null errs then do
        insert ge
        setFlash "notice" "Successfully added your message!"
        else
          setErrors "guestbookEntry" errs
      return ()
    _ -> return ()

deleteAction :: StateController ()
deleteAction = do
  guestbookEntry <- readParam "id" >>= liftMaybe findMaybe :: StateController (Maybe GuestbookEntry)
  case guestbookEntry of
    Nothing -> 
      setFlash "error" "The requested customer does not exist"
    Just ge -> do
      delete ge
      setFlash "notice" "The guestbook entry has been deleted"
  return ()

