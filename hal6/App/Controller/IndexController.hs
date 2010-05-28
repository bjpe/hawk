module App.Controller.IndexController where

import App.View.IndexView

import App.Model.User

import Hawk.Controller
import Hawk.View hiding (select)
import Hawk.Model

routes :: [Routing]
routes = 
  [ ("index",indexAction >>= render (typedView "index" indexXhtml))
  , ("save",saveAction >> redirectToAction "index" "index")
  , ("login",loginAction >> redirectToAction "index" "index")] 
  ++ combine (authF' "You are not logged in." "index" "index")
  [ ("logout",logoutAction >> redirectToAction "index" "index")
  , ("delete",deleteAction >> redirectToAction "index" "logout")
  ]

indexAction :: StateController ([User], Maybe User)
indexAction = do 
  l <- select (setOrder [desc "_id"] newCriteria)
  a <- isAuthed 
  if a then getUser >>= (\u -> return (l,Just u))
    else return (l, Nothing)

saveAction :: StateController ()
saveAction = isAuthed 
         >>= mkUser 
         >>= updateUser
         >>= saveToDB
  where mkUser True = getUser
        mkUser False = new :: StateController User
        updateUser u = getParams >>= updateAndValidate u ""
        saveToDB (u, err) | null err = isAuthed >>= insOrUpd
                          | otherwise = setErrors "saveError" err
                          where insOrUpd True = update u 
                                             >> setFlash "success" "You've updated your Settings."
                                insOrUpd False = insert u
                                             >> setFlash "success" "You're registrated to HaL."

loginAction :: StateController ()
loginAction = tryLogin >>= showFlash

logoutAction :: StateController ()
logoutAction = logout

deleteAction :: StateController ()
deleteAction = do
  a <- isAuthed
  if a then getUser >>= delete >>= showDelete
   else setFlash "error" "You are not logged in."
    where showDelete True = setFlash "success" "Your Registration is canceled."
          showDelete False = setFlash "error" "Failed to cancel your Registration."

-- private
getUser :: StateController User
getUser = isAuthedAs >>= (\u -> selectOne $ restrictionCriteria $ (val u) .==. (col "username"))

showFlash :: AuthResult -> StateController ()
showFlash AuthSuccess = setFlash "success" "You're logged in."
showFlash (AuthFailureUnknown s) = setFlash "error" ("Authentication Failure: " ++ s)
showFlash AuthFailureIdNotFound = setFlash "notice" "The given ID doen't exist."
showFlash AuthFailureAmbiguousId = setFlash "error" "Your ID is not unique, please contact the webmaster."
showFlash AuthFailureInvalidCredential = setFlash "notice" "You mistyped your password."
