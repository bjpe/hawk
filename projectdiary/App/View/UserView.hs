module App.View.UserView where

import App.Action.Common
import App.Action.MainController
import App.Action.UserController
import App.Model.User as User
import qualified App.Model.Project as Project
import qualified App.Model.Worker as Worker
import Hawk.Controller
import Hawk.View.TemplateView
import Hawk.View.EmptyView
import Hawk.View.Template.HtmlHelper as Html

views :: [Routing]
views =  [ ("list"        , adminAuthorize listAction >>= render (TemplateView "list" listXhtml) )
         , ("edit"        , adminAuthorize editAction >>= render (TemplateView "edit" editXhtml) )
         , ("delete"      , adminAuthorize deleteAction >>= render emptyView )
         ]

listXhtml :: XmlTree -> [User] -> StateController [XmlTree]
listXhtml template' users = do
  header <- adminHeaderXhtml $ head $ template' `subTemplate` "admin.header"
  rendered <- concatMapM (oneXhtml $ head $ subTemplate template' "user") users
  return $ bind template'
    [ ("admin.header" , header)
    , ("user"   , rendered)
    ] []
  where
    oneXhtml :: XmlTree -> User -> StateController [XmlTree]
    oneXhtml template'' u = do
      projects <- getWorkers u >>= mapM Worker.getProject >>= concatMapM (\p ->
        return $ bind (head $ template'' `subTemplate` "projects")
        [("projectname", [Html.text $ Project.name p])] []
        )
      return $ bind template''
        [ ("name"        , [Html.text $ User.name u])
        , ("admin"       , [Html.text $ if admin u then "Admin" else "User"])
        , ("projects"    , projects)
        , ("editAction"  , [Html.link ("/user/edit?id=" ++ show (_id u)) [Html.image "Edit user" "/images/edit.png" []]])
        , ("deleteAction", [Html.link ("/user/delete?id=" ++ show (_id u)) [Html.image "Delete user" "/images/delete.png" []]])
        ] []


editXhtml :: XmlTree -> (User, Bool) -> StateController [XmlTree]
editXhtml template (u, isNew) = do
  header <- adminHeaderXhtml $ head $ template `subTemplate` "admin.header"
  let pwSection = if isNew then bind (head $ template `subTemplate` "ifNew")
                                [ ("password", [Html.password "password" "" []])
                                , ("confirmation", [Html.password "confirmation" "" []])
                                ] []
                           else []
  return $ bind template
    [ ("admin.header", header)
    , ("name"        , [Html.textfield "name"      (User.name u)      [("maxlength", "64")]])
    , ("admin"       , [ Html.text "Yes: ", Html.radio "admin" "True"  (admin u) []
                       , Html.text "No: " , Html.radio "admin" "False" (not $ admin u) []
                       ])
    , ("ifNew"       , pwSection)
    , ("id"          , [Html.hidden    "id"          (show $ _id u) [] | not isNew])
    ] []
