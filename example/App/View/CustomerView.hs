{-# LANGUAGE TemplateHaskell #-}
module App.View.CustomerView (routes) where

import App.Controller.CustomerController
import App.Model.Customer
import qualified App.Model.Category as Cat

import Hawk.Controller
import Hawk.Model
import Hawk.View.EmptyView
import Hawk.View.TemplateView
import Hawk.View.Template.DataType
import qualified Hawk.View.Template.HtmlHelper as H

import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, toGregorian)

formatDay :: Day -> [XmlTree]
formatDay d = [H.showtext d]

$(viewDataType "Customer" "customer")
$(viewDataType "Customer" "list")

routes :: [Routing]
routes = 
  [ ("list",listAction >>= render (typedView "list" listXhtml))
  , ("edit",editAction >>= render (TemplateView "edit" editXhtml))
  , ("delete",deleteAction >>= render emptyView) ]

-- version with data type
listXhtml :: [Customer] -> StateController CustomerList
listXhtml cs = do
  cc <- mapM customerXhtml cs
  return CustomerList
    { customerCustomer = cc
    , title = [H.text "Customers"]
    , newlink = [H.textlink "/customer/edit" "New customer"]
    }

customerXhtml :: Customer -> StateController CustomerCustomer
customerXhtml c = do
  cat <- getCategory c
  return CustomerCustomer 
    { name     = firstName c ++ ' ' : lastName c
    , dob      = dateOfBirth c
    , category = [H.text $ Cat.name cat]
    , links    = [ H.textlink ("/customer/edit?id=" ++ show (_id c)) "Edit"
      , H.text " | ", H.textlink ("/customer/delete?id=" ++ show (_id c)) "Delete" ]
    }

-- version without data type
editXhtml :: XmlTree -> (Customer, Bool) 
          -> StateController [XmlTree]
editXhtml template (c, isNew) = do
  cats <- select newCriteria
  let (y, m, d) = toGregorian $ dateOfBirth c
  return $ bind template 
    [ ( "firstname", [H.textfield "firstname" (firstName c) []])
    , ( "initials", [H.textfield "initials" (fromMaybe "" $ initials c) []])
    , ( "lastname", [H.textfield "lastname" (lastName c) []])
    , ( "dateofbirth"
      , [ H.selectDay   "dateofbirth.day" d []
        , H.selectMonth "dateofbirth.month" m H.monthNames []
        , H.selectYear  "dateofbirth.year" y 1900 2009 [] ])
    , ( "category", [H.select "category" (H.options (show . Cat._id) Cat.name cats) []])
    , ( "listlink", [H.textlink "/customer/list" "Customer list"])
    , ("id", [H.hidden "id" (show $ _id c) [] | not isNew])
    ] []
