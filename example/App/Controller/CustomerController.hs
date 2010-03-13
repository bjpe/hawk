{-# LANGUAGE TemplateHaskell #-}
module App.Controller.CustomerController 
  ( listAction
  , editAction
  , deleteAction
  ) where

import App.Model.Customer
import Hawk.Controller
import Hawk.Model

-- list all customers
listAction :: StateController [Customer]
listAction = select newCriteria

-- edit a single customer
editAction :: StateController (Customer, Bool)
editAction = do
  (customer, isNew) <- findOrCreate
  method <- getRequestMethod
  case method of
    POST -> do
      (c, errs) <- getParams >>= updateAndValidate customer ""
      if null errs then do
        catId <- readParam "category"
        let c' = c { categoryId = catId }
        if isNew then insert c' else update c'
        setFlash "notice" "Your changes have been saved"
        redirectToAction "customer" "list"
        else do
          setErrors "customer" errs
          return (c, isNew)
    _ -> return (customer, isNew)

-- delete a customer
deleteAction :: StateController ()
deleteAction = do
  (customer, isNew) <- findOrCreate
  if isNew then setFlash "error" "The requested customer does not exist"
    else do
      delete customer
      setFlash "notice" "The customer has been deleted"
  redirectToAction "customer" "list"

-- retrieve the customer to be edited or deleted
findOrCreate :: StateController (Customer, Bool)
findOrCreate = do
  customer <- readParam "id" >>= liftMaybe findMaybe
  case customer of
    Nothing -> do
      newCustomer <- new
      return (newCustomer, True)
    Just c  -> return (c, False)
