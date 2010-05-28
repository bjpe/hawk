module Hawk.Controller.Auth.ResultType (AuthResult (..)) where

data AuthResult = AuthSuccess
                | AuthFailureUnknown String -- can contain a sql exception string
                | AuthFailureIdNotFound
                | AuthFailureAmbiguousId
                | AuthFailureInvalidCredential

