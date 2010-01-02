module Hawk.Controller.Auth.ResultType where

data AuthResult = AuthSuccess
                | AuthFailureUnknown String -- can contain a sql exception string
                | AuthFailureIdNotFound
                | AuthFailureAmbiguousId
                | AuthFailureInvalidCredential

