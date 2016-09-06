module Language.Static.Error
  ( StaticError(..)
  ) where

data StaticError e
  = NoRules
  | TypeMismatch String
  | InferenceError
  | Other e
  deriving (Eq, Show)

