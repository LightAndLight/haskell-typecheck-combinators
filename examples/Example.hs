{-# language TypeFamilies #-}

module Example where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Language.Static

data Ty = TInt | TBool | TRef Ty deriving (Eq, Show)

data Expr
  = Add Expr Expr
  | Or Expr Expr
  | Equals Expr Expr
  | Not Expr
  | Constant Int Ty
  | Dereference Expr
  | Variable String Ty

instance AST Expr where
  type TypeFor Expr = Ty
  hasType ty = do
    ty' <- infer =<< ask
    if ty == ty'
      then return ty
      else throwError . TypeMismatch $ "Expected " ++ show ty ++ ", got " ++ show ty'

  infer (Add left right) = op2 TInt left right
  infer (Or left right) = op2 TBool left right
  infer (Equals left right) = do
    op2p left right
    return TBool
  infer (Not exp) = require exp $ hasType TBool
  infer (Constant _ ty) = return ty
  infer (Variable _ ty) = return ty
  infer (Dereference expr) = do
    t <- infer expr
    case t of
      TRef t' -> return t'
      _       -> throwError InferenceError

data Statement
  = Statements [Statement]
  | While Expr Statement
  | IfThenElse Expr Statement Statement
  | Assign Expr Expr

instance AST Statement where
  type TypeFor Statement = ()

  hasType _ = return ()

  infer (Statements sts) = traverse_ infer sts
  infer (While cond st) = do
    require cond $ hasType TBool
    infer st
  infer (Assign left right) = do
    t <- run infer right
    void . require left $ hasType (TRef t)
  infer (IfThenElse cond one two) = do
    require cond $ hasType TBool
    infer one
    infer two

success
  = While
      (Dereference . Variable "x" $ TRef TBool)
      (Statements
        [ IfThenElse
            (Equals (Dereference (Variable "y" $ TRef TInt)) (Constant 2 TInt))
            (Assign (Variable "x" $ TRef TBool) (Not . Dereference . Variable "x" $ TRef TBool))
            (Assign (Variable "y" $ TRef TInt)
              (Add (Dereference . Variable "y" $ TRef TInt) (Constant 1 TInt)))
        ])

failure
  = Equals (Constant 2 TInt) (Constant 2 TBool) 
