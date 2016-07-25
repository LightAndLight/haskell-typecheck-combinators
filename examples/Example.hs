{-# language TypeFamilies #-}

module Example where

import Control.Monad.Except
import Control.Monad.State
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
  type EnvFor Expr = [(String,Ty)]
  type ErrorFor Expr = String
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
  infer (Variable name ty) = do
    ty <- gets $ lookup name
    case ty of
      Nothing -> throwError . Other $ "Variable not defined"
      Just ty -> return ty
  infer (Dereference expr) = do
    t <- infer expr
    case t of
      TRef t' -> return t'
      t'      -> throwError . TypeMismatch $ "Can't dereference " ++ show t'

data Statement
  = Statements [Statement]
  | While Expr Statement
  | IfThenElse Expr Statement Statement
  | Assign Expr Expr
  | VarDecl String Ty (Maybe Expr)

instance AST Statement where
  type TypeFor Statement = ()
  type EnvFor Statement = [(String,Ty)]
  type ErrorFor Statement = String

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
  infer (VarDecl name ty right) = do
    modify ((name,ty) :)
    case ty of
      TRef ty' -> case right of
        Just expr -> void . require expr $ hasType ty'
        Nothing -> return ()
      _ -> throwError . Other $ "Variable not of type TRef"

-- bool x = false;
-- int y = 0;
-- while (x) {
--   if (y == 2)
--     x = !x;
--   else
--     y = y + 1;
-- }
success
  = Statements
      [ VarDecl "x" (TRef TBool) (Just $ Constant 0 TBool)
      , VarDecl "y" (TRef TInt) (Just $ Constant 0 TInt)
      , While
          (Dereference . Variable "x" $ TRef TBool)
          (Statements
            [ IfThenElse
                (Equals (Dereference (Variable "y" $ TRef TInt)) (Constant 2 TInt))
                (Assign
                  (Variable "x" $ TRef TBool)
                  (Not . Dereference . Variable "x" $ TRef TBool))
                (Assign (Variable "y" $ TRef TInt)
                  (Add (Dereference . Variable "y" $ TRef TInt) (Constant 1 TInt)))
            ])
      ]

failure
  = Equals (Constant 2 TInt) (Constant 2 TBool) 
