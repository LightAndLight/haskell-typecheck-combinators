{-# language TypeFamilies #-}

module Language.Static.Combinators
  ( given
  , oneOf
  , op2
  , op2p
  , require
  , run
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable

import Language.Static.Error
import Language.Static.Type

given :: (Monad m, AST ast) => ast -> StaticT m e ast ty -> StaticT m e ast ty
given ast = local (const ast)

oneOf :: (Monad m, AST ast) => [StaticT m e ast ()] -> StaticT m e ast ()
oneOf [] = throwError NoRules
oneOf (r:rs) = catchError r handler
  where
    handler TypeMismatch{} = r
    handler _ = oneOf rs

op2 :: (Monad m, AST ast)
    => TypeFor ast
    -> ast
    -> ast
    -> StaticT m e ast (TypeFor ast)
op2 ty left right = do
  require left $ hasType ty
  require right $ hasType ty

op2p :: (Monad m, AST ast)
     => ast
     -> ast
     -> StaticT m e ast (TypeFor ast)
op2p left right = infer left >>= require right . hasType

require :: (Monad m, AST a, AST b)
        => a
        -> StaticT m e a (TypeFor a)
        -> StaticT m e b (TypeFor a)
require ast rule = StaticT . ExceptT . ReaderT $ const (checkStaticT ast rule)

run :: (Monad m, AST a, AST b)
    => (a -> StaticT m e a (TypeFor a))
    -> a
    -> StaticT m e b (TypeFor a)
run f = require <*> f
