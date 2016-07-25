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
import Control.Monad.State
import Data.Foldable

import Language.Static.Error
import Language.Static.Type

given :: (Monad m, AST ast) => ast -> StaticT s e ast m ty -> StaticT s e ast m ty
given ast = local (const ast)

oneOf :: (Monad m, AST ast) => [StaticT s e ast m ()] -> StaticT s e ast m ()
oneOf [] = throwError NoRules
oneOf (r:rs) = catchError r handler
  where
    handler TypeMismatch{} = r
    handler _ = oneOf rs

op2 :: (Monad m, AST ast)
    => TypeFor ast
    -> ast
    -> ast
    -> StaticT (EnvFor ast) (ErrorFor ast) ast m (TypeFor ast)
op2 ty left right = do
  require left $ hasType ty
  require right $ hasType ty

op2p :: (Monad m, AST ast)
     => ast
     -> ast
     -> StaticT (EnvFor ast) (ErrorFor ast) ast m (TypeFor ast)
op2p left right = infer left >>= require right . hasType

require :: (Monad m, AST a, AST b)
        => a
        -> StaticT s e a m (TypeFor a)
        -> StaticT s e b m (TypeFor a)
require ast rule = StaticT . ExceptT . ReaderT $ const stateLevel
  where
    stateLevel = (runReaderT . runExceptT . runStaticT $ rule) ast

run :: (Monad m, AST a, AST b)
    => (a -> StaticT s e a m (TypeFor a))
    -> a
    -> StaticT s e b m (TypeFor a)
run f = require <*> f
