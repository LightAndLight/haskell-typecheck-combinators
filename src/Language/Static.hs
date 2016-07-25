{-# language TypeFamilies #-}

module Language.Static
  ( module S
  , Static
  , checkWithT
  , checkT
  , check
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity

import Language.Static.Combinators as S
import Language.Static.Error as S
import Language.Static.Type as S

type Static s e ast a = StaticT s e ast Identity a

checkWithT :: (Monad m, AST ast)
          => (ast -> StaticT s e ast m a)
          -> s
          -> ast
          -> m (Either (StaticError e) a)
checkWithT f s = checkStaticT s <*> f

checkT :: (Monad m, AST ast)
      => EnvFor ast
      -> ast
      -> m (Either (StaticError (ErrorFor ast)) (TypeFor ast))
checkT = checkWithT infer

check :: AST ast => EnvFor ast -> ast -> Either (StaticError (ErrorFor ast)) (TypeFor ast)
check s = runIdentity . checkT s
