{-# language TypeFamilies #-}

module Language.Static
  ( module S
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

type Static e ast a = StaticT Identity e ast a

checkWithT :: (Monad m, AST ast)
          => (ast -> StaticT m e ast a)
          -> ast
          -> m (Either (StaticError e) a)
checkWithT f = checkStaticT <*> f

checkT :: (Monad m, AST ast)
      => ast
      -> m (Either (StaticError e) (TypeFor ast))
checkT = checkWithT infer

check :: AST ast => ast -> Either (StaticError e) (TypeFor ast)
check = runIdentity . checkT
