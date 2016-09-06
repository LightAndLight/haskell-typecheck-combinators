{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}

module Language.Static.Type
  ( AST(..)
  , StaticT(..)
  , checkStaticT
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Language.Static.Error

newtype StaticT m e ast a
  = StaticT { runStaticT :: ExceptT (StaticError e) (ReaderT ast m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError (StaticError e)
    , MonadReader ast
    )

checkStaticT :: (Monad m, AST ast)
          => ast
          -> StaticT m e ast a
          -> m (Either (StaticError e) a)
checkStaticT ast rule = (runReaderT . runExceptT . runStaticT $ rule) ast


class AST ast where
  type TypeFor ast
  hasType :: Monad m => TypeFor ast -> StaticT m e ast (TypeFor ast)
  infer :: Monad m => ast -> StaticT m e ast (TypeFor ast)

