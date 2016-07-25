{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Language.Static.Type
  ( AST(..)
  , StaticT(..)
  , checkStaticT
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans

import Language.Static.Error

newtype StaticT s e ast m a
  = StaticT { runStaticT :: ExceptT (StaticError e) (ReaderT ast (StateT s m)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError (StaticError e)
    , MonadReader ast
    )

instance MonadTrans (StaticT s e ast) where
  lift = StaticT . lift . lift . lift

instance Monad m => MonadState s (StaticT s e ast m) where
  get = StaticT get
  put = StaticT . put

checkStaticT :: (Monad m, AST ast)
          => s
          -> ast
          -> StaticT s e ast m a
          -> m (Either (StaticError e) a)
checkStaticT s ast rule = evalStateT ((runReaderT . runExceptT . runStaticT $ rule) ast) s


class AST ast where
  type TypeFor ast
  type EnvFor ast
  type ErrorFor ast
  hasType :: Monad m
          => TypeFor ast
          -> StaticT (EnvFor ast) (ErrorFor ast) ast m (TypeFor ast)
  infer :: Monad m
        => ast
        -> StaticT (EnvFor ast) (ErrorFor ast) ast m (TypeFor ast)



