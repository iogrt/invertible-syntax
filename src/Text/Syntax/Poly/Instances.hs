{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Text.Syntax.Poly.Instances
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains basic instance definitions for classes defined in "Text.Syntax.Poly.Class".
module Text.Syntax.Poly.Instances () where

import Control.Monad.Fail (MonadFail)
import qualified Control.Applicative as Applicative
import Control.Applicative (Alternative(..), liftA2)

import Control.Isomorphism.Partial (IsoFunctor)
import Text.Syntax.Poly.Class
  (ProductFunctor((/*/)),
   IsoAlternative((<||>), empty), TryAlternative,
   AbstractSyntax(syntax, syntaxError))

-- | 'ProductFunctor' instance on 'Applicative' context
-- which is a prerequisite for 'Syntax' definitions.
instance Applicative f => ProductFunctor f where
  (/*/) :: Applicative f => f alpha -> f beta -> f (alpha, beta)
  (/*/) = liftA2 (,)

-- | 'IsoAlternative' instance on 'MonadPlus' context
-- which is a prerequisite for 'Syntax' definitions.
instance Alternative m => IsoAlternative m where
  (<||>) = (Applicative.<|>)
  empty :: m alpha
  empty  = Applicative.empty


-- | 'AbstractSyntax' instance on 'MonadPlus' context
-- which is a prerequisite for 'Syntax' definitions.
instance (IsoFunctor m, Alternative m, TryAlternative m, MonadFail m) => AbstractSyntax m where
  syntax = return
  syntaxError = fail
