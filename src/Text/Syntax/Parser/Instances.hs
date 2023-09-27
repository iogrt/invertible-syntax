{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Text.Syntax.Parser.Instances
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains basic parsers instances for 'Syntax'.
module Text.Syntax.Parser.Instances () where

import Control.Isomorphism.Partial (IsoFunctor((/$/)))
import Control.Monad (MonadPlus (mzero))

import Control.Isomorphism.Partial.Ext.Prim (apply')
import Text.Syntax.Poly.Instances ()
import Control.Applicative
import Text.Syntax.Poly

-- Both useful instances for printers and parsers here
-- Parser implementation needs:
   -- MonadPlus and Syntax (token function)
-- Printer implementation needs:
  -- Monoid and Syntax (token function)
-- TODO: nvm this is simply not possible because printing can be generalized but needs 

-- ALL OF THIS IS DEPRECATED, WILL BE IMPLEMENTED IN Generic for printer and parser for better safeties.

instance MonadPlus m => IsoFunctor m where
  iso /$/ p = p >>= maybe mzero return . apply' iso