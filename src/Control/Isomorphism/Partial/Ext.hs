-- |
-- Module      : Control.Isomorphism.Partial.Ext
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown

module Control.Isomorphism.Partial.Ext (
  -- * Original definitions
  module Control.Isomorphism.Partial,

  -- * Operator to compose Iso
  module Control.Category,

  -- * Additional definitions
  module Control.Isomorphism.Partial.Ext.Prim,
  module Control.Isomorphism.Partial.Ext.Constructors,
  module Control.Isomorphism.Partial.Ext.Data,
  (/$/)
  ) where

import Control.Isomorphism.Partial
import qualified Control.Isomorphism.Partial as PartialIso

import Control.Category ((.))

import Control.Isomorphism.Partial.Ext.Prim
import Control.Isomorphism.Partial.Ext.Constructors
import Control.Isomorphism.Partial.Ext.Data

-- rename IsoFunctor infix, the infix operators were inspired from the 'syntax' package.
(/$/) :: IsoFunctor f => Iso alpha beta -> f alpha -> f beta
(/$/) = (PartialIso.<$>)
infixr 6 /$/

{-# ANN module "ignore import/export shortcut" #-}
