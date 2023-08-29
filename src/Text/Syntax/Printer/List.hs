{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Text.Syntax.Printer.List
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a naive printer implementation for 'Syntax'.
module Text.Syntax.Printer.List (
  -- * Syntax instance Printer type
  Printer, runPrinter,
  -- * Print action
  printM,
  -- * Poly-morphic wrapper of runPrinter
  RunAsPrinter, RunAsStringPrinter, runAsPrinter
  ) where

import Control.Isomorphism.Partial (IsoFunctor ((/$/)), unapply)
import Control.Applicative

import Text.Syntax.Poly.Class
  (ProductFunctor ((/*/)), TryAlternative, IsoAlternative(..),
   AbstractSyntax (syntax), Syntax (token))
import Text.Syntax.Poly.Type (ErrorString, errorString)
import qualified Text.Syntax.Poly.Type as T
import Control.Monad.Fail (MonadFail)
import Control.Monad ((>=>),liftM2,mplus)

-- | Naive 'Printer' type. Print @alpha@ into @tok@.
-- | NEW PAPER: not list. anything monoidal!
newtype Printer tok alpha =
  Printer {
    -- | Function to run printer
    runPrinter :: alpha -> Maybe tok
    }

-- Expect print side effect.
printM :: MonadFail m => Printer tok alpha -> alpha -> m tok
printM p x = maybe (fail "print error") return $ runPrinter p x

-- | 'IsoFunctor' instance for 'Printer'. Unapplying 'Iso' and print.
instance IsoFunctor (Printer tok) where
  iso /$/ Printer p
    = Printer (unapply iso >=> p)

-- | 'ProductFunctor' instance for 'Printer'. Just print sequential.
-- ACCORDING TO NEW PAPER!
instance Monoid tok => ProductFunctor (Printer tok) where
  Printer p /*/ Printer q
    = Printer (\(x, y) -> p x <> q y)

-- | 'Alternative' instance for 'Printer'. Print first or second.
instance IsoAlternative (Printer tok) where
  Printer p <||> Printer q
    -- it's mplus of function! wow...
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (const Nothing)

-- | 'TryAlternative' instance for 'Printer'. Along with default definition.
instance TryAlternative (Printer tok)

-- | 'AbstractSyntax' instance for 'Printer'. Match parsed result and success.
instance Monoid tok => AbstractSyntax (Printer tok) where
  syntax x = Printer (\y ->  if x == y
                             then Just mempty
                             else Nothing)

-- | 'Syntax' instance for 'Printer'. Print token into singleton.
instance (Eq tok,Monoid tok) => Syntax tok (Printer tok) where
  -- TODO: Might need to do the pureprinting Eq guard?
  token  = Printer Just

-- | Specialized 'RunAsPrinter' type into list.
type RunAsPrinter tok a e = T.RunAsPrinter tok tok a e

-- | Specialized 'RunAsPrinter' type into 'String'.
type RunAsStringPrinter a e = RunAsPrinter Char a e

-- | Run 'Syntax' type as 'Printer'.
runAsPrinter :: (Monoid tok,Eq tok) => RunAsPrinter tok a ErrorString
runAsPrinter printer = maybe (Left . errorString $ "print error") Right
                       . runPrinter printer

