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
  (ProductFunctor ((/*/)), IsoAlternative(..),
   AbstractSyntax (..), Syntax(..))
import Text.Syntax.Poly.Type
import Control.Monad.Fail (MonadFail)
import Control.Monad ((>=>),liftM2,mplus)

-- List specific. TODO: make the generic one like Parsing is
newtype Printer tok alpha =
  Printer {
    -- | Function to run printer
    runPrinter :: alpha -> Maybe [tok]
    }

-- Expect print side effect.
printM :: MonadFail m => Printer tok alpha -> alpha -> m [tok]
printM p x = maybe (fail "print error") return $ runPrinter p x

-- | 'IsoFunctor' instance for 'Printer'. Unapplying 'Iso' and print.
instance IsoFunctor (Printer tok) where
  iso /$/ Printer p
    = Printer (unapply iso >=> p)

-- | 'ProductFunctor' instance for 'Printer'. Just print sequential.
instance ProductFunctor (Printer tok) where
  Printer p /*/ Printer q
    = Printer (\(x, y) -> liftA2 (<>) (p x) (q y))

-- | 'Alternative' instance for 'Printer'. Print first or second.
instance IsoAlternative (Printer tok) where
  Printer p /+/ Printer q
    -- it's mplus of function! wow...
    = Printer (\s -> p s <|> q s)
  emptyIso = Printer (const Nothing)

-- | 'AbstractSyntax' instance for 'Printer'. Match parsed result and success.
instance AbstractSyntax (Printer tok) where
  syntax x = Printer (\y ->  if x == y
                             then Just [] 
                             else Nothing)
  syntaxError s = Printer (const Nothing)
-- | 'Syntax' instance for 'Printer'. Print token into singleton.
instance Syntax tok (Printer tok) where
  token  = Printer (\x -> Just [x])

-- | Specialized 'RunAsPrinter' type into 'String'.
type RunAsStringPrinter a e = RunAsPrinter Char String a e

-- | Run 'Syntax' type as 'Printer'.
runAsPrinter :: Eq tok => RunAsPrinter tok [tok] a SyntaxError
runAsPrinter printer = maybe (Left . ErrorString $ "print error") Right
                       . runPrinter printer