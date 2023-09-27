{-# LANGUAGE Rank2Types #-}

-- |
-- Module      : Text.Syntax.Poly.Type
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains handy Rank2 type synonyms which has 'Syntax' contexts.
module Text.Syntax.Poly.Type (
  -- * Syntax type
  SyntaxT,
  -- * Type to run syntax as Parser \/ Printer.
  RunAsParser, RunAsParserM,
  RunAsPrinter, RunAsPrinterM,
  -- * Error type
  SyntaxError(..)
  ) where

import Text.Syntax.Poly.Class (Syntax)

-- | Type synonym for 'Syntax' includes contexts. Rank2Types extension is needed.
type SyntaxT tok a = forall delta . Syntax tok delta => delta a

-- | Type to run syntax as parser
type RunAsParser     tok tks a e = SyntaxT tok a -> tks -> Either e a
-- | Same as 'RunAsParser' other than with computation @m@
type RunAsParserM  m tok tks a e = SyntaxT tok a -> tks -> m (Either e a)

-- | Type to run syntax as printer
type RunAsPrinter    tok tks a e = SyntaxT tok a -> a   -> Either e tks
-- | Same as 'RunAsPrinter' other than with computation @m@
type RunAsPrinterM m tok tks a e = SyntaxT tok a -> a   -> m (Either e tks)

-- | String type which is 'Show' instance not to show but just return String
-- alpha is unprocessed, beta is processed
data SyntaxError =
  ErrorString String 
  | EndOfStream 
  | UnspecifiedError 

instance Show SyntaxError where
  show s = show $ case s of
    ErrorString s -> s
    EndOfStream -> "Reached the end of the token stream"
    UnspecifiedError -> "Unspecified error occured, use `syntaxError`"
