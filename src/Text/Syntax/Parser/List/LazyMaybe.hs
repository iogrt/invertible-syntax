{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
--remove

{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Text.Syntax.Parser.List.LazyMaybe
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a lazy parser implementation for "Text.Syntax.Poly". Result does not have error informations.
module Text.Syntax.Parser.List.LazyMaybe (
  -- * Syntax instance Parser type
  runParser,
  -- * Poly- morphic wrapper of runParser
  runAsParser,
  go
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))

import Text.Syntax.Parser.Generic
import Text.Syntax.Poly.Class
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorString, errorString)
import Control.Applicative (Alternative(..))



--remove
import Text.Syntax.Poly.Combinators
import Control.Isomorphism.Partial.Ext
import Prelude hiding ((.))

-- | Naive 'Parser' type
instance Syntax tok (Parsing [tok] Maybe) where
  token = Parsing (\case
                     t:ts -> Just (t, ts)
                     []   -> Nothing)

-- | Run 'Syntax' as @'Parser' tok@.
runAsParser :: RunAsParser tok a ErrorString
runAsParser parser s = case runParser parser s of
  Just (a, [])    -> Right a
  Just (_, _:_) -> Left . errorString $ "Not the end of token stream."
  Nothing         -> Left . errorString $ "parse error"