{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}


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
  Parser, runParser,
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (IsoAlternative, Syntax (token))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorString, errorString)
import Control.Applicative (Alternative(..))

-- | Naive 'Parser' type. Parse @[tok]@ into @alpha@.
-- lazy maybe parser
newtype LMParser tok alpha =
  Parser {
    -- | Function to run parser
    runParser :: [tok] -> Maybe (alpha, [tok])
    }

instance Functor (LMParser tok) where
instance Applicative (LMParser tok) where
instance Monad (LMParser tok) where
  return a = Parser $ \s -> Just (a, s)
  Parser p >>= fb = Parser (\s -> do (a, s') <- p s
                                     runParser (fb a) s')
instance MonadFail (LMParser tok) where
  fail = const mzero

instance Alternative (LMParser tok) where
instance MonadPlus (LMParser tok) where
  mzero = Parser $ const Nothing
  Parser p1 `mplus` p2' =
    Parser (\s -> p1 s `mplus` runParser p2' s)


instance Eq tok => Syntax tok (LMParser tok) where
  token = Parser (\case
                     t:ts -> Just (t, ts)
                     []   -> Nothing)

-- | Run 'Syntax' as @'Parser' tok@.
runAsParser :: Eq tok => RunAsParser tok a ErrorString
runAsParser parser s = case runParser parser s of
  Just (a, [])    -> Right a
  Just (_, _:_) -> Left . errorString $ "Not the end of token stream."
  Nothing         -> Left . errorString $ "parse error"

