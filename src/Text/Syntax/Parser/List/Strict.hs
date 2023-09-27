{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}

-- |
-- Module      : Text.Syntax.Parser.List.Strict
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a strict parser implementation for "Text.Syntax.Poly".
module Text.Syntax.Parser.List.Strict (
  -- * Syntax instance Parser type
  Parser, runParser, Result(..), ErrorStack,
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (IsoAlternative, Syntax (token))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorStack)
import Control.Applicative (Alternative(..))

-- | Result type of 'Parser'
data Result a tok = Good !a ![tok] | Bad !ErrorStack

-- | Naive 'Parser' type. Parse @[tok]@ into @alpha@.
newtype Parser tok alpha =
  Parser {
    -- | Function to run parser
    runParser :: [tok] -> ErrorStack -> Result alpha tok
    }

instance Functor (Parser tok) where
instance Applicative (Parser tok) where
instance Monad (Parser tok) where
  return !a = Parser $ \s _ -> Good a s
  Parser !p >>= fb = Parser (\s e -> case p s e of
                                Good a s'   -> case runParser (fb a) s' e of
                                  !rv -> rv
                                Bad e'      -> Bad $ e' ++ e)
instance MonadFail (Parser tok) where
  fail msg  = Parser (\_ e -> Bad $ ErrorString msg : e)

instance Alternative (Parser tok) where
instance MonadPlus (Parser tok) where
  mzero = Parser $ const Bad
  Parser p1 `mplus` p2' =
    Parser (\s e -> case p1 s e of
               (Bad e')        -> case runParser p2' s e' of
                 !rv -> rv
               good@(Good _ _) -> good)


instance Eq tok => Syntax tok (Parser tok) where
  token = Parser (\s e -> case s of
                     t:ts -> Good t ts
                     []   -> Bad $ ErrorString "eof" : e)

-- | Run 'Syntax' as @'Parser' tok@.
runAsParser :: Eq tok => RunAsParser tok a ErrorStack
runAsParser parser s = case runParser parser s [] of
  Good x []    -> Right x
  Good _ (_:_) -> Left  [ErrorString "Not the end of token stream."]
  Bad  err     -> Left  err
