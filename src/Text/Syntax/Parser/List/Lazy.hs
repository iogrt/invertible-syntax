{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Text.Syntax.Parser.List.Lazy
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module includes a lazy parser implementation for "Text.Syntax.Poly".
module Text.Syntax.Parser.List.Lazy (
  -- * Syntax instance Parser type
  Parser, runP , ErrorStack,
  -- * Poly- morphic wrapper of runParser
  runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative(Alternative(..))

import Text.Syntax.Parser.Generic
import Text.Syntax.Poly.Class
  (IsoAlternative, Syntax (..))
import Text.Syntax.Parser.List.Type (RunAsParser, ErrorStack, ErrorString)

newtype ErrorStacker a = ErrorStacker {
    unErrorStacker :: Either [String] a
  } --deriving (Functor,Applicative,Monad...)

instance Functor ErrorStacker where -- implement with deriving pls
instance Applicative ErrorStacker where -- implement
instance Monad ErrorStacker where
  ErrorStacker a >>= fb = case a of
    Right r -> fb r
    Left l -> ErrorStacker $ Left l

instance Alternative ErrorStacker where
  empty = ErrorStacker $ Left []
  ErrorStacker ea <|> ErrorStacker eb = case (ea,eb) of
    (Left a, Left b) -> ErrorStacker $ Left (a<>b)
    (_, r) -> ErrorStacker r

instance MonadPlus ErrorStacker where
  mzero = empty
  mplus = (<|>)


instance Syntax tok (Parsing [tok] ErrorStacker) where
  token = Parsing (\case
                     t:ts -> ErrorStacker $ Right (t, ts)
                     []   -> ErrorStacker $ Left ["The end of token stream."])

runAsParser :: (Eq tok, Show tok, Show a) => RunAsParser tok a ErrorStack
runAsParser parser s = case unErrorStacker <$> runParser parser s of
  Right (a,[]) -> a
  Right (a, _:s') -> Left $ "Not the end of token stream, tokens missing: " ++ show s'
                ++ "\n\nand the content parsed:" ++ show a
  Left [] -> Left $ "unspecified parse error (use syntaxError)"
  Left x -> Left x