{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

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
module Text.Syntax.Parser.List (
  -- * Poly- morphic wrapper of runParser
  runAsParser,
  ErrorStacker(..) -- temporary
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative(Alternative(..))

import Text.Syntax.Poly
import Text.Syntax.Parser.Generic
import Text.Syntax.Poly.Class
  (IsoAlternative, Syntax (..))

newtype ErrorStacker a = ErrorStacker {
    -- toks = remaining tokens when error occured
    unErrorStacker :: Either [SyntaxError] a
  } deriving (Functor,Applicative,Monad)

instance Alternative ErrorStacker where
  empty = ErrorStacker $ Left [ErrorString "debug: using empty"]
  ErrorStacker ea <|> ErrorStacker eb = case (ea,eb) of
    -- TODO: make this less verbose, make sure tests stay ok
    (Left a, Left b) -> ErrorStacker $ Left (a<>b)
    (Left a, r) -> ErrorStacker r
    (Right r, _) -> ErrorStacker (Right r)

instance MonadPlus ErrorStacker where
  mzero = empty
  mplus = (<|>)


-- TODO: could be done as a for some kind of foldable class? would a tuple be able to be processed?
instance Syntax tok (Parsing [tok] ErrorStacker) where
  token = Parsing (\case
                     t:ts -> ErrorStacker $ Right (t, ts)
                     []   -> ErrorStacker $ Left [EndOfStream])

runAsParser :: (Eq tok, Show tok, Show a) => RunAsParser tok [tok] a [SyntaxError]
runAsParser parser s = 
  case unErrorStacker $ runParser parser s of
    Right (a,[]) -> Right a
    -- TODO: extract this into general errorstacker printing, once errorstacker is generalized
    Right (a, _:s') -> Left $ pure $ ErrorString $ "Not the end of token stream, tokens missing: " ++ show s'
                  ++ "\n\nand the content parsed:" ++ show a
    Left [] -> Left [UnspecifiedError]
    Left x -> Left x 