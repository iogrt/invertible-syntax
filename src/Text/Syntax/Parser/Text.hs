{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Text.Syntax.Parser.Text (
  -- * Poly- morphic wrapper of runParser
  --runAsParser
  ) where

import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative(Alternative(..))
import Data.Bifunctor (first)

import Data.Text as T
import Text.Syntax.Poly
import Text.Syntax.Parser.Generic
import Text.Syntax.Poly.Class
  (IsoAlternative, Syntax (..))
import Text.Syntax.Parser.List(ErrorStacker(..))
import Data.Bool

{-
instance Syntax T.Text (Parsing T.Text ErrorStacker) where
  -- kinda inneficient for single tokens, because I'm giving preference to string
  token = Parsing (ErrorStacker . maybe (Left [EndOfStream]) (Right . first T.singleton) . T.uncons)
   string s = Parsing (\text ->
        maybe 
            (ErrorStacker $ Left $ pure $ ErrorString $ "expected '" <> show s <> "', got "<>show text)
            (\rest -> pure ((),rest))
            (s `T.stripPrefix` text)
    ) -}