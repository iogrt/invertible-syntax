{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
--remove
{-# LANGUAGE AllowAmbiguousTypes #-}

-- This module includes a lazy parser implementation for "Text.Syntax.Poly". Result does not have error informations.
module Text.Syntax.Parser.ListSilent (
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
import Control.Applicative (Alternative(..))
import Text.Syntax.Poly


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
runAsParser :: RunAsParser tok [tok] a SyntaxError
runAsParser parser s = case runParser parser s of
  Just (a, [])    -> Right a
  Just (_, _:_) -> Left . ErrorString $ "Not the end of token stream."
  Nothing         -> Left . ErrorString $ "parse error"



--synA :: (Eq t,Syntax t y) => y ()
--synA = this 'a'

--synB :: (Eq t ,Syntax t y) => y ()
--synB = this 'b'

go :: Either SyntaxError Char
go = runAsParser ((element 'A' /$/ this 'a') /+/ (element 'B' /$/ this 'b')) "ab"