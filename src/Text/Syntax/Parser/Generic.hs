{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Text.Syntax.Parser.Generic where

import Prelude hiding ((.))
import Control.Monad
import Control.Applicative
import Control.Isomorphism.Partial.Prim
import Control.Isomorphism.Partial.Ext
import Text.Syntax.Poly.Class
import Text.Syntax.Poly (AbstractSyntax)
import Debug.Trace


-- Parsing in a context p :: * -> *
newtype Parsing p a = Parsing 
 { runParser :: p a } -- deriving (Functor, Applicative, Monad, Alternative, MonadPlus)
 --{ runParser :: forall p . MonadPlus p => p a } 

-- | 'IsoFunctor' instance for parsers on 'MonadPlus' context
instance MonadPlus p => IsoFunctor (Parsing p) where
  -- TODO: is apply' actually strict? I don't think so, hls tells me it's not
  iso /$/ (Parsing mp) = Parsing (mp >>= maybe mzero return . apply' iso)

instance Applicative p => ProductFunctor (Parsing p) where
  Parsing a /*/ Parsing b = Parsing (liftA2 (,) a b)

instance Alternative p => IsoAlternative (Parsing p) where
  emptyIso = Parsing empty
  Parsing a /+/ Parsing a' = Parsing (a <|> a')


instance (MonadPlus p) => AbstractSyntax (Parsing p) where
  syntax = Parsing . pure

-- hopes: make a Toki or Parser typeclass for a fine way to define token.
-- that way you create parsers easy
--token = Parsing runParser
-- easy way out first:
-- MaybeList implementation!
-- this is the list implementation, regardless of the Thingie type! (when you have toki stuff)
-- so the idea is to have Toki Thingie (...) (which means Parsing Thingie exists), which means syntax exists!
-- in another perspective.. token is how parsing gets implemented. it's how you get tokens!