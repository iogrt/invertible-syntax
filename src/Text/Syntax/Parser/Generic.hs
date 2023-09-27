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
import Data.Maybe


-- Parsing in a context p :: * -> *
-- inp :: input (token collection)
-- rm:: result monoid (maybe, Either String, etc..)
-- out :: result

-- INP SHOULD BE A MONOID for shit to work accordingly!
newtype Parsing inp out t = Parsing
 { runParser :: inp -> out (t, inp)
 }

-- | 'IsoFunctor' instance for parsers on 'MonadPlus' context
-- TODO: More generic instance: instance Alternative out => IsoFunctor (Parsing Maybe inp) where
  -- TODO: think it works? might need constraint
instance MonadPlus out => IsoFunctor (Parsing inp out) where
  -- TODO: is apply' actually strict? I don't think so, hls tells me it's not
  iso /$/ (Parsing mp) = Parsing (mp >=>
      -- TODO: make sure it's working properly please!!!
      firstM (maybe empty pure . apply' iso)
    )
    where
      firstM :: Functor m => (a -> m a') -> (a, b) -> m (a', b)
      firstM f ~(a,b) = (,b) <$> f a

instance Monad m => ProductFunctor (Parsing i m) where
--  Parsing a /*/ Parsing b = Parsing (liftA2 (liftA2 (,)) a b)
  -- Optimize this one somehow
  Parsing a /*/ Parsing b = Parsing (a >=> \(aa,i) -> do
    (bb,ii) <- b i
    pure ((aa,bb), ii)
    )

instance Alternative m => IsoAlternative (Parsing i m) where
  emptyIso = Parsing $ const empty
  Parsing a /+/ Parsing a' = Parsing (liftA2 (<|>) a a')


instance (Monoid i, MonadPlus m) => AbstractSyntax (Parsing i m) where
  syntax x = Parsing $ const $ pure (x, mempty)

-- hopes: make a Toki or Parser typeclass for a fine way to define token.
-- that way you create parsers easy
--token = Parsing runParser
-- easy way out first:
-- MaybeList implementation!
-- this is the list implementation, regardless of the Thingie type! (when you have toki stuff)
-- so the idea is to have Toki Thingie (...) (which means Parsing Thingie exists), which means syntax exists!
-- in another perspective.. token is how parsing gets implemented. it's how you get tokens!