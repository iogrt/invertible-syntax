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
  iso /$/ (Parsing mp) = Parsing (mp >=> firstM (maybe empty pure . apply iso))
    where
      firstM :: Functor m => (a -> m a') -> (a, b) -> m (a', b)
      firstM f (a,b) = (,b) <$> f a

instance Monad m => ProductFunctor (Parsing i m) where
--  Parsing a /*/ Parsing b = Parsing (liftA2 (liftA2 (,)) a b)
  -- Optimize this one somehow

  -- TODO: This is not correct at all! Look at how /* will consume inputs aswell, making this a non-backtrabacle operation!
  Parsing a /*/ Parsing b = Parsing (\i -> do
    (aa,ii) <- a i
    (bb,iii) <- b ii
    pure ((aa,bb), iii)
    )

instance Alternative m => IsoAlternative (Parsing i m) where
  emptyIso = Parsing $ const empty
  Parsing a /+/ Parsing a' = Parsing (liftA2 (<|>) a a')


instance (MonadPlus m) => AbstractSyntax (Parsing i m) where
  syntax x = Parsing (\inp -> pure (x, inp))
  -- 
  --TODO: Non-generic, better way of using syntax Error, will have to somehow pass it in through probably a different typeclass!
  -- Maybe "Parsing" is a typeclass itself, and then all Parsing's get transformed into the other
  -- like class Parsing where (token,syntaxError)
  syntaxError x = Parsing (const mzero)


-- Parser composition TODO. Look in old "Compose.hs" for reference

-- make Monad and Alternative... 
{- instance Alternative m => Alternative (Parsing i m) where
  empty = Parsing $ const empty
  Parsing a <|> b = Parsing $ const empty
instance Monad (Parsing i m) where
  Parsing a >>= fb = _ -}