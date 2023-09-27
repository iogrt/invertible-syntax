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
 { runParser :: p a } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

-- | 'IsoFunctor' instance for parsers on 'MonadPlus' context
instance MonadPlus p => IsoFunctor (Parsing p) where
  -- TODO: is apply' actually strict?
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



class Toki p a b where
  toki :: p a b

-- TODO: switch p [a] to p b? or just one letter? should p and a be related somehow?
-- atleast [a] can just be a ...
instance (Syntax b (f a), MonadPlus (f a)) => Syntax b (Parsing (f a)) where
  token = Parsing toki

-- could probably generalize this for a -> m b where m i a monad!
-- this is like, parser type numer one. Genericly gives a maybe
newtype Thingie t y = Thingie {
  getThingie :: t -> Maybe (y,t)
}

instance Monad (Thingie t) where
  return a = Thingie $ \s -> Just (a, s)
  Thingie p >>= fb = Thingie (\s -> do 
    (a, s') <- p s
    getThingie (fb a) s'
    )

instance Applicative (Thingie t) where
  pure = return 
  (<*>) = ap
instance Functor (Thingie t) where
  fmap = liftM

instance Alternative (Thingie t) where
  empty = Thingie (const Nothing)
  (Thingie a) <|> (Thingie b) = Thingie (\x -> a x >> b x)

instance MonadPlus (Thingie t)

instance Toki (Thingie [t]) t where
  toki = Thingie (\case 
    t:ts -> Just (t, ts)
    []   -> Nothing)

-- TODO: provide token logic for parser!
-- can later rename Toki to parser!
-- will be the high level glue here
--instance Toki (Thingie String String) where
--  toki = Thingie f where
--    f [] = Nothing
--    f (x:xs) = Just (x,xs)



tryy :: Show t => Parsing (Thingie [t]) (t,t)
tryy = gori

bye :: Show t => [t] -> Maybe((t,t),[t])
bye = getThingie $ runParser tryy
--bye2 = getThingie $ runParser simpli
bye3 = getThingie $ runParser apli 


--- ISO Product functors not working!!! DANGER!
gori :: (Syntax t y, Show t) => y (t,t)
gori = token /*/ token


apli :: Parsing (Thingie [t]) (String,String)
apli = liftA2 (,) (pure "a") (pure "a")

-- what that'd look like:
-- instance Syntax (Pars) where

-- TODO: bring the alternative=>isoalternative and applicative=>ProductFunctor instances here??