{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.Syntax.Poly.Combinators
-- Copyright   : 2010-11 University of Marburg, 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains combinators for classes defined in "Text.Syntax.Poly.Classes".
module Text.Syntax.Poly.Combinators (
  -- * Lexemes
  this,
  list,
  -- * Repetition
  none,
  many,
  some,
  replicate,
  sepBy, sepBy1,
  chainl1,
  count,
  -- * Skipping
  skipMany,
  skipSome,
  -- * Sequencing
  (*/),
  (/*),
  between,
  -- * Alternation
  choice,
  optional, optional_ , bool,
  (/$?/), (/?$/),
  -- * Printing
  format,
  -- others
  isoFail,
  notFollowedBy,
  nonEmptyIso,
  someNE,
  manyUntil,
  someUntil
  ) where

import Prelude hiding (foldl, succ, replicate, (.))
import qualified Data.List.NonEmpty as NE

import Control.Applicative ((<*>), empty)
import Control.Isomorphism.Partial.Prim (Iso (Iso))
import Control.Isomorphism.Partial.Ext
  (nothing, just, nil, cons, left, right, foldl,
   (.), Iso, (/$/), inverse, element, unit, commute, ignore,
   mayAppend, mayPrepend, succ, iso)

import Text.Syntax.Poly.Class
  ((/*/), (/+/),AbstractSyntax(syntax), Syntax(token), IsoAlternative(..))
import Data.List.NonEmpty (NonEmpty(..))

-- | 'none' parses\/prints empty tokens stream consume\/produces a empty list.
none :: AbstractSyntax delta => delta [alpha]
none =  nil /$/ syntax ()

-- TODO: put these in IsoAlternative file
-- | The 'many' combinator is used to repeat syntax.
-- @many p@ repeats the passed syntax @p@
-- zero or more than zero times.
many :: AbstractSyntax delta => delta alpha -> delta [alpha]
many p = some p /+/ none

-- | The 'some' combinator is used to repeat syntax.
-- @some p@ repeats the passed syntax @p@
-- more than zero times.
some :: AbstractSyntax delta => delta alpha -> delta [alpha]
some p = cons /$/ p /*/ many p

manyUntil :: AbstractSyntax delta => delta a -> delta b -> delta ([a],b)
manyUntil synGo synEnd = go where
  go = (Iso (Just . ([],)) (\(as,b) -> case as of
      [] -> Just b
      a : as' -> Nothing
    ) /$/ synEnd)
    /+/ (iso (\(a,(as,b)) -> (a:as,b)) (\(a:as,b) -> (a,(as,b))) /$/ synGo /*/ go)

someUntil :: AbstractSyntax delta => delta a -> delta b -> delta (NonEmpty a,b)
someUntil synGo synEnd =
  iso
    (\(a,(as,b)) -> (a:|as,b))
    (\(a:|as,b) -> (a,(as,b)))
    /$/ synGo /*/ manyUntil synGo synEnd

-- | The 'replicate' combinator is used to repeat syntax.
-- @replicate n p@ repeats the passwd syntax @p@
-- @n@ times.
replicate :: AbstractSyntax delta => Int -> delta alpha -> delta [alpha]
replicate n' p = rec n' where
  rec n | n <= 0    = none
        | otherwise = cons /$/ p /*/ rec (n - 1)


either :: AbstractSyntax delta => delta alpha -> delta beta -> delta (Either alpha beta)
either p q = (left /$/ p) /+/ (right /$/ q)

-- | The 'this' combinator parses\/prints a fixed token
this :: (Syntax tok delta, Eq tok) => tok -> delta ()
this t = inverse (element t) /$/ token

-- | The 'list' combinator parses\/prints a fixed token list and consumes\/produces a unit value.
list :: (Syntax tok delta, Eq tok) => [tok] -> delta ()
list []      =    syntax ()
list (c:cs)  =    inverse (element ((), ()))
             /$/  this c
             /*/  list cs
-- | This variant of '/*/' ignores its left result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost. 

(*/) :: AbstractSyntax delta => delta () -> delta alpha -> delta alpha
p */ q = inverse unit . commute /$/ p /*/ q

-- | This variant of '/*/' ignores its right result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost. 

(/*) :: AbstractSyntax delta => delta alpha -> delta () -> delta alpha
p /* q = inverse unit /$/ p /*/ q

infixl 7 */, /*

-- | The 'between' function combines '*/' and '/*' in the obvious way.
between :: AbstractSyntax delta => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p */ r /* q

-- | The 'chainl1' combinator is used to parse a
-- left-associative chain of infix operators. 
chainl1 :: AbstractSyntax delta =>
           delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f
  = foldl f /$/ arg /*/ many (op /*/ arg)

-- | The 'count' combinator counts fixed syntax.
count :: (Eq beta, Enum beta, AbstractSyntax delta) => delta () -> delta beta
count p = succ /$/ p */ count p /+/ syntax (toEnum 0)

-- | The @skipMany p@ parse the passed syntax @p@
--   zero or more than zero times, and print nothing.
skipMany :: AbstractSyntax delta => delta alpha -> delta ()
skipMany p = ignore [] /$/ many p

-- | The @skipSome v p@ parse the passed syntax @p@
--   more than zero times, and print @p@.
skipSome :: AbstractSyntax delta => delta alpha -> delta alpha
skipSome p = p /* skipMany p

-- | 'choice' a syntax from list.
choice :: AbstractSyntax delta => [delta alpha] -> delta alpha
choice = foldr (/+/) emptyIso

-- | The 'optional' combinator may parse \/ print passed syntax.
optional :: AbstractSyntax delta => delta alpha -> delta (Maybe alpha)
optional x = just /$/ x /+/ nothing /$/ syntax ()

-- | optional but discards output
optional_ :: AbstractSyntax delta => delta () -> delta ()
optional_ x = x /+/ syntax ()

-- | The 'bool' combinator parse \/ print passed syntax or not.
bool :: AbstractSyntax delta => delta () -> delta Bool
bool x = x */ syntax True /+/ syntax False

-- | The 'sepBy' combinator separates syntax into delimited list.
-- @sepBy p d@ is @p@ list syntax delimited by @d@ syntax.
sepBy :: AbstractSyntax delta => delta alpha -> delta () -> delta [alpha]
sepBy x sep
  =    x `sepBy1` sep
  /+/  none

-- | The 'sepBy1' combinator separates syntax into delimited non-empty list.
-- @sepBy p d@ is @p@ list syntax delimited by @d@ syntax.
sepBy1 :: AbstractSyntax delta => delta alpha -> delta () -> delta [alpha]
sepBy1 x sep = cons /$/ x /*/ many (sep */ x)

-- | May append not to repeat prefix syntax.
(/$?/) :: AbstractSyntax delta => Iso (a, b) a -> delta (a, Maybe b) -> delta a
cf /$?/ pair = mayAppend  cf /$/ pair

-- | May prepend not to repeat suffix syntax.
(/?$/) :: AbstractSyntax delta => Iso (a, b) b -> delta (Maybe a, b) -> delta b
cf /?$/ pair = mayPrepend cf /$/ pair

infix 5 /$?/, /?$/

-- | The 'format' combinator just print passed tokens
-- or may parse passed tokens.
-- This is useful in cases when just formatting with indents.
format :: (Syntax tok delta, Eq tok) => [tok] -> delta ()
format tks = ignore (Just ()) /$/ optional (list tks)


-- succeed if op fails, fail if op succeeds
-- isoFail False so it doesn't print
-- implementation: run op with bool to see if it succeeded or not,
-- giving the success to isoFail, meaning that success will invert into failure
notFollowedBy :: (AbstractSyntax delta) => delta () -> delta ()
notFollowedBy op = inverse (isoFail False id) /$/ bool op


nonEmptyIso = iso NE.fromList NE.toList

someNE a = nonEmptyIso /$/ some a


-- shorthand for giving fail conditions
-- arguments are ordered by the order of iso construction
-- easy to use with applicative:
-- g :: a -> a -> a
-- (isoFail <*> g) x == isoFail x (g x)

-- v: arg to give for printing,
-- f: parse fail condition
isoFail :: v -> (v -> Bool) -> Iso () v
isoFail v f = Iso
  (const $ Just v)
  (\x -> if f x then Nothing else Just ())
