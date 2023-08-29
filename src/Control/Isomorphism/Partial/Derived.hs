module Control.Isomorphism.Partial.Derived 
  ( foldl
  ) where

import Prelude ()
import Control.Category (Category (id, (.)))
import Control.Isomorphism.Partial.Prim (Iso, inverse, unit, associate, iterateIso, (***))
import Control.Isomorphism.Partial.Constructors (cons, nil)

foldl :: Iso (alpha, beta) alpha -> Iso (alpha, [beta]) alpha
foldl i = inverse unit
        . (id *** inverse nil)
        . iterateIso (step i) where

  step i' = (i' *** id)
         . associate
         . (id *** inverse cons)
