-- |
-- Module      : Text.Syntax.Poly
-- Copyright   : 2012 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Integrated namespace for invertible syntax
--
--
module Text.Syntax.Poly (
  -- * Invertible Syntax
  -- $invertibleSyntax

  -- * How to use
  -- $howToUse

  -- ** How to define partial isomorphisms
  -- $definePartialIsomorphisms

  -- ** How to define invertible syntax
  -- $howToDefine

  -- ** Use with parser combinator implementation which has try
  -- $tryCombinators

  -- ** Call defined invertible syntax
  -- $howToCall
  
  -- * Example
  -- $jsonExample

  -- * Exported modules
  module Text.Syntax.Poly.Class,
  module Text.Syntax.Poly.Type,
  -- module Text.Syntax.Poly.Instances,
  module Text.Syntax.Poly.Combinators,
  module Text.Syntax.Poly.Combinators.Char,
  ) where

import Text.Syntax.Poly.Class
import Text.Syntax.Poly.Type
-- import Text.Syntax.Poly.Instances
import Text.Syntax.Poly.Combinators
import Text.Syntax.Poly.Combinators.Char

{-# ANN module "ignore import/export shortcut" #-}

{- $invertibleSyntax
This library extended definition of invertible-syntax library

<http://hackage.haskell.org/package/invertible-syntax>

Define parser and printer in a single syntax definition.
Major extended feature from original invertible-syntax implementation is
that this library can use with polymorphic token type other than 'Char'.
-}

{- $howToUse
To use invertible syntax,

1. You may need to define isomorphisms.

2. You need to define invertible syntax to use isomorphisms.

3. And call defined invertible syntax as parser or printer.
-}

{- $definePartialIsomorphisms
Convenient template-haskell function is provided to define partial isomorphisms which is needed to compose \/ destruct data types you want.

> import Control.Isomorphism.Partial.TH (defineIsomorphisms)
>
> data Foo = ...
>
> $(defineIsomorphisms ''Foo)
-}

{- $howToDefine
You can define invertible syntax like applicative style parser.

> import Control.Isomorphism.Partial.Ext ((<$>))
> import Text.Syntax.Poly ((<|>), (<*>))
>
> syntaxFoo = consFoo0 <$> syntaxFoo0Left <*> syntaxFoo0Right <|>
>             consFoo1 <$> syntaxFoo1                         <|>
>             ....

-}

{- $tryCombinators
Syntax instance of parser combinator implementation which has 'try' combinator,
implements combinator for alternative syntax '<|>' like below.

> p <|> q = try p <||> q

When you may want not to use try, use '<||>' instead of '<|>'.

-}

{- $howToCall

To call defined invertible syntax, you need parser \/ printer instances of Syntax class. For example ReadP is provided as instance of 'Syntax'.

> import Text.Syntax.Parser.ReadP ()
> import Text.ParserCombinators.ReadP (readP_to_S)
> import Text.Syntax.Poly (Syntax)
>
> syntaxFoo :: Syntax Char delta => delta Foo
> syntaxFoo =  ....
>
> ....
>
> -- 'Syntax Char delta => delta Foo' is super type of 'ReadP Foo'
> parseFoo = readP_to_S syntaxFoo

-}

{- $jsonExample
Here is an example of JSON syntax. 
This definition is runnable as both JSON parser and JSON printer.
 
- JsonData.hs

> { -# LANGUAGE TemplateHaskell #- }
>
> module JsonData where
>
> import Control.Isomorphism.Partial.TH (defineIsomorphisms)
>
> data JValue = JString String
>             | JNumber Double
>             | JBool Bool
>             | JNull
>             | JObject [(String, JValue)]
>             | JArray [JValue]
>             deriving (Eq, Ord, Show)
> 
> 
> $(defineIsomorphisms ''JValue)
> $(defineIsomorphisms ''Bool)

- json.hs

> { -# LANGUAGE FlexibleContexts #- }
> { -# LANGUAGE Rank2Types #- }
> 
> import JsonData
> 
> import Prelude hiding ((.), negate, replicate)
> import Control.Isomorphism.Partial.Ext
>   (Iso, (<$>), (.), inverse, subset, cons, readShow,
>    chrOrd, hex, signumAbs, digitsFloat, floatTripleDigits)
> import Text.Syntax.Poly
>   ((<|>), (<*>), syntax, syntaxError, token, SyntaxT,
>    list, this, between, (*>), (<*), many, some, sepBy, replicate,
>    choice, optSpace)
> import Text.Syntax.Parser.List.Lazy (runAsParser)
> import Text.Syntax.Printer.List (RunAsStringPrinter, runAsPrinter)
> 
> import System.Environment (getArgs)
> 
> type JSyntax a = SyntaxT Char a
> 
> s_text :: JSyntax JValue
> s_text =  optSpace *> text <|>
>           syntaxError "JSON text"  where
>   text = jObject <$> s_object <|>
>          jArray  <$> s_array
> 
> s_series :: Char -> JSyntax a -> Char -> JSyntax [a]
> s_series left parser right =
>   between
>   (this left  <* optSpace)
>   (this right <* optSpace)
>   ((parser <* optSpace) `sepBy` (this ',' <* optSpace))
> 
> s_array :: JSyntax [JValue]
> s_array =  s_series '[' s_value ']'
> 
> s_object :: JSyntax [(String, JValue)]
> s_object =  s_series '{' s_field '}'  where
>   s_field :: JSyntax (String, JValue)
>   s_field =  s_string <* optSpace <* this ':' <* optSpace <*> s_value
> 
> 
> s_value :: JSyntax JValue
> s_value =  (jString <$> s_string    <|>
>             jNumber <$> s_number    <|>
>             jObject <$> s_object    <|>
>             jArray  <$> s_array     <|>
>             jBool   <$> s_bool      <|>
>             jNull   <$> list "null" <|>
>             syntaxError "JSON value")   <* optSpace
> 
> s_bool   :: JSyntax Bool
> s_bool   =  true  <$> list "true"  <|>
>             false <$> list "false"
> 
> s_digit :: JSyntax Char
> s_digit =  subset (`elem` ['0'..'9']) <$> token
> 
> s_digit_nz :: JSyntax Char
> s_digit_nz =  subset (`elem` ['1'..'9']) <$> token
> 
> s_digits0 :: JSyntax String
> s_digits0 =  many s_digit
> 
> s_digits1 :: JSyntax String
> s_digits1 =  some s_digit
> 
> s_hexdigit :: JSyntax Char
> s_hexdigit =  subset (`elem` (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']))
>               <$> token
> 
> int :: Iso String Int
> int =  readShow
> 
> char   :: Char -> JSyntax Char
> char c =  this c *> syntax c
> 
> s_float :: JSyntax Double
> s_float =  digitsFloat . floatTripleDigits
>            <$> (this '0' *> syntax "" <|>
>                 cons <$> s_digit_nz <*> s_digits0)
>            <*> (this '.' *> s_digits1 <|> syntax "")
>            <*> ((this 'e' <|> this 'E')
>                 *>  (int <$> (cons <$> char '-' <*> s_digits1 <|>
>                               this '+' *> s_digits1           <|>
>                               s_digits1)
>                     )  <|>
>                 syntax 0)
> 
> s_number :: JSyntax Double
> s_number =  signumAbs
>             <$> ((this '-' *> syntax (-1)) <|>
>                  syntax 1)
>             <*> s_float
> 
> s_string :: JSyntax String
> s_string =  between (this '\"') (this '\"') (many jchar)
>   where jchar = this '\\' *> (s_escape <|> s_unicode) <|>
>                 (subset (`notElem` "\"\\") <$> token)
> 
> escapeMap :: [(Char, Char)]
> escapeMap =  [('b', '\b'), ('n', '\n'),
>               ('f', '\f'), ('r', '\r'),
>               ('t', '\t'), ('\\', '\\'),
>               ('\"', '\"'), ('/', '/')]
> 
> s_escape :: JSyntax Char
> s_escape =  choice $ map (uncurry decode) escapeMap
>   where decode c r = this c *> syntax r
> 
> s_unicode :: JSyntax Char
> s_unicode =  inverse chrOrd . hex <$> this 'u' *> replicate 4 s_hexdigit
> 
> 
> runStringParser :: RunAsStringParser a ErrorStack
> runStringParser =  runAsParser
> 
> runStringPrinter :: RunAsStringPrinter a SyntaxError
> runStringPrinter =  runAsPrinter
> 
> main :: IO ()
> main =  do (fn:_) <- getArgs
>            input <- readFile fn
>            case runStringParser s_text input of
>              Left e       -> putStrLn $ "Parse error: " ++ show e
>              Right parsed ->
>                do putStrLn $ "parsed: " ++ show parsed
>                   putStrLn $ "printed: " ++ show (runStringPrinter s_text parsed)
-}
