{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Text.Syntax.Parser.Generic
import Control.Isomorphism.Partial.TH
import Text.Syntax.Poly.Class
import Control.Isomorphism.Partial.Ext
import Text.Syntax.Poly
import Text.Syntax.Parser.List (runAsParser)
import Text.Syntax.Printer.List (runAsPrinter)
import Prelude hiding ((.))
import Test.HUnit
import qualified System.Exit as Exit
import Data.Bifunctor

type DocSyntax a = SyntaxT Char a

data Document = Document {
    title :: String
} deriving (Eq,Show)

$(defineIsomorphisms ''Document)

docSyn :: SyntaxT Char Document
docSyn = document /$/ (this '#' */ manyUntil token (this '\n'))
    -- consume remaining token
    /* this '\n'


singleTokenSyn :: SyntaxT Char Char
singleTokenSyn = token

isoFunctoredSyn :: SyntaxT Char (Maybe Char)
isoFunctoredSyn = just /$/ token

isoFunctorBacktrackingSyn :: SyntaxT Char (Maybe Char)
isoFunctorBacktrackingSyn =
    -- similar to optional and bool combinators
    -- 'A' will always actually fail, so you should always get Just, even when using A as a token
    (Iso (const Nothing) (const Nothing) /$/ list "AB" ) /+/ just /$/ token


productFunctorSyn :: SyntaxT Char (Char,Char)
productFunctorSyn = token /*/ token

isoAlternativeSyn :: SyntaxT Char ()
isoAlternativeSyn = this 'A' /+/ this 'B'

isoAlternativeBacktrackSyn :: SyntaxT Char ()
isoAlternativeBacktrackSyn = list "AB" /+/ list "CD"

syntaxSyn :: SyntaxT Char Int
syntaxSyn = syntax 10

productFunctorDiscardSyn :: SyntaxT Char ()
productFunctorDiscardSyn = this 'a' */ this 'A'

productFunctorDiscardPrefixSyn :: SyntaxT Char Char
productFunctorDiscardPrefixSyn = this 'a' */ token

notFollowedBySyn :: SyntaxT Char Char
notFollowedBySyn = notFollowedBy (this 'a') */ token

manyUntilSyn :: SyntaxT Char String
manyUntilSyn = 
    manyUntil token (this 'b') 
    -- consume the missing token
      /* this 'b'

isoFailSyn :: SyntaxT Char Char
isoFailSyn =
    -- Z is only for printing
    isoFail 'Z' (=='b') /$/ this 'c'


boolSyn :: SyntaxT Char Bool
boolSyn = bool (this 'A')


mkParseTest :: (Eq a,Show a) => SyntaxT Char a -> String -> a -> Test
mkParseTest syn input result = 
    TestList 
    [ TestCase (assertEqual "should parse" (Right result) (runAsParser syn input))
    -- parse + print + parse should equal first parse
    , TestCase (assertEqual "should parse print parse" (Right result) (
        let 
            printer :: RunAsPrinter Char String a SyntaxError
            printer = runAsPrinter
        in
        runAsParser syn input >>= first (:[ErrorString "test: print error"]) . printer syn >>= runAsParser syn
    ))
    ]


mkPrintTest :: (Eq a,Show a) => SyntaxT Char a -> String -> a -> Test
mkPrintTest syn result input = TestCase (assertEqual "should print" (Right result) (runAsPrinter syn input))

tests :: Test
tests = TestList
    [ TestLabel "singleTokenTest" $ mkParseTest singleTokenSyn "H" 'H'
    , TestLabel "isoFunctoredTest" $ mkParseTest isoFunctoredSyn "H" (Just 'H')


    , TestLabel "isoFunctoredBacktrackTest" $ mkParseTest isoFunctorBacktrackingSyn "A" (Just 'A')
    , TestLabel "isoFunctoredBacktrackTest" $ mkParseTest isoFunctorBacktrackingSyn "B" (Just 'B')

    , TestLabel "productFunctorTest" $ mkParseTest productFunctorSyn "aB" ('a','B')
    , TestLabel "isoAlternativeTest" $ mkParseTest isoAlternativeSyn "A" ()
    , TestLabel "isoAlternativeTest second" $ mkParseTest isoAlternativeSyn "B" ()
    , TestLabel "isoAlternativeBacktrackTest" $ mkParseTest isoAlternativeBacktrackSyn "AB" ()
    , TestLabel "isoAlternativeBacktrackTest" $ mkParseTest isoAlternativeBacktrackSyn "CD" ()
    , TestLabel "syntax function test" $ mkParseTest syntaxSyn "" 10
    , TestLabel "docSyn" $ mkParseTest docSyn "#Hello boys\n" (Document "Hello boys")

    , TestLabel "productFunctor discard" $ mkParseTest productFunctorDiscardSyn "aA" ()
    , TestLabel "productFunctorDiscardPrefixSyn" $ mkParseTest productFunctorDiscardPrefixSyn "aA" 'A'
    --  TODO: mkFailingParseTest
    --, TestLabel "notFollowedBy" $ mkParseTest notFollowedBySyn "a" 'B' -- clearly shouldn't work. It consumes 'a' !!!
    , TestLabel "notFollowedBy2" $ mkParseTest notFollowedBySyn "C" 'C'
    , TestLabel "bool operator true" $ mkParseTest boolSyn "A" True
    ,TestLabel " syntax print" $ mkPrintTest syntaxSyn "" 10

    , TestLabel "bool operator false" $ mkParseTest boolSyn "" False


    , TestLabel "manyUntil string" $ mkParseTest manyUntilSyn "AABBCCb" "AABBCC"
    , TestLabel "manyUntil empty" $ mkParseTest manyUntilSyn "b" ""
    --, TestLabel "isoFail syn" $ mkParseTest isoFailSyn "A" 'A'
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess