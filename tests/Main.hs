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

type DocSyntax a = SyntaxT Char a

data Document = Document {
    title :: String
} deriving (Eq,Show)

$(defineIsomorphisms ''Document)

docSyn :: SyntaxT Char Document
docSyn = document . inverse unit /$/ (this '#' */ manyUntil token (this '\n'))


singleTokenSyn :: SyntaxT Char Char 
singleTokenSyn = token

isoFunctoredSyn :: SyntaxT Char (Maybe Char)
isoFunctoredSyn = just /$/ token

productFunctorSyn :: SyntaxT Char (Char,Char)
productFunctorSyn = token /*/ token

isoAlternativeSyn :: SyntaxT Char ()
isoAlternativeSyn = this 'A' /+/ this 'B'


mkParseTest :: (Eq a,Show a) => SyntaxT Char a -> String -> a -> Test
mkParseTest syn input result = TestCase (assertEqual "should parse" (Right result) (runAsParser syn input))

tests :: Test
tests = TestList 
    [ TestLabel "singleTokenTest" $ mkParseTest singleTokenSyn "H" 'H'
    , TestLabel "isoFunctoredTest" $ mkParseTest isoFunctoredSyn "H" (Just 'H')
    , TestLabel "productFunctorTest" $ mkParseTest productFunctorSyn "aB" ('a','B')
    , TestLabel "isoAlternativeTest" $ mkParseTest isoAlternativeSyn "A" ()
    , TestLabel "isoAlternativeTest second" $ mkParseTest isoAlternativeSyn "B" ()
    , TestLabel "docSyn" $ mkParseTest docSyn "#Hello boys\n" (Document "Hello boys")
    ]
 
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess