module Flapjax.Parser(parseScript,parseInline,parseInlineAttribute) where

import Control.Monad
import Text.ParserCombinators.Parsec
import BrownPLT.JavaScript.Parser (parseStatement, parseExpression)
import BrownPLT.JavaScript.Lexer (whiteSpace)
import Flapjax.Syntax

parseScript =
  whiteSpace >> liftM2 FlapjaxScript getPosition (parseStatement `sepBy` spaces)

parseInline =
  whiteSpace >> (liftM2 Inline getPosition parseExpression <?> 
             "expression within {! ... !}")

parseInlineAttribute =
  whiteSpace >> (liftM2 InlineAttribute getPosition parseExpression <?>
             "expression without {! ... !} in an attribute")
