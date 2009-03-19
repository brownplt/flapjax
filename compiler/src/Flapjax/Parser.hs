module Flapjax.Parser(parseScript,parseInline,parseInlineAttribute) where

import Control.Monad
import Text.ParserCombinators.Parsec
import WebBits.JavaScript.Parser (parseStatement,parseExpression)
import Flapjax.Syntax

parseScript =
  spaces >> liftM2 FlapjaxScript getPosition (parseStatement `sepBy` spaces)

parseInline =
  spaces >> (liftM2 Inline getPosition parseExpression <?> 
             "expression within {! ... !}")

parseInlineAttribute =
  spaces >> (liftM2 InlineAttribute getPosition parseExpression <?>
             "expression without {! ... !} in an attribute")
