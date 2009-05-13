{- This module doesn't export any names, but importing it makes 
 - Syntax.Flapjax an instance of HtmlScript.
 -}
module Flapjax.HtmlEmbedding(FjHtml) where

import Control.Monad
import BrownPLT.Html.Syntax (Script (..),
  attributeValue, Html)
import qualified Flapjax.Parser
import qualified BrownPLT.JavaScript.Parser
import Flapjax.Syntax
import Text.ParserCombinators.Parsec(SourcePos)

type FjHtml = Html SourcePos Flapjax

-- We recognize lang= and type= methods for identifying a script's language.
scriptLang attrs =
  case attributeValue "lang" attrs of
    (Just v) -> Just v
    Nothing  -> case attributeValue "type" attrs of
                  (Just v) -> Just v
                  Nothing  -> Nothing


instance Script Flapjax where

  prettyPrintScript script = prettyFlapjax script
  
  parseScriptBlock attrs =
    case scriptLang attrs of
      (Just "flapjax")      -> Flapjax.Parser.parseScript
      (Just "text/flapjax") -> Flapjax.Parser.parseScript
      otherwise             -> 
        liftM Javascript BrownPLT.JavaScript.Parser.parseScript
  
  parseInlineScript =
    Just Flapjax.Parser.parseInline
  
  parseAttributeScript = 
    Just Flapjax.Parser.parseInlineAttribute
