module Flapjax.Syntax where

import BrownPLT.JavaScript.Crawl()
import qualified BrownPLT.JavaScript.PrettyPrint as JsPrettyPrint
import BrownPLT.JavaScript
import Text.ParserCombinators.Parsec (SourcePos)
import Data.Generics (Data, Typeable)
import qualified Text.PrettyPrint.HughesPJ as Pp

data Flapjax
  -- <script lang="flapjax"> ... </script>
  = FlapjaxScript SourcePos [Statement SourcePos]
  -- <script lang=other> ... </script>
  | Javascript (JavaScript SourcePos)
  -- {! expression !}
  | Inline SourcePos (Expression SourcePos) 
  -- {! expression in attribute !}
  | InlineAttribute SourcePos (Expression SourcePos)
  deriving (Data,Typeable)

prettyFlapjax :: Flapjax -> Pp.Doc
prettyFlapjax fx = case fx of
  FlapjaxScript _ stmts -> Pp.vcat (map JsPrettyPrint.stmt stmts)
  Javascript js -> JsPrettyPrint.javaScript js
  Inline p e -> JsPrettyPrint.expr e
  InlineAttribute p e -> JsPrettyPrint.expr e

instance Show Flapjax where
  show fx = Pp.render (prettyFlapjax fx)
