module Flapjax.Syntax where

import WebBits.JavaScript
import Text.ParserCombinators.Parsec(SourcePos)
import Data.Generics(Data,Typeable)
import Misc
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

instance PrettyPrintable Flapjax where
  pp (FlapjaxScript _ stmts) = Pp.vcat (map pp stmts)
  pp (Javascript js) = pp js
  pp (Inline p expr) = pp expr
  pp (InlineAttribute p expr) = pp expr

instance Show Flapjax where
  show f = Pp.render (pp f)
