module BrownPLT.Flapjax.CompilerMessage
  ( CompilerMessage (..)
  ) where

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.List as L

import Text.XHtml.Transitional

data CompilerMessage
  = Warning String String SourcePos
  | Error String SourcePos

instance HTML ParseError where
  toHtml err =
    p <<  toHtml (errorPos err)
      +++ (concatHtml $ map unMsg (errorMessages err))

unMsg (Message s) = s +++ br
unMsg (UnExpect s) = s +++ br
unMsg (Expect s) = s +++ br
unMsg (SysUnExpect s) = s +++ br

instance HTML SourcePos where
  toHtml p = thespan << 
    ("line " ++ show (sourceLine p) ++ ", column " ++ show (sourceColumn p))

instance HTML CompilerMessage where
  toHtml (Warning msg expr pos) =
    p <<
      (tag "b" << toHtml pos) +++ msg +++ (pre << expr)
  toHtml (Error msg pos) =
    p <<
      (tag "b" << toHtml pos) +++ msg


instance Show CompilerMessage where
  show (Warning desc expr pos) =
    show pos ++ ": " ++ desc ++ "\nOffending expression:\n" ++ expr
  show (Error msg pos) =
    show pos ++ ": " ++ msg
