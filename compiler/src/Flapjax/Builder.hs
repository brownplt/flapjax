{- This module contains function for building Javascript expressions.  It is
 - a little easier to use than directly using Javascript.Syntax.
 -} 
module Flapjax.Builder where

import qualified Prelude as P
import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos)
import BrownPLT.JavaScript.Syntax



p:: SourcePos
p = initialPos "Javascript.Builder"

type E = Expression SourcePos
type S = Statement SourcePos
type I = Id SourcePos


string = StringLit p
-- regexp =
number = NumLit p
bool   = BoolLit p
null   = NullLit p
array  = ArrayLit p
-- object = 
this   = ThisRef p
--ref:: I -> E
ref    = VarRef p
vref s = VarRef p (Id p s)
bracket = BracketRef p
func   = FuncExpr p
new     = NewExpr p
prefix = PrefixExpr p
inf    = InfixExpr p
-- etc
--dot:: E -> I -> E
dot = DotRef p
vdot e f = DotRef p e (Id p f)
parens = ParenExpr p
call   = CallExpr p
id = Id p

return val = ReturnStmt p (P.Just val)

