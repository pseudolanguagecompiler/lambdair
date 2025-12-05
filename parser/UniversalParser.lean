import Parser
import Ast.UniversalIR

namespace UniversalParser

open Parser Char

-- ID: letter followed by alphanum
def pID : Parser String := 
  letter *> (alpha <|> digit)* |>.map fun ls => String.mk' ls.toArray

-- Number → Expr.num
def pNumber : Parser UniversalIR.Expr := 
  nat.map UniversalIR.Expr.num

-- Atom: number | id | ( expr )
def pAtom : Parser UniversalIR.Expr := pNumber <|> 
  pID.map UniversalIR.Expr.var <|> 
  (satisfy (· == '(') *> pExpr <* satisfy (· == ')'))

-- Binary expressions (left-associative)
def pExpr : Parser UniversalIR.Expr := do
  let mut left ← pAtom
  while true do
    if (← test "+") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp .add left right
    else if (← test "-") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp .sub left right
    else if (← test ">") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp .gt left right
    else
      break left
  pure left

-- Statements
def pStatement : Parser UniversalIR.Stmt := choice [
  -- set x := expr ;
  do
    ← keyword "set"
    let x ← pID
    ← symbol ":="
    let e ← pExpr
    ← symbol ";"
    pure (UniversalIR.Stmt.set x e),
  
  -- print expr ;
  do
    ← keyword "print"
    let e ← pExpr
    ← symbol ";"
    pure (UniversalIR.Stmt.print e)
]

def pProgram : Parser (List UniversalIR.Stmt) := many pStatement

def parseProgram (input : String) : Except String (List UniversalIR.Stmt) := 
  runParser pProgram input "" |>.toExcept toString

end UniversalParser [web:101][web:102]
