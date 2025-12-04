import Parser
import Ast.UniversalIR

namespace UniversalParser

-- Parser combinators for UniversalIR
def pID : Parser String := letter *> (letter <|> digit)* |>.map String.ofList

def pNumber : Parser UniversalIR.Expr := nat.map UniversalIR.Expr.num
def pAtom : Parser UniversalIR.Expr := 
  pNumber <|> pID.map UniversalIR.Expr.var <|> (symbol "(" *> pExpr <* symbol ")")

def pExpr : Parser UniversalIR.Expr := 
  -- Left-associative binary ops (simplified)
  pAtom >>= λleft => 
    (symbol "+" *> pAtom).orElse (symbol "-" *> pAtom).map (λright op => 
      UniversalIR.Expr.binOp (if op == "+" then .add else .sub) left right) 
    |>.star >>= λops => 
      ops.foldl left (λacc (op, right) => UniversalIR.Expr.binOp op acc right)

def pStatement : Parser UniversalIR.Stmt := 
  choice [
    ("set" *> pID <* symbol ":=" <*> pExpr <* symbol ";").map (λ⟨x, e⟩ => UniversalIR.Stmt.assign x e),
    ("print" *> pExpr <* symbol ";").map UniversalIR.Stmt.print,
    -- if/while simplified for now
    pure (UniversalIR.Stmt.print (UniversalIR.Expr.num 0))
  ]

def pProgram : Parser (List UniversalIR.Stmt) := many pStatement

def parseProgram (input : String) : Except String (List UniversalIR.Stmt) := 
  Parser.runParser pProgram input |>.toExcept (λe => s!"Parse error at {e}")

end UniversalParser
