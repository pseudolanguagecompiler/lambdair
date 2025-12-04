import Parser
import Ast.UniversalIR

namespace UniversalParser

-- Parser combinators for UniversalIR
def pID : Parser String := letter *> (letter <|> digit)* |>.map String.ofList

def pNumber : Parser UniversalIR.Expr := nat.map UniversalIR.Expr.num

def pExpr : Parser UniversalIR.Expr := 
  -- Simple recursive descent for expr (left-associative binary ops)
  let pTerm := pNumber <|> pID.map UniversalIR.Expr.var <|> (symbol "(" *> pExpr <* symbol ")")
  let pFactor := pTerm <*>* (symbol "+" *> pTerm).map (λes => 
    es.foldl (init := es.head!) (fun acc e => UniversalIR.Expr.binOp UniversalIR.BinOp.add acc e))
  pFactor <*>* (symbol "-" *> pFactor).map (λes => 
    es.foldl (init := es.head!) (fun acc e => UniversalIR.Expr.binOp UniversalIR.BinOp.sub acc e))
  |>.map (·.head!)

def pStatement : Parser UniversalIR.Stmt := 
  ("set" *> pID <* symbol ":=" <*> pExpr <* symbol ";").map (λ⟨x, e⟩ => 
    UniversalIR.Stmt.assign x e)
  <|> ("if" *> pExpr <* tag "then" <*> many pStatement 
       <*> optional ("else" *> many pStatement) <* tag "end")
      .map (λ⟨cond, then_, maybeElse⟩ => 
        UniversalIR.Stmt.if_ cond then_ (maybeElse.getD []))
  <|> ("while" *> pExpr <* tag "do" <*> many pStatement <* tag "end")
      .map (λ⟨cond, body⟩ => UniversalIR.Stmt.while cond body)
  <|> ("print" *> pExpr <* symbol ";").map UniversalIR.Stmt.print

def pProgram : Parser (List UniversalIR.Stmt) := many pStatement

def parseProgram (input : String) : Except String (List UniversalIR.Stmt) := 
  Parser.runParser pProgram input |>.toExcept (λe => s!"Parse error at {e}")

end UniversalParser
