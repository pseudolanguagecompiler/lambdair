import Ast.UniversalIR

namespace Codegen.ToLean

def exprToLean : UniversalIR.Expr → String
  | .var x => x
  | .num n => toString n
  | .binOp UniversalIR.BinOp.add l r => s!"({exprToLean l} + {exprToLean r})"
  | .binOp UniversalIR.BinOp.sub l r => s!"({exprToLean l} - {exprToLean r})"
  | .binOp UniversalIR.BinOp.gt l r => s!"({exprToLean l} > {exprToLean r})"

def stmtToLean : UniversalIR.Stmt → String
  | .assign x e => s!"let {x} := {exprToLean e}"
  | .print e => s!"IO.println {exprToLean e}"
  | .if_ cond then_ else_ => 
    s!"if {exprToLean cond} then do\n" ++ 
    (then_.map stmtToLean).joinSep "\n  " ++ 
    (if else_.isEmpty then "" else s!" else do\n  " ++ (else_.map stmtToLean).joinSep "\n  ") ++
    "\n"
  | .while cond body => 
    s!"while {exprToLean cond} do\n  " ++ (body.map stmtToLean).joinSep "\n  " ++ "\n"

def fromUniversal (stmts : List UniversalIR.Stmt) : String :=
  "def main : IO Unit := do\n" ++ 
  (stmts.map stmtToLean).joinSep "\n  " ++ "\n"

end Codegen.ToLean
