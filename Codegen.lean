import Ast

def Codegen.toLean : List Stmt → String
  | [] => "def main : IO Unit := pure ()"
  | stmts => 
    "def main : IO Unit := do\n" ++ 
    (stmts.map codegenStmt).joinSep "\n  " ++ "\n"

def codegenStmt : Stmt → String
  | .assign x (.num n) => s!"IO.println \"set {x} := {n}\""
  | .print e => s!"IO.println {codegenExpr e}"
  | _ => "sorry -- implement me"

def codegenExpr : Expr → String
  | .var x => x
  | .num n => toString n
  | _ => "0" -- simplify
