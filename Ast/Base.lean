namespace Ast

class IR where
  Expr : Type
  Stmt : Type
  stateType : Type
  evalExpr : Expr → stateType → Nat
  evalStmt : Stmt → stateType → stateType
  evalProgram : List Stmt → stateType → IO Nat

end Ast
