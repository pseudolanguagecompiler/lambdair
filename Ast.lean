inductive BinOp where
  | add | sub | gt -- greater than for conditions
deriving Repr, BEq

inductive Expr where
  | var (name : String)
  | num (n : Nat)
  | binOp (op : BinOp) (left right : Expr)
deriving Repr, BEq

inductive Stmt where
  | assign (var : String) (expr : Expr)
  | if_ (cond : Expr) (then_ else_ : List Stmt)
  | while (cond : Expr) (body : List Stmt)
  | print (expr : Expr)
deriving Repr, BEq

abbrev State = String → Option Nat
