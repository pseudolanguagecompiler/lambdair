-- GROUNDED IN MINIMALIST PROGRAM (Chomsky 1995): Merge + Move + Phases
-- Bare Phrase Structure: Merge(α,β) = {α,{α,β}} where α=label
-- Empirical basis: hierarchical dominance, island effects, recursion

inductive LexCat where  -- Functional/lexical heads (v, T, C minimal)
  | N | V | v | T | C
deriving Repr, BEq

structure LexItem where
  cat : LexCat
  phon : String  -- PF spellout: "dog", "runs"
deriving Repr, BEq

inductive SyntacticObject where  -- Bare: {H, XP} no projections [web:51]
  | lex (item : LexItem)
  | merge (head specOrComp : SyntacticObject)  -- External/Internal Merge
deriving Repr, BEq

inductive Derivation where
  | root (so : SyntacticObject) (phase : PhaseType)  -- CP/vP phases
  | move (target from : SyntacticObject) (toEdge : SyntacticObject)  -- Copy+Delete PF
deriving Repr, BEq

inductive PhaseType where  -- Chomsky: CP/vP spellout domains [web:51][web:53]
  | CP | vP
deriving Repr, BEq
