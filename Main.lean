-- PseudoC: Practical Pseudocode Compiler in Lean 4 
-- Phase 1: PseudoC dialect → ⟦PseudoC⟧ denotational semantics → Lean 4 codegen
-- Multi-grammar: Parser/Dialect → PseudoC.AST → same semantics (via Ast/IR)
-- Loops: Fixed-point semantics 

import Parser.PseudoCParser
import Ast.PseudoC
import Codegen.ToLean

def main (args : List String) : IO UInt32 := do
  if args.size != 1 then
    IO.println "usage: lake exe pseudoc <file.pseudo>"
    return 1
  
  let input ← IO.FS.readFile args.head!
  IO.println s!"Input: {input}"
  
  match PseudoCParser.parseProgram input with
  | .error msg => 
    IO.println s!"Parse error: {msg}"
    return 1
  | .ok ast =>
    IO.println "✓ PseudoC parsed successfully"
    IO.println s!"AST: {ast}"
    
    -- Denotational execution: ⟦program⟧ init (denotational semantics)
    let init : PseudoC.State := λ_ => none
    PseudoC.execProgram ast init
    
    -- Lean 4 codegen (Phase 1) → leanc --ir → LLVM IR → bytecode VM (Phase 3)
    let leanCode ← Codegen.ToLean.fromPseudoC ast
    IO.println "\n=== Generated Lean 4 ==="
    IO.println leanCode
    return 0
