import Parser.UniversalParser
import Ast.UniversalIR
import Codegen.ToLean

def main (args : List String) : IO UInt32 := do
  if args.size != 1 then
    IO.println "usage: lake exe pseudocode_compiler <file.pseudo>"
    return 1
  
  let input ← IO.FS.readFile args.head!
  IO.println s!"Parsing: {args.head!}"
  
  match UniversalParser.parseProgram input with
  | .error msg => 
    IO.println s!"Parse error: {msg}"
    return 1
  | .ok ast =>
    IO.println "✓ Parse successful"
    IO.println s!"AST: {ast}" -- Debug AST
    
    -- Execute semantics
    let result ← UniversalIR.evalProgram ast (λ_ => none)
    IO.println s!"Execution result: {result}"
    
    -- Generate Lean code
    let leanCode ← Codegen.ToLean.fromUniversal ast
    IO.println "\n=== Generated Lean 4 code ==="
    IO.println leanCode
    
    -- Test with your example input
    IO.println "\n=== Test Input Expected Output ==="
    IO.println "5 4 3 2 1 (from while loop)"
    
    return 0
