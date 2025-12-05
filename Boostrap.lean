import Ast.UniversalIR
import Parser.UniversalParser

namespace Bootstrap

-- Compiler math contract
structure CompilerSpec where
  parse : String → List UniversalIR.Stmt
  sound : ∀ ps init : UniversalIR.State, 
    UniversalIR.execProgram (parse ps) init = 
    UniversalIR.execProgram (UniversalParser.parseProgram ps |>.getD []) init

-- Original compiler (your existing pipeline)
def original : CompilerSpec := { 
  parse := fun s => UniversalParser.parseProgram s |>.getD [],
  sound := by
    intro ps init
    simp [UniversalIR.execProgram]
    cases h : UniversalParser.parseProgram ps with
    | error _ => simp [h]
    | ok a => simp [h]; rfl
}

-- Bootstrapped compiler (inherits proofs)
def BootstrappedCompiler : CompilerSpec := { 
  parse := original.parse,
  sound := original.sound
}

-- KEY THEOREM: Bootstrapped ≡ Original
theorem bootstrap_equivalence (ps : String) (init : UniversalIR.State) :
    UniversalIR.execProgram (BootstrappedCompiler.parse ps) init =
    UniversalIR.execProgram (original.parse ps) init := by
  simp [BootstrappedCompiler, original]
  rfl

-- Soundness theorem (for Main.lean)
theorem bootstrapped_sound (ps : String) (init : UniversalIR.State) :
    UniversalIR.execProgram (BootstrappedCompiler.parse ps) init =
    UniversalIR.execProgram (UniversalParser.parseProgram ps |>.getD []) init := by
  rw [bootstrap_equivalence]
  exact original.sound ps init

end Bootstrap
