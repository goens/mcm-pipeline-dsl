import PipelineDsl
import Lean
import Cli
open Lean Pipeline

-- def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-nosq/iter-1/load-controller.file"
-- NOTE: This is the "rewrite" DSL filename
def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-henn-patt/o3/lsq-phys-reg-dsl-rewrite-flattened-simple.file"

def parseFile : Environment → String → IO (Option String × AST)
 | env, filename => do
   let lines <- IO.FS.lines filename
   let preprocessed := preprocess lines
   let fileStr := preprocessed.foldl (λ s₁ s₂ => s₁ ++ "\n" ++ s₂) ""
   println! s!"parsing {filename}: \n {fileStr}"
   return (parse fileStr env)

  -- AG: these names are not great, very non-descriptive...
  -- Not only are the numbers not descriptive, but the names are abbreviations and thus harder to read.
  -- Transform is more readable than tsfm, just as controller is more readable than ctrler, etc...
def transformTesting : AST → Array Nat → IO Unit
  | ast, tests => do
    println! s!"===== Transform Testing ====="
    -- TODO: make it into array
    if tests.elem 0 then
      println!  s!"=== Transform 0 ==="
      let tsfm0_controllers := ast0002_get_controllers ast
      println!  s!"controller entries: \n{tsfm0_controllers}"

    if tests.elem 1 then
      println!  s!"=== Transform 1 ==="
      let tsfm1_last_assn_stmt := ast0004 (ast0002_get_controllers ast)
      println!  s!"controller inits: \n{tsfm1_last_assn_stmt}"

    if tests.any (fun x => x == 2 || x == 3) then
    let tsfm2_entries := ast0010_get_entries ast
    if tests.elem 2 then
      println!  s!"=== Transform 2 ==="
      println!  s!"controller entries: \n{tsfm2_entries}"
    if tests.elem 3 then
        let tsfm3_last_assn_stmt := ast0013_map_entries tsfm2_entries
        println!  s!"=== Transform 3 ==="
        println!  s!"controller entries: \n{tsfm3_last_assn_stmt}"

def runMainCmd : Cli.Parsed → IO UInt32
  | args => do
  initSearchPath (← Lean.findSysroot) ["build/lib"]
  let env ← importModules [{ module := `PipelineDsl.Parser }] {}
  let input : String := args.positionalArg! "input" |>.as! String
  let (err, ast) ← parseFile env input
  -- Ugly imperative side-effect code
  let _  ← match err with
    | some msg => IO.println $ "syntax error:\n" ++ msg
    | none => pure ()
  if args.hasFlag "round-trip" then
    let (_, round) := roundTrip env ast
    println! s!"round-trip: \n---\n{round}\n---\n"
    if args.hasFlag "sanity-check" then
      let sanity_check := toString ast == toString round
      println! s!"parse . toString . parse . toString == parse . toString? : {sanity_check}"

  if args.hasFlag "pretty-print-ast" then
    println!  s!"The AST:"
    let ast00 := ast
    println!  s!"{ast00}"

  let _ ← match args.flag? "transform-testing" with
      | none => pure ()
      | some tests =>
        transformTesting ast $ tests.as! $ Array Nat
  if args.hasFlag "emit-murphi" || args.hasFlag "murphiTesting" then
    let _ ← emitMurphiIO (args.hasFlag "emit-murphi") (args.hasFlag "murphiTesting") ast

  return 0


def mainCmd := `[Cli|
    aqlc VIA runMainCmd;
    "Execute litmus tests on the LOST-POP model."
    FLAGS:
      r, "round-trip";                       "Round trip the AST through the parser and pretty printer"
      s, "sanity-check";                     "Run sanity check on the round tripped code"
      a, "pretty-print-ast";                 "Pretty print the AST"
      m, "emit-muprhi";                      "Emit output Murphi"
      M, "muprhi-testing";                   "Run Murphi-specific tests"
      t, "transformer-testing" : Array Nat;  "Print witnesses when exploring"
    ]


def main (args : List String): IO UInt32 :=
  mainCmd.validate args
