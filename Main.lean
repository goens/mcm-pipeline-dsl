import PipelineDsl
import Lean
import PipelineDsl.Preprocess
import PipelineDsl.translation
import PipelineDsl.MurphiTests
open Lean Pipeline

-- def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-nosq/iter-1/load-controller.file"
-- NOTE: This is the "rewrite" DSL filename
def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-henn-patt/o3/lsq-phys-reg-dsl-rewrite-flattened-simple.file"

def main (args : List String): IO Unit := do
  let filename := match args.get? 0 with
   | some fn => fn
   | nothing => default_filename
  let lines <- IO.FS.lines filename
  let preprocessed := preprocess lines
  let fileStr := preprocessed.foldl (λ s₁ s₂ => s₁ ++ "\n" ++ s₂) ""
  println! s!"parsing {filename}: \n {fileStr}"
  -- let pipeline := Lean.quote [file]
  initSearchPath (← Lean.findSysroot) ["build/lib"]
  let env ← importModules [{ module := `PipelineDsl.Parser }] {}
  let parsed := parse fileStr env
  println! s!""
  let res_str := match parsed.1 with
    | some msg => "syntax error:\n" ++ msg
    | none => s!"parse ok! parsed: \n---\n{parsed.2}\n---\n"
  let round := roundTrip env parsed.2
  println! res_str
  println! s!"round-trip: \n---\n{round.2}\n---\n"
  let sanity_check := toString parsed.2 == toString round.2
  println! s!"parse . toString . parse . toString == parse . toString? : {sanity_check}"
  println! "---- test murhpi ----"
  println! testprog.toString

  -- transform tests...
  println! s!"===== Transform Testing ====="

  println! s!"The AST:"
  let ast00 := parsed.2
  println! s!"{ast00}"

  println! s!"=== tsfm0 ==="
  let tsfm0_controllers := ast0002_get_controllers parsed.2
  println! s!"controller entries: \n{tsfm0_controllers}"

  println! s!"=== tsfm1 ==="
  let tsfm1_last_assn_stmt := ast0004 (ast0002_get_controllers parsed.2)
  println! s!"controller inits: \n{tsfm1_last_assn_stmt}"

  println! s!"=== tsfm2 ==="
  let tsfm2_entries := ast0010_get_entries parsed.2
  println! s!"controller entries: \n{tsfm2_entries}"

  println! s!"=== tsfm3 ==="
  let tsfm3_last_assn_stmt := ast0013_map_entries tsfm2_entries
  println! s!"controller entries: \n{tsfm3_last_assn_stmt}"

  -- Controller descriptions in one struct
  println! s!"=== all ctrlers ==="
  let ctrlers := ast0019_controller_info parsed.2
  println! s!"controller entries: \n{ctrlers}"

  -- Get basic Murphi Records
  println! s!"=== murphi records for each ctrler ==="
  let murphi_records := ctrlers.map ast0048_generate_controller_murphi_record
  println! s!"ctrler records: \n{murphi_records}"

  println! s!"===== Transform Testing Concluding ====="

  return ()

