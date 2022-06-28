import PipelineDsl
import Lean
import PipelineDsl.Preprocess
import PipelineDsl.translation
open Lean Pipeline

def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-nosq/iter-1/load-controller.file"

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
  let res_str := match parsed.1 with
    | some msg => "syntax error:\n" ++ msg
    | none => s!"parse ok! parsed: \n---\n{parsed.2}\n---\n"
  let round := roundTrip env parsed.2
  println! res_str
  println! s!"round-trip: \n---\n{round.2}\n---\n"
  let sanity_check := toString parsed.2 == toString round.2
  println! s!"parse . toString . parse . toString == parse . toString? : {sanity_check}"

  -- -- transform tests...
  -- let tsfm0 := ast0002_get_controllers parsed.2
  -- let tsfm1 := ast0004 (ast0002_get_controllers parsed.2)
  -- println! s!"controller entries: {tsfm0}"
  -- println! s!"controller inits: {tsfm1}"

  return ()

