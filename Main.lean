import PipelineDsl
import Lean
import PipelineDsl.Preprocess
open Lean Pipeline

def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-nosq/iter-1/load-controller.file"

def main (args : List String): IO Unit := do
  let filename := match args.get? 0 with
   | some fn => fn
   | nothing => default_filename
  let lines <- IO.FS.lines filename
  let preprocessed := Preprocess lines
  let fileStr := preprocessed.foldl (λ s₁ s₂ => s₁ ++ "\n" ++ s₂) ""
  println! s!"parsing {filename}: \n {fileStr}"
  -- let pipeline := Lean.quote [file]
  initSearchPath (← Lean.findSysroot) ["build/lib"]
  let env ← importModules [{ module := `PipelineDsl.Parser }] {}
  let parsed := parse fileStr env
  let res_str := match parsed.1 with
    | some msg => "syntax error:\n" ++ msg
    | none => s!"parse ok! round-trip: \n {parsed.2}"
  IO.println res_str
