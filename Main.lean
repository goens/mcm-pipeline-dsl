import PipelineDsl
import Lean
-- import PipelineDsl.Preprocess
import PipelineDsl.AnalysisHelpers
import PipelineDsl.LitmusTests
import PipelineDsl.InOrderTransformation
-- import PipelineDsl.Translation
-- import PipelineDsl.Transformation
-- -- import PipelineDsl.MurphiTests
open Lean Pipeline

-- def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-nosq/iter-1/load-controller.file"
-- NOTE: This is the "rewrite" DSL filename
def default_filename := "Examples/graph-prototype/operational-axiomatic/lsq-henn-patt/o3/lsq-phys-reg-dsl-rewrite-flattened-simple.file"
def output_file := "generated_output.m"

def main (args : List String): IO Unit := do
  let filename := match args.get? 0 with
   | some fn => fn
   | _ => default_filename
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
  -- println! testprog.toString

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

  -- println! s!"=== ctrlers with both loads & memory access ==="
  -- let ctrlers_that_do_load_and_mem_access :=
  --   find_load_begin_perform_info ctrlers
  -- println! s!"ctrlers with both loads and mem access:\n{ctrlers_that_do_load_and_mem_access}"

  -- let should_be_one_ctrler :=
  --   match ctrlers_that_do_load_and_mem_access with
  --   | [one_ctrler] => one_ctrler
  --   | _ => ast0021_empty_controller

  -- println! s!"=== stall-inserted ctrler ==="
  -- let in_order_load := get_ctrler_state_with_mem_load_req should_be_one_ctrler

  -- let just_one_state : String := in_order_load[0]!

  -- AZ NOTE: Testing in-order-transform
  let ctrlers := ctrlers.map (
    λ ctrl => 
      let output := naive_update_add_stall_to_global_perform_ld (ctrl, ctrl.transition_list)
      match output with
      | .ok ctrler_info => ctrler_info
      | .error str => dbg_trace s!"ERROR: doing in-order TSFM: {str}"
        ast0021_empty_controller
  )
  println! s!"------ begin in-order transformation ------\n"
  println! s!"{ctrlers}"
  println! s!"------ end in-order transformation ------\n"

  let all_joined_ctrlers : List (List dsl_trans_info) :=
  ctrlers.map (
    λ ctrler =>
      ctrler.transition_list.map
        λ trans =>
          {
            ctrler_name := ctrler.name,
            ctrler_lst := ctrlers,
            trans := trans
          }
  )

-- structure dsl_trans_info where
-- ctrler_name: Identifier
-- ctrler_lst : List controller_info
-- trans : Description -- Description.transition
  let all_rules : List Murϕ.Rule := List.join (
    (List.join all_joined_ctrlers).map dsl_trans_descript_to_murphi_rule)
  println! s!"All Rules:\n{all_rules}"

  -- translate other parts, and get the murphi file..
  let ctrler_names : List Identifier := ctrlers.map λ ctrler => ctrler.name
  let buffer_idx_seq_num_search_funcs := gen_buffer_ctrler_seq_num_search_func ctrler_names
  let (const_decls, ctrler_decls) := gen_murphi_file_decls ctrlers

-- def compose_murphi_file_components
-- -- Consts, like num entries per buffer-type ctrler
-- ( const_decls : List Murϕ.Decl)
-- -- Types, like ctrler defns
-- ( type_decls : List Murϕ.Decl)
-- ( func_decls : List Murϕ.ProcDecl)
-- ( rules : List Murϕ.Rule)
-- : Murϕ.Program

  -- Just for testing one thing, make sure to provide a Litmus
  -- and import the litmustests...
  -- let murphi_file : Murϕ.Program := compose_murphi_file_components const_decls ctrler_decls buffer_idx_seq_num_search_funcs all_rules ctrlers
  -- IO.FS.writeFile output_file murphi_file.toString

  let murphi_files : List MurphiFile := --Murϕ.Program :=
  gen_murphi_litmus_test_programs const_decls ctrler_decls buffer_idx_seq_num_search_funcs all_rules ctrlers

  let _ ← murphi_files.mapM (
    fun file =>
      IO.FS.writeFile (file.filename.append ".m") file.program.toString
  )

  -- AZ TODO:
  -- add code to filter for certain controllers (LSQ ctrlers)
  -- and just run translation / transformation on those controllers
  -- But we still supply the list of all controllers for
  -- the translation and transformation steps

  println! s!"===== Transform Testing Concluding ====="

  return ()

