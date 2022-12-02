import PipelineDsl
import Lean
-- import PipelineDsl.Preprocess
import PipelineDsl.AnalysisHelpers
import PipelineDsl.LitmusTests
import PipelineDsl.InOrderTransformation
-- import PipelineDsl.EmitMurphiNoROB
-- import PipelineDsl.EmitMurphiNoIQNoROB
import PipelineDsl.EmitMurphiNoRenameNoIQNoROB
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
  println! s!"-- print 1 --"
  let ctrlers_except := ast0019_controller_info parsed.2
  println! s!"-- print 2 --"
  let ctrlers := match ctrlers_except with
    | .ok lst_ctrlers => lst_ctrlers
    | .error msg =>
      dbg_trace s!"Error getting the ctrler info from the parsed AST! ({msg})"
      dbg_trace s!"Returning default for now, empty list."
      default
  println! s!"controller entries: \n{ctrlers}"

  -- Get basic Murphi Records
  println! s!"=== murphi records for each ctrler ==="
  let murphi_records : List ctrler_decl_entry_decl_const_decl :=
    ctrlers.map ast0048_generate_controller_murphi_record
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
  println! s!"------ begin in-order transformation ------\n"
  -- NOTE: Naive initial implementation
  -- let ctrlers := ctrlers.map (
  --   λ ctrl => 
  --     let output := naive_update_add_stall_to_global_perform_ld (ctrl, ctrl.transition_list)
  --     match output with
  --     | .ok ctrler_info => ctrler_info
  --     | .error str => dbg_trace s!"ERROR: doing in-order TSFM: {str}"
  --       ast0021_empty_controller
  -- )
  let in_order_ld_ctrlers : Except String (List controller_info) := more_generic_core_in_order_stall_transform ctrlers load load
  let in_order_ld_ctrlers : List controller_info := match in_order_ld_ctrlers with
  | .ok ctrl_list => ctrl_list
  | .error msg =>
    let msg' : String :=
      "ERROR: when trying to run 'more_generic_core_in_order_stall_transform' on load -> load\n" ++
      s!"thrown message: ({msg})" ++
      "\nReturn original ctrler list for now.."
    dbg_trace msg'
    ctrlers
  println! s!"------ do store -> store in-order transformation ------\n"
  let in_order_ld_and_st_ctrlers : Except String (List controller_info) := more_generic_core_in_order_stall_transform in_order_ld_ctrlers store store
  let ctrlers : List controller_info := match in_order_ld_and_st_ctrlers with
  | .ok ctrl_list => ctrl_list
  | .error msg =>
    let msg' : String :=
      "ERROR: when trying to run 'more_generic_core_in_order_stall_transform' on store -> store\n" ++
      s!"thrown message: ({msg})" ++
      "\nReturn original ctrler list for now.."
    dbg_trace msg'
    ctrlers
      
  println! s!"{ctrlers}"
  println! s!"------ end in-order transformation ------\n"

  let all_joined_ctrlers : List (List dsl_trans_info) :=
  ctrlers.map (
    λ ctrler =>
      dbg_trace s!"preparing to emit murphi for ctrler: ({ctrler})"
      let states : List Description :=
        if ctrler.init_trans.isSome then
          ctrler.transition_list.get!
        else if ctrler.ctrler_init_trans.isSome then
          ctrler.ctrler_trans_list.get!
        else
          dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
          default
      states.map
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
    (List.join all_joined_ctrlers).map 
    ( λ dsl_trans : dsl_trans_info =>
      let this_ctrler : controller_info :=
        -- dbg_trace "===== list_ident_to_murphi_designator_ctrler_var_check ====="
        get_ctrler_matching_name dsl_trans.ctrler_name dsl_trans.ctrler_lst
      
      if this_ctrler.init_trans.isSome then
        dsl_trans_entry_descript_to_murphi_rule dsl_trans
      else if this_ctrler.ctrler_init_trans.isSome then
        let except_murphi_rule : Except String ( List Murϕ.Rule ) :=
          dsl_trans_ctrler_to_murphi dsl_trans
        match except_murphi_rule with
        | .ok list_rule => list_rule
        | .error msg => dbg_trace s!"Hit an error while translating a ctrler-type structure: ({msg})"
          default
      else
        dbg_trace s!"A ctrler that doesn't have either an entry-type or ctrler-type init statement?"
        default
    )
    )
  println! s!"All Rules:\n{all_rules}"

  -- translate other parts, and get the murphi file..
  let ctrler_names : List Identifier := ctrlers.map λ ctrler => ctrler.name
  let buffer_idx_seq_num_search_funcs := gen_buffer_ctrler_seq_num_search_func ctrler_names
  let (const_decls, ctrler_decls) := gen_murphi_file_decls ctrlers
  println! s!"Ctrlers: ({ctrlers})"

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
  gen_murphi_litmus_test_programs_no_RENAME_no_IQ_no_ROB const_decls ctrler_decls buffer_idx_seq_num_search_funcs all_rules ctrlers

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

