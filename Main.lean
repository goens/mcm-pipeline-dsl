import PipelineDsl
import Cli
import Lean

import PipelineDsl.ControllerHelpers

open Lean Pipeline

def parseFile : Environment → String → IO (Option String × AST)
 | env, filename => do
   let lines <- IO.FS.lines filename
   let preprocessed := preprocess lines
   let fileStr := preprocessed.foldl (λ s₁ s₂ => s₁ ++ "\n" ++ s₂) ""
   --println! s!"parsing {filename}: \n {fileStr}"
   return (parse fileStr env)

  -- AG: these names are not great, very non-descriptive...
  -- Not only are the numbers not descriptive, but the names are abbreviations and thus harder to read.
  -- Transform is more readable than tsfm, just as controller is more readable than ctrler, etc...
def transformTesting : AST → Array Nat → IO Unit
  | ast, tests => do
    println! s!"===== Transform Testing ====="
    println! s!"Transform Tests: ({tests})"
    -- TODO: make it into array
    -- if tests.elem 0 then
    --   println!  s!"=== Transform 0 ==="
    --   let tsfm0_controllers := ast0002_get_controllers ast
    --   println!  s!"controller entries: \n{tsfm0_controllers}"

    -- if tests.elem 1 then
    --   println!  s!"=== Transform 1 ==="
    --   let tsfm1_last_assn_stmt := ast0004 (ast0002_get_controllers ast)
    --   println!  s!"controller inits: \n{tsfm1_last_assn_stmt}"

    -- if tests.any (fun x => x == 2 || x == 3) then
    -- let tsfm2_entries := ast0010_get_entries ast
    -- if tests.elem 2 then
    --   println!  s!"=== Transform 2 ==="
    --   println!  s!"controller entries: \n{tsfm2_entries}"
    -- if tests.elem 3 then
    --     let tsfm3_last_assn_stmt := ast0013_map_entries tsfm2_entries
    --     println!  s!"=== Transform 3 ==="
    --     println!  s!"controller entries: \n{tsfm3_last_assn_stmt}"

    if tests.elem 4 then
      -- Controller descriptions in one struct
      println! s!"=== all ctrlers ==="
      println! s!"-- print 1 --"
      let ctrlers_except := ast0019_controller_info ast
      println! s!"-- print 2 --"
      let ctrlers : Ctrlers := match ctrlers_except with
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

      -- let cdfg_nodes : Except String (List CDFG.Node) := DSLtoCDFG ctrlers
      -- AZ NOTE: Testing in-order-transform
      println! s!"------ begin in-order transformation ------\n"

      -- println! s!"------ do store -> store in-order transformation ------\n"

      -- ===== CDFG Initial Implementation =====
      -- let ctrlers := match CDFG.InOrderTfsm ctrlers load load with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->ld in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let ctrlers := match CDFG.InOrderTfsm ctrlers store store with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
      --     []

      -- ===== Test Fence =====
      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk load' mfence' load' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let st_fence_st := ( MCMOrdering.ternary_ordering (TernaryOrdering.mk store' mfence' store' Addresses.any) )
      -- let ctrlers := match CDFG.InOrderTransform ctrlers st_fence_st with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk store' mfence' load' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- -- just for sanity, shouldn't break anything
      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk load' mfence' store' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []
      -- ===== Test Fence =====

      -- let ctrlers := match CDFG.InOrderTfsm ctrlers store load with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->ld in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let st_st := ( MCMOrdering.binary_ordering (BinaryOrdering.mk [ store' ] [ store' ] Addresses.any) )
      -- let ctrlers := match CDFG.InOrderTransform ctrlers st_st none with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
      --     []

      let st_st := ( MCMOrdering.binary_ordering (BinaryOrdering.mk [ store' ] [ store' ] Addresses.any) )
      let ctrlers := match CDFG.InOrderTransform ctrlers st_st none with
        | .ok ctrler_list => ctrler_list
        | .error msg => 
          dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
          []

      let ctrlers := match ctrlers.AddInvalidationBasedLoadOrdering with
        | .ok ctrler_list => ctrler_list
        | .error msg => 
          dbg_trace s!"Error adding invalidation listener: ({msg})"
          []
          
      -- let st_ld'_fence_ld := ( MCMOrdering.ternary_ordering (TernaryOrdering.mk [ store', load' ] mfence' [ load' ] Addresses.any) )
      -- let ctrlers := match Ctrlers.CDFGLoadReplayTfsm ctrlers st_ld'_fence_ld with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying Load-Replay ld->ld in CDFG LoadReplayTfsm: ({msg})"
      --     []

      println! s!"What ctrlers look like after TFSM:"
      println! s!"Ctrlers: {ctrlers}"
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

      -- dbg_trace "== CDFG GRAPH =="
      -- dbg_trace s!"CDFG Graph: {cdfg_nodes}"
      -- dbg_trace "== End CDFG GRAPH =="

      println! s!"===== Transform Testing Concluding ====="

    if tests.elem 5 then
      -- Controller descriptions in one struct
      println! s!"=== all ctrlers ==="
      println! s!"-- print 1 --"
      let ctrlers_except := ast0019_controller_info ast
      println! s!"-- print 2 --"
      let ctrlers : Ctrlers := match ctrlers_except with
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

      -- let cdfg_nodes : Except String (List CDFG.Node) := DSLtoCDFG ctrlers
      -- AZ NOTE: Testing in-order-transform
      println! s!"------ begin in-order transformation ------\n"

      -- println! s!"------ do store -> store in-order transformation ------\n"

      -- ===== CDFG Initial Implementation =====
      -- let ctrlers := match CDFG.InOrderTfsm ctrlers load load with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->ld in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let ctrlers := match CDFG.InOrderTfsm ctrlers store store with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
      --     []

      -- ===== Test Fence =====
      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk load' mfence' load' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let st_fence_st := ( MCMOrdering.ternary_ordering (TernaryOrdering.mk store' mfence' store' Addresses.any) )
      -- let ctrlers := match CDFG.InOrderTransform ctrlers st_fence_st with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk store' mfence' load' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []

      -- -- just for sanity, shouldn't break anything
      -- let ctrlers := match CDFG.InOrderTransform ctrlers ( MCMOrdering.ternary_ordering (TernaryOrdering.mk load' mfence' store' Addresses.any) ) with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying ld->mfence in CDFG InOrderTfsm: ({msg})"
      --     []
      -- ===== Test Fence =====

      -- let ctrlers := match CDFG.InOrderTfsm ctrlers store load with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->ld in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let st_st := ( MCMOrdering.binary_ordering (BinaryOrdering.mk [ store' ] [ store' ] Addresses.any) )
      -- let ctrlers := match CDFG.InOrderTransform ctrlers st_st none with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
      --     []

      -- let st_st := ( MCMOrdering.binary_ordering (BinaryOrdering.mk [ store' ] [ store' ] Addresses.any) )
      -- let ctrlers := match CDFG.InOrderTransform ctrlers st_st none with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying st->st in CDFG InOrderTfsm: ({msg})"
      --     []

      let ctrlers := match ctrlers.AddInvalidationBasedLoadOrdering with
        | .ok ctrler_list => ctrler_list
        | .error msg => 
          dbg_trace s!"Error adding invalidation listener: ({msg})"
          []
          
      -- let st_ld'_fence_ld := ( MCMOrdering.ternary_ordering (TernaryOrdering.mk [ store', load' ] mfence' [ load' ] Addresses.any) )
      -- let ctrlers := match Ctrlers.CDFGLoadReplayTfsm ctrlers st_ld'_fence_ld with
      --   | .ok ctrler_list => ctrler_list
      --   | .error msg => 
      --     dbg_trace s!"Error applying Load-Replay ld->ld in CDFG LoadReplayTfsm: ({msg})"
      --     []

      println! s!"What ctrlers look like after TFSM:"
      println! s!"Ctrlers: {ctrlers}"
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

      -- dbg_trace "== CDFG GRAPH =="
      -- dbg_trace s!"CDFG Graph: {cdfg_nodes}"
      -- dbg_trace "== End CDFG GRAPH =="

      println! s!"===== Transform Testing Concluding ====="

def runMainCmd : Cli.Parsed → IO UInt32
  | args => do
  let executablePath : System.FilePath ← IO.appPath
  let libPath : System.FilePath := match executablePath.parent >>= System.FilePath.parent with
    | some parent => parent / "lib"
    | none => "build/lib"
  initSearchPath (← Lean.findSysroot) [libPath]
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

  let directory := match args.flag? "output-dir" with
      | none => "murphi_output"
      | some dir => dir.as! $ String

  let _ ← match args.flag? "transform-testing" with
      | none => pure ()
      | some tests =>
        transformTesting ast $ tests.as! $ Array Nat
  if args.hasFlag "emit-murphi" || args.hasFlag "murphi-testing" then
    let _ ← emitMurphiIO (args.hasFlag "emit-murphi") (args.hasFlag "murphi-testing") directory ast


  return 0


def mainCmd := `[Cli|
    aqlc VIA runMainCmd;
    "The AQL Compiler"
    FLAGS:
      r, "round-trip";                       "Round trip the AST through the parser and pretty printer"
      s, "sanity-check";                     "Run sanity check on the round tripped code"
      a, "pretty-print-ast";                 "Pretty print the AST"
      m, "emit-murphi";                      "Emit output Murphi"
      M, "muprhi-testing";                   "Run Murphi-specific tests"
      D, "output-dir" : String;              "Output directory for emitting Murphi"
      t, "transform-testing" : Array Nat;  "Print witnesses when exploring"
    ARGS:
      input : String;      "Input AQL file"
    ]

def main (args : List String): IO UInt32 :=
  mainCmd.validate args
