import PipelineDsl.EmitMurphiNoRenameNoIQNoROB
import PipelineDsl.AST
open Pipeline

def emitMurphiIO (emit : Bool) (testing : Bool) (ast : AST) : IO Unit := do
    let mut teststr := ""
    teststr := teststr ++ "---- test murhpi ----"

    -- Controller descriptions in one struct
    teststr := teststr ++ s!"=== all ctrlers ==="
    teststr := teststr ++ s!"-- print 1 --"
    let ctrlers_except := ast0019_controller_info ast
    teststr := teststr ++ s!"-- print 2 --"
    let ctrlers := match ctrlers_except with
      | .ok lst_ctrlers => lst_ctrlers
      | .error msg =>
        dbg_trace s!"Error getting the ctrler info from the parsed AST! ({msg})"
        dbg_trace s!"Returning default for now, empty list."
        default
    teststr := teststr ++ s!"controller entries: \n{ctrlers}"

    -- Get basic Murphi Records
    teststr := teststr ++ s!"=== murphi records for each ctrler ==="
    let murphi_records : List ctrler_decl_entry_decl_const_decl :=
      ctrlers.map ast0048_generate_controller_murphi_record
    teststr := teststr ++ s!"ctrler records: \n{murphi_records}"


    let cdfg_nodes : Except String (List CDFG.Node) := DSLtoCDFG ctrlers
    -- AZ NOTE: Testing in-order-transform
    teststr := teststr ++ s!"------ begin in-order transformation ------\n"
    teststr := teststr ++ s!"What ctrlers look like after TFSM:"
    teststr := teststr ++ s!"Ctrlers: {ctrlers}"
    teststr := teststr ++ s!"------ end in-order transformation ------\n"
    teststr := teststr ++ "== CDFG GRAPH =="
    teststr := teststr ++ s!"CDFG Graph: {cdfg_nodes}"
    teststr := teststr ++ "== End CDFG GRAPH =="
    -- AZ TODO:
    -- add code to filter for certain controllers (LSQ ctrlers)
    -- and just run translation / transformation on those controllers
    -- But we still supply the list of all controllers for
    -- the translation and transformation steps


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
    teststr := teststr ++ s!"All Rules:\n{all_rules}"

    -- translate other parts, and get the murphi file..
    let ctrler_names : List Identifier := ctrlers.map λ ctrler => ctrler.name
    let buffer_idx_seq_num_search_funcs := gen_buffer_ctrler_seq_num_search_func ctrler_names
    let (const_decls, ctrler_decls) := gen_murphi_file_decls ctrlers
    teststr := teststr ++ s!"Ctrlers: ({ctrlers})"

--   def compose_murphi_file_components

    let murphi_files : List MurphiFile := --Murϕ.Program :=
    gen_murphi_litmus_test_programs_no_RENAME_no_IQ_no_ROB const_decls ctrler_decls buffer_idx_seq_num_search_funcs all_rules ctrlers

    if emit then
      let _ ← murphi_files.mapM
        fun file => IO.FS.writeFile (file.filename.append ".m") file.program.toString
    if testing then
      IO.println teststr
