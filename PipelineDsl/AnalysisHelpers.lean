
import PipelineDsl.AST
import Murphi
import PipelineDsl.LoadReplayHelpers
import PipelineDsl.InstructionHelpers
import PipelineDsl.ControllerHelpers
import PipelineDsl.PipelineHelpers

-- AZ NOTE: Consider placing into a namespace for instruction related things, definitions, etc.

-- Just defining as string for now
-- might be nicer to define them as structs later with meta-data
-- Inst in Litmus Test


-- #eval InstType.toMurphiString (InstType.load)
-- #eval (InstType.load).toMurphiString



structure file_name_and_output where
name : String
murphi_code : Murϕ.Program

open Pipeline
open Murϕ

def get_val_decl_stmt_var
(stmt : Pipeline.Statement)
:=
  match stmt with
  | Statement.value_declaration typed_ident expr =>
  -- | Statement.variable_assignment qual_name expr =>
    match expr with
    | Expr.some_term term =>
      match term with
      | Term.var ident =>
        ident
      | _ => dbg_trace "Error: unexpected Term"
      default
    | _ => dbg_trace "Error: unexpected Expr"
      default
  | _ => dbg_trace s!"Error: Expected value_declaration Stmt in get_var_decl_stmt_var. instead got ({stmt})"
    -- dbg_trace "BEGIN Stmt:\n"
    -- dbg_trace stmt
    -- dbg_trace "END Stmt:\n"
    default

def get_ordering_from_ctrler_descript
(ctrler_descript : Description)
:=
  match ctrler_descript with
  | Description.controller ident stmt =>
    match stmt with
    | Statement.block lst_stmts =>
      let ordering_stmt_lst := (
        filter_lst_of_stmts_for_ordering_asn
        lst_stmts
      )
      -- should only be 1 stmt for ordering
      let ordering_stmt :=
        match ordering_stmt_lst with
        | [one_stmt] => one_stmt
        | _ => dbg_trace "Error: unexpected List size?"
          -- dbg_trace "List:\n"
          -- dbg_trace ordering_stmt_lst
          -- dbg_trace "List_stmts:\n"
          -- dbg_trace lst_stmts
          default
      -- as an Identifier
      let ordering_type :=
        get_val_decl_stmt_var ordering_stmt

      ordering_type
    | _ => dbg_trace "Error: unexpected stmt in order search"
      default
  | _ => dbg_trace "Error: unexpected ctrler in order search"
    default

def get_ctrler_elem_ordering
(ctrler : controller_info)
:=
  let ctrler_description := ctrler.controller_descript
  let ctrler_ordering :=
    get_ordering_from_ctrler_descript (
      ctrler_description
    )
  ctrler_ordering


open Murϕ in
structure ctrler_decl_entry_decl_const_decl where
-- Decl.type ctrl.name (TypeExpr.record, ID/String TypeExpr.record)
ctrler_decl : Decl
entry_decl : Option Decl -- Decl.type, ID/String TypeExpr.record
const_decl_lst : List Decl -- Decl.const, ID/String Expr.integerConst
range_enum_decl : List Decl --
entry_state_decl : Decl

open Murϕ in
instance : ToString ctrler_decl_entry_decl_const_decl := ⟨
  λ i =>
    "=== Controller, Entry, Const, and Entry Range Decls ===\n" ++
    "<<<MURPHI CONTROLLER DECL>>>\n" ++ toString i.ctrler_decl ++ "\n\n" ++
    "<<<MURPHI ENTRY DECL>>>\n" ++ toString i.entry_decl ++ "\n\n" ++
    "<<<MURPHI CONST DECL LST>>>\n" ++ toString i.const_decl_lst ++ "\n\n" ++
    "<<<MURPHI ENUM DECL>>>\n" ++ toString i.range_enum_decl ++
    "\n=== End Controller Defn Decls ===\n\n"
  ⟩

def dsl_type_to_murphi_type
( dsl_type : Identifier )
: Murϕ.TypeExpr
:=
  -- Types that one may use in the DSL.. & it's Murphi Test Harness "Equivalent" (for now that is)
  -- DSL        |    Murphi Test Harness
  -- -----------------------------------
  -- address    |    addr_idx_t
  -- u32        |    val_t
  -- packet     |    N/A
  -- seq_num    |    inst_count_t
  -- inst       |    INST
  -- boolean    |    boolean
  let murphi_type_name : ID :=
  if dsl_type == "address" then
  "addr_idx_t"
  else if dsl_type == "u32" then
  "val_t"
  else if dsl_type == "seq_num" then
  "inst_count_t"
  else if dsl_type == "inst" then
  "INST"
  else if dsl_type == "bool" then
  "boolean"
  -- else if dsl_type == "boolean" then
  -- "boolean"
  else
  panic! s!"ERROR: ===== ENCOUNTERED UNEXPECTED DSL TYPE: ({dsl_type}) ====="

  let murphi_type_expr : Murϕ.TypeExpr :=
  Murϕ.TypeExpr.previouslyDefined murphi_type_name

  murphi_type_expr

-- open /-Murphi-/Murϕ in
def ast0048_generate_controller_murphi_record
( ctrl : Ctrler )
:=
  -- TODO: read the entry description
  -- build the per entry record of state vars
  -- read the transition names
  -- build allowable states murphi enum
  -- Also add state for searching

  let entry_or_ctrler : Bool :=
    if ctrl.init_trans.isSome then
      true
    else if ctrl.ctrler_init_trans.isSome then
      false
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrl})"
        default

  let state_vars : List TypedIdentifier :=
    if ctrl.init_trans.isSome then
      ctrl.state_vars.get!
    else if ctrl.ctrler_init_trans.isSome then
      ctrl.ctrler_state_vars.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrl})"
      default
  let transitions : List Description :=
    if ctrl.init_trans.isSome then
      ctrl.transition_list.get!
    else if ctrl.ctrler_init_trans.isSome then
      ctrl.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrl})"
      default

  -- dbg_trace s!"gen murphi_record for ctrler: ({ctrl})"
  if !entry_or_ctrler then
    -- ===== Gen the state vars for the ctrler
    let murphi_state_vars : List Murϕ.Decl :=
      state_vars.map (
        λ dsl_typed_ident =>
        let (typed, ident) :=
        match dsl_typed_ident with
        | TypedIdentifier.mk typed ident => (typed, ident)

          -- use ident for the Decl name
          -- map type (typed) to a type for the decl
          -- inductive Decl
          --   | const : ID → Expr → Decl
          --   | type  : ID → TypeExpr → Decl
          --   | var   : List ID → TypeExpr → Decl
          let murphi_type_expr : Murϕ.TypeExpr := dsl_type_to_murphi_type typed

          Murϕ.Decl.var [ident] murphi_type_expr
      )
    let murphi_decls_list : List Murϕ.Decl :=
    murphi_state_vars ++ [
      Decl.var ["state"] (
        TypeExpr.previouslyDefined
        (ctrl.name.append "_state")
      )
    ]-- ++ valid
    let murphi_ctrler_record : Murϕ.Decl :=
      Decl.var [ctrl.name] (TypeExpr.record murphi_decls_list)

    -- ===== Gen the ctrler's states
    let list_of_transition_names : List String :=
      transitions.map λ trans => match trans with
      | Pipeline.Description.state ident _ => ident
      | _ => dbg_trace "shouldn't reach here???!"
        panic! "TODO: Throw.."
    let ctrler_entry_state :=
      Murϕ.Decl.type (ctrl.name.append "_state") (
      Murϕ.TypeExpr.enum list_of_transition_names)

    let ctrler_entry_const_decls : ctrler_decl_entry_decl_const_decl := {
      ctrler_decl := murphi_ctrler_record,
      entry_decl := Option.none,
      const_decl_lst := [],
      range_enum_decl := [],
      entry_state_decl := ctrler_entry_state

      }
    ctrler_entry_const_decls
  else
    -- let state_vars : List TypedIdentifier :=
    --   ctrl.state_vars.get!
    let murphi_state_vars : List Murϕ.Decl :=
      state_vars.map (
        λ dsl_typed_ident =>
        let (typed, ident) :=
        match dsl_typed_ident with
        | TypedIdentifier.mk typed ident => (typed, ident)

          -- use ident for the Decl name
          -- map type (typed) to a type for the decl
          -- inductive Decl
          --   | const : ID → Expr → Decl
          --   | type  : ID → TypeExpr → Decl
          --   | var   : List ID → TypeExpr → Decl
          let murphi_type_expr : Murϕ.TypeExpr := dsl_type_to_murphi_type typed

          Murϕ.Decl.var [ident] murphi_type_expr
      )

    let ctrler_type :=
      match ctrl.type with
      | .ok c_type => c_type
      | .error msg =>
        dbg_trace s!"ERROR getting c_type: {msg}"
        panic! msg
    dbg_trace s!"Getting Ctrler Type in Murphi Translation: Ctrler: ({ctrl.name}), Type: ({ctrler_type})"
    let valid := match ctrler_type with
      | .Unordered =>
        dbg_trace s!"Found Unordered ctrler: ({ctrl.name})"
        [murϕ_var_decls| var valid : boolean]
      | .FIFO
      | .BasicCtrler => []
    let murphi_decls_lst :=
      murphi_state_vars.concat (
        Decl.var ["state"] (
          TypeExpr.previouslyDefined
          (ctrl.name.append "_state")
        )
      ) ++ valid

    -- This is a record of what an entry looks like
    let murphi_entry_record := TypeExpr.record murphi_decls_lst
    -- make the controller record with a given num of entries
    let murphi_entry_record_decl_name := (String.join [ctrl.name, "_entry_values"])
    -- NOTE: key item to return for code gen
    let murphi_entry_record_decl :=
      Decl.type
        murphi_entry_record_decl_name
        murphi_entry_record
    -- Get the num of entries in a controller
    let num_entries :=
      match ctrl.controller_descript with
      | Description.controller ident stmt =>
        match stmt with
        | Statement.block lst_stmt =>
          let num_entries_stmt :=
          lst_stmt.filter (
            λ stmt => match stmt with
            | Statement.value_declaration typed_iden expr =>
              match typed_iden with
              | TypedIdentifier.mk tiden iden =>
                if iden == "num_entries"
                  then true
                  else false
            | _ => false
          )
          let num_entries_value_decl :=
            match num_entries_stmt with
            | [one] => one
            -- Shouldn't have more than one, or an empty list?
            | _ => dbg_trace "FAIL! No entries number for controller"
              dbg_trace s!"Controller: {ctrl}"
            default
          let num_entries' :=
            match num_entries_value_decl with
            | Statement.value_declaration typed_iden expr =>
              match expr with
              | Expr.some_term term =>
                match term with
                | Term.const cnst =>
                  match cnst with
                  | Const.num_lit num => num
                  | _ => dbg_trace "FAIL!"
                    default
                | _ => default
              | _ => default
            | _ => default
          num_entries'
        -- another bad case
        | _ => default
      -- another bad case
      | _ => default
    let num_entries_range_enum := Nat.sub num_entries 1

    -- ========== Ctrler Num Entries =============
    let ctrler_num_entries_const_name := (String.join [ctrl.name, "_NUM_ENTRIES_CONST"])
    let ctrler_num_entries_const :=
      Decl.const
      ctrler_num_entries_const_name
      (Expr.integerConst num_entries)

    let ctrler_num_entries_name_designator :=
      Designator.mk ctrler_num_entries_const_name []

    let ctrler_entries_count :=
      TypeExpr.integerSubrange
      (Expr.integerConst 0)
      (Expr.designator ctrler_num_entries_name_designator)
    let ctrler_entries_count_decl_name :=
      -- (String.join [ctrl.name, "_COUNT_ENUM"])
      (String.join [ctrl.name, "_count_t"])
    -- NOTE: Another decl to return
    let ctrler_entries_count_decl :=
      Decl.type (
        ctrler_entries_count_decl_name
      )
      ctrler_entries_count

    -- ========== Ctrler Entries Enum =============
    let ctrler_num_entries_range_enum_const_name := (String.join [ctrl.name, "_NUM_ENTRIES_ENUM_CONST"])
    let ctrler_num_entries_range_enum_const :=
      Decl.const
      ctrler_num_entries_range_enum_const_name
      (Expr.integerConst num_entries_range_enum)

    let ctrler_entries_enum_name_designator :=
      Designator.mk ctrler_num_entries_range_enum_const_name []

    -- Build Decl for the range of values the entry can take
    -- We should also build a constant to reference this
    -- controller's upper bound on num of entries
    let ctrler_entries_range :=
      TypeExpr.integerSubrange
      (Expr.integerConst 0)
      (Expr.designator ctrler_entries_enum_name_designator)
    let ctrler_entries_range_decl_name :=
      -- (String.join [ctrl.name, "_ENTRIES_ENUM"])
      (String.join [ctrl.name, "_idx_t"])
    -- NOTE: Another decl to return
    let ctrler_entries_range_decl :=
      Decl.type (
        ctrler_entries_range_decl_name
      )
      ctrler_entries_range

    -- ========== Entries States =============
    let list_of_transition_names : List String :=
      transitions.map λ trans => match trans with
      | Pipeline.Description.state ident _ => ident
      | _ => dbg_trace "shouldn't reach here???!"
        panic! "TODO: Throw.."
    let ctrler_entry_state :=
      Murϕ.Decl.type (ctrl.name.append "_state") (
      Murϕ.TypeExpr.enum list_of_transition_names)

    -- Now we can build a Decl for the controller record
    let murphi_ctrler_record_name := "entries"
    let decl_lst : List Murϕ.Decl := [
      Decl.var [
        -- Name of this array of entries
        murphi_ctrler_record_name
        ]
      (
      -- and the array entries and number of entries
      TypeExpr.array
      (
        TypeExpr.previouslyDefined
        ctrler_entries_range_decl_name
      )
      (
      TypeExpr.previouslyDefined
      murphi_entry_record_decl_name
      )
      ),
      -- Decl.var ["state"] (
      --   TypeExpr.previouslyDefined
      --   (ctrl.name.append "_state")
      --   ),
      -- Head, tail, and num_entries counter
      Decl.var ["num_entries"] (
      TypeExpr.previouslyDefined
      ctrler_entries_count_decl_name
      )
    ]

    let ctrler_ordering := get_ctrler_elem_ordering ctrl
    let is_fifo : Bool := ctrler_ordering == FIFO.name
    let total_decl_lst : List Murϕ.Decl :=
      if is_fifo then
        decl_lst ++ [
        Decl.var ["head"] (
        TypeExpr.previouslyDefined
        ctrler_entries_range_decl_name
        ),
        Decl.var ["tail"] (
        TypeExpr.previouslyDefined
        ctrler_entries_range_decl_name
        )]
      else
        decl_lst

    let murphi_ctrler_record :=
      Decl.var [ctrl.name] (
        TypeExpr.record total_decl_lst
          -- The array of entries, which is also a record
          -- NOTE: don't do msg buffers!
      )

    -- We must also send back the supporting Decl's
    -- for translation/code generation
    let ctrler_entry_const_decls : ctrler_decl_entry_decl_const_decl := {
      ctrler_decl := murphi_ctrler_record,
      entry_decl := Option.some murphi_entry_record_decl,
      const_decl_lst := [
        ctrler_num_entries_range_enum_const,
        ctrler_num_entries_const
        ],
      range_enum_decl := [ctrler_entries_range_decl, ctrler_entries_count_decl],
      entry_state_decl := ctrler_entry_state
      }
    ctrler_entry_const_decls

def find_speculative_ld_ctrler
: String
:=
  "LQ"

def find_speculative_st_ctrler
: String
:=
  "SQ"

partial def get_only_transitions_recursively
(stmt : Pipeline.Statement)
:=

  match stmt with
  | Statement.transition ident => [ident]
  | Statement.reset ident => []
  | Statement.complete ident => []
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [get_only_transitions_recursively stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            get_only_transitions_recursively stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_only_transitions_recursively)
    | Conditional.if_statement expr1 stmt1 => get_only_transitions_recursively stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_only_transitions_recursively)
  | Statement.await _ lst_stmt1 => List.join (lst_stmt1.map get_only_transitions_recursively)
  | Statement.when qname list_idens stmt => get_only_transitions_recursively stmt
  -- | Statement.listen_handle  =>
  | _ => default

partial def get_complete_transition
(stmt : Pipeline.Statement)
:=
          -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
          -- dbg_trace stmt
          -- dbg_trace "==END GET-TRANSITIONS ==\n"

  match stmt with
  | Statement.transition _ => []
  | Statement.reset _ => []
  | Statement.complete ident => [ident]
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [get_complete_transition stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            get_complete_transition stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_complete_transition)
    | Conditional.if_statement expr1 stmt1 => get_complete_transition stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_complete_transition)
  | Statement.await _ lst_stmt1 => List.join (lst_stmt1.map get_complete_transition)
  | Statement.when qname list_idens stmt => get_complete_transition stmt
  -- | Statement.listen_handle  =>
  | _ => default

def get_init_state_stmts -- use this to get the init state, & translate to
(init_state : String)
(state_list : List Pipeline.Description)
: Except String Pipeline.Statement
:= do
  let init_state_list_stmts : List Pipeline.Statement :=
  List.join (
  state_list.map (λ state : Description =>
    match state with
    | .state name stmt =>
      if name == init_state then
        [stmt]
      else
        []
    | _ => []
  ));
  match init_state_list_stmts with
  | [] =>
    let msg : String := "Trying to get init state stmts, found nothing?" ++
    s!"init_state_list stmts found: ({init_state_list_stmts}), state_list: ({state_list})"
    throw msg
  | [one_stmt] => return one_stmt
  | _ :: _ =>
    let msg : String := "Trying to get init state stmts, found multiple states??" ++
    s!"init_state_list stmts found: ({init_state_list_stmts}), state_list: ({state_list})"
    throw msg

def return_stmts_without_transitions
(stmts : List Pipeline.Statement)
: List Pipeline.Statement
:=
  List.join (
  stmts.map (λ stmt : Pipeline.Statement =>
    match stmt with
    | .transition _ => []
    | .reset _ => []
    | .complete _ => []
    | _ => [stmt]
  ))

def return_blk_without_transitions
(stmt : Pipeline.Statement)
: Except String Pipeline.Statement
:= do
  match stmt with
  | .block lst_stmts =>
    return Pipeline.Statement.block (return_stmts_without_transitions lst_stmts)
  | _ =>
    let msg : String := "This function takes a Pipeline.Statement.block, didn't get one as input!"
    throw msg

def get_init_stmts_without_transition
(ctrler : controller_info)
: Except String Pipeline.Statement
:= do
  let init_trans_extract : String := ctrler.init_trans.get!
  let transition_list_extract : List Description := ctrler.transition_list.get!
  let init_stmt_except : Except String Pipeline.Statement :=
    get_init_state_stmts init_trans_extract transition_list_extract
  let init_stmt : Pipeline.Statement ← match init_stmt_except with
  | .ok stmt => pure stmt
  | .error msg =>
    -- dbg_trace s!"Error when getting init state stmt: ({msg})"
    throw msg
    -- default

  let init_stmt_without_transition_except : Except String Pipeline.Statement :=
    return_blk_without_transitions init_stmt
  let init_stmt_without_transition : Pipeline.Statement ←
  match init_stmt_without_transition_except with
  | .ok stmt => pure stmt
  | .error msg =>
    -- dbg_trace s!"Error when removing transition stmts in init blk: ({msg})"
    throw msg
    -- default

  return init_stmt_without_transition
  -- TODO: Split the bottom code into a function in translation

def get_listen_handle_blks_from_stmts
(stmts : List Pipeline.Statement)
: Except String ( List HandleBlock )
:= do
  let just_top_level_listen_handle : List Pipeline.Statement :=
  stmts.filter (λ stmt =>
    match stmt with
    | .listen_handle /- stmts -/_ _ => true
    | _ => false
  )

  let just_one_listen_handle : Pipeline.Statement ←
    match just_top_level_listen_handle with
    | [] => return []
    | [one_listen_handle] => pure one_listen_handle
    | _ :: _ =>
      let msg : String :=
        "Not supporting multiple Listen Handle blocks in a state.. But found multiple."
      throw msg

  let list_handle_blks : (List HandleBlock) :=
    match just_one_listen_handle with
    | .listen_handle _ handle_blks => handle_blks
    | _ => []

  return list_handle_blks

def get_when_stmt_src_ctrler
(when_stmt : Pipeline.Statement)
: Except String (String)
:= do
  match when_stmt with
  | .when qname _ _ =>
    match qname with
    | .mk lst_str => return lst_str[0]!
  | _ =>
    let msg : String := "Error: function expected a when stmt\n"++
      s!"Instead got this stmt: ({when_stmt})"
    throw msg

partial def recursive_await_when_stmt_search
(lst_stmts : List Pipeline.Statement)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: (List Pipeline.Statement)
:=
  dbg_trace s!"Recursive await-when search stmts: ({lst_stmts})"
  let lst_of_lst_stmts :=
  lst_stmts.map (
    λ stmt =>
      match stmt with
      | Statement.await _ lst_stmts =>
        let ret_val :=
        recursive_await_when_stmt_search lst_stmts func_name curr_ctrler_name
        -- needed to do this explicitly so
        -- Lean4 will type check :D
        ret_val
      | Statement.when qual_name _ _ =>
        -- stmt is the block of code, of course
        -- lst_ident would be the arguments of the when stmt
        -- qual_name should be the structure name and func
        -- If we've found one with a matching
        -- (a) func name and
        -- (b) dest struct name
        -- then we can return this in a list form
        match qual_name with
        | QualifiedName.mk lst_ident =>
          let contains_func := lst_ident.contains func_name
          let contains_ctrler := lst_ident.contains curr_ctrler_name
          let contains_func_from_ctrler :=
          and contains_func contains_ctrler
          dbg_trace s!"When, list_ident: ({lst_ident})"++
          s!"\ncontains_func: ({contains_func}), func_name: ({func_name})"++
          s!"\ncontains_ctrler: ({contains_ctrler}), curr_ctrler_name: ({curr_ctrler_name})"

          if contains_func_from_ctrler
          then
            [stmt]
          else
            []
      -- AZ NOTE:
      -- just a thought...
      -- is there any case where there are multiple
      -- await-when statments for one function call from
      -- another structure?
      -- Maybe if there's an if statement,
      -- but in this case, we can say the developer should
      -- put the if-statement inside the await-when, not outside

      -- AZ NOTE: the rest of these cases are just
      -- cases of nesting, to recursively search
      -- for the await-when
      | Statement.block lst =>
        let ret_val :=
        recursive_await_when_stmt_search lst func_name curr_ctrler_name
        ret_val
      | Statement.conditional_stmt cond =>
        match cond with
        | Conditional.if_else_statement expr stmt1 stmt2 =>
          let ret_val :=
          recursive_await_when_stmt_search [stmt1, stmt2] func_name curr_ctrler_name
          ret_val
        | Conditional.if_statement expr stmt =>
          let ret_val :=
          recursive_await_when_stmt_search [stmt] func_name curr_ctrler_name
          ret_val
      | Statement.listen_handle stmt lst =>
        let ret_val :=
        recursive_await_when_stmt_search [stmt] func_name curr_ctrler_name
        ret_val
      | _ => []
  )
  let lst_of_stmts := List.join lst_of_lst_stmts
  lst_of_stmts

partial def find_when_stmt_from_transition
(trans_list : List Pipeline.Description)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: Pipeline.Statement
:=
  let when_with_matching_func_and_src_ctrler_name :=
  trans_list.map (
    λ trans =>
      -- get the transition stmts, find stmts
      -- which
      match trans with
      | Description.state ident stmt =>
        match stmt with
        | Statement.block lst_stmts =>
          dbg_trace s!"first When stmt from find when stmt: ({stmt})"
          let when_blk :=
          recursive_await_when_stmt_search lst_stmts func_name curr_ctrler_name
          when_blk
        | _ => dbg_trace "stmt under transition should be blk!"
          []
      | _ => dbg_trace "wasn't passed a transition?"
        []
  )
  let when_stmts_lst := List.join when_with_matching_func_and_src_ctrler_name
  let when_stmt :=
  match when_stmts_lst with
  | [one_stmt] => one_stmt
  | h::t =>
  dbg_trace "found multiple matching when stmts?"
  default
  | [] =>
  dbg_trace "found no matching when stmts?"
  default

  when_stmt

partial def find_when_stmt_from_stmts
(stmts : List Pipeline.Statement)
(msg_name : Identifier)
(src_ctrler_name : Identifier)
: Bool
:=
  let when_with_matching_func_and_src_ctrler_name :=
    let when_blk :=
    recursive_await_when_stmt_search stmts msg_name src_ctrler_name
    when_blk
  let when_stmt :=
    match when_with_matching_func_and_src_ctrler_name with
    | [/-one_stmt-/ _] => true -- one_stmt
    | _::_ =>
      -- NOTE: Constrain this to make it easier to parse
      false
    | [] =>
      false
  when_stmt

def get_ctrler_state_vars
(ctrler : controller_info)
: Except String (List TypedIdentifier)
:= do
  if ctrler.ctrler_state_vars.isSome then
    pure ctrler.ctrler_state_vars.get!
  else if ctrler.state_vars.isSome then
    pure ctrler.state_vars.get!
  else
    let msg : String := s!"Error: Ctrler doesn't have state vars? ({ctrler})"
    throw msg

def get_ctrler_states
(ctrler : controller_info)
: Except String (List Pipeline.Description)
:= do
  if ctrler.ctrler_trans_list.isSome then
    pure ctrler.ctrler_trans_list.get!
  else if ctrler.transition_list.isSome then
    pure ctrler.transition_list.get!
  else
    let msg : String := s!"Error: Ctrler doesn't have states? ({ctrler})"
    throw msg

def get_state_name_stmts
(state : Pipeline.Description)
: Except String (String × (List Pipeline.Statement))
:= do
  match state with
  | .state identifier stmt =>
    match stmt with
    | .block lst_stmt => pure (identifier, lst_stmt)
    | _ =>
      let msg : String := s!"Error: Description .state doesn't have a stmt block ({state})"
      throw msg
  | _ =>
    let msg : String := s!"Error: Was not passed a Description .state object ({state})"
    throw msg

def get_ctrler_init_state_name
(ctrler : controller_info)
: Except String (String)
:= do
  if ctrler.init_trans.isSome then
    pure ctrler.init_trans.get!
  else if ctrler.ctrler_init_trans.isSome then
    pure ctrler.ctrler_init_trans.get!
  else
    let msg : String := s!"Error: Ctrler doesn't have init state? ({ctrler})"
    throw msg

def get_max_from_nat_list (lst : List Nat) : Nat :=
  match lst with
  | [] => 0
  | h::t =>
    let other : Nat := (get_max_from_nat_list t)
    if h > other then h else other
#eval get_max_from_nat_list [10]

def get_ctrler_from_ctrlers_list (ctrler_name : CtrlerName) (ctrlers : List controller_info)
: Except String controller_info := do
  let ctrler_match_list := ctrlers.filter (·.name = ctrler_name)
  match ctrler_match_list with
  | [ctrler] => pure ctrler
  | [] =>
    let msg : String := s!"Error: No ctrler with name ({ctrler_name}) found in list ({ctrlers})"
    throw msg
  | _::_ =>
    let msg : String := s!"Error: Multiple ctrlers with name ({ctrler_name}) found in list ({ctrlers})"
    throw msg

def Pipeline.QualifiedName.is_ident_ordering : Pipeline.QualifiedName → Except String Bool
| qual_name =>
  match qual_name.idents with
  | [ident] => pure $ ident == "ordering"
  | [] => throw s!"QualifiedName is empty? ({qual_name})"
  | _ => pure false

mutual
partial def Pipeline.Term.is_head_api : Pipeline.Term → Bool
| term =>
  match term with
  | .function_call qual_name _ => qual_name.idents == ["is_head"]
  | .expr expr' =>
    expr'.is_contains_is_head_api
  | _ => false

partial def Pipeline.Expr.is_contains_is_head_api : Pipeline.Expr → Bool
| expr =>
  match expr with
  | .some_term term => term.is_head_api
  | .binand term1 term2 => term1.is_head_api || term2.is_head_api
  | _ => false
end

mutual
partial def Pipeline.Term.is_instruction_not_eq_ld : Pipeline.Term → Bool
| term =>
  match term with
  | .expr expr' =>
    expr'.is_contains_instruction_not_eq_ld
  | _ => false

partial def Pipeline.Expr.is_contains_instruction_not_eq_ld : Pipeline.Expr → Bool
| expr =>
  match expr with
  | .equal term1 term2 =>
    match term1, term2 with
    | .qualified_var qual_var, .var ident =>
      (qual_var == [instruction, op].to_qual_name) &&
      (
        (ident == load.toMurphiString) ||
        (ident == ldar.toMurphiString)
      )
    | .var ident, .qualified_var qual_var =>
      (qual_var == [instruction, op].to_qual_name) &&
      (
        (ident == load.toMurphiString) ||
        (ident == ldar.toMurphiString)
      )
    | _, _ => false
  | .binand term1 term2 => term1.is_instruction_not_eq_ld || term2.is_instruction_not_eq_ld
  -- | .binor term1 term2 => term1.is_instruction_not_eq_ld || term2.is_instruction_not_eq_ld
  | _ => false
end

mutual
partial def Pipeline.Term.is_instruction_not_eq_type : Pipeline.Term → InstType → Bool
| term, inst_type =>
  match term with
  | .expr expr' =>
    expr'.is_contains_instruction_not_eq_type inst_type
  | .logical_negation term' =>
    match term' with
    | .expr expr' =>
      match expr' with
      | .equal term1 term2 =>
        match term1, term2 with
        | .qualified_var qual_var, .var ident
        | .var ident, .qualified_var qual_var =>
          (qual_var == [instruction, op].to_qual_name) && (ident == inst_type.toMurphiString)
        | _, _ => false
      | _ => false
    | _ => false
  | _ => false

partial def Pipeline.Expr.is_contains_instruction_not_eq_type : Pipeline.Expr → InstType → Bool
| expr, inst_type =>
  match expr with
  | .equal term1 term2 =>
    match term1, term2 with
    | .qualified_var qual_var, .var ident
    | .var ident, .qualified_var qual_var =>
      (qual_var == [instruction, op].to_qual_name) && (ident != inst_type.toMurphiString)
    | _, _ => false
  | .binand term1 term2 => term1.is_instruction_not_eq_type inst_type || term2.is_instruction_not_eq_type inst_type
  -- | .binor term1 term2 => term1.is_instruction_not_eq_type || term2.is_instruction_not_eq_type
  | .not_equal term1 term2 =>
    match term1, term2 with
    | .qualified_var qual_var, .var ident
    | .var ident, .qualified_var qual_var =>
      (qual_var == [instruction, op].to_qual_name) && (ident == inst_type.toMurphiString)
    | _, _ => false
  -- NOTE: Could also check negation
  | .some_term term => term.is_instruction_not_eq_type inst_type
  | _ => false
end

mutual
partial def Pipeline.Term.is_instruction_eq_type : Pipeline.Term → InstType → Bool
| term, inst_type =>
  match term with
  | .expr expr' =>
    expr'.is_contains_instruction_eq_type inst_type
  | .logical_negation term' =>
    match term' with
    | .expr expr' =>
      match expr' with
      | .not_equal term1 term2 =>
        match term1, term2 with
        | .qualified_var qual_var, .var ident
        | .var ident, .qualified_var qual_var =>
          (qual_var == [instruction, op].to_qual_name) && (ident == inst_type.toMurphiString)
        | _, _ => false
      | _ => false
    | _ => false
  | _ => false

partial def Pipeline.Expr.is_contains_instruction_eq_type : Pipeline.Expr → InstType → Bool
| expr, inst_type =>
  match expr with
  | .equal term1 term2 =>
    match term1, term2 with
    | .qualified_var qual_var, .var ident
    | .var ident, .qualified_var qual_var =>
      (qual_var == [instruction, op].to_qual_name) && (ident == inst_type.toMurphiString)
    | _, _ => false
  | .binand term1 term2 => term1.is_instruction_eq_type inst_type || term2.is_instruction_eq_type inst_type
  -- | .binor term1 term2 => term1.is_instruction_not_eq_type || term2.is_instruction_not_eq_type
  | .some_term term => term.is_instruction_eq_type inst_type
  | _ => false
end

-- partial def Pipeline.Term.is_search_older_seq_num_success : Pipeline.Term → CtrlerName → Bool
-- | term, ctrler_name =>
--   match term with
--   | .function_call qual_name args =>
--     let first_arg := match args with
--     | [] => throw "await-api is empty?"
--     | h::_ => h
--     let first_arg_is_older_seq_num_inst :=
--       match h with
--       |
--     (qual_name.idents == [ctrler_name,"search"]) &&
--     (entry.instruction.seq_num < instruction.seq_num)
--   | _ => false

-- def Pipeline.Statement.is_self_search_older_seq_num_success
-- (stmt : Pipeline.Statement) (ctrler_name : CtrlerName)
-- : Bool :=
--   match stmt with
--   | .await (some term) stmts =>
--     let term_is_search_api :=
--       term.is_search_older_seq_num_success ctrler_name
--   | _ => false

def controller_info.init_trans_name (ctrler : controller_info) : Except String Identifier :=
  if ctrler.init_trans.isSome then
    pure ctrler.init_trans.get!
  else if ctrler.ctrler_init_trans.isSome then
    pure ctrler.ctrler_init_trans.get!
  else
    throw s!"Error: ctrler doesn't have init_trans ({ctrler})"

def controller_info.state_list (ctrler : controller_info) : Except String (List Pipeline.Description) :=
  if ctrler.transition_list.isSome then
    pure ctrler.transition_list.get!
  else if ctrler.ctrler_trans_list.isSome then
    pure ctrler.ctrler_trans_list.get!
  else
    throw s!"Error: ctrler doesn't have state_list ({ctrler})"

def controller_info.init_trans_descript (ctrler : controller_info) : Except String Pipeline.Description
:= do
  let state_list ← ctrler.state_list
  let init_trans_name ← ctrler.init_trans_name
  let init_trans ← state_list.filterM (λ state => do pure $ (← state.state_name) == init_trans_name)
  match init_trans with
  | [state] => pure state
  | _ => throw s!"Error: Couldn't find matching init state? ({state_list})"

def Pipeline.Statement.is_transition : Pipeline.Statement → Bool
| .transition _ => true
| _ => false

def Pipeline.Description.stmts_without_transition : Pipeline.Description → Except String (List Pipeline.Statement)
| .state /- ident -/ _ stmt => do
  let stmts ← stmt.stmt_block
  let stmts_without_transition := stmts.filter (!·.is_transition)
  pure stmts_without_transition
| _ => throw "Error: Description is not a state"

-- def controller_info.init_trans_descript_without_trans (ctrler : controller_info) : Except String Pipeline.Description
-- := do
--   let

def Pipeline.Statement.init_trans_dest (stmt : Pipeline.Statement) : List Identifier
:= match stmt with
| .transition ident => [ident]
| _ => []

def Pipeline.Statement.block_init_trans_dest (stmt : Pipeline.Statement) : Except String (Identifier)
:= do
  let stmts ← stmt.stmt_block
  let init_trans_dests := List.join $ stmts.map (·.init_trans_dest)
  match init_trans_dests with
  | [dest_name] => pure dest_name
  | _ => throw s!"Error: either 0 or more than 1 transition ({init_trans_dests}) in the stmt ({stmt})"

def Pipeline.Description.top_level_dest_name : Pipeline.Description → Except String Identifier
| .state /- ident -/ _ stmt => do
  stmt.block_init_trans_dest
| _ => throw "Error: Description is not a state"


def controller_info.init_trans_dest (ctrler : controller_info)
: Except String StateName
:= do
  let state_list ← ctrler.state_list
  let init_trans_name ← ctrler.init_trans_name
  let init_trans ← state_list.filterM (λ state => do pure $ (← state.state_name) == init_trans_name)
  let state ←
    match init_trans with
    | [state] => pure state
    | _ => throw s!"Error: Couldn't find matching init state? ({state_list})"
  state.top_level_dest_name

def Pipeline.Statement.is_variable_assignment : Pipeline.Statement → Bool
| .variable_assignment _ _ => true
| _ => false

def Pipeline.Description.state_asgn_stmts : Pipeline.Description → Except String (List Pipeline.Statement)
| .state /- ident -/ _ stmt => do
  let block ← stmt.stmt_block
  let asgn_stmts := block.filter (·.is_variable_assignment)
  pure asgn_stmts
| _ => throw "Error: Description is not a state"

def Pipeline.Const.is_lit : Pipeline.Const → Bool
| .str_lit _ => true
| .num_lit _ => true

def Pipeline.Term.is_lit : Pipeline.Term → Bool
| .const const_ => const_.is_lit
| _ => false

def Pipeline.Expr.is_lit : Pipeline.Expr → Bool
| .some_term term => term.is_lit
| _ => false

def Pipeline.Const.is_bool_lit : Pipeline.Const → Bool
| .str_lit str => if str == "true" || str == "false" then true else false
| _ => false

def Pipeline.Term.is_bool_lit : Pipeline.Term → Bool
| .const const_ => const_.is_bool_lit
| _ => false

def Pipeline.Expr.is_bool_lit : Pipeline.Expr → Bool
| .some_term term => term.is_bool_lit
| _ => false

def Pipeline.Const.bool_str : Pipeline.Const → Except String String
| .str_lit str => if str == "true" || str == "false" then pure str else throw "Error: Const is not a string literal"
-- | .num_lit _ => -- something
| _ => throw "Error: Const is not a string literal"

def Pipeline.Term.bool_str : Pipeline.Term → Except String String
| .const const_ => const_.bool_str
| _ => throw "Term is not bool literal"

def Pipeline.Statement.is_variable_assignment_to_lit : Pipeline.Statement → Bool
| .variable_assignment _ expr => expr.is_bool_lit
| _ => false

def Pipeline.Description.state_asgn_to_lit_stmts : Pipeline.Description → Except String (List Pipeline.Statement)
| state_descript => do
  pure $ (← state_descript.state_asgn_stmts).filter (·.is_variable_assignment_to_lit)

def get_ctrler_state_handle_blocks
-- (ctrlers : List controller_info)
(ctrler : controller_info)
(state_name : String)
: Except String (Option (List HandleBlock))
:= do
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let list_state_matching_name : List Description :=
  transitions.filter (λ state : Description =>
    match state with
    | .state name _ =>
      if name == state_name then true
      else false
    | _ => false
    )
  let state_matching_name : Description ←
    match list_state_matching_name with
    | [matching_state] => pure matching_state
    | _ =>
      let msg : String :=
        "While trying to match a state with a name:" ++
        "Found either multiple or no states\n" ++
        s!"List contents: ({list_state_matching_name})"
      throw msg

  -- TODO: Get the stmt block, get the stmts from the block
  -- Call the get_listen_handle_blks_from_stmts function
  -- Check if there was anything, if there is use Option.some...
  let state_stmt_blk : Pipeline.Statement ←
  match state_matching_name with
  | .state _ stmt => pure stmt
  | _ =>
    let msg : String := "Shouldn't have something besides .state type ..."
    throw msg

  let state_stmts : List Pipeline.Statement ←
  match state_stmt_blk with
  | .block stmts => pure stmts
  | _ =>
    let msg : String :=
      "Error opening stmt from state: state should have had a block statement?\n" ++
      s!"State stmt: ({state_stmt_blk})"
    throw msg

  let handle_blks : List HandleBlock ←
    get_listen_handle_blks_from_stmts state_stmts

  if handle_blks.isEmpty then
    return Option.none
  else
    return Option.some handle_blks

structure CtrlerStateExpr where
ctrler : CtrlerName
state : StateName
constraints : List Pipeline.Expr -- equality checks!
def CtrlerStateExpr.toString : CtrlerStateExpr → String
| ctrler_state_expr =>
  s!"== Ctrler & State & Constraint Expr ==\nCtrler: ({ctrler_state_expr.ctrler})\nState: ({ctrler_state_expr.state})\nState: ({ctrler_state_expr.constraints})\n== End Ctrler & State & Constraint Exprs =="
instance : ToString CtrlerStateExpr where toString := CtrlerStateExpr.toString

structure CtrlerState where
ctrler : CtrlerName
state : StateName
def CtrlerState.toString : CtrlerState → String
| ctrler_state =>
  s!"== Ctrler & State ==\nCtrler: ({ctrler_state.ctrler})\nState: ({ctrler_state.state})\n== End Ctrler & State =="
instance : ToString CtrlerState where toString := CtrlerState.toString

-- TODO: Write func to create the stall node.
-- would have similar logic to the cut & paste code from the inordertransformation file

def ExprsToAndTreeExpr (exprs : List Pipeline.Expr) : Except String Pipeline.Expr := do
  match exprs with
  | [] => throw s!"Empty List of Exprs was provided!"
  | [expr] => -- just do this one comparison
    pure expr
  | h :: t =>
    -- use recursion
    pure $ Pipeline.Expr.binand (Pipeline.Term.expr h) (Pipeline.Term.expr (← ExprsToAndTreeExpr t))

def convert_state_names_to_dsl_or_tree_state_check
( state_names : List StateName ) (ctrler_type : CtrlerType) (ctrler_name : CtrlerName)
: Except String Pipeline.Expr
-- : Pipeline.Expr
:= do
  let term_var : Pipeline.Term :=
    match ctrler_type with
    | .BasicCtrler => Pipeline.Term.qualified_var [ctrler_name, "curr_state"].to_qual_name
    | .FIFO => ( Pipeline.Term.qualified_var [entry, "curr_state"].to_qual_name )
    | .Unordered => ( Pipeline.Term.qualified_var [entry, "curr_state"].to_qual_name )

  match state_names with
  | [a_state_name] => -- just do this one comparison
    return Pipeline.Expr.equal term_var ( Pipeline.Term.var a_state_name )
  | h :: t =>
    -- use recursion
    let head_equal_check : Pipeline.Expr := Pipeline.Expr.equal term_var ( Pipeline.Term.var h )
    let expr : Pipeline.Expr ← (convert_state_names_to_dsl_or_tree_state_check t ctrler_type ctrler_name)
    return Pipeline.Expr.binor (Pipeline.Term.expr head_equal_check) (Pipeline.Term.expr expr)
  | [] => throw s!"Blank List of Strings was provided!"

structure CtrlerStates where
ctrler : CtrlerName
states : List StateName
deriving Inhabited, BEq
def CtrlerStates.toString : CtrlerStates → String
| ctrler_states =>
  s!"== Ctrler & States ==\nCtrler: ({ctrler_states.ctrler})\nStates: ({ctrler_states.states})\n== End Ctrler & States =="
instance : ToString CtrlerStates where toString := CtrlerStates.toString

abbrev StateOrConstraintToStallOn := CtrlerStates ⊕ CtrlerStateExpr

def StateOrConstraintToStallOn.is_just_reset : StateOrConstraintToStallOn → Bool
| Sum.inl ctrler_states =>
  match ctrler_states.states with
  | [] => true
  | _ => false
| Sum.inr /- ctrler_states_constraint_expr -/ _ => false

def StateOrConstraintToStallOn.stall_condition_expr_from_post_receive_state : StateOrConstraintToStallOn → CtrlerType → CtrlerName → Except String Pipeline.Expr
| states_or_constraints, ctrler_type, ctrler_name => do
  let no_longer_stall_cond ←
    match states_or_constraints with
    | Sum.inl ctrler_states => do
      convert_state_names_to_dsl_or_tree_state_check ctrler_states.states ctrler_type ctrler_name
    | Sum.inr ctrler_states_constraint_expr => do
      pure $ ← ExprsToAndTreeExpr ctrler_states_constraint_expr.constraints
  pure $ Pipeline.Expr.some_term (
    Pipeline.Term.logical_negation (
      Pipeline.Term.expr no_longer_stall_cond
    )
  )

def StateOrConstraintToStallOn.ctrler : StateOrConstraintToStallOn → CtrlerName
| Sum.inl ctrler_states => ctrler_states.ctrler
| Sum.inr ctrler_states_constraint_expr => ctrler_states_constraint_expr.ctrler

def gen_stall_dsl_state
  (new_stall_state_name : StateName)
  (original_state_name : StateName)
  (ctrler_name : CtrlerName)
  (state_check_cond? : Option Pipeline.Expr)
  (stall_on_inst_type : InstType)
  (inst_to_stall_type : InstType)
  (original_state's_handleblks : Option ( List HandleBlock ))
  (just_reset : Bool)
  : Except String Description
  := do

    -- (entry.instruction.seq_num < instruction.seq_num)
    let entry_is_earlier_than_this_one : Term :=
      Pipeline.Term.expr (
      Pipeline.Expr.less_than
      (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"]))
      (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
      )
    -- (entry.instruction.op == stall_on_inst_type)
    let entry_is_of_desired_type : Term :=
      Pipeline.Term.expr (
      Pipeline.Expr.equal
      (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "op"]))
      (Pipeline.Term.const (Const.str_lit (stall_on_inst_type.toMurphiString)))
      )
    -- let entry_is_valid : Term :=
    --   Pipeline.Term.expr (
    --   Pipeline.Expr.not_equal
    --   (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"]))
    --   (Pipeline.Term.const (Const.num_lit (0))) -- using 0 as an "invalid inst"
    --   )

    let search_condition : Pipeline.Expr := -- ERROR! TODO: Adjust to ignore invalid entries!
      -- (Pipeline.Expr.binand
        -- (Pipeline.Term.expr
        (Pipeline.Expr.binand
        entry_is_earlier_than_this_one
        entry_is_of_desired_type)
        -- )
        -- entry_is_valid
      -- )

    --  min(instruction.seq_num - entry.instruction.seq_num)
    let search_min : Pipeline.Expr :=
      Pipeline.Expr.some_term (
        Pipeline.Term.function_call
        (QualifiedName.mk ["min"])
        [(Pipeline.Expr.sub
          (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
          (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"])))]
      )

    let ctrler_search_call : Term :=
      Pipeline.Term.function_call
      (QualifiedName.mk [ctrler_name, "search"])
      [search_condition, search_min]

    -- If we just reset, then reset. Else use the Expr check.
    let when_search_success_stmts : List Pipeline.Statement ←
      match just_reset with
      | true => do pure [
        Pipeline.Statement.reset new_stall_state_name
      ]
      | false => do
        if let some state_check_expr := state_check_cond? then
          pure [
            Pipeline.Statement.conditional_stmt (
            Pipeline.Conditional.if_else_statement
            state_check_expr
            (Pipeline.Statement.reset new_stall_state_name)
            (Pipeline.Statement.transition original_state_name)
            )
          ]
        else
          throw s!"Error: Trying to generate stall state, but was not given a state check condition!"
    -- let stall_state_name := ctrler_name ++ "stall" ++ original_state_name
    let when_success_stmt_blk : Pipeline.Statement :=
      Pipeline.Statement.block when_search_success_stmts

    let when_success : Pipeline.Statement :=
      Pipeline.Statement.when
      (QualifiedName.mk [ctrler_name, "search_success"])
      (["curr_state"])
      when_success_stmt_blk

    let when_fail_stmt_blk : Pipeline.Statement :=
      Pipeline.Statement.block [
        (Pipeline.Statement.transition original_state_name)
      ]
    let when_fail : Pipeline.Statement :=
      Pipeline.Statement.when
      (QualifiedName.mk [ctrler_name, "search_fail"])
      ([])
      when_fail_stmt_blk


    -- TODO: Fill in the when stmts, and the condition in it
    let await_stmt : Pipeline.Statement :=
      Statement.await ctrler_search_call [when_success, when_fail]

    let if_inst_is_of_to_stall_type : Pipeline.Statement :=
      Pipeline.Statement.conditional_stmt $
        Pipeline.Conditional.if_else_statement
          (Pipeline.Expr.equal (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "op"]))
            (Pipeline.Term.const (Const.str_lit (inst_to_stall_type.toMurphiString))))
          await_stmt
          $ Pipeline.Statement.transition original_state_name

    let stall_state_stmt : Pipeline.Statement :=
      if original_state's_handleblks.isSome then
        Statement.listen_handle (Statement.block [if_inst_is_of_to_stall_type]) original_state's_handleblks.get!
      else
        if_inst_is_of_to_stall_type

    pure $ Description.state new_stall_state_name (
      Statement.block [stall_state_stmt])

def CreateStallNode (stall_state : CtrlerState) (stall_on_constraint : StateOrConstraintToStallOn)
(ctrlers : Ctrlers) (inst_type_to_stall_on : InstType) (inst_to_stall_type : InstType)
: Except String Pipeline.Description := do
  /- 3. Gen the new stall state's name -/
  let new_stall_state_name := String.join [stall_state.ctrler, "_stall_", stall_state.state]

  /- 5. Generate the new stall state -/
  let stall_ctrler ← ctrlers.ctrler_matching_name stall_state.ctrler
  /- Consider if original state has a listen-handle -/
  let handle_blks : Option (List HandleBlock) ←
    get_ctrler_state_handle_blocks stall_ctrler stall_state.state

  let just_reset := stall_on_constraint.is_just_reset

  -- i.e. stall when constraints are false, continue when the post receive constraints are true
  let not_yet_gotten_mem_resp_state_check? : Option Pipeline.Expr ←
    if just_reset then
      pure none
    else
      pure $ some $ ← stall_on_constraint.stall_condition_expr_from_post_receive_state (← stall_ctrler.type) stall_ctrler.name
  dbg_trace s!"just_reset: ({just_reset})"
  dbg_trace s!"stall_on_constraint: {stall_on_constraint}"
  dbg_trace s!"not_yet_gotten_mem_resp_state_check?: {not_yet_gotten_mem_resp_state_check?}"
  let new_stall_state : Description ←
    gen_stall_dsl_state new_stall_state_name stall_state.state
    stall_on_constraint.ctrler not_yet_gotten_mem_resp_state_check? inst_type_to_stall_on inst_to_stall_type
    handle_blks just_reset

  dbg_trace s!"New stall state: \n{new_stall_state}"
  pure new_stall_state

partial def recursively_find_stmt_with_transition_to_arg
( state_stmt : String × Pipeline.Statement )
: List Bool
:=
  let state : String := state_stmt.1
  let stmt : Pipeline.Statement := state_stmt.2

  -- try to match statment
  let bool_list :=
  match stmt with
  | Statement.transition ident =>
    if ident == state then [true]
    else []
  -- TODO NOTE: Should count both "reset" & "transition"
  -- but ignore "completion...?"
  | Statement.reset _ => []
  | Statement.complete _ => []
  | Statement.stall _ => []
  | Statement.return_stmt _ => []
  | Statement.return_empty => []
  | Statement.block list_statment =>
    let stmts_with_state_name : List (String × Pipeline.Statement) :=
      list_statment.map (λ stmt => (state, stmt))

    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )

    bool_list
  | Statement.stray_expr _ => []
  | Statement.when _ _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)
  | Statement.await (some _) list_stmt =>
    let stmts_with_state_name : List (String × Pipeline.Statement) :=
      list_stmt.map (λ stmt => (state, stmt))

    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )

    bool_list
  | Statement.await none list_stmt =>
    let stmts_with_state_name : List (String × Pipeline.Statement) :=
      list_stmt.map (λ stmt => (state, stmt))

    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )

    bool_list
  | Statement.listen_handle stmt list_handles =>
    List.join
    (
      [recursively_find_stmt_with_transition_to_arg (state, stmt)]
      ++
      ( list_handles.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk _ _ stmt1 =>
            recursively_find_stmt_with_transition_to_arg (state, stmt1)
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement _ stmt1 stmt2 => List.join (
      [(state, stmt1),(state, stmt2)].map recursively_find_stmt_with_transition_to_arg
      )
    | Conditional.if_statement _ stmt1 => recursively_find_stmt_with_transition_to_arg (state, stmt1)
  | Statement.variable_assignment _ _ => []
  | Statement.value_declaration _ _ => []
  | Statement.variable_declaration _ => []
  | Statement.labelled_statement _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)

  bool_list

def get_states_directly_leading_to_given_state
( state_and_state_list : (String × List Description) )
-- : Except String (List String)
: List String
:=
  let state : String := state_and_state_list.1
  let state_list : List Description := state_and_state_list.2
  -- 1. check for states which transition to the given state
  -- 2. recursively check if those states have states which transition to them
  -- 3. combine these with the previously found states
  -- 4. return all found states

  -- 1.
  let states_that_transition_to_state : List Description :=
    state_list.filter ( λ state_descript =>
      -- filter for stmts which have a transition that matches this input state var
      match state_descript with
      | Description.state _ stmt =>
        -- check stmt, if there's a transition to this state
        let bool_list : List Bool := recursively_find_stmt_with_transition_to_arg (state, stmt)
        -- return bool
        match bool_list with
        | [] => false
        | [true] => true
        | _::_ => bool_list.all (λ bool' => bool' == true)
      -- | _ => throw state_descript
      | _ => false
    )

  let state_names : List String :=
    List.join (
    states_that_transition_to_state.map ( λ state_descript =>
      match state_descript with
      | Description.state ident _ => [(ident)]
      | _ => []
      ))

  state_names

mutual

partial def update_transitions_if_else
(old_name : StateName) (new_name : StateName) (expr : Pipeline.Expr)
(stmt1 stmt2 : Pipeline.Statement) (inst_type? : Option InstType) : Pipeline.Statement :=
  Statement.conditional_stmt (
  Conditional.if_else_statement expr (
    recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) inst_type?
  ) (
    recursively_find_stmt_and_update_transitions (old_name, new_name, stmt2) inst_type?
  ))

partial def update_transitions_if
(old_name : StateName) (new_name : StateName) (expr : Pipeline.Expr)
(stmt1 : Pipeline.Statement) (inst_type? : Option InstType) : Pipeline.Statement :=
  Statement.conditional_stmt (
  Conditional.if_statement expr (
    recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) inst_type?
  ))

partial def recursively_find_stmt_and_update_transitions
( old_name_new_name_and_stmt : String × (String × Pipeline.Statement) )
(inst_type? : Option InstType)
: Pipeline.Statement
:=
  let old_name : String := old_name_new_name_and_stmt.1
  let new_name : String := old_name_new_name_and_stmt.2.1
  let stmt : Pipeline.Statement := old_name_new_name_and_stmt.2.2

  -- try to match statment
  let bool_list :=
  match stmt with
  | Statement.transition ident =>
    dbg_trace s!"--== BEGIN OLD TRANS NAME: {stmt}"
    dbg_trace s!"--== THE OLD TRANS NAME: {old_name}"
    dbg_trace s!"--== END REPLACEMENT TRANS NAME: {new_name}"
    if ident == old_name then Statement.transition new_name
    else stmt
  -- TODO NOTE: Should count both "reset" & "transition"
  -- but ignore "completion...?"
  | Statement.reset ident =>
    dbg_trace s!"--== BEGIN OLD TRANS NAME: {stmt}"
    dbg_trace s!"--== THE OLD TRANS NAME: {old_name}"
    dbg_trace s!"--== END REPLACEMENT TRANS NAME: {new_name}"
    if ident == old_name then Statement.transition new_name
    else stmt

  | Statement.complete _ => stmt
  | Statement.stall _ => stmt
  | Statement.return_stmt _ => stmt
  | Statement.return_empty => stmt
  | Statement.block list_statment =>
    let stmts_with_name_info : List (String × String × Pipeline.Statement) :=
      list_statment.map (λ stmt' => (old_name, new_name, stmt'))

    let blk_stmt : Pipeline.Statement :=
    Statement.block ( stmts_with_name_info.map ( recursively_find_stmt_and_update_transitions · inst_type? ) )

    blk_stmt
  | Statement.stray_expr _ => stmt
  | Statement.when q_name list_ident stmt' =>
    let updated_stmt : Pipeline.Statement := recursively_find_stmt_and_update_transitions (old_name, new_name, stmt') inst_type?
    let new_when : Pipeline.Statement := Statement.when q_name list_ident updated_stmt
    new_when
  | Statement.await term' list_stmt =>
    let stmts_with_name_info : List (String × String × Pipeline.Statement) :=
      list_stmt.map (λ stmt' => (old_name, new_name, stmt'))

    let await_stmt : Pipeline.Statement :=
    Statement.await term' ( stmts_with_name_info.map ( recursively_find_stmt_and_update_transitions · inst_type? ) )

    await_stmt
  -- | Statement.await none list_stmt =>
  --   let stmts_with_name_info : List (String × String × Statement) :=
  --     list_stmt.map (λ stmt' => (old_name, new_name, stmt'))

  --   let await_stmt : Statement :=
  --   Statement.await none ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )

  --   await_stmt
  | Statement.listen_handle stmt' list_handles =>
    Statement.listen_handle (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt') inst_type?)
      ( list_handles.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qual_name list_ident stmt1 =>
            HandleBlock.mk qual_name list_ident ( recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) inst_type? )
        )
      )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr stmt1 stmt2 =>
      match inst_type? with
      | none =>
        update_transitions_if_else old_name new_name expr stmt1 stmt2 inst_type?
      | some inst_type =>
        -- AZ NOTE: Should check if is pred on the inst type as well, then stmt2 can be un-updated
        let expr_is_not_eq_type : Bool := expr.is_contains_instruction_not_eq_type inst_type
        match expr_is_not_eq_type with
        | false => -- control path is either of this inst type or for all insts
          let expr_is_eq_type : Bool := expr.is_contains_instruction_eq_type inst_type
          match expr_is_eq_type with
          | false => -- control path is for all insts
            update_transitions_if_else old_name new_name expr stmt1 stmt2 inst_type?
          | true => -- control path is of this inst type, thus in the else case, don't change anything
            Statement.conditional_stmt (Conditional.if_else_statement
              expr
                ( recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) inst_type? )
                stmt2
                )
        | true => -- control path is of either another inst type or !this_type
          Statement.conditional_stmt (Conditional.if_else_statement
            expr
              stmt1
              ( recursively_find_stmt_and_update_transitions (old_name, new_name, stmt2) inst_type? ))

    | Conditional.if_statement expr stmt1 =>
      match inst_type? with
      | none =>
        update_transitions_if old_name new_name expr stmt1 inst_type?
      | some inst_type =>
        match expr.is_contains_instruction_not_eq_type inst_type with
        | false =>
          update_transitions_if old_name new_name expr stmt1 inst_type?
        | true =>
          Statement.conditional_stmt (Conditional.if_statement expr stmt1)
  | Statement.variable_assignment _ _ => stmt
  | Statement.value_declaration _ _ => stmt
  | Statement.variable_declaration _ => stmt
  | Statement.labelled_statement label stmt => Statement.labelled_statement label (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt) inst_type?)

  bool_list

end

def update_state_transitions_matching_name_to_replacement_name
( orig_name : String)
( replacement_name : String)
( list_states_names_to_update : List String)
( all_state_descriptions : List Description)
( inst_type? : Option InstType)
: (List Description)
:= --do

  -- take the state_descriptions, and see if it's name matches one we need to update
  -- if it matches, map all of it's statements
  --   map func should check if stmt is a transition & if it is then replace the identifier
  --   map func should recursively descend into anything which holds list(s) of statements
  --   map func should return the original otherwise
  -----------------------------------------
  -- high level pseudo code : Do the match on the state descriptions
  -- match all_state_descriptions with
  -- if state name == orig name
  -- then do recursive update rewrite
  -- else return original state description.

  let updated_states : (List Description) :=
    List.join (
    all_state_descriptions.map ( λ state_descript =>
      match state_descript with
      | Description.state this_state_name stmt =>
        if list_states_names_to_update.contains this_state_name then
          [Description.state this_state_name (recursively_find_stmt_and_update_transitions (orig_name, replacement_name, stmt) inst_type?)]
        else
          [state_descript]
      | _ => []
    ))
  updated_states

-- TODO: write func to add a node to a controller, as if
-- if get's inserted between a given old state and the states that pointed to the given old state

def controller_info.update_my_state_list (ctrler : controller_info) (new_state_list : List Description) : controller_info :=
  let new_ctrler : controller_info := {
    name := ctrler.name,
    controller_descript := ctrler.controller_descript,
    entry_descript := ctrler.entry_descript,
    init_trans := ctrler.init_trans,
    state_vars := ctrler.state_vars,
    transition_list := -- new_state_machine_with_stall_state
      if ctrler.init_trans.isSome then
        Option.some new_state_list
      else
        Option.none
    ctrler_init_trans := ctrler.ctrler_init_trans, -- NOTE to self: Consider strongly typing things...
    ctrler_state_vars := ctrler.ctrler_state_vars,
    ctrler_trans_list := -- new_state_machine_with_stall_state
      if ctrler.ctrler_init_trans.isSome then
        Option.some new_state_list
      else
        Option.none
  }
  new_ctrler

def AddNodeAndUpdateCtrler
(ctrler : controller_info) (new_state_name : StateName) (new_state : Description) (old_state_name : StateName)
(inst_type? : Option InstType)
: Except String controller_info := do
  /-
  6. Update states that point to the previous perform
  -/

  let ctrler's_states : List Description ← ctrler.state_list

  let states_transitioning_to_state_to_stall : List String :=
    get_states_directly_leading_to_given_state (old_state_name, ctrler's_states)
  dbg_trace s!"======= STATES directly leading to state {old_state_name}"
  dbg_trace states_transitioning_to_state_to_stall
  dbg_trace s!"======= END States that directly lead to state {old_state_name}"

  let updated_state_list_transition_to_stall_state : List Description :=
    update_state_transitions_matching_name_to_replacement_name
    old_state_name new_state_name states_transitioning_to_state_to_stall ctrler's_states inst_type?

  let new_state_machine_with_stall_state : List Description :=
    updated_state_list_transition_to_stall_state ++ [new_state]

  let new_ctrler : controller_info := ctrler.update_my_state_list new_state_machine_with_stall_state

  pure new_ctrler

def UpdateCtrlerWithNode
(ctrlers : List controller_info) (ctrler_name : CtrlerName) (new_state_name : StateName) (new_state : Description) (old_state_name : StateName) (inst_type? : Option InstType)
: Except String (List controller_info) := do
  ctrlers.mapM (λ ctrler => do
    if ctrler.name == ctrler_name then (AddNodeAndUpdateCtrler ctrler new_state_name new_state old_state_name inst_type?)
    else pure ctrler)

def Pipeline.Term.func_idents_args : Pipeline.Term → Except String (List Identifier × List Pipeline.Expr)
| term => do
  match term with
  | .function_call qual_name arg_exprs => do
    pure (qual_name.idents , arg_exprs)
  | _ => throw s!"Expected function call, but got {term}"

-- def Pipeline.HandleBlock.ctrler_msg_names : Pipeline.HandleBlock → Except String (CtrlerName × MsgName)
-- | handle_blk => do
--   match handle_blk with
--   | .mk qual_name /- List Identifier -/ _ /- Statement -/ _ => do
--     let idents := qual_name.idents
--     let ctrler? := idents[0]?
--     let msg? := idents[1]?
--     match (ctrler?, msg?) with
--     | (some ctrler, some msg) => pure (ctrler, msg)
--     | _ => throw s!"Error: Expected controller and message name, but got {handle_blk}"

-- def Pipeline.HandleBlock.ret_if_match_msg_ctrler : Pipeline.HandleBlock → MsgName → CtrlerName → Except String (Bool)
-- | handle_blk, msg_name, ctrler_name => do
--   let (ctrler, msg) ← handle_blk.ctrler_msg_names
--   if ctrler == ctrler_name && msg == msg_name then
--     pure true
--   else
--     pure false

-- -- NOTE: Could also match args..
-- abbrev Args := List Identifier

-- def Pipeline.Description.handle_blk_matching_msg_ctrler : Pipeline.Description → MsgName → CtrlerName /- → Args-/ → Except String (StateName × Pipeline.HandleBlock)
-- | state_ , msg_name, ctrler_name => do
--   match state_ with
--   | .state ident stmt => do
--     let stmts ← stmt.stmt_block;
--     let handle_blks ← get_listen_handle_blks_from_stmts stmts
--     let handle_blks_listening_to_msg ← handle_blks.filterM (·.ret_if_match_msg_ctrler msg_name ctrler_name)
--     let handle_blk ← match handle_blks_listening_to_msg[0]? with
--       | some blk => pure blk
--       | none => do throw s!"Error: No handle block found for message {msg_name} in blocks {handle_blks}"
--     pure (ident, handle_blk)
--   | _ => throw s!"Error: Expected state, but got {state_}"

-- abbrev MsgFnName := String

-- def find_handle_blks_matching_msg : List Pipeline.Description → MsgFnName → CtrlerName → Except String (List (StateName × Pipeline.HandleBlock))
-- | states_to_search, api_func_name, ctrler_name => do
--   let handle_blks := states_to_search.mapM (·.handle_blk_matching_msg_ctrler api_func_name ctrler_name);
--   handle_blks

def ZipWithList (list : List (α : Type)) (thing : (β : Type)) : List (α × β) :=
  list.zip (List.replicate list.length thing)

-- def Pipeline.Description.when_stmt_awaiting_msg_ctrler_name : Pipeline.Description → MsgName → CtrlerName → StateName


-- def controller_info.when_states_awaiting_msg_ctrler_name : controller_info → MsgName → CtrlerName → Except String StateName
-- | ctrler, msg_name, ctrler_name => do
--   -- get ctrler states
--   let states ← ctrler.state_list
--   -- get states with await stmt with when stmt
--   -- check if when stmt awaits a msg of msg_name from ctrler_name
--   let states_awaiting_msg ← states.filter (·.when_stmt_awaiting_msg_ctrler_name msg_name ctrler_name)
--   -- if so, return state name
--   -- Should also check we only found 1 match.
--   default

-- abbrev IdentList := List Identifier

def Pipeline.Statement.stmt_of_labelled_stmt (stmt : Pipeline.Statement) : Pipeline.Statement :=
  match stmt with
  | .labelled_statement _ stmt => stmt
  | _ => stmt

def Pipeline.Statement.is_rf_write (stmt : Pipeline.Statement) : Bool :=
  match stmt with
  | .stray_expr expr =>
    match expr with
    | .some_term term =>
      match term with
      | .function_call qual_name _ =>
        match qual_name.idents with
        | [ctrler, msg] => ctrler == reg_file && msg == write
        | _ => false
      | _ => false
    | _ => false
  | _ => false

def CreateDSLMsgCall (ctrler_name : CtrlerName) (msg_name : MsgName) (args : List Pipeline.Expr)
  : Pipeline.Statement :=
  let qual_name := [ctrler_name, msg_name].to_qual_name
  Pipeline.Statement.stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name args))

-- NOTE: Should create a Helper file for DSL AST Helper functions
def CreateDSLRefFileReadExpr (ctrler_name : CtrlerName) (msg_name : MsgName) (args : List Pipeline.Expr)
  : Pipeline.Expr :=
  let qual_name := [ctrler_name, msg_name].to_qual_name
  let expr := Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name args)
  expr

def CreateDSLVarAssignmentStmt (var_name : String) (expr : Pipeline.Expr) : Pipeline.Statement :=
  let stmt := Pipeline.Statement.variable_assignment [var_name].to_qual_name expr
  stmt

def CreateDSLBoolEqualExpr (lhs : Identifier) (rhs : Identifier) : Pipeline.Expr :=
  let lhs := Pipeline.Term.var lhs
  let rhs := Pipeline.Term.var rhs
  let expr := Pipeline.Expr.equal lhs rhs
  expr

def CreateDSLBoolNotEqualExpr (lhs : Identifier) (rhs : Identifier) : Pipeline.Expr :=
  let lhs := Pipeline.Term.var lhs
  let rhs := Pipeline.Term.var rhs
  let expr := Pipeline.Expr.not_equal lhs rhs
  expr

def CreateDSLTypedIdentifier (ident : Identifier) (var_name : Identifier) : Pipeline.TypedIdentifier :=
  Pipeline.TypedIdentifier.mk ident var_name

def CreateDSLDeclAssignExpr (ident : Identifier) (var_name : Identifier) (expr : Pipeline.Expr) : Pipeline.Statement :=
  Pipeline.Statement.value_declaration (CreateDSLTypedIdentifier ident var_name) expr

def CreateDSLIfStmt (expr : Pipeline.Expr) (stmt : Pipeline.Statement) : Pipeline.Statement :=
  Pipeline.Statement.conditional_stmt <| Pipeline.Conditional.if_statement expr stmt

def CreateDSLFuncCallStmt (func_name : String) (args : List Pipeline.Expr) : Pipeline.Statement :=
  let qual_name := [func_name].to_qual_name
  Pipeline.Statement.stray_expr $ Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name args)

def String.to_dsl_var_expr : String → Pipeline.Expr
| var_name =>
  Pipeline.Expr.some_term (Pipeline.Term.var var_name)

def Ctrlers.complete_to_ctrler's_first_state_stmt : Ctrlers → CtrlerName → Except String Pipeline.Statement
| ctrlers, ctrler_name => do
  let ctrler : Ctrler ← ctrlers.ctrler_from_name ctrler_name
  let ctrler_first_state : StateName ← ctrler.init_trans_dest
  pure $ Pipeline.Statement.complete ctrler_first_state

def Pipeline.Description.append_when_case_to_state's_await_stmt : Pipeline.Description → Pipeline.Statement → Except String (Pipeline.Description)
| descript, stmt_to_append => do
  match descript with
  | .state state_name stmt_blk => do
    let stmts ← stmt_blk.stmt_block
    if H : stmts.length = 1 then
      have hyp : 0 < stmts.length := by simp[H]
      let stmt := stmts[0]'hyp

      match stmt with
      | .await none await_stmts =>
        pure
        $ Pipeline.Description.state state_name
        $ (Pipeline.Statement.await none $ await_stmts ++ [stmt_to_append]).to_block
      | .listen_handle listen_stmt handle_blks =>
        let stmts' ← listen_stmt.stmt_block
        if H' : stmts'.length = 1 then
          have hyp' : 0 < stmts'.length := by simp[H']
          let stmt' := stmts'[0]

          match stmt' with
          | .await none await_stmts' =>
            pure
            $ Pipeline.Description.state state_name
            $ (Pipeline.Statement.listen_handle (Pipeline.Statement.await none $ await_stmts' ++ [stmt_to_append]) handle_blks).to_block
          | _ => throw "Error: Expected to find 'await' stmt within a listen_handle ({stmt'})"
        else
          let msg : String := s!"Error: Listen stmt's stmt block has more than one stmt ({stmts'}), expected only 1, await"
          throw msg

      | _ => throw "Error: Expected to find 'await' or 'listen_handle' stmt"
    else
      let msg : String := s!"Error: State's stmt block has more than one stmt ({stmts}), expected only 1, await or listen_handle"
      throw msg
  | _ => throw "Error: Pipeline Description is not a 'state'"

def controller_info.state_from_name : Ctrler → StateName → Except String Pipeline.Description
| ctrler, match_state_name => do
  let states : List Pipeline.Description := ← ctrler.state_list;
  let state ← states.findM? (do let state's_name ← ·.state_name; pure $ state's_name == match_state_name)
  match state with
  | some state' => pure state'
  | none => throw "Error: Could not find state in controller"

partial def List.split_off_stmts_at_commit_and_inject_stmts
(stmts : List Pipeline.Statement) (stmts_to_inject : List Pipeline.Statement)
-- returns: (the stmts we kinda re-build with, the commit stmts)
: Except String (List Pipeline.Statement × List Pipeline.Statement) := do
  -- try tail recursion
  match stmts with
  | h :: t =>
    let (tail_re_build_stmts, tail_commit_stmts) ← t.split_off_stmts_at_commit_and_inject_stmts stmts_to_inject

    match h with
    -- The case of interest...
    | .labelled_statement label stmt =>
      match label with
      | .commit =>
        -- return the two stmt & t
        -- pure (stmts_to_inject, stmts) -- i.e. continue keeping the "commit" label by using 'stmts'
        pure ( [labelled_stmt label stmts_to_inject.to_block] , [stmt] ++ t)
      | _ =>
        pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .block stmts' =>
      let (re_build_stmts, commit_stmts) ← stmts'.split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
      pure ([re_build_stmts.to_block] ++ tail_re_build_stmts, commit_stmts ++ tail_commit_stmts) -- NOTE: don't touch commit_stmts
    -- Recursive cases, stmts that may contain stmts, then collect stmts, before recursing on t
    | .when qual_name idents stmt =>
      let (re_build_stmts, commit_stmts) ← [stmt].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
      pure ([Pipeline.Statement.when qual_name idents re_build_stmts.to_block] ++ tail_re_build_stmts, commit_stmts ++ tail_commit_stmts)
    | .await term? stmts' =>
      let (re_build_stmts, commit_stmts) ← stmts'.split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
      pure ([Pipeline.Statement.await term? re_build_stmts] ++ tail_re_build_stmts, commit_stmts ++ tail_commit_stmts)
    | .listen_handle stmt handle_blks =>
      let (re_build_stmts, commit_stmts) ← [stmt].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
      pure ([Pipeline.Statement.listen_handle re_build_stmts.to_block handle_blks] ++ tail_re_build_stmts, commit_stmts ++ tail_commit_stmts)
    | .conditional_stmt cond =>
      match cond with
      | .if_statement expr stmt =>
        let (re_build_stmts, commit_stmts) ← [stmt].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
        pure (
          [Pipeline.Statement.conditional_stmt $ Pipeline.Conditional.if_statement expr re_build_stmts.to_block] ++ tail_re_build_stmts,
          commit_stmts ++ tail_commit_stmts
          )
      | .if_else_statement expr stmt stmt' =>
        let (re_build_stmts, commit_stmts) ← [stmt].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
        let (re_build_stmts', commit_stmts') ← [stmt'].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
        let ret_commit_stmts :=
          match commit_stmts, commit_stmts' with
          | _::_, _::_ =>
            if commit_stmts == commit_stmts' then
              commit_stmts
            else
              commit_stmts ++ commit_stmts'
          | _::_, [] => commit_stmts
          | [], _::_ => commit_stmts'
          | [], [] => tail_commit_stmts
        pure (
          [
            Pipeline.Statement.conditional_stmt
            $ Pipeline.Conditional.if_else_statement expr re_build_stmts.to_block re_build_stmts'.to_block
          ] ++ tail_re_build_stmts
          ,
          ret_commit_stmts
          )
    -- Non recursive cases, collect stmt, simply recurse on t
    | .stall _ => throw "Error while injecting stmts to replace commit stmts: Stall stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .return_stmt _ => --throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .return_empty => --throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .stray_expr _ =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .complete _ =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .reset (String.mk _) =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .transition (String.mk _) =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .variable_assignment _ _ =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .value_declaration _ _ =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .variable_declaration _ =>
      pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
  | [] => pure ([], [])
-- termination_by _  => stmts.length - 1
-- decreasing_by exact (by simp [List.length])

def Pipeline.Statement.replace_listen_handle_stmts (stmt : Pipeline.Statement) (replacement_stmts : List Pipeline.Statement) : Except String Pipeline.Statement := do
  match stmt with
  | .listen_handle /-stmt-/ _ handle_blks => pure $ Pipeline.Statement.listen_handle replacement_stmts.to_block handle_blks
  | _ => throw "Error while replacing a listen handle stmt's block: input stmt is not a listen statement"

def List.has_listen_handle? (stmts : List Pipeline.Statement) : Option Pipeline.Statement := do
  match stmts with
  | [stmt] => do
    match stmt with
    | .listen_handle /-stmt-/ _ /- handle_blks -/ _ => some stmt
    | _ => none
  | _ => none

def Pipeline.Description.listen_handle_stmt? (state : Pipeline.Description) : Except String (Option Pipeline.Statement) := do
  let state_stmt_blk ← state.stmt
  dbg_trace s!"<<< (listen-handle?) Get stmt block: ({state_stmt_blk})"
  let stmts ← state_stmt_blk.stmt_block
  dbg_trace s!"<<< (listen-handle?) Get stmts: ({stmts})"
  pure stmts.has_listen_handle?

def Pipeline.Description.wrap_stmt_with_node's_listen_handle_if_exists (state : Pipeline.Description) (stmt : Pipeline.Statement) : Except String Pipeline.Statement := do
  let listen_handle? ← state.listen_handle_stmt?
  match listen_handle? with
  | none => do pure stmt
  | some listen_handle => do listen_handle.replace_listen_handle_stmts [stmt]

def Pipeline.Description.split_off_stmts_at_commit_and_inject_stmts
(state : Pipeline.Description) (stmts_to_inject : List Pipeline.Statement)
-- Return
-- 1. the state with commit smts replaced with injected stmts
-- 2. the stmt at commit and stmts after
: Except String (Pipeline.Description × Pipeline.Description) := do
  -- open up state, look through stmts
  match state with
  | .state state_name stmt => do
    let (updated_stmt_with_injected, stmts_after_commit)
      : (List Pipeline.Statement × List Pipeline.Statement) :=
        ← [stmt].split_off_stmts_at_commit_and_inject_stmts stmts_to_inject
    let updated_state := Pipeline.Description.state state_name updated_stmt_with_injected.to_block

    let original_state_name := original_commit_code_prefix.append "_" |>.append state_name

    let stmts_after_commit_wrapped_in_listen_handle ← state.wrap_stmt_with_node's_listen_handle_if_exists stmts_after_commit.to_block
    let stmts_after_commit_state := Pipeline.Description.state original_state_name stmts_after_commit_wrapped_in_listen_handle.to_block
    pure (updated_state, stmts_after_commit_state)
  | _ => throw "Error: Expected input Pipeline.Description to be a state. Instead got ({state})"

-- def instruction := "instruction"
-- def seq_num := "seq_num"

def CreateDSLFIFOSearchAPI (dest_ctrler_name : CtrlerName) (success_case_stmts : List Pipeline.Statement) : Pipeline.Statement :=
  -- await LQ.tail_search(entry.instruction.seq_num == instruction.seq_num)
  -- when search_success() from LQ
  -- when search_fail() from LQ
  let when_search_fail := Pipeline.Statement.when [dest_ctrler_name, search_fail].to_qual_name [] [].to_block
  let when_search_success := Pipeline.Statement.when [dest_ctrler_name, search_success].to_qual_name [] success_case_stmts.to_block

  let entry_seq_num_match : Pipeline.Expr :=
    Pipeline.Expr.equal
    (Pipeline.Term.qualified_var [entry, instruction, seq_num].to_qual_name)
    (Pipeline.Term.qualified_var [instruction, seq_num].to_qual_name)

  let search_api_term : Pipeline.Term := Pipeline.Term.function_call [dest_ctrler_name, tail_search].to_qual_name [entry_seq_num_match]
  let search_api := Pipeline.Statement.await (some search_api_term) [when_search_success, when_search_fail]
  search_api


def CreateDSLUnorderedSearchAPI (dest_ctrler_name : CtrlerName) (success_case_stmts : List Pipeline.Statement) : Pipeline.Statement :=
  -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
  -- when search_fail() from SB
  -- when search_success(write_value) from SB
  let when_search_fail := Pipeline.Statement.when [dest_ctrler_name, search_fail].to_qual_name [] [].to_block
  let when_search_success := Pipeline.Statement.when [dest_ctrler_name, search_success].to_qual_name [] success_case_stmts.to_block

  let entry_inst := (Pipeline.Term.qualified_var [entry, instruction, seq_num].to_qual_name)
  let curr_ctrler_inst := (Pipeline.Term.qualified_var [instruction, seq_num].to_qual_name)
  let entry_seq_num_match : Pipeline.Expr := Pipeline.Expr.equal entry_inst curr_ctrler_inst

  let inst_and_entry_inst_seq_num_diff := Pipeline.Expr.sub curr_ctrler_inst entry_inst
  let min_of_diff := Pipeline.Expr.some_term $ Pipeline.Term.function_call [(min : Identifier)].to_qual_name [inst_and_entry_inst_seq_num_diff]

  -- AZ NOTE: min_of_diff may not be necessary...
  let search_api_term : Pipeline.Term := Pipeline.Term.function_call [dest_ctrler_name, search].to_qual_name [entry_seq_num_match, min_of_diff]
  let search_api := Pipeline.Statement.await (some search_api_term) [when_search_success, when_search_fail]
  search_api

/- Send a msg to this ctrler -/
def Ctrler.queue_search_api_to_send_msg (dest_ctrler : Ctrler) (msg_call : Pipeline.Statement) (additional_stmts : List Pipeline.Statement)
: Except String (Pipeline.Statement) := do
  let dest_ctrler_name := dest_ctrler.name
  let dest_ctrler_type ← dest_ctrler.type
  match dest_ctrler_type with
  | .BasicCtrler => pure $ ([msg_call] ++ additional_stmts).to_block
  | .FIFO =>
    -- await LQ.tail_search(entry.instruction.seq_num == instruction.seq_num)
    -- when search_success() from LQ
    -- when search_fail() from LQ
    pure $ CreateDSLFIFOSearchAPI dest_ctrler_name $ [msg_call] ++ additional_stmts
  | .Unordered =>
    -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
    -- when search_fail() from SB
    -- when search_success(write_value) from SB
    pure $ CreateDSLUnorderedSearchAPI dest_ctrler_name $ [msg_call] ++ additional_stmts

def List.update_dsl_state (state_list : List Pipeline.Description) (state_to_update_with : Pipeline.Description) : Except String (List Pipeline.Description) := do
  let state_to_update_name ← state_to_update_with.state_name
  let updated_states := state_list.mapM (do
    let list_state := ·;
    let list_state_name := ← list_state.state_name;
    if list_state_name == state_to_update_name then
      pure state_to_update_with
    else
      pure list_state
  )

  updated_states

def Ctrler.update_ctrler_state_list (ctrler : Ctrler) (updated_state_list : List Pipeline.Description) : Ctrler :=
  {
    name := ctrler.name
    controller_descript := ctrler.controller_descript
    entry_descript := ctrler.entry_descript
    init_trans := ctrler.init_trans
    state_vars := ctrler.state_vars
    transition_list := ctrler.transition_list
    ctrler_init_trans := ctrler.ctrler_init_trans
    ctrler_trans_list := updated_state_list
    ctrler_state_vars := ctrler.ctrler_state_vars
  }

def Ctrler.update_queue_state_list (ctrler : Ctrler) (updated_state_list : List Pipeline.Description) : Ctrler :=
  {
    name := ctrler.name
    controller_descript := ctrler.controller_descript
    entry_descript := ctrler.entry_descript
    init_trans := ctrler.init_trans
    state_vars := ctrler.state_vars
    transition_list := updated_state_list
    ctrler_init_trans := ctrler.ctrler_init_trans
    ctrler_trans_list := ctrler.ctrler_trans_list
    ctrler_state_vars := ctrler.ctrler_state_vars
  }

def Ctrler.update_state_list (ctrler : Ctrler) (updated_state_list : List Pipeline.Description) : Except String Ctrler := do
  match ← ctrler.type with
  | .BasicCtrler => return ctrler.update_ctrler_state_list updated_state_list
  | .FIFO => return ctrler.update_queue_state_list updated_state_list
  | .Unordered => return ctrler.update_queue_state_list updated_state_list

def Ctrler.update_ctrler_state (ctrler : Ctrler) (updated_state : Pipeline.Description) : Except String Ctrler := do
  -- let state_to_update_name ← updated_state.state_name
  let state_list ← ctrler.state_list
  let updated_state_list ← state_list.update_dsl_state updated_state
  let updated_ctrler := ctrler.update_state_list updated_state_list

  updated_ctrler

def Ctrlers.update_ctrler (ctrlers : Ctrlers) (ctrler : Ctrler) : Except String Ctrlers := do
  let ctrler_to_update_name := ctrler.name
  let updated_ctrlers := ctrlers.map (
    let list_ctrler := ·;
    let list_ctrler_name := list_ctrler.name;
    if list_ctrler_name == ctrler_to_update_name then
      ctrler
    else
      list_ctrler
  )

  return updated_ctrlers

def Ctrlers.update_ctrler_state (ctrlers : Ctrlers) (ctrler_name : CtrlerName) (updated_state : Pipeline.Description) : Except String Ctrlers := do
  let ctrler ← ctrlers.ctrler_from_name ctrler_name
  let updated_ctrler ← ctrler.update_ctrler_state updated_state
  ctrlers.update_ctrler updated_ctrler

def Ctrler.add_states (ctrler : Ctrler) (states_to_add : List Pipeline.Description) : Except String Ctrler := do
  let state_list ← ctrler.state_list
  let updated_state_list := state_list ++ states_to_add
  let updated_ctrler ← ctrler.update_state_list updated_state_list

  return updated_ctrler

def Ctrlers.add_ctrler_states (ctrlers : Ctrlers) (ctrler_name : CtrlerName) (states_to_add : List Pipeline.Description) : Except String Ctrlers := do
  let ctrler ← ctrlers.ctrler_from_name ctrler_name
  let ctrler_with_states_added ← ctrler.add_states states_to_add
  let updated_ctrlers ← ctrlers.update_ctrler ctrler_with_states_added

  return updated_ctrlers

def Pipeline.Statement.labelled_stmt's_stmt (labelled_stmt : Pipeline.Statement)
: Except String Pipeline.Statement :=
  match labelled_stmt with
  | .labelled_statement /-label-/ _ stmt => pure stmt
  | _ => throw "Error while trying to get a labelled_stmt's_stmt: input stmt is not a labelled statement"

-- def update_state_transitions_matching_name_to_replacement_name
-- ( orig_name : String)
-- ( replacement_name : String)
-- ( list_states_names_to_update : List String)
-- ( all_state_descriptions : List Description)
def Ctrler.update_ctrler_basic_and_reset_transitions
(ctrler : Ctrler) (prev_trans_to_state : StateName) (replacement_state_name : StateName) (inst_type? : Option InstType)
: Except String Ctrler := do
  let all_states : List Description ← ctrler.state_list
  let states_transitioning_to_state_to_stall : List StateName :=
    get_states_directly_leading_to_given_state (prev_trans_to_state, all_states)
  let new_states_trans_to_new_state_name := update_state_transitions_matching_name_to_replacement_name prev_trans_to_state replacement_state_name states_transitioning_to_state_to_stall all_states inst_type?

  let updated_ctrler := ctrler.update_my_state_list new_states_trans_to_new_state_name
  pure updated_ctrler

def Ctrlers.update_ctrler_states_trans_to_specific_state (ctrlers : Ctrlers) (ctrler_name : CtrlerName)
(prev_trans_to_state : StateName) (replacement_state_name : StateName) (inst_type? : Option InstType)
: Except String Ctrlers := do
  let ctrler ← ctrlers.ctrler_from_name ctrler_name
  let updated_ctrler ← ctrler.update_ctrler_basic_and_reset_transitions prev_trans_to_state replacement_state_name inst_type?
  ctrlers.update_ctrler updated_ctrler

def controller_info.init_trans_dest_state (ctrler : controller_info)
: Except String Pipeline.Description
:= do
  let init_trans_dest : StateName ← ctrler.init_trans_dest
  ctrler.state_from_name init_trans_dest

def List.isNotEmpty (list : List α) : Bool :=
  match list with
  | [] => false
  | _ => true

-- def ficticious_ctrler_API_msg_names : List MsgName := [load_completed, store_completed]

-- ============== AZ NOTE: New "correct" stall based on query for un-complete state ==============

def CtrlerStates.to_if_state_check (ctrler_states : CtrlerStates) (if_at_state : VarName) (ctrlers : Ctrlers) : Except String (BoolDecl × BoolSetIfStmt) := do
  let not_completed_true := Pipeline.Statement.variable_assignment [if_at_state].to_qual_name (Pipeline.Expr.some_term (Pipeline.Term.const (Pipeline.Const.str_lit "true" )))
  let not_completed_false := Pipeline.Statement.variable_assignment [if_at_state].to_qual_name (Pipeline.Expr.some_term (Pipeline.Term.const (Pipeline.Const.str_lit "false" )))

  let ctrler ← ctrlers.ctrler_from_name ctrler_states.ctrler |>.throw_exception_nesting_msg s!"Error trying to get ctrler: ({ctrler_states.ctrler}) from Ctrlers ({ctrlers.map (·.name)})"
  let ctrler_type ← ctrler.type
  let state_check_expr := ← convert_state_names_to_dsl_or_tree_state_check ctrler_states.states ctrler_type ctrler_states.ctrler
  let if_not_completed := Pipeline.Statement.conditional_stmt (Pipeline.Conditional.if_else_statement state_check_expr not_completed_true not_completed_false)

  let ctrler_not_completed := Pipeline.Statement.value_declaration (Pipeline.TypedIdentifier.mk "bool" if_at_state) (Pipeline.Expr.some_term (Pipeline.Term.const (Pipeline.Const.str_lit "false" )))
  pure (ctrler_not_completed, if_not_completed)

def CtrlerStates.to_if_state_then_do
(ctrler_states : CtrlerStates)
(when_success_stmts : List Pipeline.Statement)
(when_fail_stmts : List Pipeline.Statement)
(ctrlers : Ctrlers)
: Except String (Pipeline.Statement) := do
  -- let not_completed_true := Pipeline.Statement.variable_assignment [if_at_state].to_qual_name (Pipeline.Expr.some_term (Pipeline.Term.const (Pipeline.Const.str_lit "true" )))
  -- let not_completed_false := Pipeline.Statement.variable_assignment [if_at_state].to_qual_name (Pipeline.Expr.some_term (Pipeline.Term.const (Pipeline.Const.str_lit "false" )))

  let ctrler ← ctrlers.ctrler_from_name ctrler_states.ctrler |>.throw_exception_nesting_msg s!"Error trying to get ctrler: ({ctrler_states.ctrler}) from Ctrlers ({ctrlers.map (·.name)})"
  let ctrler_type ← ctrler.type
  let state_check_expr := ← convert_state_names_to_dsl_or_tree_state_check ctrler_states.states ctrler_type ctrler_states.ctrler
  let if_on_state := Pipeline.Statement.conditional_stmt (Pipeline.Conditional.if_else_statement state_check_expr when_success_stmts.to_block when_fail_stmts.to_block)

  pure if_on_state

def InstType.to_const_term : InstType → Pipeline.Term
| inst_type => Pipeline.Term.const $ str_lit inst_type.toMurphiString

-- AZ NOTE: mirrors functions CreateDSLFIFOSearchAPI, and CreateDSLUnorderedSearchAPI
-- maybe able to compose

def Pipeline.Expr.to_term (expr : Pipeline.Expr) : Pipeline.Term :=
  expr_term expr

def CtrlerName.FIFOOlderInstSearch
(dest_ctrler_name : CtrlerName)
(stall_on_inst_type : InstType)
(success_case_stmts : List Pipeline.Statement)
(fail_case_stmts : List Pipeline.Statement)
(search_api : MsgName)
: Pipeline.Statement :=
  -- await LQ.tail_search(entry.instruction.seq_num < instruction.seq_num) {
  --   when search_fail() from LQ {
  --     ld_seq_num = NULL # 0
  --   }
  --   when search_success(instruction) from LQ {
  --     ld_seq_num = instruction.seq_num
  --   }
  -- }
  let when_search_fail := when_stmt [dest_ctrler_name, search_fail].to_qual_name [] fail_case_stmts.to_block
  let when_search_success := when_stmt [dest_ctrler_name, search_success].to_qual_name [] success_case_stmts.to_block

  let entry_inst := (qualified_var [entry, instruction, seq_num].to_qual_name)
  let curr_ctrler_inst := (qualified_var [instruction, seq_num].to_qual_name)
  let entry_seq_num_less_than : Pipeline.Expr := less_than entry_inst curr_ctrler_inst

  let entry_inst_type := (qualified_var [entry, instruction, op].to_qual_name)
  let stall_inst_type := stall_on_inst_type.to_const_term
  let entry_is_stall_type := equal entry_inst_type stall_inst_type

  let entry_is_older_and_stall_type := binand entry_seq_num_less_than.to_term entry_is_stall_type.to_term

  let search_api_term : Pipeline.Term := function_call [dest_ctrler_name, search_api].to_qual_name [entry_is_older_and_stall_type]
  let search_api := await (some search_api_term) [when_search_success, when_search_fail]
  search_api

def CtrlerName.UnorderedOlderInstSearch
(dest_ctrler_name : CtrlerName)
(stall_on_inst_type : InstType)
(success_case_stmts : List Pipeline.Statement)
(fail_case_stmts : List Pipeline.Statement)
(search_api : MsgName)
(current_ctrler_seq_num : List Identifier)
: Pipeline.Statement :=
  -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
  -- when search_fail() from SB
  -- when search_success(write_value) from SB
  let when_search_fail := when_stmt [dest_ctrler_name, search_fail].to_qual_name [] fail_case_stmts.to_block
  let when_search_success := when_stmt [dest_ctrler_name, search_success].to_qual_name [] success_case_stmts.to_block

  let entry_inst := (qualified_var [entry, instruction, seq_num].to_qual_name)
  let curr_ctrler_inst := (qualified_var current_ctrler_seq_num.to_qual_name)
  let entry_older_seq_num : Pipeline.Expr := less_than entry_inst curr_ctrler_inst

  let entry_inst_type := (qualified_var [entry, instruction, op].to_qual_name)
  let stall_inst_type := stall_on_inst_type.to_const_term
  let entry_is_stall_type := equal entry_inst_type stall_inst_type

  let entry_is_older_and_stall_type := binand entry_older_seq_num.to_term entry_is_stall_type.to_term

  let inst_and_entry_inst_seq_num_diff := sub curr_ctrler_inst entry_inst
  let min_of_diff : Pipeline.Expr := some_term $ function_call [(min : Identifier)].to_qual_name [inst_and_entry_inst_seq_num_diff]

  -- AZ NOTE: min_of_diff may not be necessary...
  let search_api_term : Pipeline.Term := function_call [dest_ctrler_name, /-search-/ search_api].to_qual_name [entry_is_older_and_stall_type, min_of_diff]
  let search_api := await (some search_api_term) [when_search_success, when_search_fail]
  search_api

open Pipeline in
def List.to_qual_or_var_term
(idents : List Identifier)
: Except String Pipeline.Term := do
  match idents with
  | [one] => do pure $ var_term one
  | _::_ => do pure $ qual_var_term idents
  | _ => do throw "List.to_qual_or_var_expr: empty list"

open Pipeline in
def CtrlerName.unordered_query_match
(if_stmt : Pipeline.Statement)
(else_stmt : Pipeline.Statement)
(dest_ctrler_name : CtrlerName)

(dest_ctrler_var : List Identifier)
(searched_var : List Identifier)
: Except String SearchStatement := do
  -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
  let when_search_success := when_stmt [dest_ctrler_name, search_success].to_qual_name [] if_stmt.to_block
  let when_search_fail    := when_stmt [dest_ctrler_name, search_fail   ].to_qual_name [] else_stmt.to_block

  let entry_var := ← ([entry].append dest_ctrler_var).to_qual_or_var_term
  let search_var := ← searched_var.to_qual_or_var_term
  let entry_match := equal entry_var search_var

  let diff := sub search_var entry_var
  let min_of_diff : Pipeline.Expr := some_term $ function_call [(min : Identifier)].to_qual_name [diff]

  let search_api_call := function_call [dest_ctrler_name, search].to_qual_name [entry_match, min_of_diff]
  let search_stmt := await (some search_api_call) [when_search_success, when_search_fail]
  pure search_stmt

inductive SearchType where
| hit_one : SearchType
| hit_all : SearchType

def SearchType.search_api
(search_type : SearchType)
(ctrler_type : CtrlerType)
: Option MsgName :=
  match search_type with
  | SearchType.hit_one =>
    match ctrler_type with
    | .FIFO => some tail_search
    | .Unordered => some search
    | .BasicCtrler => none
  | SearchType.hit_all =>
    match ctrler_type with
    | .FIFO
    | .Unordered => some search_all
    | .BasicCtrler => none

open Pipeline in
def CtrlerStates.query_older_insts
(ctrler_states : CtrlerStates)
(stall_on_inst_type : InstType)
-- (original_state_name : StateName)
(if_stmt : Pipeline.Statement)
(else_stmt : Pipeline.Statement)
(ctrlers : Ctrlers)
(search_type : SearchType)
(current_ctrler_seq_num : List Identifier)
: Except String SearchStatement := do
  let ctrler ← ctrlers.ctrler_from_name ctrler_states.ctrler |>.throw_exception_nesting_msg s!"Error (gen query for ctrler/states) trying to get ctrler: ({ctrler_states.ctrler}) from Ctrlers ({ctrlers.map (·.name)})"
  let ctrler_type ← ctrler.type
  match ctrler_type with
  | .BasicCtrler =>
    let entry_inst_type := (qualified_var [ctrler_states.ctrler, instruction, op].to_qual_name)
    let stall_inst_type := stall_on_inst_type.to_const_term
    let entry_is_stall_type := equal entry_inst_type stall_inst_type

    let entry_seq_num_valid :=
      less_than
        (qualified_var [ctrler_states.ctrler, instruction, seq_num].to_qual_name)
        (qualified_var [instruction, seq_num].to_qual_name)

    let entry_is_stall_type_and_valid := binand entry_is_stall_type.to_term entry_seq_num_valid.to_term

    pure $ conditional_stmt $
      if_else_statement
        entry_is_stall_type_and_valid
        if_stmt
        else_stmt
  | .FIFO => -- use the search API here... probably exists in the old generate stall state function...
  -- TODO: replace default. add helper to create search API with stmts in it
    if let some search_api := search_type.search_api ctrler_type then
      pure $ ctrler_states.ctrler.FIFOOlderInstSearch stall_on_inst_type [if_stmt] [] search_api
    else
      throw s!"Error (gen query for ctrler/states) trying to get search API for ctrler type: ({ctrler_type})"
  | .Unordered =>
    if let some search_api := search_type.search_api ctrler_type then
      pure $ ctrler_states.ctrler.UnorderedOlderInstSearch stall_on_inst_type [if_stmt] [] search_api current_ctrler_seq_num
    else
      throw s!"Error (gen query for ctrler/states) trying to get search API for ctrler type: ({ctrler_type})"

open Pipeline in
def InstructionEntryIsOlder
(ctrler_name : Identifier)
(ct : CtrlerType)
: Pipeline.Expr :=
  let dest_ctrler_ref : Identifier :=
    match ct with
    | .BasicCtrler => ctrler_name
    | .FIFO | .Unordered => entry
  less_than
    (qualified_var [dest_ctrler_ref, instruction, seq_num].to_qual_name)
    (qualified_var [instruction, seq_num].to_qual_name)

open Pipeline in
def CtrlerEntryFieldMatches
(ctrler_name : Identifier)
(ct : CtrlerType)
(this_field : List Identifier) -- can be single id too.
(ctrler_field : List Identifier) -- ensure single id is translated properly
: Except String Pipeline.Expr := do
  let dest_ctrler_ref : Identifier :=
    match ct with
    | .BasicCtrler => ctrler_name
    | .FIFO | .Unordered => entry
  pure $
    equal
      (← ids_to_var_term this_field)
      (← (ids_to_var_term <| [dest_ctrler_ref] ++ ctrler_field))

open Pipeline in
def CtrlerEntryIsStore
(ctrler_name : Identifier)
(ct : CtrlerType)
-- (this_field : List Identifier) -- can be single id too.
-- (ctrler_field : List Identifier) -- ensure single id is translated properly
: Except String Pipeline.Expr := do
  let dest_ctrler_ref : Identifier :=
    match ct with
    | .BasicCtrler => ctrler_name
    | .FIFO | .Unordered => entry
  pure $
    or_expr
      (expr_term $ equal
        (← ids_to_var_term [dest_ctrler_ref, instruction, op])
        (← ids_to_var_term [store.toMurphiString]))
      (expr_term $ equal
        (← ids_to_var_term [dest_ctrler_ref, instruction, op])
        (← ids_to_var_term [stlr.toMurphiString]))

def MinDifferenceBetween
-- (ctrler_name : Identifier)
-- (ct : CtrlerType)
(larger_num_field : List Identifier) -- can be single id too.
(smaller_num_field : List Identifier) -- ensure single id is translated properly
(larger_or_smaller_is_entry : Bool)
: Except String Pipeline.Expr := do
  let larger_prefix : List Identifier :=
    if larger_or_smaller_is_entry then [entry]
    else []
  let larger_field_term ← ids_to_var_term (larger_prefix ++ larger_num_field)

  let smaller_prefix : List Identifier :=
    if !larger_or_smaller_is_entry then [entry]
    else []
  let smaller_field_term ← ids_to_var_term (smaller_prefix ++ smaller_num_field)

  -- let entry_inst := (qualified_var [entry, instruction, seq_num].to_qual_name)
  -- let curr_ctrler_inst := (qualified_var current_ctrler_seq_num.to_qual_name)

  let inst_and_entry_inst_seq_num_diff := sub larger_field_term smaller_field_term
  let min_of_diff : Pipeline.Expr := some_term $ function_call [(min : Identifier)].to_qual_name [inst_and_entry_inst_seq_num_diff]
  pure min_of_diff

def AndExprs (exprs : List Pipeline.Expr) : Except String Pipeline.Expr := do
  match exprs with
  | [] => throw "AndExprs: empty list"
  | [one] => pure one
  | exp :: rest_exp => pure $ rest_exp.foldl ( λ acc an_expr => binand (expr_term acc) (expr_term an_expr) ) exp

def UnorderedSearch
(dest_c_name : CtrlerName)
(cond_e : Pipeline.Expr)
(min_constraint? : Option Pipeline.Expr)
(success_case_stmts : Pipeline.Statement)
(fail_case_stmts : Pipeline.Statement)
-- (search_api : MsgName)
-- (current_ctrler_seq_num : List Identifier)
: Pipeline.Statement :=
  -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
  -- when search_fail() from SB
  -- when search_success(write_value) from SB
  let when_search_fail := when_stmt [dest_c_name, search_fail].to_qual_name [] fail_case_stmts.to_block
  let when_search_success := when_stmt [dest_c_name, search_success].to_qual_name [] success_case_stmts.to_block

  let min_constraint :=
    match min_constraint? with
    | none => []
    | some min_constraint => [min_constraint]
  -- let min_of_diff : Pipeline.Expr := some_term $ function_call [(min : Identifier)].to_qual_name [inst_and_entry_inst_seq_num_diff]

  -- AZ NOTE: min_of_diff may not be necessary...
  let search_api_term : Pipeline.Term :=
    function_call [dest_c_name, search /- search_api -/].to_qual_name ([cond_e] ++ min_constraint)
  let search_api := await (some search_api_term) [when_search_success, when_search_fail]
  search_api

def FIFOTailSearch
(dest_c_name : CtrlerName)
(cond_e : Pipeline.Expr)
(success_case_stmts : Pipeline.Statement)
(fail_case_stmts : Pipeline.Statement)
-- (search_api : MsgName)
-- (current_ctrler_seq_num : List Identifier)
: Pipeline.Statement :=
  -- await SB.search((entry.phys_addr == phys_addr) & (entry.instruction.seq_num < instruction.seq_num), min(instruction.seq_num - entry.instruction.seq_num) )
  -- when search_fail() from SB
  -- when search_success(write_value) from SB
  let when_search_fail := when_stmt [dest_c_name, search_fail].to_qual_name [] fail_case_stmts.to_block
  let when_search_success := when_stmt [dest_c_name, search_success].to_qual_name [] success_case_stmts.to_block
  -- let min_of_diff : Pipeline.Expr := some_term $ function_call [(min : Identifier)].to_qual_name [inst_and_entry_inst_seq_num_diff]

  -- AZ NOTE: min_of_diff may not be necessary...
  let search_api_term : Pipeline.Term :=
    function_call [dest_c_name, tail_search /- search_api -/].to_qual_name ([cond_e])
  let search_api := await (some search_api_term) [when_search_success, when_search_fail]
  search_api

open Pipeline in
def QueryCtrlerWithConstraints
(cond_e : Pipeline.Expr)
(unordered_constraint_e? : Option Pipeline.Expr)
(if_found_stmt : Pipeline.Statement)
(else_nfound_stmt : Pipeline.Statement)
-- (ctrlers : Ctrlers)
(dest_c_type : CtrlerType)
(dest_c_name : CtrlerName)
: Except String SearchStatement := do
  -- let ctrler ← ctrlers.ctrler_from_name ctrler_states.ctrler |>.throw_exception_nesting_msg s!"Error (gen query for ctrler/states) trying to get ctrler: ({ctrler_states.ctrler}) from Ctrlers ({ctrlers.map (·.name)})"
  -- let ctrler_type ← ctrler.type
  match dest_c_type with
  | .BasicCtrler =>

    pure $ conditional_stmt $
      if_else_statement
        cond_e
        if_found_stmt
        else_nfound_stmt
  | .FIFO => -- use the search API here... probably exists in the old generate stall state function...
    -- FIFOSearch cond_e if_found_stmt else_nfound_stmt
    pure $ FIFOTailSearch dest_c_name cond_e if_found_stmt else_nfound_stmt
  | .Unordered =>
    pure $ UnorderedSearch dest_c_name cond_e unordered_constraint_e? if_found_stmt else_nfound_stmt

open Pipeline in
def CtrlerStates.query_insts_with_constraints
(ctrler_states : CtrlerStates)
(stall_on_inst_type : InstType)
-- (original_state_name : StateName)
(if_stmt : Pipeline.Statement)
(else_stmt : Pipeline.Statement)
(ctrlers : Ctrlers)
(search_type : SearchType)
(current_ctrler_seq_num : List Identifier)
: Except String SearchStatement := do
  let ctrler ← ctrlers.ctrler_from_name ctrler_states.ctrler |>.throw_exception_nesting_msg s!"Error (gen query for ctrler/states) trying to get ctrler: ({ctrler_states.ctrler}) from Ctrlers ({ctrlers.map (·.name)})"
  let ctrler_type ← ctrler.type
  match ctrler_type with
  | .BasicCtrler =>
    let entry_inst_type := (qualified_var [ctrler_states.ctrler, instruction, op].to_qual_name)
    let stall_inst_type := stall_on_inst_type.to_const_term
    let entry_is_stall_type := equal entry_inst_type stall_inst_type

    let entry_seq_num_valid :=
      less_than
        (qualified_var [ctrler_states.ctrler, instruction, seq_num].to_qual_name)
        (qualified_var [instruction, seq_num].to_qual_name)

    let entry_is_stall_type_and_valid := binand entry_is_stall_type.to_term entry_seq_num_valid.to_term

    pure $ conditional_stmt $
      if_else_statement
        entry_is_stall_type_and_valid
        if_stmt
        else_stmt
  | .FIFO => -- use the search API here... probably exists in the old generate stall state function...
  -- TODO: replace default. add helper to create search API with stmts in it
    if let some search_api := search_type.search_api ctrler_type then
      pure $ ctrler_states.ctrler.FIFOOlderInstSearch stall_on_inst_type [if_stmt] [] search_api
    else
      throw s!"Error (gen query for ctrler/states) trying to get search API for ctrler type: ({ctrler_type})"
  | .Unordered =>
    if let some search_api := search_type.search_api ctrler_type then
      pure $ ctrler_states.ctrler.UnorderedOlderInstSearch stall_on_inst_type [if_stmt] [] search_api current_ctrler_seq_num
    else
      throw s!"Error (gen query for ctrler/states) trying to get search API for ctrler type: ({ctrler_type})"

-- also return the not-completed-check var_name, so we can OR them together
def CtrlerStates.to_query
(ctrler_states : CtrlerStates)
(inst_type : InstType)
/- (stall_on_inst_type : InstType) -/
(original_state_name : StateName)
(ctrlers : Ctrlers)
-- NOTE: Consider if I actually just need a fixed number of stmts, like a tuple
: Except String (BoolDecl × SearchStatement × VarName) := do
  let var_name : VarName := ("_".intercalate [ctrler_states.ctrler, inst_type.toString, "is_in_state_set"]);

  -- Based on the ctrler type, if it's a queue, use the search API generation code below
  -- if it's a ctrler, just access the it's state vars to check if it's an older inst
  -- Write this in a helper function, pass the if condition to it to fill in the query
  -- Also, make the if condition expr generator also generate <ctrler_name>.state instead of just "state" since
  -- basic ctrlers won't be accessed with a await-when api call...

  -- This is the if stmt
  let (ctrler_not_completed_var, if_not_completed) ← ctrler_states.to_if_state_check var_name ctrlers

  -- TODO: Create a helper to check if the ctrler is a queue, and use the right search API
  let reset_to_original := (reset original_state_name)
  let query_older_insts ← ctrler_states.query_older_insts inst_type if_not_completed reset_to_original ctrlers SearchType.hit_one [instruction, seq_num]

  pure (ctrler_not_completed_var, query_older_insts, var_name)

-- #eval (1,2,3).2.2

def List.to_expr (var_names : List VarName) : Except String Pipeline.Expr :=
  match var_names with
  | [var_name] => pure $ var_expr var_name
  | var_name :: var_names => do
    -- Left var term
    let var_term' := var_term var_name

    -- Right var term (recursive from tail)
    let tail_expr ← var_names.to_expr |>.throw_exception_nesting_msg s!"Error converting list of var names to expr?"
    let tail_term := expr_term tail_expr

    -- OR the two terms
    let var_or_tree := or_expr var_term' tail_term
    pure var_or_tree
  | [] => throw s!"Error converting list of var names to expr: Empty list passed"

-- #eval ["a", "b", "c"].to_expr
def List.to_queries
(states_to_query : List (List CtrlerStates × InstType))
/-(stall_on_inst_type : InstType)-/
(original_state_name : StateName)
(ctrlers : Ctrlers)
: Except String (List (BoolDecl × SearchStatement × VarName)) := do
  -- convert ctrlerstates to a query on the controller / or entry, if the inst is older, and the state
  let decl_search_var : List (BoolDecl × SearchStatement × VarName) := List.join $ ← states_to_query.mapM (do
    let (states_list, stall_on_inst_type') := ·;
    let gen_checks := states_list.mapM (do
      let states := ·;
      CtrlerStates.to_query states stall_on_inst_type' /-stall_on_inst_type-/ original_state_name ctrlers
      )
      |>.throw_exception_nesting_msg s!"Error converting ctrlerstates to queries"
    gen_checks
    )
  pure decl_search_var

def List.to_query_result (vars : List VarName) : Except String (BoolDecl × Pipeline.Statement) := do
  -- logic OR the results
  -- create a var name to hold the result of the query
  let is_instruction_on_any_state : VarName := "is_instruction_on_any_state"
  let is_instruction_on_any_state_decl : BoolDecl := bool_decl ("bool", is_instruction_on_any_state).to_typed_identifier
  let var_or_tree : Pipeline.Expr := ← vars.to_expr |>.throw_exception_nesting_msg s!"Error creating OR tree for query results"

  let is_inst_on_any_state_stmt := variable_assignment [is_instruction_on_any_state].to_qual_name var_or_tree
  pure (is_instruction_on_any_state_decl, is_inst_on_any_state_stmt)

def Pipeline.Statement.decl_name : Pipeline.Statement → Except String VarName
| stmt => do
  match stmt with
  | .variable_declaration (Pipeline.TypedIdentifier.mk _ var_name) => pure var_name
  | _ => throw s!"Error (stmt is not a var decl) stmt: ({stmt})"


def Pipeline.QualifiedName.to_term : Pipeline.QualifiedName → Pipeline.Term
| qual_name => qualified_var qual_name

def Prod.to_list (tuple : Prod α α) : List α :=
  [tuple.1, tuple.2]

def List.tuple_to_list (tuple_list : List (Prod α α)) : List α :=
  tuple_list.map (·.to_list) |>.join

def stall_state_querying_states
(new_stall_state_name : StateName)
(original_state_name : StateName)
-- (ctrler_name : CtrlerName) -- current or this ctrler
(ctrler_states_to_query : List (List CtrlerStates × InstType))
/- (stall_on_inst_type : InstType) -/
(inst_to_stall_type : InstType)
(orig_body_stmts : List Pipeline.Statement)
(original_state's_handleblks : Option ( List HandleBlock ))
(ctrlers : Ctrlers)
: Except String Description
:= do
  let query_stmts := ← ctrler_states_to_query.to_queries /-stall_on_inst_type-/ original_state_name ctrlers |>.throw_exception_nesting_msg s!"Error (gen stall state) converting ctrler_states to queries, ctrler_states: ({ctrler_states_to_query})"

  -- result of querying the states
  let query_vars := query_stmts.map (·.2.2)
  let (is_instruction_on_any_state_decl, is_inst_on_any_state_stmt) := ← query_vars.to_query_result |>.throw_exception_nesting_msg s!"Error (gen stall state) converting query results to OR tree, query_vars: ({query_vars})"

  let query_result_var := ← is_instruction_on_any_state_decl.decl_name |>.throw_exception_nesting_msg s!"Error (gen stall state) getting var name from decl, decl: ({is_instruction_on_any_state_decl})"

  --##let stall_if_on_state_stmt : Pipeline.Statement :=
  --##  conditional_stmt $
  --##    if_else_statement
  --##      (var_expr query_result_var)
  --##      (reset new_stall_state_name).to_block -- should be same as the original
  --##      (orig_body_stmts.to_block)

  --##-- make query if this inst is of the type to stall on
  --##let stall_if_inst_is_of_to_stall_type : Pipeline.Statement :=
  --##  conditional_stmt $
  --##    if_else_statement
  --##      (equal [instruction, op].to_qual_name.to_term inst_to_stall_type.to_const_term)
  --##      stall_if_on_state_stmt.to_block
  --##      (orig_body_stmts.to_block)

  let stall_if_inst_type_matches_and_query_succeeds : Pipeline.Statement :=
    conditional_stmt $
      if_else_statement
        (Pipeline.Expr.binand
          (Pipeline.Term.expr (equal [instruction, op].to_qual_name.to_term inst_to_stall_type.to_const_term))
          (Pipeline.Term.expr (var_expr query_result_var))
        ) -- Inst op matches, and query succeeded
        ([reset new_stall_state_name,
          Pipeline.Statement.return_empty]).to_block -- should be same as the original
        (orig_body_stmts.to_block)

  -- the decls are query results
  let decls_queries : List (BoolDecl × SearchStatement) := query_stmts.map (let decl_query:= ·; (decl_query.1, decl_query.2.1))

  let new_state_body_stmts :=
    (decls_queries.tuple_to_list ++ [is_instruction_on_any_state_decl, is_inst_on_any_state_stmt] ++ [stall_if_inst_type_matches_and_query_succeeds])
  let new_body_in_listen_handle :=
    match original_state's_handleblks with
    | some handle_blks =>
      Statement.listen_handle (Statement.block new_state_body_stmts) handle_blks
      |>.to_block
    | none =>
      new_state_body_stmts.to_block

  pure <| state
    new_stall_state_name
    new_body_in_listen_handle

open Pipeline in
def Ctrlers.StallNode
(stall_state : StateName)
(stall_ctrler : CtrlerName)
(ctrler_states_to_query : List (List CtrlerStates × InstType))
(ctrlers : Ctrlers)
/- (inst_type_to_stall_on : InstType) -/
(inst_to_stall_type : InstType)
(new_stall_state_name : StateName)
: Except String Pipeline.Description := do
  /- 3. Gen the new stall state's name -/
  -- let new_stall_state_name := String.join [stall_ctrler, "_stall_", stall_state]

  /- 5. Generate the new stall state -/
  let stall_ctrler : Ctrler ← ctrlers.ctrler_matching_name stall_ctrler
  let state_to_stall ← stall_ctrler.state_of_name stall_state
  let stall_state_body : List Pipeline.Statement ← state_to_stall.body_stmts

  /- Consider if original state has a listen-handle -/
  let handle_blks : Option (List HandleBlock) ←
    get_ctrler_state_handle_blocks stall_ctrler stall_state

-- (new_stall_state_name : StateName)
-- (original_state_name : StateName)
-- (ctrler_states_to_query : List CtrlerStates)
-- (stall_on_inst_type : InstType)
-- (inst_to_stall_type : InstType)
-- (original_state's_handleblks : Option ( List HandleBlock ))
-- (ctrlers : Ctrlers)
  let new_stall_state : Description ← stall_state_querying_states
    new_stall_state_name stall_state
    ctrler_states_to_query
    /- inst_type_to_stall_on -/
    inst_to_stall_type
    stall_state_body handle_blks
    ctrlers

  dbg_trace s!"=== New Stall State: ===\n({new_stall_state})\n=== End New Stall State ==="
  pure new_stall_state

def CtrlerStates.is_member (ctrler_states : CtrlerStates) (ctrler_name : CtrlerName) (state_name : StateName) : Bool :=
  let is_member := (ctrler_states.ctrler == ctrler_name) && (ctrler_states.states.contains state_name)
  dbg_trace s!"is ctrler ({ctrler_name}) and state ({state_name}) ∈ ctrler_states ({ctrler_states})?"
  dbg_trace s!"is_member: ({is_member})"
  is_member

def List.is_ctrler_state_member (ctrler_states_list : List CtrlerStates) (ctrler_name : CtrlerName) (state_name : StateName) : Bool :=
  dbg_trace s!"test"
  ctrler_states_list.any (·.is_member ctrler_name state_name)

def List.add_ctrler_state (ctrler_states_list : List CtrlerStates) (ctrler_name : CtrlerName) (state_name : StateName) : Except String (List CtrlerStates) := do
  let ctrler_states_matchs : List CtrlerStates := ctrler_states_list.filter (·.ctrler == ctrler_name)
  let ctrler_states_match : CtrlerStates := ←
    match ctrler_states_matchs with
    | [ctrler_states] => do pure ctrler_states
    | [] => do throw s!"Error, couldn't find ctrler ({ctrler_name}) in list ({ctrler_states_list})"
    | _ => do throw s!"Error, found multiple ctrlers ({ctrler_name}) in list ({ctrler_states_list})"

  let added_state : CtrlerStates := ⟨ctrler_states_match.ctrler, ctrler_states_match.states ++ [state_name]⟩

  ctrler_states_list.map (let ctrler_states := ·; if ctrler_states.ctrler == ctrler_name then added_state else ctrler_states) |> pure

def List.to_query_match_do
(states_to_query : List (List CtrlerStates × InstType))
(when_success_stmts : List Pipeline.Statement)
(ctrlers : Ctrlers)
(current_ctrler_seq_num : List Identifier)
: Except String (List SearchStatement) := do
  -- convert ctrlerstates to a query on the controller / or entry, if the inst is older, and the state
  let decl_search_var : List (SearchStatement) := List.join $ ← states_to_query.mapM (do
    let (states_list, inst_to_query) := ·;
    let gen_checks := states_list.mapM (do
      let states := ·;

      let if_on_state_do := ←
        CtrlerStates.to_if_state_then_do states when_success_stmts [] ctrlers

      CtrlerStates.query_older_insts
        states
        inst_to_query
        if_on_state_do
        [].to_block
        ctrlers
        SearchType.hit_all
        current_ctrler_seq_num
      )
      |>.throw_exception_nesting_msg s!"Error converting ctrlerstates to queries"
    gen_checks
    )
  pure decl_search_var

def QueryAll
(ctrler_states_list : List (List CtrlerStates × InstType))
(when_search_success_stmts : List Pipeline.Statement)
(ctrlers : Ctrlers)
(current_ctrler_seq_num : List Identifier)
: Except String (List Pipeline.Statement) := do
  ctrler_states_list.to_query_match_do when_search_success_stmts ctrlers current_ctrler_seq_num


def Pipeline.Statement.key_assignment? (stmt : Pipeline.Statement) : Option Pipeline.Expr :=
  match stmt with
  | .variable_assignment qual_name expr =>
    match qual_name == ["key"].to_qual_name with
    | true => some expr
    | false => none
  | _ => none

def List.find_key? (stmts : List Pipeline.Statement) : Except String ( Option Pipeline.Expr ) := do
  let key_assignments? := stmts.map (·.key_assignment?)
  let key_assignments := key_assignments?.filterMap id
  match key_assignments with
  | [key] => do pure $ some key
  | [] => do pure none
  | _ => do throw s!"Error, found multiple key assignments ({key_assignments})"

open Pipeline in
def Ctrler.key (ctrler : Ctrler) : Except String Pipeline.Expr := do
  let description := ctrler.controller_descript
  let stmts_block := ← match description with
    | Description.controller /- name -/_ stmts => do pure stmts
    | _ => do throw s!"Error, expected controller description, got ({description})"
  let stmts := ← stmts_block.stmt_block
  let key? := ← stmts.find_key?
  match key? with
  | some key => do pure $ key
  | none => do throw s!"Error, couldn't find key assignment in controller ({ctrler})"

def Pipeline.Statement.when_stmt_arguments
(when_stmt : Pipeline.Statement)
: Except String (List Identifier) := do
  (get_when_stmt_src_args when_stmt)
