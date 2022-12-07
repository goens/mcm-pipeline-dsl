import PipelineDsl.AST
import Murphi

-- Just defining as string for now
-- might be nicer to define them as structs later with meta-data
-- Inst in Litmus Test
inductive InstType
| load : InstType
| store : InstType
deriving Inhabited, BEq
-- inductive InstType
-- | load : InstType
-- | store : InstType
-- deriving Inhabited, BEq

def load : InstType := InstType.load
def store : InstType := InstType.store

def InstType.toString : InstType → String
| .load => "Load"
| .store => "Store"  
instance : ToString InstType where toString := InstType.toString

def InstType.toMurphiString : InstType → String
| .load => "ld"
| .store => "st"  

-- #eval InstType.toMurphiString (InstType.load)
-- #eval (InstType.load).toMurphiString

structure CtrlerType where
name : String
deriving Inhabited, BEq

def FIFO : CtrlerType := {name := "FIFO"}
def Unordered : CtrlerType := {name := "Unordered"}

def IndexableCtrlerTypes : List CtrlerType := [FIFO, Unordered]
def IndexableCtrlerTypesStrings : List String := IndexableCtrlerTypes.map (fun ctrler_type => ctrler_type.name )


structure file_name_and_output where
name : String
murphi_code : Murϕ.Program

open Pipeline
open Murϕ

structure controller_info where
  -- Name, like LQ, SQ, SB, etc.
  name : Identifier
  -- The controller description, probably some info here...
  controller_descript : Description
  -- The entry description, probably some info here...
  entry_descript : Option Description
  -- The init transition
  init_trans : Option Identifier
  -- Entry vars, like seq_num, ld_seq_num, inst, read_value
  -- NOTE: leave for now, figure out tomorrow
  -- Or translate from the entry_descript
  state_vars : Option (List TypedIdentifier)
  -- list of transitions this structure takes
  -- should be: Description.transition
  transition_list : Option (List Description)
  -- ======== CTRLER State Machine STUFF ========
  ctrler_init_trans : Option Identifier
  ctrler_trans_list : Option (List Description)
  ctrler_state_vars : Option (List TypedIdentifier)
deriving Inhabited

instance : ToString controller_info := ⟨
  λ i =>
    "===controller===\n" ++
    "NAME: " ++ toString i.name ++ "\n" ++
    "CONTROLLER_DESCRIPTION: " ++ toString i.controller_descript ++ "\n" ++
    "ENTRY_DESCRIPT: " ++ toString i.entry_descript ++ "\n" ++
    "INIT_TRANS: " ++ toString i.init_trans ++ "\n" ++
    "STATE_VARS: " ++ toString i.state_vars ++ "\n" ++
    "TRANSITION_LIST: " ++ toString i.transition_list ++ "\n" ++
    s!"CTRLER_init_trans: ({i.ctrler_init_trans})\n" ++
    s!"CTRLER_state_vars: ({i.ctrler_state_vars})\n" ++
    s!"CTRLER_trans_list: ({i.ctrler_trans_list})\n" ++
    "\n=== End Controller ===\n\n"
  ⟩ 


def thing : controller_info := default
#eval thing

def filter_lst_of_stmts_for_ordering_asn
(lst_stmts : List Pipeline.Statement)
:=
  List.filter (
    λ stmt => 
      match stmt with
      -- | Statement.variable_assignment qual_name expr =>
      --   match qual_name with
      --   | QualifiedName.mk lst_idents' =>
      --     if (lst_idents'.contains "element_ordering")
      --       then true
      --       else false
      | Statement.value_declaration typed_ident expr =>
        match typed_ident with
        | TypedIdentifier.mk tident ident =>
          if (
            or
            (tident == "element_ordering")
            (ident == "ordering")
          )
          then true
          else false
      | _ => false
        
  )
  lst_stmts

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
  | _ => dbg_trace "Error: unexpected Stmt"
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
( ctrl : controller_info )
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
    murphi_state_vars.concat (
      Decl.var ["state"] (
        TypeExpr.previouslyDefined
        (ctrl.name.append "_state")
      )
    )
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

    let murphi_decls_lst :=
      murphi_state_vars.concat (
        Decl.var ["state"] (
          TypeExpr.previouslyDefined
          (ctrl.name.append "_state")
        )
      )

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

-- Future TODO: let any insert operations be auto-generated
-- by any structures that use them
-- The framework is already there... nothing needs to be done..

-- def create_murphi_ctrler_insert_func
-- ( ctrler_name : String )
-- : Murϕ.ProcDecl
-- :=
--   let insert_name := String.join [ctrler_name, "_insert"]
--   let ld_spec_ctrler := find_speculative_ld_ctrler
--   let st_spec_ctrler := find_speculative_st_ctrler
--   let lq_idx_t := ld_spec_ctrler.append "_idx_t"

--   let proc : Murϕ.ProcDecl :=
--   [murϕ_proc_decl|
-- function £insert_name(
--              lq : £ld_spec_ctrler;
--              sq : £st_spec_ctrler;
--              inst : INST;
--            ) : LQ;
--   var lq_new : LQ;
--   var lq_tail : ld_idx_t;

--   --# ADDED NOTE
--   --#var sq : SQ;
--   var curr_tail_entry : LD_ENTRY_VALUES;
-- begin
--   --
--   lq_new := lq;
--   curr_tail_entry := lq_new.ld_entries[lq.ld_tail];

--   assert curr_tail_entry.ld_state = await_creation "to insert, load should be awaiting creation";
--   curr_tail_entry.ld_state := await_scheduled;
--   --# AZ TODO: do the store_queue check

--   --# Consider placing the Check Store_queue latest
--   --# Entry here!
--   --# Though if I do, this means this action is atomic,
--   --# and no other message passing operations 
--   --# NOTE should be add the assert in automatically?
--   --# or allow the user to specify asserts as well?
--   --# Or both?
--   --# Generated asserts shouldn't cause problems for the user
--   assert (curr_tail_entry.st_seq_num = 0) "should first be 0?";
--   if (sq.num_entries != 0) then
--     --#NOTE: REMEMBER TO CLEAR ST SEQ NUM
--     --# at the end...
--     curr_tail_entry.st_seq_num := sq.entries[sq.head].instruction.seq_num;
--   else
--     --# Keep at none
--     --# 0 is "none" here...
--     curr_tail_entry.st_seq_num := 0;
--   end;

--   --# NOTE: Auto generate the standard "insert" part
--   curr_tail_entry.instruction := inst;
--   lq_new.ld_tail := ( lq.ld_tail + 1 ) % (LD_ENTRY_NUM + 1);
--   lq_new.num_entries := lq.num_entries + 1;
--   --
--   --# NOTE: assert, but not technically required, since
--   --# if it's out of the range, Murphi throws an error
--   assert (lq.num_entries < ( LD_ENTRY_NUM + 1)) "can't add more!";

--   lq_new.ld_entries[lq.ld_tail] := curr_tail_entry;

--   return lq_new;
-- end
--   ]

partial def get_only_transitions_recursively
(stmt : Pipeline.Statement)
:=
          -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
          -- dbg_trace stmt
          -- dbg_trace "==END GET-TRANSITIONS ==\n"

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

def get_when_stmt_src_args
(when_stmt : Pipeline.Statement)
: Except String (List Identifier)
:= do
  match when_stmt with
  | .when _ args _ => return args
  | _ =>
    let msg : String := "Error: function expected a when stmt\n"++
      s!"Instead got this stmt: ({when_stmt})"
    throw msg

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
