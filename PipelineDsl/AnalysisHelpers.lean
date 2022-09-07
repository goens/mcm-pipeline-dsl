import PipelineDsl.AST
import PipelineDsl.Murphi

open Pipeline
open Murϕ

structure controller_info where
  -- Name, like LQ, SQ, SB, etc.
  name : Identifier
  -- The controller description, probably some info here...
  controller_descript : Description
  -- The entry description, probably some info here...
  entry_descript : Description
  -- The init transition
  init_trans : Identifier
  -- Entry vars, like seq_num, ld_seq_num, inst, read_value
  -- NOTE: leave for now, figure out tomorrow
  -- Or translate from the entry_descript
  state_vars : List TypedIdentifier
  -- list of transitions this structure takes
  -- should be: Description.transition
  transition_list : List Description
deriving Inhabited

instance : ToString controller_info := ⟨
  λ i =>
    "===controller===\n" ++
    "NAME: " ++ toString i.name ++ "\n" ++
    "CONTROLLER_DESCRIPTION: " ++ toString i.controller_descript ++ "\n" ++
    "ENTRY_DESCRIPT: " ++ toString i.entry_descript ++ "\n" ++
    "INIT_TRANS: " ++ toString i.init_trans ++ "\n" ++
    "STATE_VARS: " ++ toString i.state_vars ++ "\n" ++
    "TRANSITION_LIST: " ++ toString i.transition_list ++ "\n=== End Controller ===\n\n"
  ⟩ 


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
entry_decl : Decl -- Decl.type, ID/String TypeExpr.record
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

-- open /-Murphi-/Murϕ in
def ast0048_generate_controller_murphi_record
( ctrl : controller_info )
:=
  -- TODO: read the entry description
  -- build the per entry record of state vars
  -- read the transition names
  -- build allowable states murphi enum
  -- Also add state for searching
  let murphi_decls_lst :=
  -- NOTE: Lean likes it when the parentheses
  -- starts immediately on the same line as map
  -- nightly-2022-07-13
  ctrl.state_vars.map (
    λ dsl_typed_ident => 
    match dsl_typed_ident with
    | TypedIdentifier.mk tiden ident =>
      Decl.var [ident] (TypeExpr.previouslyDefined tiden)
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
    (String.join [ctrl.name, "_COUNT_ENUM"])
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

  let list_of_transition_names : List String :=
    ctrl.transition_list.map λ trans => match trans with
    | Pipeline.Description.transition ident _ => ident
    | _ => dbg_trace "shouldn't reach here???!"
      panic! "TODO: Throw.."
  let ctrler_entry_state :=
    Murϕ.Decl.type (ctrl.name.append "_state") (
    Murϕ.TypeExpr.enum list_of_transition_names)

  -- Now we can build a Decl for the controller record
  let murphi_ctrler_record_name := String.join [ctrl.name, "_entries"]
  let murphi_ctrler_record :=
    Decl.var [ctrl.name] (
      TypeExpr.record [
        -- The array of entries, which is also a record
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
        Decl.var ["state"] (
          TypeExpr.previouslyDefined
          (ctrl.name.append "_state")
          ),
        -- Head, tail, and num_entries counter
        Decl.var ["head"] (
          TypeExpr.previouslyDefined
          ctrler_entries_range_decl_name
          ),
        Decl.var ["tail"] (
          TypeExpr.previouslyDefined
          ctrler_entries_range_decl_name
          ),
        Decl.var ["num_entries"] (
          TypeExpr.previouslyDefined
          ctrler_entries_count_decl_name
          )
        -- NOTE: don't do msg buffers!
      ]
    )

  -- We must also send back the supporting Decl's
  -- for translation/code generation
  let ctrler_entry_const_decls : ctrler_decl_entry_decl_const_decl := {
    ctrler_decl := murphi_ctrler_record,
    entry_decl := murphi_entry_record_decl,
    const_decl_lst := [
      ctrler_num_entries_range_enum_const,
      ctrler_num_entries_const
      ],
    range_enum_decl := [ctrler_entries_range_decl, ctrler_entries_count_decl],
    entry_state_decl := ctrler_entry_state

    }
  ctrler_entry_const_decls