
import PipelineDsl.LoadReplayHelpers
import PipelineDsl.InstructionHelpers
import PipelineDsl.AST

import PipelineDsl.PipelineHelpers
import PipelineDsl.DSLHelpers

-- === BEGIN Legacy code for translation ===
-- NOTE: This ControllerType is now more "legacy", use the CtrlerType now..
structure ControllerType where
name : String
deriving Inhabited, BEq

def FIFO : ControllerType := {name := "FIFO"}
def Unordered : ControllerType := {name := "Unordered"}

def IndexableCtrlerTypes : List ControllerType := [FIFO, Unordered]
def IndexableCtrlerTypesStrings : List String := IndexableCtrlerTypes.map (fun ctrler_type => ctrler_type.name )
-- === END Legacy code for translation ===


open Pipeline in
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

inductive CtrlerType
| FIFO : CtrlerType
| Unordered : CtrlerType
| BasicCtrler : CtrlerType
deriving Inhabited, BEq

-- abbrev CtrlerType := ControllerType

def CtrlerType.toString : CtrlerType → String
| .FIFO => "FIFO"
| .Unordered => "Unordered"
| .BasicCtrler => "BasicCtrler"
instance : ToString CtrlerType where toString := CtrlerType.toString

def CtrlerType.is_a_queue : CtrlerType → Bool
| .BasicCtrler => false
| _ => true

inductive entry_or_ctrler
| entry : entry_or_ctrler
| ctrler : entry_or_ctrler
deriving Inhabited
def entry_or_ctrler.toString : entry_or_ctrler → String
| .entry => "Currently translating for an entry-type structure"
| .ctrler => "Currently translating for a ctrler-type structure"
instance : ToString entry_or_ctrler where toString := entry_or_ctrler.toString

def CtrlerType.entry_or_ctrler
(ctrler_type : CtrlerType)
: entry_or_ctrler :=
  match ctrler_type with
  | .FIFO
  | .Unordered => entry_or_ctrler.entry
  | .BasicCtrler => entry_or_ctrler.ctrler

abbrev Ctrlers := List controller_info
abbrev Ctrler := controller_info

-- == Translation Helpers ==

def Ctrlers.ctrler_matching_name : Ctrlers → CtrlerName → Except String controller_info
| ctrlers, ctrler_name => do
  match ctrlers.filter (·.name == ctrler_name) with
  | [ctrler] => pure ctrler
  | [] => throw s!"No ctrlers matching name ({ctrler_name}) in ctrlers list ({ctrlers})"
  | _::_ => throw s!"Multiple ctrlers matching name ({ctrler_name}) in ctrlers list ({ctrlers})"

def Ctrler.entry_or_ctrler_translation
(dest_ctrler : Ctrler)
: Except String entry_or_ctrler := do
  if dest_ctrler.init_trans.isSome then do
    pure entry_or_ctrler.entry
  else if dest_ctrler.ctrler_init_trans.isSome then do
    pure entry_or_ctrler.ctrler
  else do
    throw s!"ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"

open Pipeline in
def Ctrler.states (ctrler : Ctrler) : Except String (List Description)
:= do
  if let some entry_states := ctrler.transition_list then do
    if ctrler.init_trans.isSome then do
      pure entry_states
    else
      throw s!"ERROR, (malformed entry-ctrler) doesn't have init transition info? ({ctrler})"
  else if let some basic_states := ctrler.ctrler_trans_list then do
    if ctrler.ctrler_init_trans.isSome then do
      pure basic_states
    else do
      throw s!"ERROR, (malformed basic-ctrler) doesn't have init transition info? ({ctrler})"
  else do
    throw s!"ERROR, ctrler doesn't have states? ({ctrler})"

open Pipeline in
def List.state_matching_name
(states : List Description)
(state_name : StateName)
: Except String Description := do
  let states_list := ←
    states.filterM (do
      let a_state_name ← ·.state_name
      pure (a_state_name == state_name)
    )
  match states_list with
  | [state] => pure state
  | _::_ => throw s!"Error: multiple states matching a name ({state_name})? States: ({states_list})"
  | [] => throw s!"Error: no states matching name ({state_name}) found?"

open Pipeline in
def Ctrler.state_of_name
(ctrler : Ctrler)
(state_name : StateName)
: Except String Description := do
  let states ← ctrler.states
  states.state_matching_name state_name

open Pipeline in
def CreateTableQueue -- CAM like table
(table_name : String)
(table_size : Nat)
(entries : List (VarType × VarName))
(entry_key : VarName)
-- Insert/Remove msg
(insert_args : List VarName)
(insert_actions : List Statement)
(insert_from : CtrlerName)
(remove_args : List VarName)
(remove_actions : List Statement)
(remove_from : CtrlerName)

(commit_ctrler : CtrlerName)
(squash_sender : CtrlerName)
(commit_squasher : CtrlerName)
: Except String Ctrler := do
  -- ====== controller description ======
  -- Stmts needed:
  -- (1) something to identify key
  -- key = seq_num
  -- (2) num entries of course
  -- num_entries = num_entries
  -- (3) determine the queue type
  -- element_ordering ordering = Unordered
  let key_asgn := variable_assignment [key].to_qual_name <| var_expr entry_key
  let num_entries_asgn := value_decl ("int", num_entries).to_typed_identifier <| num_lit_expr table_size
  let ordering_asgn :=
    value_decl
      (element_ordering, ordering).to_typed_identifier
      $ var_expr CtrlerType.Unordered.toString
  let ast_table_ctrler := Description.controller table_name [key_asgn, num_entries_asgn, ordering_asgn].to_block

  -- ===== entry description =====
  -- stmts needed
  -- (1) list the entries
  -- entries can be elaborated here
  -- (2) indicate the initial state
  -- init_state = init_table_"table_name"
  let entry_stmts := entries.map ( let (var_type, var_name) := ·
    variable_declaration (var_type, var_name).to_typed_identifier )
  let (init_state_asgn, init_state_name) := let init_state_name := ("init_table_" ++ table_name)
    ( variable_assignment [init_state].to_qual_name <| var_expr init_state_name ,
    init_state_name )

  let ast_entry := Description.entry table_name (entry_stmts ++ [init_state_asgn]).to_block

  -- ===== state description =====
  -- (1) Need the init state. We can just assign a default to the
  -- entry vars
  -- (2) Need a state to await for an insertion of some item with some key.
  -- This state also handles removal with remove_key().
  -- I could just re-use insert() with a key
  -- let states := []
  -- (3) need one more state! after inserting, entries shouldn't be able to be inserted again..

  -- Trying an implementation which just has 1 state
  let await_insert_remove_state_name := table_name ++ "_await_insert_remove"
  let when_insert_stmt := Statement.when
    [insert_from, insert_key].to_qual_name
    insert_args
    (insert_actions ++ [reset await_insert_remove_state_name]).to_block
  let when_remove_stmt := Statement.when
    [remove_from, remove_key].to_qual_name
    remove_args
    (remove_actions ++ [reset await_insert_remove_state_name]).to_block
  let await_when_remove_stmt := await none [when_insert_stmt, when_remove_stmt]

  -- a handle block to handle squashing
  -- handle block checks if an entry's seq_num is >= a provided seq_num
  -- if it is, then remove the entry
  let violating_seq_num := "violating_seq_num"
  -- if expr
  let squash_cond := VarCompare [entry_key] Expr.geq [violating_seq_num]
  -- remove this entry
  let remove_entry := stray_expr $ term_expr $ function_call [table_name, remove_key].to_qual_name [var_expr entry_key]
  -- squash Commit Ctrler entries as well
  let squash_commit_entry := stray_expr $ term_expr $ function_call [commit_ctrler, squash].to_qual_name [var_expr violating_seq_num]
  -- reset to self
  let reset_await := reset await_insert_remove_state_name
  -- if stmt
  let if_squash := conditional_stmt $ if_statement squash_cond [remove_entry, squash_commit_entry, reset_await].to_block
  let user_commit_squash := conditional_stmt $ if_statement squash_cond [remove_entry, /- squash_commit_entry, -/ reset_await].to_block

  -- handle squash
  let handle_squashes : List HandleBlock := [
    (handle [squash_sender, squash].to_qual_name [violating_seq_num] [if_squash].to_block),
    (handle [commit_squasher, squash].to_qual_name [violating_seq_num] [user_commit_squash].to_block)
  ]
  let listen_handle_squash := listen await_when_remove_stmt.to_block handle_squashes

  -- Create the state
  let await_insert_remove_state := state await_insert_remove_state_name listen_handle_squash.to_block

/- -- If I later decide it's better to have 2 states....
  let await_remove_state_name := table_name ++ "_await_remove"
  let await_insert_state_name := table_name ++ "_await_insert"
  let trans_to_insert := transition await_insert_state_name
  let when_remove_stmt := Statement.when
    [remove_from, remove_key].to_qual_name
    remove_args
    (remove_actions ++ [trans_to_insert]).to_block
  let await_when_remove_stmt := await none [when_remove_stmt]
  let await_remove_state := state await_remove_state_name await_when_remove_stmt.to_block

  -- (2) await insert state
  -- Create await stmt, to await insert, and then assign the key and other state vars
  -- Transition to (3) to await removal
  let trans_to_remove := transition await_remove_state_name
  let when_insert_stmt := Statement.when
    [insert_from, insert_key].to_qual_name
    insert_args
    (insert_actions ++ [trans_to_remove]).to_block
  let await_when_insert_stmt := await none [when_insert_stmt]
  let await_insert_state := state await_insert_state_name await_when_insert_stmt.to_block
-/
  -- (1) init state
  let init_stmts := ← entries.mapM ( λ (var_type, var_name) => do
    let default_val := ← default_value_expr var_type
      |>.throw_exception_nesting_msg s!"(Create Invalidation Listener): Could not find default value for ({var_type})";
    pure $ variable_assignment [var_name].to_qual_name <| default_val)
  let trans_to_first_state := transition await_insert_remove_state_name
  let init_table_state := state init_state_name $ ( init_stmts ++ [trans_to_first_state] ).to_block


  -- ===== Create the controller =====
  let state_vars := entries.map (let (var_type, var_name) := ·; (var_type, var_name).to_typed_identifier)
  let ctrler : Ctrler := ⟨ table_name, ast_table_ctrler,
    some ast_entry, some init_state_name, some state_vars,
    some [init_table_state, await_insert_remove_state /-await_insert_state, await_remove_state-/],
    none, none, none⟩ 
  
  pure ctrler

def CreateLoadAddressTableCtrler
(perform_load_node_ctrler_name : CtrlerName)
(commit_node_ctrler_name : CtrlerName)
(squash_sender : CtrlerName)
(commit_squasher : CtrlerName)
: Except String (Ctrler × CtrlerName × VarName × VarName) := do
  let lat_address_var := "_".intercalate ["lat", address]
  let lat_seq_num_var := "_".intercalate ["lat", seq_num]

  let input_address := "input_address"
  let input_seq_num := "input_seq_num"

  let lat_name := "load_address_table"
  let lat_size := 2 -- chosing a number for now..
  let entries := [(seq_num, lat_seq_num_var), (address, lat_address_var)]
  let entry_key := lat_seq_num_var
  let insert_args := [input_seq_num, input_address]
  let insert_actions := [var_asn_var [lat_seq_num_var] input_seq_num, var_asn_var [lat_address_var] input_address]
  let insert_from := perform_load_node_ctrler_name

  let remove_args := [input_seq_num]
  let remove_actions :=
    [variable_assignment [lat_seq_num_var].to_qual_name (← default_value_expr seq_num)]
  let remove_from := commit_node_ctrler_name

  let lat : Ctrler ← CreateTableQueue
    lat_name lat_size
    entries entry_key
    insert_args insert_actions insert_from
    remove_args remove_actions remove_from
    commit_node_ctrler_name
    squash_sender commit_squasher
  
  pure (lat, lat_name, lat_seq_num_var, lat_address_var)

def Pipeline.Description.inject_state_stmts
(state : Description)
(state_name : StateName)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(var_to_insert_after? : Option Statement)
(arg_in_when_stmt? : Option Identifier)
(UpdateState : Description → InstType → List Statement → Option Statement → Option Identifier → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Description := do
  match (← state.state_name) == state_name with
  | true => do UpdateState state inst_type stmts_to_inject var_to_insert_after? arg_in_when_stmt? InjectStmtsAt
  | false => pure state

def Pipeline.Statement.var_asgn_ordering : Pipeline.Statement → Except String (CtrlerType)
| stmt => do
  match stmt with
  | .value_declaration typed_ident expr => do
    -- let (type', ident) := typed_ident.type_ident
    match typed_ident.is_ident_ordering with
    | true => do
      let var_ident ← expr.var_ident 
      if var_ident == "FIFO" then
        pure $ CtrlerType.FIFO
      else if var_ident == "Unordered" then
        pure $ CtrlerType.Unordered
      else
        throw "Expr.var_ident is not a valid ordering"
    | false => throw "Statement's LHS isn't Ordering"
  | _ =>
    let msg := s!"Statement is not a variable assignment: ({stmt})"
    throw msg

def Pipeline.Statement.ordering_from_stmt_block : Pipeline.Statement → Except String (CtrlerType)
| stmt => do
  let blk ← stmt.stmt_block
  let ordering_list : List Pipeline.Statement:= filter_lst_of_stmts_for_ordering_asn blk
  let ordering_asgn ←
    match ordering_list with
    | [] => throw "No ordering found in stmt block"
    | [ordering] => pure ordering
    | _ => throw "Multiple orderings found in stmt block"
  ordering_asgn.var_asgn_ordering

def Pipeline.Description.ctrler_type : Pipeline.Description → Except String CtrlerType
| descript => do
  match descript with
  | .controller /- identifier -/ _ stmt =>
    stmt.ordering_from_stmt_block
  | _ => throw "Description is not a controller: ({descript})"

def controller_info.type : controller_info → Except String CtrlerType
| ctrler =>
  if ctrler.entry_descript.isSome then
    ctrler.controller_descript.ctrler_type
  else
    pure CtrlerType.BasicCtrler

open Pipeline in
def Ctrler.get_state_vars (ctrler : Ctrler)
: Except String (List TypedIdentifier)
:= do
  match ← ctrler.type with
  | .BasicCtrler => do
    match ctrler.ctrler_state_vars with
    | some state_vars => do pure state_vars
    | none => do throw "BasicCtrler doesn't have state vars"
  | .FIFO | .Unordered => do
    match ctrler.state_vars with
    | some state_vars => do pure state_vars
    | none => do throw "FIFO/Unordered Queue doesn't have state vars"

def Ctrler.state_var_names (ctrler : Ctrler)
: Except String (List Identifier)
:= do
  let state_vars ← ctrler.get_state_vars
  let state_var_names : List Identifier := state_vars.map (·.type_ident.snd)
  pure state_var_names

open Pipeline in
def Ctrler.inject_states
(ctrler : Ctrler)
(states : List Description)
: Except String Ctrler := do
  let c_type ← ctrler.type
  match c_type with
  | .FIFO | .Unordered => do
    let new_ctrler : Ctrler := {
      name := ctrler.name,
      controller_descript := ctrler.controller_descript,
      entry_descript := ctrler.entry_descript,
      init_trans := ctrler.init_trans,
      state_vars := ctrler.state_vars,
      transition_list := some states,
      ctrler_init_trans := ctrler.ctrler_init_trans,
      ctrler_trans_list := ctrler.ctrler_trans_list,
      ctrler_state_vars := ctrler.ctrler_state_vars
    }
    pure new_ctrler
  | .BasicCtrler => do
    let new_ctrler : Ctrler := {
      name := ctrler.name,
      controller_descript := ctrler.controller_descript,
      entry_descript := ctrler.entry_descript,
      init_trans := ctrler.init_trans,
      state_vars := ctrler.state_vars,
      transition_list := ctrler.transition_list,
      ctrler_init_trans := ctrler.ctrler_init_trans,
      ctrler_trans_list := some states,
      ctrler_state_vars := ctrler.ctrler_state_vars
    }
    pure new_ctrler

open Pipeline in
def Ctrler.inject_ctrler_state
(ctrler : Ctrler)
(ctrler_name : CtrlerName)
(state_name : StateName)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(stmt_to_insert_after? : Option Statement)
(arg_in_when_stmt? : Option Identifier)
(UpdateState : Description → InstType → List Statement → Option Statement → Option Identifier → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Ctrler := do
  match ctrler.name == ctrler_name with
  | true => do
    let states := ← ctrler.states
    let updated_states := ← states.mapM (·.inject_state_stmts state_name inst_type stmts_to_inject stmt_to_insert_after? arg_in_when_stmt? UpdateState InjectStmtsAt)
    ctrler.inject_states updated_states
  | false => do
    pure ctrler

-- i.e. use with inject_stmts_at_perform
open Pipeline in
def Ctrlers.inject_ctrler_state
(ctrlers : Ctrlers)
(ctrler_name : CtrlerName)
(state_name : StateName)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(stmt_to_insert_after? : Option Statement)
(arg_in_when_stmt? : Option Identifier)
(UpdateState : Description → InstType → List Statement → Option Statement → Option Identifier → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Ctrlers := do
  ctrlers.mapM (let ctrler : Ctrler := ·;
    ctrler.inject_ctrler_state ctrler_name state_name inst_type stmts_to_inject stmt_to_insert_after? arg_in_when_stmt? UpdateState InjectStmtsAt)

open Pipeline in
def Ctrlers.AddInsertToLATWhenPerform -- Load Address Table
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(perform_load_ctrler_name : CtrlerName)
(perform_load_state_name : StateName)
(load_req_address : Expr)
(load_req_seq_num : Expr)
(stmt_to_insert_after? : Option Statement)
(arg_in_when_stmt? : Option Identifier)
(InjectStmtsFn : InjectStmtsFunction)
: Except String Ctrlers := do

  -- Insert a stmt to insert_key(seq_num, address) into the LAT  
  -- TODO: Update the function so it uses a provided argument for the seq_num / address
  -- So it finds the address from the used var expr from the send_load_request API
  let insert_key_stmt : Statement := stray_expr $ some_term $
    -- function_call [lat_name, insert_key].to_qual_name [qual_var_expr [instruction, seq_num], var_expr load_req_address]
    function_call [lat_name, insert_key].to_qual_name [load_req_seq_num, load_req_address]
  let inst_is_load := equal (qual_var_term [instruction, op]) (var_term load.toMurphiString)
  let if_load_then_insert_key :=
    Statement.conditional_stmt $
      Conditional.if_statement inst_is_load
        insert_key_stmt

  let ctrlers_insert_key_into_lat :=
    ctrlers.inject_ctrler_state
      perform_load_ctrler_name perform_load_state_name
      load
      [if_load_then_insert_key]
      stmt_to_insert_after?
      arg_in_when_stmt?
      Pipeline.Description.inject_stmts_at_stmt
      -- List.inject_stmts_at_perform
      -- List.inject_stmts_after_stmt_at_ctrler_state
      InjectStmtsFn
  
  ctrlers_insert_key_into_lat

open Pipeline in
def Ctrlers.AddRemoveFromLATWhenCommit
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(commit_ctrler_name : CtrlerName)
(commit_state_name : StateName)
(function_inject_stmts_at_point : List Statement → InstType → List Statement → Option Statement → Option Identifier → Except String (Bool × List Statement))
(key_to_remove : List Identifier)
: Except String Ctrlers := do
  -- Insert a stmt to remove_key(seq_num) from the LAT
  let remove_key_stmt : Statement := stray_expr $ some_term $
    function_call [lat_name, remove_key].to_qual_name [← key_to_remove.to_dsl_var_expr]
  let inst_is_type := VarCompare [instruction, op] equal [load.toMurphiString]
  let if_inst_is_type := conditional_stmt <| if_statement inst_is_type remove_key_stmt
  
  let ctrlers_remove_key_from_lat :=
    ctrlers.inject_ctrler_state
      commit_ctrler_name commit_state_name load [if_inst_is_type] none none Pipeline.Description.inject_stmts_at_stmt function_inject_stmts_at_point
  
  ctrlers_remove_key_from_lat

--/--
--  Get A Ctrler's State Var that is of the "inst" type (i.e. the instruction).
--  NOTE: Create an option version if it's needed.
---/

open Pipeline in
def List.is_var_in_state_vars
(typed_idents : List TypedIdentifier)
(var_name : VarName)
: Bool :=
  typed_idents.any (let (/- type_name -/ _, ident) := ·.type_ident; ident == var_name)

def Ctrler.is_a_state_var_of_ctrler
(ctrler : Ctrler)
(var_name : Identifier)
: Except String Bool := do
  let state_vars ← ctrler.get_state_vars
  pure $ state_vars.is_var_in_state_vars var_name

open Pipeline in
def Ctrler.instruction_var
(ctrler : Ctrler)
: Except String Identifier := do
  let state_vars ← ctrler.get_state_vars

  match state_vars.filter (·.is_inst_type) with
  | [] => throw s!"Error: Expected to find an inst type state var in Ctrler?: State Vars: ({state_vars})"
  | [inst] => pure inst.var_name
  | _::_ => throw s!"Error: Expected to find one inst type state var in Ctrler?: State Vars: ({state_vars})"

-- Newer, better, version of get_ctrler_from_ctrlers_list
def Ctrlers.ctrler_from_name (ctrlers : Ctrlers) (ctrler_name : CtrlerName)
: Except String Ctrler := do
  let ctrler_match_list := ctrlers.filter (·.name = ctrler_name)
  match ctrler_match_list with
  | [ctrler] => pure ctrler
  | [] =>
    let msg : String := s!"Error: No ctrler with name ({ctrler_name}) found in list ({ctrlers})"
    throw msg
  | _::_ =>
    let msg : String := s!"Error: Multiple ctrlers with name ({ctrler_name}) found in list ({ctrlers})"
    throw msg

open Pipeline in
def Ctrler.add_stmt_to_ctrler_descript
(ctrler : Ctrler)
(stmt : Statement)
: Except String Ctrler := do
  let ctrler_type ← ctrler.type
  match ctrler_type with
  | .BasicCtrler => pure {
      name := ctrler.name
      controller_descript := ← ctrler.controller_descript.add_stmt_to_ctrler stmt
      entry_descript := none
      init_trans := none
      state_vars := none
      transition_list := none
      ctrler_init_trans := ctrler.ctrler_init_trans
      ctrler_state_vars := ctrler.ctrler_state_vars
      ctrler_trans_list := ctrler.ctrler_trans_list
    }
  | .Unordered
  | .FIFO => do
    let updated_entry ←
      match ctrler.entry_descript with
      | .some descript => do
        descript.add_stmt_to_entry stmt
      | .none => throw s!"Error: Queue ({ctrler_type}) has no entry_descript! Ctrler Name: ({ctrler.name})"

    pure {
      name := ctrler.name
      controller_descript := ctrler.controller_descript
      entry_descript := updated_entry
      init_trans := ctrler.init_trans
      state_vars := ctrler.state_vars
      transition_list := ctrler.transition_list
      ctrler_init_trans := none
      ctrler_state_vars := none
      ctrler_trans_list := none
    }

open Pipeline in
def Ctrler.assign_state_vars
(ctrler : Ctrler)
(state_vars : List TypedIdentifier)
: Except String Ctrler := do
  let ctrler_type ← ctrler.type
  match ctrler_type with
  | .BasicCtrler => pure {
      name := ctrler.name
      controller_descript := ctrler.controller_descript
      entry_descript := none
      init_trans := none
      state_vars := none
      transition_list := none
      ctrler_init_trans := ctrler.ctrler_init_trans
      ctrler_state_vars := state_vars
      ctrler_trans_list := ctrler.ctrler_trans_list
    }
  | .Unordered
  | .FIFO => pure {
    name := ctrler.name
    controller_descript := ctrler.controller_descript
    entry_descript := ctrler.entry_descript
    init_trans := ctrler.init_trans
    state_vars := state_vars
    transition_list := ctrler.transition_list
    ctrler_init_trans := none
    ctrler_state_vars := none
    ctrler_trans_list := none
  }


open Pipeline in
def Ctrler.add_var_decl_to_ctrler
(ctrler : Ctrler)
(t_ident : TypedIdentifier)
: Except String Ctrler := do
  let state_vars ← ctrler.get_state_vars

  let ctrler' ← ctrler.assign_state_vars (state_vars ++ [t_ident])
  ctrler'.add_stmt_to_ctrler_descript (variable_declaration t_ident)

open Pipeline in
def Ctrlers.add_var_decl_to_ctrler
(ctrlers : Ctrlers)
(ctrler_name : CtrlerName)
(var_decl : TypedIdentifier)
: Except String Ctrlers :=
  ctrlers.mapM (
    let ctrler : Ctrler := ·;
    if ctrler.name == ctrler_name then
      ctrler.add_var_decl_to_ctrler var_decl
    else
      pure ctrler)

