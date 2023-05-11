
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

abbrev Ctrlers := List controller_info
abbrev Ctrler := controller_info

-- == Translation Helpers ==

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
  let num_entries_asgn := variable_assignment [num_entries].to_qual_name <| num_lit_expr table_size
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
    (remove_actions ++ [complete await_insert_remove_state_name]).to_block
  let await_when_remove_stmt := await none [when_insert_stmt, when_remove_stmt]
  let await_insert_remove_state := state await_insert_remove_state_name await_when_remove_stmt.to_block

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
: Except String (Ctrler × CtrlerName × VarName) := do
  let lat_address_var := "_".intercalate ["lat", address]

  let lat_name := "load_address_table"
  let lat_size := 2 -- chosing a number for now..
  let entries := [(seq_num, seq_num), (address, lat_address_var)]
  let entry_key := seq_num
  let insert_args := [seq_num, "insert_address"]
  let insert_actions := [var_asn_var [seq_num] seq_num, var_asn_var [lat_address_var] "insert_address"]
  let insert_from := perform_load_node_ctrler_name

  let remove_args := [seq_num]
  let remove_actions :=
    [variable_assignment [seq_num].to_qual_name (← default_value_expr seq_num)]
  let remove_from := commit_node_ctrler_name

  let lat : Ctrler ← CreateTableQueue
    lat_name lat_size
    entries entry_key
    insert_args insert_actions insert_from
    remove_args remove_actions remove_from
  
  pure (lat, lat_name, lat_address_var)

def Pipeline.Description.inject_state_stmts
(state : Description)
(state_name : StateName)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(UpdateState : Description → InstType → List Statement → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Description := do
  match (← state.state_name) == state_name with
  | true => do UpdateState state inst_type stmts_to_inject InjectStmtsAt
  | false => do pure state

open Pipeline

def Ctrler.inject_states
(ctrler : Ctrler)
(states : List Description)
: Ctrler := {
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

def Ctrler.inject_ctrler_state
(ctrler : Ctrler)
(ctrler_name : CtrlerName)
(state_name : StateName)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(UpdateState : Description → InstType → List Statement → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Ctrler := do
  match ctrler.name == ctrler_name with
  | true => do
    let states := ← ctrler.states
    let updated_states := ← states.mapM (·.inject_state_stmts state_name inst_type stmts_to_inject UpdateState InjectStmtsAt)
    pure $ ctrler.inject_states updated_states
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
(UpdateState : Description → InstType → List Statement → InjectStmtsFunction → Except String Description)
(InjectStmtsAt : InjectStmtsFunction)
: Except String Ctrlers := do
  ctrlers.mapM (let ctrler : Ctrler := ·;
    ctrler.inject_ctrler_state ctrler_name state_name inst_type stmts_to_inject UpdateState InjectStmtsAt)

open Pipeline in
def Ctrlers.AddInsertToLATWhenPerform -- Load Address Table
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(perform_load_ctrler_name : CtrlerName)
(perform_load_state_name : StateName)
: Except String Ctrlers := do

  -- Insert a stmt to insert_key(seq_num, address) into the LAT  
  -- TODO: Update the function so it uses a provided argument for the seq_num / address
  -- So it finds the address from the used var expr from the send_load_request API
  let insert_key_stmt : Statement := stray_expr $ some_term $
    function_call [lat_name, insert_key].to_qual_name [var_expr seq_num, var_expr address]

  let ctrlers_insert_key_into_lat :=
    ctrlers.inject_ctrler_state
      perform_load_ctrler_name perform_load_state_name load [insert_key_stmt] Pipeline.Description.inject_stmts_at_stmt List.inject_stmts_at_perform
  
  ctrlers_insert_key_into_lat

open Pipeline in
def Ctrlers.AddRemoveFromLATWhenCommit
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(commit_ctrler_name : CtrlerName)
(commit_state_name : StateName)
: Except String Ctrlers :=
  -- Insert a stmt to remove_key(seq_num) from the LAT
  let remove_key_stmt : Statement := stray_expr $ some_term $
    function_call [lat_name, remove_key].to_qual_name [var_expr seq_num]
  let inst_is_type := VarCompare [instruction, op] equal [load.toString]
  let if_inst_is_type := conditional_stmt <| if_statement inst_is_type remove_key_stmt
  
  let ctrlers_remove_key_from_lat :=
    ctrlers.inject_ctrler_state
      commit_ctrler_name commit_state_name load [if_inst_is_type] Pipeline.Description.inject_stmts_at_stmt List.inject_stmts_at_commit
  
  ctrlers_remove_key_from_lat
