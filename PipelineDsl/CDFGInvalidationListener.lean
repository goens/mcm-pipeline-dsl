import PipelineDsl.CDFG
import PipelineDsl.CDFGInOrderTfsm
import PipelineDsl.DSLtoCDFG

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
    [insert_from, insert].to_qual_name
    insert_args
    (insert_actions ++ [trans_to_remove]).to_block
  let await_when_insert_stmt := await none [when_insert_stmt]
  let await_insert_state := state await_insert_state_name await_when_insert_stmt.to_block

  -- (1) init state
  let init_stmts := ← entries.mapM ( λ (var_type, var_name) => do
    let default_val := ← default_value_expr var_type
      |>.throw_exception_nesting_msg s!"(Create Invalidation Listener): Could not find default value for ({var_type})";
    pure $ variable_assignment [var_name].to_qual_name <| default_val)
  let trans_to_first_state := transition await_insert_state_name
  let init_table_state := state init_state_name $ ( init_stmts ++ [trans_to_first_state] ).to_block


  -- ===== Create the controller =====
  let state_vars := entries.map (let (var_type, var_name) := ·; (var_type, var_name).to_typed_identifier)
  let ctrler : Ctrler := ⟨ table_name, ast_table_ctrler,
    some ast_entry, some init_state_name, some state_vars,
    some [init_table_state, await_insert_state, await_remove_state],
    none, none, none⟩ 
  
  pure ctrler

open Pipeline in
def CDFG.Graph.CreateInvalidationListener
(graph : Graph)
(commit_node global_perform_load_node : Node)
(ctrlers : Ctrlers)
: Except String Ctrler := do
  -- create a ctrler to listen for invalidation

  -- 1. create 2 states:
  -- (a) an await state that waits for the invalidation
  -- (b) a state that searches for speculative loads

  -- 2.
  -- (a) await when memory_interface.invalidation(), save the relevant data (none for the first iteration, maybe the address for the 2nd)
  -- transition to (b)
  -- (b) do a post-perform load reaching analysis, and make a query for all loads on those states
  -- with a match, send a squash msg to the commit controller
  -- Send a msg to the memory_interface at the end, ack the invalidation
  -- transition to (a)

  -- (a) await invalidation 
  let squash_speculative_loads_state_name := "squash_speculative_loads"

  let await_inval_stmts := [
    variable_assignment [seq_num].to_qual_name <| var_expr seq_num,
    variable_assignment [address].to_qual_name <| var_expr address,
    transition squash_speculative_loads_state_name
  ]
  let when_invalidation := Statement.when
    [memory_interface, invalidation].to_qual_name [seq_num, address] await_inval_stmts.to_block
  let await_invalidation := await none [ when_invalidation ]

  let await_state_name := "await_invalidation"
  -- The await invalidation state
  let await_invalidation_state := state await_state_name await_invalidation.to_block

  -- (b)
  -- (i) squash in-flight loads matching address
  -- (ii) send ack msg to memory_interface
  -- (iii) transiction back to (a)

  -- TODO: Still need to add the if address of the invalidation & load matches, then squash

  -- squash msg call
  let commit_ctrler_name := commit_node.ctrler_name
  let squash_search_all_msg := function_call [commit_ctrler_name, squash].to_qual_name [var_expr seq_num]
  let search_squash_stmt := stray_expr $ some_term squash_search_all_msg

  let post_send_ld_req := ← graph.reachable_nodes_from_node_up_to_option_node global_perform_load_node none load [] none
  let post_send_with_inst : List (List CtrlerStates × InstType) :=
    [( post_send_ld_req.to_ctrler_states, load )]
  -- Create queries for all loads in speculatively executed state

  -- (i)
  -- TODO: Make the function also check if the address matches? (somewhat hard, need to know where/how to get the address)
  /-
  Could try to do this by adding an if statement to the squash.
  Need to get the address of the load if it is in "speculatively executed state".
  -/
  let query_squash : List Statement := ← QueryAll post_send_with_inst [search_squash_stmt] ctrlers

  -- (ii)
  let invalidation_ack_msg : Statement := stray_expr $ some_term $
    function_call [memory_interface, invalidation_ack].to_qual_name []

  -- (iii)
  let transition_to_await_invalidation : Statement := complete await_state_name

  let squash_speculative_loads_stmts := query_squash ++ [invalidation_ack_msg, transition_to_await_invalidation]
  let squash_state := state squash_speculative_loads_state_name squash_speculative_loads_stmts.to_block

  -- 0. Create the ctrler
  let invalidation_listener_ctrler_name := "invalidation_listener"

  -- (i) Create init state
  let init_state_name := "init_inval_listener"
  let init_stmts := [
    variable_assignment [seq_num].to_qual_name <| some_term $ Term.const $ str_lit "0",
    variable_assignment [address].to_qual_name <| some_term $ Term.const $ str_lit "0",
    transition await_state_name
  ]
  let init_state := state init_state_name init_stmts.to_block

  -- (ii) create the ctrler
  let seq_num_tident : TypedIdentifier := ⟨ seq_num, seq_num ⟩ 
  let address_tident : TypedIdentifier := ⟨ address, address ⟩
  let seq_num_decl := variable_declaration seq_num_tident
  let address_decl := variable_declaration address_tident
  let init_state_decl := variable_assignment [ "init_state" ].to_qual_name <| var_expr init_state_name

  let inval_controller : Description := Description.controller invalidation_listener_ctrler_name [
    seq_num_decl, address_decl, init_state_decl ].to_block
  
  let inval_ctrler : Ctrler :=
    ⟨invalidation_listener_ctrler_name, inval_controller,
      none, none, none, none,
      some init_state_name, some [init_state, squash_state, await_invalidation_state], some [seq_num_tident, address_tident]⟩  

  pure inval_ctrler

open CDFG in
def Ctrlers.AddInvalidationListener
(ctrlers : Ctrlers)
: Except String Ctrlers := do
  let graph : Graph := { nodes := ← DSLtoCDFG ctrlers |>.throw_exception_nesting_msg s!"Error converting DSL to Graph" }

  let commit_node : Node := ← graph.commit_state_ctrler |>.throw_exception_nesting_msg s!"Error getting commit node"
  let global_perform_load_node : Node := ← graph.load_global_perform_state_ctrler |>.throw_exception_nesting_msg s!"Error getting global perform load node"

  let inval_listener_ctrler : Ctrler := ← graph.CreateInvalidationListener commit_node global_perform_load_node ctrlers

  let ctrlers' := inval_listener_ctrler :: ctrlers

  pure ctrlers'

