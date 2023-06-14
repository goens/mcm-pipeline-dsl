import PipelineDsl.CDFG
import PipelineDsl.CDFGInOrderTfsm
import PipelineDsl.DSLtoCDFG

open Pipeline in
def CDFG.Graph.CreateInvalidationListener
(graph : Graph)
(commit_node global_perform_load_node : Node)
-- (ctrlers : Ctrlers)
(table_name : CtrlerName) -- "seq_num_to_address_table"
(table_seq_num_name : VarName) -- "table_address"
(table_address_name : VarName) -- "table_address"
(inval_listener_name : CtrlerName) -- "invalidation_listener"
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

  let invalidation_seq_num := "invalidation_seq_num"
  let invalidation_address := "invalidation_address"
  -- (a) await invalidation 
  let squash_speculative_loads_state_name := "squash_speculative_loads"

  let await_inval_stmts := [
    variable_assignment [seq_num].to_qual_name <| var_expr invalidation_seq_num,
    variable_assignment [address].to_qual_name <| var_expr invalidation_address,
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
  -- something like: ROB.squash(LAT_seq_num)
  -- let commit_ctrler_name := commit_node.ctrler_name
  -- let squash_search_all_msg := function_call [commit_ctrler_name, squash].to_qual_name [var_expr table_seq_num_name]
  -- let search_squash_stmt := stray_expr $ some_term squash_search_all_msg

  let (post_send_ld_req', _) := ← graph.reachable_nodes_from_node_up_to_option_node global_perform_load_node none load [] none
  let post_send_ld_req := post_send_ld_req'.filter (· != global_perform_load_node)
  let post_send_with_inst : List (List CtrlerStates × InstType) :=
    [( post_send_ld_req.to_ctrler_states, load )]
  -- Create queries for all loads in speculatively executed state

  -- TODO NOTE: Need to update the LAT generator, to only have 1 state, and to implement the API insert_key()
  -- NOTE: check the generated Load-Address-Table (LAT) to see if the address matches the invalidation

  -- something like:
  -- 1: if (LAT.address == inval.address) & (entry.valid == true) { squash() }
  -- let expr_address_eq_table_address := VarCompare [invalidation_address] equal [table_address_name]
  -- let if_address_matches := conditional_stmt <| if_statement expr_address_eq_table_address search_squash_stmt

  -- Remove the entry upon match as well, so that it doesn't get squashed again
  -- let remove_entry_at_key := stray_expr $ some_term $
  --   function_call [table_name, remove_key].to_qual_name [var_expr table_seq_num_name]

  -- table_address_name is the LAT's address, and check if invalidation listener's address (from the inval msg) matches
  -- let search_for_addr := table_name.TableUnorderedSearchAll
  --   table_address_name invalidation_address
  --   [search_squash_stmt, remove_entry_at_key] []

  let squash_search_all_msg := function_call [table_name, squash].to_qual_name [var_expr table_seq_num_name]
  let squash_table_stmt := stray_expr $ some_term squash_search_all_msg

  let search_for_addr := table_name.TableUnorderedSearch
    table_address_name invalidation_address
    [squash_table_stmt /-search_squash_stmt, remove_entry_at_key-/] []
    $ term_expr [table_seq_num_name].EntryQualVar


  -- (i)
  -- TODO: Make the function also check if the address matches? (somewhat hard, need to know where/how to get the address)
  /-
  Could try to do this by adding an if statement to the squash.
  Need to get the address of the load if it is in "speculatively executed state".
  -/
  -- let query_squash : List Statement := ← QueryAll post_send_with_inst [search_for_addr] ctrlers [invalidation_seq_num]

  -- (ii)
  let invalidation_ack_msg : Statement := stray_expr $ some_term $
    function_call [memory_interface, invalidation_ack].to_qual_name []

  -- (iii)
  let transition_to_await_invalidation : Statement := complete await_state_name

  let squash_speculative_loads_stmts := /- query_squash -/ [search_for_addr, invalidation_ack_msg, transition_to_await_invalidation]
  let squash_state := state squash_speculative_loads_state_name squash_speculative_loads_stmts.to_block

  -- 0. Create the ctrler
  -- let inval_listener_name := "invalidation_listener"

  -- (i) Create init state
  let init_state_name := "init_inval_listener"
  let init_stmts := [
    variable_assignment [invalidation_seq_num].to_qual_name <| some_term $ Term.const $ str_lit "0",
    variable_assignment [invalidation_address].to_qual_name <| some_term $ Term.const $ str_lit "0",
    transition await_state_name
  ]
  let init_state := state init_state_name init_stmts.to_block

  -- (ii) create the ctrler
  let seq_num_tident : TypedIdentifier := ⟨ seq_num, invalidation_seq_num ⟩ 
  let address_tident : TypedIdentifier := ⟨ address, invalidation_address ⟩
  let seq_num_decl := variable_declaration seq_num_tident
  let address_decl := variable_declaration address_tident
  let init_state_decl := variable_assignment [ "init_state" ].to_qual_name <| var_expr init_state_name

  let inval_controller : Description := Description.controller inval_listener_name [
    seq_num_decl, address_decl, init_state_decl ].to_block
  
  let inval_ctrler : Ctrler :=
    ⟨inval_listener_name, inval_controller,
      none, none, none, none,
      some init_state_name, some [init_state, squash_state, await_invalidation_state], some [seq_num_tident, address_tident]⟩  

  pure inval_ctrler

open CDFG in
def Ctrlers.AddInvalidationListener
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(lat_seq_num_name : VarName)
(lat_address_name : VarName)
(graph : Graph)
(commit_node : Node)
(global_perform_load_node : Node)

(inval_listener_name : CtrlerName)
: Except String Ctrlers := do
  let inval_listener_ctrler : Ctrler :=
    ← graph.CreateInvalidationListener
      commit_node global_perform_load_node
      -- ctrlers
      lat_name lat_seq_num_name lat_address_name
      -- name of the invalidation listener we're creating
      inval_listener_name

  let ctrlers' := inval_listener_ctrler :: ctrlers

  pure ctrlers'

open CDFG in
def Ctrlers.AddInvalidationBasedLoadOrdering
(ctrlers : Ctrlers)
: Except String Ctrlers := do
  -- === Setup: Get graph info, perform_load & commit Nodes ===
  let graph : Graph := { nodes := ← DSLtoCDFG ctrlers
    |>.throw_exception_nesting_msg s!"Error converting DSL to Graph" }
  let perform_load_node := ← graph.load_global_perform_state_ctrler
    |>.throw_exception_nesting_msg s!"Error getting perform load node"
  let commit_node := ← graph.commit_state_ctrler
    |>.throw_exception_nesting_msg s!"Error getting perform load node"

  -- A few things to do:
  -- 1. Create a LAT to store the addresses of the loads
  -- 2. Create the invalidation listener (this will read from the LAT)
  -- 3. Add a stmt to insert_key into the invalidation listener to perform load
  -- 4. Add a stmt to remove_key from the invalidation listener to commit load

  let inval_listener_name := "invalidation_listener"
  -- let lat_squashing_ctrlers := [
  --   inval_listener_name, -- the inval listener squashes the LAT where necessary
  --   commit_node.ctrler_name -- Commit ctrler squashes LAT in case of
  --     -- {branch mispeculation, st -> ld fwding, etc.}
  -- ]

  -- (1) Create the LAT
  let (lat, lat_name, lat_seq_num_var, lat_address_var) ← CreateLoadAddressTableCtrler
    perform_load_node.ctrler_name commit_node.ctrler_name inval_listener_name commit_node.ctrler_name

  let ctrlers' := ctrlers ++ [lat]

  -- (2) Create the invalidation listener
  let ctrlers''   ← AddInvalidationListener
    ctrlers' lat_name lat_seq_num_var lat_address_var graph commit_node perform_load_node
    inval_listener_name

  -- (3) Add a stmt to insert_key into the invalidation listener to perform load
  -- Get the global_perform_load API's args: (the seq_num expr, and address expr) (should be in fixed positions..)
  -- pass to the AddInsertToLATWhenPerform function
  let (load_req_address, load_req_seq_num) ← perform_load_node.load_req_address_seq_num

  let addr_var_name ← load_req_address.var's_identifier
  let perform_load_ctrler ← ctrlers.ctrler_from_name perform_load_node.ctrler_name
  let is_addr_a_state_var ← perform_load_ctrler.is_a_state_var_of_ctrler addr_var_name

  let (is_found_addr_var, ctrlers''')  ← LoadAddress.CDFG.Graph.update_ctrlers_at_node_where_load_addr_obtained_search -- AddInsertToLATWhenPerform
    graph
    perform_load_node
    addr_var_name is_addr_a_state_var
    ctrlers''
    []
    lat_name load_req_address load_req_seq_num

  match is_found_addr_var with
  | true =>

    -- let ctrlers'''  ← AddInsertToLATWhenPerform
    --   ctrlers'' lat_name perform_load_node.ctrler_name perform_load_node.current_state load_req_address load_req_seq_num

    -- (4) Add a stmt to remove_key from the invalidation listener to commit load
    let ctrlers'''' ← AddRemoveFromLATWhenCommit
      ctrlers''' lat_name commit_node.ctrler_name commit_node.current_state List.inject_stmts_at_commit [seq_num]

    pure ctrlers''''
  | false =>
    throw s!"Couldn't find the point where the address is created/assigned?"
