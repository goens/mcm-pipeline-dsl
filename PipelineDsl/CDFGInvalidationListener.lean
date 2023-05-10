import PipelineDsl.CDFG
import PipelineDsl.CDFGInOrderTfsm
import PipelineDsl.DSLtoCDFG

open Pipeline in
def CDFG.Graph.CreateInvalidationListener
(graph : Graph)
(commit_node global_perform_load_node : Node)
(ctrlers : Ctrlers)
(table_name : CtrlerName) -- "seq_num_to_address_table"
(table_address_name : VarName) -- "table_address"
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

  -- TODO NOTE: Need to update the LAT generator, to only have 1 state, and to implement the API insert_key()
  -- NOTE: check the generated Load-Address-Table (LAT) to see if the address matches the invalidation

  let expr_address_eq_table_address := VarCompare [address] equal [table_address_name]
  let if_address_matches := conditional_stmt <| if_statement expr_address_eq_table_address search_squash_stmt
  let search_for_addr := table_name.TableUnorderedSearch seq_num "instruction.seq_num" [if_address_matches] []


  -- (i)
  -- TODO: Make the function also check if the address matches? (somewhat hard, need to know where/how to get the address)
  /-
  Could try to do this by adding an if statement to the squash.
  Need to get the address of the load if it is in "speculatively executed state".
  -/
  let query_squash : List Statement := ← QueryAll post_send_with_inst [search_for_addr] ctrlers

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
(lat_name : CtrlerName)
(lat_address_name : CtrlerName)
: Except String Ctrlers := do
  let graph : Graph := { nodes := ← DSLtoCDFG ctrlers |>.throw_exception_nesting_msg s!"Error converting DSL to Graph" }

  let commit_node : Node := ← graph.commit_state_ctrler |>.throw_exception_nesting_msg s!"Error getting commit node"
  let global_perform_load_node : Node := ← graph.load_global_perform_state_ctrler |>.throw_exception_nesting_msg s!"Error getting global perform load node"

  let inval_listener_ctrler : Ctrler := ← graph.CreateInvalidationListener commit_node global_perform_load_node ctrlers lat_name lat_address_name

  let ctrlers' := inval_listener_ctrler :: ctrlers

  pure ctrlers'

def Ctrlers.AddInvalidationBasedLoadOrdering
(ctrlers : Ctrlers)
: Except String Ctrlers := do
  -- A few things to do (in no particular order):
  -- 1. Create a LAT to store the addresses of the loads
  -- 2. Create the invalidation listener (this will read from the LAT)
  -- 3. Add a stmt to insert_key into the invalidation listener to perform load
  -- 4. Add a stmt to remove_key from the invalidation listener to commit load

  -- (1) Create the LAT
  let lat_name := "load_address_table"
  let lat_size := 2 -- chosing a number for now..
  let entry_key := seq_num
  let insert_args := [seq_num, address]
  let insert_actions := [var_asn_var [seq_num] seq_num, var_asn_var [address] address]

  default

-- NOTE: remember to get perform_load_node
-- def Ctrlers.AddInsertToLATWhenPerform -- Load Address Table
-- def Ctrlers.AddRemoveFromLATWhenCommit
