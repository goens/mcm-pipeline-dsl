import PipelineDsl.CDFG
import PipelineDsl.CDFGInOrderTfsm


def address := "address"
def invalidation := "invalidation"

def CDFG.Graph.CreateInvalidationListener
(graph : Graph)
(commit_node global_perform_load_node : Node)
(ctrlers : Ctrlers)
: Except String Ctrlers := do
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
  let when_invalidation := Pipeline.Statement.when
    [memory_interface, invalidation].to_qual_name [seq_num, address] await_inval_stmts.to_block
  let await_invalidation := await none [ when_invalidation ]

  let await_state_name := "await_invalidation"
  -- The await invalidation state
  let await_invalidation_state := state await_state_name await_invalidation.to_block

  -- (b) squash in-flight loads matching address
  -- send ack msg to memory_interface
  -- transiction back to (a)

  -- TODO: Still need to add the if address of the invalidation & load matches, then squash

  -- squash msg call
  let commit_ctrler_name := commit_node.ctrler_name
  let squash_search_all_msg := function_call [commit_ctrler_name, squash].to_qual_name [var_expr seq_num]
  let search_squash_stmt := stray_expr $ some_term squash_search_all_msg

  let post_send_ld_req := ← graph.reachable_nodes_from_node_up_to_option_node global_perform_load_node none load [] none
  let post_send_with_inst : List (List CtrlerStates × InstType) :=
    [( post_send_ld_req.to_ctrler_states, load )]
  -- Create queries for all loads in speculatively executed state

  -- TODO: Make the function also check if the address matches?
  let query_squash := ← QueryAll post_send_with_inst [search_squash_stmt] ctrlers



  let invalidation_listener_ctrler_name := "invalidation_listener"


  default

