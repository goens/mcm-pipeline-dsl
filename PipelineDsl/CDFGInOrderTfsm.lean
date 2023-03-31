-- import PipelineDsl.AST
import PipelineDsl.CDFG
import PipelineDsl.CDFGAnalysis
import PipelineDsl.DSLtoCDFG

open CDFG

-- TODO: Check for other ctrlers the inst to stall on can be in
def CDFGInOrderTfsm (ctrlers : List controller_info) (inst_to_stall_on_type : InstType) (inst_to_stall_type : InstType)
: Except String (List controller_info) := do
  dbg_trace "<< Starting CDFGInOrderTfsm"
  let graph_nodes ← DSLtoCDFG ctrlers
  dbg_trace "<< Got graph nodes translated"
  let graph := {nodes := graph_nodes}
  let stall_point ← find_stall_point_heuristic graph inst_to_stall_type ctrlers
  dbg_trace s!"<< Found stall point from heuristic: ({stall_point})"
  let ctrler_state_to_stall_on : StateOrConstraintToStallOn ← find_ctrler_or_state_to_query_for_stall graph inst_to_stall_on_type ctrlers
  dbg_trace "<< Found ctrler/state to stall at"

  let stall_node ← CreateStallNode stall_point ctrler_state_to_stall_on ctrlers inst_to_stall_on_type inst_to_stall_type

  let graph_constrained_by_second_inst := ← graph.states_the_'to_stall_on'_node_can_be_in inst_to_stall_on_type inst_to_stall_type stall_point ctrlers
  dbg_trace s!"======= BEGIN graph constrained by second inst ========"
  dbg_trace s!"BEGIN graph_constrained_by_second_inst: ({graph_constrained_by_second_inst.nodes.map (·.current_state)})\nEND graph_constrained_by_second_inst"
  dbg_trace s!"======= END graph constrained by second inst ========"

  let graph_pruned_of_live_subsets := ← graph.prune_ctrler_nodes_that_are_live_for_a_subset_of_another_ctrler inst_to_stall_on_type
    |>.throw_exception_nesting_msg s!"Error pruning graph of live subsets. Graph: ({graph_constrained_by_second_inst.nodes.map (·.current_state)})"
  dbg_trace s!"======= BEGIN graph_pruned_of_live_subsets ========"
  dbg_trace s!"BEGIN graph_pruned_of_live_subsets: ({graph_pruned_of_live_subsets.nodes.map (·.current_state)})\nEND graph_pruned_of_live_subsets"
  dbg_trace s!"======= END graph_pruned_of_live_subsets ========"

  let new_state_name := stall_point.ctrler ++ "_stall_" ++ stall_point.state
  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_state_name stall_node stall_point.state

  pure updated_ctrlers