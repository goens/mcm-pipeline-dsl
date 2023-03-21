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

  let new_state_name := stall_point.ctrler ++ "_stall_" ++ stall_point.state
  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_state_name stall_node stall_point.state

  pure updated_ctrlers