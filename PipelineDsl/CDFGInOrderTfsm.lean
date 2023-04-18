-- import PipelineDsl.AST
import PipelineDsl.CDFG
import PipelineDsl.CDFGAnalysis
import PipelineDsl.DSLtoCDFG

open CDFG

-- #eval ( String.intercalate "-" ["1","2"] )

-- start trying to create a parameterized version, if such a thing exists..
-- first thing to try for LoadReplay St -> Ld: make finding the stall point a parameter
-- not very paramterized at the moment...
def Ctrlers.InOrderTransformParameterized
(ctrlers : Ctrlers)
(inst_to_stall_on_type : InstType)
(inst_to_stall_type : InstType)
(stall_point_param? : Option CtrlerState)
: Except String (List controller_info) := do
  dbg_trace "<< Starting CDFGInOrderTfsm"
  let graph_nodes ← DSLtoCDFG ctrlers
  dbg_trace "<< Got graph nodes translated"
  let graph := {nodes := graph_nodes}
  let stall_point := ←
    match stall_point_param? with
    | some ctrler_state => do pure ctrler_state
    | none => do find_stall_point_heuristic graph inst_to_stall_type ctrlers
  dbg_trace s!"<< Found stall point from heuristic: ({stall_point})"

  -- === Old code to find stall state ===
  -- let ctrler_state_to_stall_on : StateOrConstraintToStallOn ← find_ctrler_or_state_to_query_for_stall graph inst_to_stall_on_type ctrlers
  -- dbg_trace "<< Found ctrler/state to stall at"

  -- let stall_node ← CreateStallNode stall_point ctrler_state_to_stall_on ctrlers inst_to_stall_on_type inst_to_stall_type
  -- === Old code to find stall state ===

  -- === New code to find stall state ===
  let nodes_to_query ← graph.find_pre_receive_stall_states inst_to_stall_on_type
  let query_ctrler_state : List CtrlerStates := nodes_to_query.to_ctrler_states

  -- let new_stall_state_name := stall_point.ctrler ++ inst_to_stall_on_type.toString ++ "_to_" ++ inst_to_stall_type.toString ++ "_stall_" ++ stall_point.state
  let new_stall_state_name := "_".intercalate [stall_point.ctrler, inst_to_stall_on_type.toString, "to", inst_to_stall_type.toString, "stall", stall_point.state]
  let is_stall_point_in_query := query_ctrler_state.is_ctrler_state_member stall_point.ctrler stall_point.state
  dbg_trace s!"<< Is stall point in query: ({is_stall_point_in_query})"
  let query_ctrler_states' : List CtrlerStates := ←
    match is_stall_point_in_query with
    | true => do query_ctrler_state.add_ctrler_state stall_point.ctrler new_stall_state_name |>.throw_exception_nesting_msg s!"Error adding new_stall_state to query_ctrler_states, assuming we're also stalling on ctrler ({stall_point.ctrler}) state ({stall_point.state})"
    | false => do pure query_ctrler_state
  
-- def Ctrlers.StallNode (stall_state : StateName) (stall_ctrler : CtrlerName) (ctrler_states_to_query : List CtrlerStates)
-- (ctrlers : Ctrlers) (inst_type_to_stall_on : InstType) (inst_to_stall_type : InstType)
  let stall_node := ← ctrlers.StallNode stall_point.state stall_point.ctrler query_ctrler_states' inst_to_stall_on_type inst_to_stall_type new_stall_state_name
    |>.throw_exception_nesting_msg s!"Error in InOrderTransformation while generating the stall state"
  -- === New code to find stall state ===

  -- let graph_constrained_by_second_inst := ← graph.states_the_'to_stall_on'_node_can_be_in inst_to_stall_on_type inst_to_stall_type stall_point ctrlers
  -- dbg_trace s!"======= BEGIN graph constrained by second inst ========"
  -- dbg_trace s!"BEGIN graph_constrained_by_second_inst: ({graph_constrained_by_second_inst.nodes.map (·.current_state)})\nEND graph_constrained_by_second_inst"
  -- dbg_trace s!"======= END graph constrained by second inst ========"

  -- let graph_pruned_of_live_subsets := ← graph.prune_ctrler_nodes_that_are_live_for_a_subset_of_another_ctrler inst_to_stall_on_type
  --   |>.throw_exception_nesting_msg s!"Error pruning graph of live subsets. Graph: ({graph_constrained_by_second_inst.nodes.map (·.current_state)})"
  -- dbg_trace s!"======= BEGIN graph_pruned_of_live_subsets ========"
  -- dbg_trace s!"BEGIN graph_pruned_of_live_subsets: ({graph_pruned_of_live_subsets.nodes.map (·.current_state)})\nEND graph_pruned_of_live_subsets"
  -- dbg_trace s!"======= END graph_pruned_of_live_subsets ========"

  -- let new_stall_state_name := stall_point.ctrler ++ "_stall_" ++ stall_point.state
  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_stall_state_name stall_node stall_point.state

  pure updated_ctrlers

-- TODO: Check for other ctrlers the inst to stall on can be in
def CDFG.InOrderTfsm (ctrlers : Ctrlers) (inst_to_stall_on_type : InstType) (inst_to_stall_type : InstType)
: Except String Ctrlers := do

  -- def Ctrlers.InOrderTransformParameterized
  -- (ctrlers : Ctrlers)
  -- (inst_to_stall_on_type : InstType)
  -- (inst_to_stall_type : InstType)
  -- (stall_point_param? : Option CtrlerState)
  -- : Except String (List controller_info) := do

  ctrlers.InOrderTransformParameterized inst_to_stall_on_type inst_to_stall_type none

-- #check Lean.Expr

