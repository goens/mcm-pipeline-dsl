-- import PipelineDsl.AST
import PipelineDsl.CDFG
import PipelineDsl.CDFGAnalysis
import PipelineDsl.DSLtoCDFG

import PipelineDsl.InstructionHelpers

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
  let graph : Graph := {nodes := graph_nodes}
  let stall_point := ←
    match stall_point_param? with
    | some ctrler_state => do pure ctrler_state
    | none => do graph.find_stall_point_heuristic inst_to_stall_type ctrlers
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
  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_stall_state_name stall_node stall_point.state (some inst_to_stall_type)
    |>.throw_exception_nesting_msg s!"Error updating ctrler ({stall_point.ctrler}) with stall node ({stall_node})"

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

-- ===== NEW In Order Transform that considers Ternary MCM Relations and Binary ======
def CDFG.Graph.pre_receive_state
(graph : Graph) (inst_to_stall_on_type : InstType)
: Except String (List CtrlerStates) := do
  let nodes_to_query ← graph.find_pre_receive_stall_states inst_to_stall_on_type
    |>.throw_exception_nesting_msg s!"Error finding pre-receive states for inst_to_stall_on_type ({inst_to_stall_on_type}) in Graph ({graph.node_names})"
  -- let query_ctrler_state : List CtrlerStates := nodes_to_query.to_ctrler_states
  pure nodes_to_query.to_ctrler_states

def CtrlerState.append_if_in_list (ctrler_state : CtrlerState) (ctrler_states : List CtrlerStates) (append_state : StateName)
: Except String (List CtrlerStates) := do
  let is_member := ctrler_states.is_ctrler_state_member ctrler_state.ctrler ctrler_state.state
  match is_member with
  | true => do ctrler_states.add_ctrler_state ctrler_state.ctrler append_state
    |>.throw_exception_nesting_msg s!"Error adding ctrler/state ({ctrler_state.ctrler}) state ({ctrler_state.state}) to list ({ctrler_states})"
  | false => do pure ctrler_states

-- def CtrlerState.add_state
def CtrlerState.new_stall_state_name (ctrler_state : CtrlerState) (inst_to_stall_on_type : InstType) (inst_to_stall_type : InstType) (suffix? : Option String)
: StateName :=
  match suffix? with
  | some suffix =>
    "_".intercalate [ctrler_state.ctrler, inst_to_stall_on_type.toString, "to", inst_to_stall_type.toString, "stall", ctrler_state.state, suffix]
  | none =>
    "_".intercalate [ctrler_state.ctrler, inst_to_stall_on_type.toString, "to", inst_to_stall_type.toString, "stall", ctrler_state.state]

def get_stall_point (stall_point? : Option CtrlerState) (graph : Graph) (inst_to_stall_type : InstType) (ctrlers : Ctrlers)
: Except String CtrlerState := do
  match stall_point? with
  | some ctrler_state => do pure ctrler_state
  | none => do graph.find_stall_point_heuristic inst_to_stall_type ctrlers

def CDFG.Graph.BinaryInOrderTransform
(graph : Graph)
(ctrlers : Ctrlers)
(inst_to_stall_on_type : InstType)
(inst_to_stall_type : InstType)
(stall_point_param? : Option CtrlerState)
: Except String (List controller_info) := do
  let stall_point := ← get_stall_point stall_point_param? graph inst_to_stall_type ctrlers
    |>.throw_exception_nesting_msg s!"Error finding stall point in BinaryInOrderTransform"

  dbg_trace s!"<< Found stall point from heuristic: ({stall_point})"

  let query_ctrler_state : List CtrlerStates := ← graph.pre_receive_state inst_to_stall_on_type
    |>.throw_exception_nesting_msg s!"Error finding states to query in BinaryInOrderTransform"

  let new_stall_state_name := stall_point.new_stall_state_name inst_to_stall_on_type inst_to_stall_type none
  let query_ctrler_states' : List CtrlerStates := ← stall_point.append_if_in_list query_ctrler_state new_stall_state_name
    |>.throw_exception_nesting_msg s!"Error appending ctrler/state ({stall_point.ctrler}) state ({stall_point.state}) to list ({query_ctrler_state})"
  
  let stall_node := ← ctrlers.StallNode stall_point.state stall_point.ctrler query_ctrler_states' inst_to_stall_on_type inst_to_stall_type new_stall_state_name
    |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while generating the stall state"

  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_stall_state_name stall_node stall_point.state (some inst_to_stall_on_type)
    |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while updating the controllers with the stall state"

  pure updated_ctrlers

def append_created_states_if_needed
(created_original : List (CtrlerState × StateName))
(ctrler_states : List CtrlerStates)
: Except String (List CtrlerStates) := do
  created_original.foldlM ( λ ctrler_states' (stall_point, new_stall_state_name) => do
    stall_point.append_if_in_list ctrler_states' new_stall_state_name
      |>.throw_exception_nesting_msg s!"Error appending ctrler/state ({stall_point.ctrler}) state ({stall_point.state}) to list ({ctrler_states})"
  ) ctrler_states

def CDFG.Graph.TernaryInOrderTransform
(graph : Graph)
(ctrlers : Ctrlers)
(inst_to_stall_on_type : InstType)
(memory_ordering_type : InstType)
(inst_to_stall_type : InstType)
-- (stall_point_param? : Option CtrlerState) -- Could try to add a fence to load-replay, so the replay is stalled on the memory-ordering-fence
: Except String (List controller_info) := do
  -- Simple implementation, break down this MCM-Ordering: stall_on -> ordering -> to_stall,
  -- into: stall_on -> ordering, ordering -> to_stall
  -- stall_on -> ordering
  let ordering_stall_point := ← get_stall_point none graph memory_ordering_type ctrlers
    |>.throw_exception_nesting_msg s!"Error finding ordering inst ({memory_ordering_type}) stall point in BinaryInOrderTransform"
  -- ordering -> to_stall
  let access_stall_point := ← get_stall_point none graph inst_to_stall_type ctrlers
    |>.throw_exception_nesting_msg s!"Error finding ordering inst ({inst_to_stall_type}) stall point in BinaryInOrderTransform"

  dbg_trace s!"<< Found ordering stall point from heuristic: ({ordering_stall_point})"
  dbg_trace s!"<< Found access stall point from heuristic: ({access_stall_point})"

  -- stall_on -> ordering
  let query_stall_on_ctrler_state : List CtrlerStates := ← graph.pre_receive_state inst_to_stall_on_type
    |>.throw_exception_nesting_msg s!"Error finding stall_on_inst states to query in BinaryInOrderTransform"
  -- ordering -> to_stall
  let query_ordering_ctrler_state : List CtrlerStates := ← graph.pre_receive_state memory_ordering_type
    |>.throw_exception_nesting_msg s!"Error finding ordering_inst states to query in BinaryInOrderTransform"

  let ternary_suffix := "_".intercalate <| [inst_to_stall_on_type, memory_ordering_type, inst_to_stall_type].map (·.toString)
  let ordering_stall_state_name := ordering_stall_point.new_stall_state_name inst_to_stall_on_type memory_ordering_type (some ternary_suffix)
  let access_stall_state_name := ordering_stall_point.new_stall_state_name memory_ordering_type inst_to_stall_type (some ternary_suffix)

  let stall_points : List (CtrlerState × StateName) := [( ordering_stall_point, ordering_stall_state_name ), ( access_stall_point, access_stall_state_name )]

  let query_stall_on_ctrler_states' : List CtrlerStates := ← append_created_states_if_needed stall_points query_stall_on_ctrler_state
  let query_ordering_ctrler_states' : List CtrlerStates := ← append_created_states_if_needed stall_points query_ordering_ctrler_state
  
  let ordering_stall_node := ← ctrlers.StallNode ordering_stall_point.state ordering_stall_point.ctrler query_stall_on_ctrler_states' inst_to_stall_on_type memory_ordering_type ordering_stall_state_name
    |>.throw_exception_nesting_msg s!"Error in InOrderTransformation while generating the ordering_inst stall state"
  let access_stall_node := ← ctrlers.StallNode access_stall_point.state access_stall_point.ctrler query_ordering_ctrler_states' memory_ordering_type inst_to_stall_type access_stall_state_name
    |>.throw_exception_nesting_msg s!"Error in InOrderTransformation while generating the ordering_inst stall state"

  let updated_ctrlers'  ← UpdateCtrlerWithNode ctrlers          ordering_stall_point.ctrler ordering_stall_state_name ordering_stall_node ordering_stall_point.state (some memory_ordering_type)
  let updated_ctrlers'' ← UpdateCtrlerWithNode updated_ctrlers' access_stall_point.ctrler   access_stall_state_name   access_stall_node   access_stall_point.state   (some inst_to_stall_type)

  pure updated_ctrlers''

def CDFG.InOrderTransform (ctrlers : Ctrlers) (mcm_ordering : MCMOrdering)
: Except String Ctrlers := do
  -- induct on mcm-ordering type
  -- Handle cases:
  -- 1. Access₁ → Access₂ 
  -- 2. Access₁ → Ordering₂ → Access₃
  -- NOTE: ignore "specific" types of address ordering for now.

  dbg_trace "<< Starting In Order Transformation"
  let graph : Graph := ← ctrlers.to_simple_instruction_graph

  match mcm_ordering with
  | .binary_ordering ⟨/- BinaryOrdering -/ access₁, access₂, /- address -/ _ ⟩ => do
    dbg_trace s!"<< In-Order-Transform binary ordering: ({mcm_ordering})"
    graph.BinaryInOrderTransform ctrlers ( InstType.memory_access access₁ ) ( InstType.memory_access access₂ ) none
  | .ternary_ordering ⟨ /- TernaryOrdering -/ access₁, ordering₂, access₃, /- address -/ _⟩ => do
    dbg_trace s!"<< In-Order-Transform ternary ordering: ({mcm_ordering})"
    graph.TernaryInOrderTransform ctrlers ( InstType.memory_access access₁ ) ( InstType.memory_ordering ordering₂ ) ( InstType.memory_access access₃ ) -- none

