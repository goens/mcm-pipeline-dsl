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
def CtrlerState.new_stall_state_name (ctrler_state : CtrlerState) (inst_to_stall_on_types : List InstType) (inst_to_stall_type : InstType) (suffix? : Option String)
: StateName :=
  let stall_on_type_names := "_".intercalate <| inst_to_stall_on_types.map (·.toString)
  let tail := match suffix? with
    | some suffix => "_".append suffix
    | none => ""
  "_".intercalate [ctrler_state.ctrler, stall_on_type_names, "to", inst_to_stall_type.toString, "stall", ctrler_state.state] |>.append tail

def get_stall_point (stall_point? : Option (CtrlerState × InstType) ) (graph : Graph) (inst_to_stall_types : List InstType) (ctrlers : Ctrlers)
: Except String ( List ( CtrlerState × ( InstType) ) ) := do
  match stall_point? with
  | some (ctrler_state, inst_type') => do
    pure [( ctrler_state, inst_type' )]
  | none => do inst_to_stall_types.mapM (do
    let inst_to_stall_type := ·;
    let stall_point_heuristic :=  ← graph.find_stall_point_heuristic inst_to_stall_type ctrlers;
    pure (stall_point_heuristic, inst_to_stall_type ) )

def Prod.new_stall_state_name
(prod : Prod CtrlerState (Option InstType)) (inst_to_stall_on_types : List InstType) (inst_to_stall_type : InstType) (suffix? : Option String)
: (StateName × (Option InstType)) :=
  let stalled_inst_type := match prod.snd with
    | some inst_type => inst_type
    | none => inst_to_stall_type
  let new_stall_state_name := prod.fst.new_stall_state_name inst_to_stall_on_types stalled_inst_type suffix?

  (new_stall_state_name, prod.snd)

def CtrlerStates.ctrler_states_without_first_state
(stall_point : CtrlerStates)
(ctrlers : Ctrlers)
: Except String CtrlerStates := do
  let (CtrlerStates.mk ctrler_name state_names) := stall_point
  let ctrler ← ctrlers.ctrler_from_name ctrler_name
  let ctrler's_first_state ← ctrler.init_trans_dest
  dbg_trace s!"** Ctrler Name: ({ctrler_name}). State_names: ({state_names}). Ctrler's_first_state: ({ctrler's_first_state})"
  let states_without_first_state := state_names.filter (· != ctrler's_first_state )
  dbg_trace s!"** Ctrler Name: ({ctrler_name}). State_names_without_first_state: ({state_names}). Ctrler's_first_state: ({ctrler's_first_state})"
  pure (CtrlerStates.mk ctrler_name states_without_first_state)

def CtrlerStates.is_state_list_not_empty
(ctrler_states : CtrlerStates)
: Bool :=
  let (CtrlerStates.mk /- ctrler_name -/ _ state_names) := ctrler_states
  !state_names.isEmpty

def Prod.remove_ctrler_state_first_state
(stall_points_type : Prod (List CtrlerStates) InstType)
(ctrlers : Ctrlers)
: Except String (Prod (List CtrlerStates) InstType) := do
  let (ctrler_state_list, inst_type) := stall_points_type
  let ctrler_state_list_without_first_state ←
    ctrler_state_list.mapM (·.ctrler_states_without_first_state ctrlers)
  let non_empty_ctrler_states := ctrler_state_list_without_first_state.filter (·.is_state_list_not_empty)
  pure (non_empty_ctrler_states, inst_type)

def CDFG.Graph.BinaryInOrderTransform
(graph : Graph)
(ctrlers : Ctrlers)
(inst_to_stall_on_types : List InstType)
(inst_to_stall_types : List InstType)
(stall_point_param? : Option ( CtrlerState × InstType))
: Except String (List controller_info) := do
  let stall_points : List (CtrlerState × InstType) := ← get_stall_point stall_point_param? graph inst_to_stall_types ctrlers
    |>.throw_exception_nesting_msg s!"Error finding stall point in BinaryInOrderTransform"

  dbg_trace s!"<< Found stall point from heuristic: ({stall_points})"

  let query_ctrler_state : List ( (List CtrlerStates) × InstType) := ← inst_to_stall_on_types.mapM (do
    let inst_type' := ·;
    let ctrler_states := ← graph.pre_receive_state inst_type'
      |>.throw_exception_nesting_msg s!"Error finding states to query in BinaryInOrderTransform"
    pure (ctrler_states, inst_type')
    )

  dbg_trace s!"<< States to query: ({query_ctrler_state})"

  let new_stall_state_names : List (CtrlerState × StateName × InstType) := stall_points.map (
    let (stall_point, inst_type) := ·;
    let new_state_name := stall_point.state
      -- stall_point.new_stall_state_name inst_to_stall_on_types inst_type none
      --   ( stall_point, new_state_name, inst_type)
    (stall_point, new_state_name, inst_type)
  )

  dbg_trace s!"<< New stall state names: ({new_stall_state_names})"

  -- let query_ctrler_states' : List CtrlerStates := ← stall_points.map (·.append_if_in_list query_ctrler_state new_stall_state_name)
  --   |>.throw_exception_nesting_msg s!"Error appending ctrler/state ({stall_point.ctrler}) state ({stall_point.state}) to list ({query_ctrler_state})"
  let query_ctrler_states' : List ( (List CtrlerStates) × InstType) := ←
    query_ctrler_state.mapM (do
      let (query_ctrler_state, inst_type) := ·;
      let updated_states := ← new_stall_state_names.foldlM (
        λ query_states new_names => do
          let (stall_point, new_name, /- stalled_inst_type -/ _) := new_names;

          let updated_query_states := ← stall_point.append_if_in_list query_states new_name
            |>.throw_exception_nesting_msg s!"Error appending ctrler/state ({stall_point.ctrler}) state ({stall_point.state}) to list ({query_ctrler_state})"

          pure (updated_query_states)
      ) query_ctrler_state

      pure (updated_states, inst_type)
    )

  dbg_trace s!"<< States to query (with new stall states?): ({query_ctrler_state})"

  let dsl_stall_nodes : List ( Pipeline.Description × StateName × CtrlerState × InstType) := ← new_stall_state_names.mapM (do
    let (stall_point, new_stall_state_name, inst_to_stall_type') := ·;
    let dsl_stall := ← ctrlers.StallNode
      stall_point.state stall_point.ctrler
      query_ctrler_states'
      /-inst_to_stall_on_types-/ inst_to_stall_type'
      -- new_stall_state_name
      stall_point.state -- just reuse the state, don't generate a stall state, and keep this simple.
      |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while generating the stall state"
    pure (dsl_stall, new_stall_state_name , stall_point, inst_to_stall_type')
  )

  dbg_trace s!"<< Generated DSL stall nodes: ({dsl_stall_nodes})"

  let updated_ctrlers ←
    dsl_stall_nodes.foldlM (
      λ ctrlers' dsl_stall_node_state => do
        let (stall_node, /- new_stall_state_name -/ _, stall_point, /- inst_to_stall_type' -/ _) := dsl_stall_node_state;

        ctrlers'.update_ctrler_state stall_point.ctrler stall_node
        -- UpdateCtrlerWithNode ctrlers' stall_point.ctrler new_stall_state_name stall_node stall_point.state (some inst_to_stall_type')
        --   |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while updating the controllers with the stall state"
    ) ctrlers
  
  dbg_trace s!"<< Finished In Order Transformation: ({inst_to_stall_on_types}) -> ({inst_to_stall_types}) or ({stall_point_param?})"

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
(inst_to_stall_on_types : List InstType)
(memory_ordering_type : InstType)
(inst_to_stall_types : List InstType)
(provided_stall_point? : Option (CtrlerState × InstType))
-- (stall_point_param? : Option CtrlerState) -- Could try to add a fence to load-replay, so the replay is stalled on the memory-ordering-fence
: Except String (List controller_info) := do
  -- Simple implementation, break down this MCM-Ordering: stall_on -> ordering -> to_stall,
  -- into: stall_on -> ordering, ordering -> to_stall
  -- stall_on -> ordering
  let ordering_stall_point : (CtrlerState × (InstType)) := (
    ( ← graph.find_stall_point_heuristic memory_ordering_type ctrlers
      |>.throw_exception_nesting_msg s!"Error finding ordering inst ({memory_ordering_type}) stall point in TernaryInOrderTransform")
    ,
    memory_ordering_type
  )
  -- ordering -> to_stall
  let access_stall_point : List (CtrlerState × (InstType)) := ← get_stall_point provided_stall_point? graph inst_to_stall_types ctrlers
    |>.throw_exception_nesting_msg s!"Error finding ordering inst ({inst_to_stall_types}) stall point in TernaryInOrderTransform"

  dbg_trace s!"<< Found ordering stall point from heuristic: ({ordering_stall_point})"
  dbg_trace s!"<< Found access stall point from heuristic: ({access_stall_point})"

  -- stall_on -> ordering
  dbg_trace s!"<< Find stall_on states to query"
  let query_stall_on_ctrler_state : List ( (List CtrlerStates) × InstType) := ← inst_to_stall_on_types.mapM (do
    let inst_type' := ·;
    let ctrler_states := ← graph.pre_receive_state inst_type'
      |>.throw_exception_nesting_msg s!"Error finding stall_on_inst states to query in TernaryInOrderTransform"
    pure (ctrler_states, inst_type')
    )
  dbg_trace s!"<< stall on states to query: ({query_stall_on_ctrler_state})"

  -- ordering -> to_stall
  dbg_trace s!"<< Find ordering states to query"
  let query_ordering_ctrler_state : List CtrlerStates × InstType :=
    (
      (← graph.pre_receive_state memory_ordering_type
        |>.throw_exception_nesting_msg s!"Error finding ordering_inst states to query in TernaryInOrderTransform")
      ,
      memory_ordering_type
    )
  dbg_trace s!"<< ordering inst states to query: ({query_ordering_ctrler_state})"

  let stall_on_types_name := "_".intercalate <| inst_to_stall_on_types.map (·.toString)
  let to_stall_types_name := "_".intercalate <| inst_to_stall_types.map (·.toString)

  let ternary_suffix := "_".intercalate <| [stall_on_types_name, memory_ordering_type.toString, to_stall_types_name]
  -- let ordering_stall_state_name := ordering_stall_point.new_stall_state_name inst_to_stall_on_type memory_ordering_type (some ternary_suffix)
  let ordering_stall_state_name : (CtrlerState × StateName × InstType) := (
    let (stall_point, inst_type) := ordering_stall_point;
    let new_state_name := stall_point.state
      -- stall_point.new_stall_state_name inst_to_stall_on_types inst_type (some ternary_suffix)
      --   ( stall_point, new_state_name, inst_type)
    (stall_point, new_state_name, inst_type)
  )
  -- let access_stall_state_name := ordering_stall_point.new_stall_state_name memory_ordering_type inst_to_stall_type (some ternary_suffix)
  let access_stall_state_name : List (CtrlerState × StateName × InstType) := access_stall_point.map (
    let (stall_point, inst_type) := ·;
    let new_state_name := stall_point.state
      -- stall_point.new_stall_state_name inst_to_stall_on_types inst_type (some ternary_suffix)
      --   (stall_point, new_state_name, inst_type)
    (stall_point, new_state_name, inst_type)
  )

  let ordering_stall_points_new_name : (CtrlerState × StateName) := (let (stall_pt, new_name, _) := ordering_stall_state_name; (stall_pt, new_name))
  let access_stall_points_new_name := access_stall_state_name.map (let (stall_pt, new_name, _) := ·; (stall_pt, new_name))
  let stall_points : List (CtrlerState × StateName) := [ ordering_stall_points_new_name ] ++ access_stall_points_new_name

  dbg_trace s!"<< Add the generated stall state to the 'stall on' states to query"
  let query_stall_on_ctrler_states' : List (List CtrlerStates × InstType) := ←
    query_stall_on_ctrler_state.mapM (do
      let (ctrler_states, inst_type) := ·;
      let updated_query_states := ← append_created_states_if_needed stall_points ctrler_states;
      pure (updated_query_states, inst_type)
    )
  let query_stall_on_ctrler_states'' : List (List CtrlerStates × InstType) := ←
    query_stall_on_ctrler_states'.mapM (·.remove_ctrler_state_first_state ctrlers)
  dbg_trace s!"<< Add the generated stall state to the 'ordering' states to query"
  let query_ordering_ctrler_states' : (List CtrlerStates × InstType) := 
    (
      (← append_created_states_if_needed stall_points query_ordering_ctrler_state.1)
      ,
      query_ordering_ctrler_state.2
    )
  let query_ordering_ctrler_states'' : (List CtrlerStates × InstType) ← query_ordering_ctrler_states'.remove_ctrler_state_first_state ctrlers

  dbg_trace s!"<< Generate the ordering_inst stall state"
  let ordering_stall_node : ( Pipeline.Description × StateName × CtrlerState × InstType) := ← (do
    let (stall_point, /- new_stall_state_name -/ _, inst_to_stall_type') := ordering_stall_state_name;
    let dsl_stall := ← ctrlers.StallNode
      stall_point.state stall_point.ctrler
      query_stall_on_ctrler_states''
      /-inst_to_stall_on_types-/ inst_to_stall_type'
      -- new_stall_state_name
      stall_point.state
      |>.throw_exception_nesting_msg s!"Error in TernaryInOrderTransformation while generating the stall state"
    pure (dsl_stall, stall_point.state , stall_point, inst_to_stall_type')
  )
  dbg_trace s!"<< Generate the access_inst stall state"
  let access_stall_nodes : List ( Pipeline.Description × StateName × CtrlerState × InstType) := ← access_stall_state_name.mapM (do
    let (stall_point, /- new_stall_state_name -/ _, inst_to_stall_type') := ·;
    let dsl_stall := ← ctrlers.StallNode
      stall_point.state
      stall_point.ctrler
      [query_ordering_ctrler_states'']
      /-inst_to_stall_on_types-/ inst_to_stall_type'
      -- new_stall_state_name
      stall_point.state
      |>.throw_exception_nesting_msg s!"Error in TernaryInOrderTransformation while generating the stall state"
    pure (dsl_stall, stall_point.state , stall_point, inst_to_stall_type')
  )

  dbg_trace s!"<< Update the controllers with the new ordering stall states"
  let updated_ctrlers' := ← (
    let (stall_node, new_stall_state_name, stall_point, inst_to_stall_type') := ordering_stall_node;

    ctrlers.update_ctrler_state stall_point.ctrler stall_node
    -- UpdateCtrlerWithNode ctrlers stall_point.ctrler new_stall_state_name stall_node stall_point.state (some inst_to_stall_type')
    --   |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while updating the controllers with the stall state"
  )
  dbg_trace s!"<< Update the controllers with the new access stall states"
  let updated_ctrlers'' ←
    access_stall_nodes.foldlM (
      λ (ctrlers' : Ctrlers) dsl_stall_node_state => do
        let (stall_node, new_stall_state_name, stall_point, inst_to_stall_type') := dsl_stall_node_state;

        ctrlers'.update_ctrler_state stall_point.ctrler stall_node
        -- UpdateCtrlerWithNode ctrlers' stall_point.ctrler new_stall_state_name stall_node stall_point.state (some inst_to_stall_type')
        --   |>.throw_exception_nesting_msg s!"Error in BinaryInOrderTransformation while updating the controllers with the stall state"
    ) updated_ctrlers'

  pure updated_ctrlers''

def CDFG.InOrderTransform (ctrlers : Ctrlers) (mcm_ordering : MCMOrdering) (provided_stall_point? : Option ( CtrlerState × InstType))
: Except String Ctrlers := do
  -- induct on mcm-ordering type
  -- Handle cases:
  -- 1. Access₁ → Access₂ 
  -- 2. Access₁ → Ordering₂ → Access₃
  -- NOTE: ignore "specific" types of address ordering for now.

  dbg_trace "<< Starting In Order Transformation"
  let graph : Graph := ← ctrlers.to_simple_instruction_graph

  match mcm_ordering with
  | .binary_ordering ⟨/- BinaryOrdering -/ accesses₁, accesses₂, /- address -/ _ ⟩ => do
    dbg_trace s!"<< In-Order-Transform binary ordering: ({mcm_ordering})"
    graph.BinaryInOrderTransform ctrlers ( accesses₁ ) ( accesses₂ ) provided_stall_point?
  | .ternary_ordering ⟨ /- TernaryOrdering -/ access₁, ordering₂, access₃, /- address -/ _⟩ => do
    dbg_trace s!"<< In-Order-Transform ternary ordering: ({mcm_ordering})"
    graph.TernaryInOrderTransform ctrlers ( access₁.to_inst_type_list ) ( InstType.memory_ordering ordering₂ ) ( access₃.to_inst_type_list ) provided_stall_point?

