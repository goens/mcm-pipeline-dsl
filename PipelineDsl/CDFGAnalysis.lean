import Mathlib.Tactic.Linarith
import PipelineDsl.AST
import PipelineDsl.CDFG

import PipelineDsl.DSLtoCDFG

/-
1. Take a CDFG Graph and find states after the receive state
2. Use post receive states to find pre-receive states
3. Identify unique states between the two that remains until the inst is committed
-/

-- #eval [].



open CDFG

def Pipeline.QualifiedName.ctrler_name (qual_name : Pipeline.QualifiedName) : Except String CtrlerName := do
  match qual_name with
  | .mk lst_ident =>
    match lst_ident[0]? with
    | some ident => pure ident
    | none => throw "QualifiedName: Couldn't get 0th element of list of identifiers"

-- abbrev MsgName := String
def Pipeline.QualifiedName.msg_name (qual_name : Pipeline.QualifiedName) : Except String MsgName := do
  match qual_name with
  | .mk lst_ident =>
    match lst_ident[1]? with
    | some ident => pure ident
    | none => throw "QualifiedName: Couldn't get 1st element of list of identifiers"

def Pipeline.Statement.when_ctrler_msg_names (stmt : Pipeline.Statement)
: Except String (CtrlerName × MsgName) :=
  match stmt with
  | .when qual_name /- args -/ _ /- stmt -/ _ =>
    let src_ctrler : Except String CtrlerName := qual_name.ctrler_name
    let msg_name : Except String MsgName := qual_name.msg_name
    match src_ctrler with
    | .ok src_ctrler' =>
      match msg_name with
      | .ok msg_name' => pure (src_ctrler', msg_name')
      | .error msg => throw s!"Couldn't check when msg_name: ({msg})"
    | .error msg => throw s!"Couldn't check when src_ctrler_name: ({msg})"
  | _ => throw "Statement: Not a when statement"
  
def Pipeline.Statement.await_when_ctrler_name_msg_name (stmt : Pipeline.Statement)
: Except String (CtrlerName × MsgName) :=
  match stmt with
  | .await (none) lst_stmts =>
    match lst_stmts with
    | [] => throw "Await: No statements in await"
    | [stmt] => stmt.when_ctrler_msg_names
    | _ :: _ => throw "Await: More than one statement in await"
  | _ => throw "Statement: Not an await statement"

def Pipeline.Statement.is_await_when_global_response (stmt : Pipeline.Statement) (inst_to_check_completion : InstType)
: Except String Bool := do
  match stmt with
  | .await (none) lst_stmts => do
    let there_is_a_receive_complete : List Bool := (←
      lst_stmts.mapM ( do
        match · with
        | .when qual_name /- args -/ _ /- stmt -/ _ => do
          let src_ctrler : Except String CtrlerName := qual_name.ctrler_name
          let msg_name : Except String MsgName := qual_name.msg_name
          let completion_msg_name : String := ← inst_to_check_completion.completion_msg_name
          match src_ctrler with
          | .ok src_ctrler' => do
            match msg_name with
            | .ok msg_name' => do
              if src_ctrler' == memory_interface && msg_name' == completion_msg_name then do
                pure [true]
              else do
                pure []
            | .error msg => throw s!"Couldn't check when msg_name: ({msg})"
          | .error msg => throw s!"Couldn't check when src_ctrler_name: ({msg})"
        | _ => do pure []
        )).join
    if there_is_a_receive_complete.length > 0 then do
      pure true
    else do
      pure false
  | _ => do pure false
  
def CDFG.Node.has_transition_or_complete_with_msg_name (node : Node) (msg_name : MsgName) : Except String Bool := do
  let transition_or_complete_trans := node.transitions.filter (·.trans_type != .Reset)
  transition_or_complete_trans.anyM (·.messages.anyM (·.is_name_equals msg_name))

def CDFG.Node.has_reset_with_msg_name (node : Node) (msg_name : MsgName) : Except String Bool := do
  let transition_or_complete_trans := node.transitions.filter (·.trans_type == .Reset)
  transition_or_complete_trans.anyM (·.messages.anyM (·.is_name_equals msg_name))

def Pipeline.Statement.await_when's_sending_node (stmt : Pipeline.Statement) (graph : Graph)
: Except String (Option Node) := do
  let (ctrler_name, msg_name) := ← stmt.await_when_ctrler_name_msg_name
  -- TODO:
  -- Filter graph nodes by ctrler_name
  let ctrler_nodes := graph.nodes.filter (·.ctrler_name == ctrler_name)
  -- Filter ctrler_nodes by ones that have transitions w/ message w/ msg_name
  let msg_nodes ← ctrler_nodes.filterM (·.has_transition_or_complete_with_msg_name msg_name)
  let msg'd_by_reset ← ctrler_nodes.filterM (·.has_reset_with_msg_name msg_name) 
  -- If more than one node, or 0, throw error
  match msg_nodes with
  | [node] => pure $ some node
  | [] =>
    match msg'd_by_reset with
    | [] =>
      if msg_name ∈ API_msg_names then
        pure none
      else
        throw s!"Statement: No node in ctrler: ({ctrler_name}) with msg: ({msg_name})\nOriginal Await-Stmt: ({stmt})"
    | _ :: _ => pure none
  | _ => throw s!"Statement: More than one node in ctrler: ({ctrler_name}) with msg: ({msg_name})\nMsg Nodes: ({msg_nodes})\nOriginal Await-Stmt: ({stmt})"

def CDFG.Condition.await_pred's_sending_node (condition : Condition) (graph : Graph)
: Except String (Option Node) := do
  match condition with
  | .AwaitCondition await_stmt =>
    await_stmt.await_when's_sending_node graph |>.throw_exception_nesting_msg "Error while getting the msging node of an await CDFG.Condition"
  | _ => throw "Condition: Not an await condition"

def CDFG.Condition.is_await_global_response (condition : Condition) (inst_to_check_completion : InstType)
: Except String Bool := do
  match condition with
  | .AwaitCondition await_stmt =>
    await_stmt.is_await_when_global_response inst_to_check_completion
  | _ => pure false

def CDFG.Transition.awaits_global_response (transition : Transition) (inst_to_check_completion : InstType)
: Except String Bool :=
  transition.predicate.anyM (·.is_await_global_response inst_to_check_completion)

def CDFG.Node.is_receive_global_response (node : Node) (inst_to_check_completion : InstType)
: Except String Bool :=
  node.transitions.anyM (·.awaits_global_response inst_to_check_completion)

-- 1. Take a CDFG Graph and find states after the receive state
-- Get receive states and transitions that receive the desired memory response
  /- i. from state nodes find receive state -/
  /- Check node transitions for receiving the memory response -/
def CDFG.Graph.getReceiveStates (graph : Graph) (inst_to_check_completion : InstType)
: Except String (List Node) := do
  -- pure {
  -- nodes :=
  (graph.nodes.filterM (·.is_receive_global_response inst_to_check_completion))
  -- }

def Pipeline.HandleBlock.ctrler_msg_names : Pipeline.HandleBlock → Except String (CtrlerName × MsgName)
| .mk qual_name /- idents -/ _ /- stmt -/ _ => do
  pure (← qual_name.ctrler_name, ← qual_name.msg_name)

def CDFG.Condition.is_pred_on_msg_from_ctrler : Condition → Message → CtrlerName  → Except String Bool
| cond, msg, ctrler_name => do
  -- Checkpoint: something to look at later

  -- dbg_trace s!"cond is_pred_on_msg_from_ctrler: src_ctrler: ({ctrler_name}) && msg: ({← msg.name})"

  match cond with
  | .AwaitCondition await_stmt =>
    let (ctrler_name', msg_name) := ← await_stmt.await_when_ctrler_name_msg_name
    -- dbg_trace s!"await_cond from ctrler: ({ctrler_name'}) && msg: ({msg_name})"
    pure $ (msg_name == (← msg.name)) && (ctrler_name' == ctrler_name)
  | .HandleCondition handle_blk =>
    let (ctrler_name', msg_name) := ← handle_blk.ctrler_msg_names
    pure $ (msg_name == (← msg.name)) && (ctrler_name' == ctrler_name)
  | _ => pure false
    
  -- | .HandleCondition handle_stmt =>

def CDFG.Condition.is_pred_inst_not_of_type (cond : Condition) (inst_type : InstType) : Bool :=
  -- dbg_trace s!"COMPUTING: is_pred_inst_not_of_type: {cond}"
  match cond with
  | .DSLExpr expr =>
    let ret := expr.is_contains_instruction_not_eq_type inst_type
    -- dbg_trace s!"COMPUTING: return: ({ret})"
    ret
  | _ =>
    -- dbg_trace s!"COMPUTING: return: false"
    false

def CDFG.Transition.is_trans_pred_by_different_inst_type_than (transition : Transition) (inst_type : InstType) : Bool :=
  let preds := transition.predicate
  preds.any (·.is_pred_inst_not_of_type inst_type)

def CDFG.Node.qualified_state_name (node : Node) : StateName := node.ctrler_name ++ "::" ++ node.current_state
def List.qualified_state_names (nodes : List Node) : List StateName := nodes.map (·.qualified_state_name)

def CDFG.Transitions.filter_by_inst_type? (transitions : Transitions) (inst_type? : Option InstType) : Transitions :=
  match inst_type? with
  | some inst_type =>
    transitions.filter (!·.is_trans_pred_by_different_inst_type_than inst_type)
  | none =>
    transitions

partial def CDFG.Node.is_node_reaches_complete : Node → Graph → Option InstType → List Node → Except String Bool
| node, graph, inst_type?, visited_nodes => do
  -- dbg_trace s!"Completion Reaching Traversal: Current Node: ({node.qualified_state_name}), inst-type: ({inst_type?}), Visited: ({visited_nodes.map (·.qualified_state_name)})"
  -- dbg_trace s!"All Transitions: ({node.transitions.map (·.if_expr_src_dest)})"
  -- let trans := node.transitions.filter (!·.is_trans_pred_by_different_inst_type_than inst_type);  
  let trans := node.transitions.filter_by_inst_type? inst_type?
  -- dbg_trace s!"Transitions Pred by InstType ({inst_type?}) : ({trans.map (·.if_expr_src_dest)})"
  match trans.any (·.trans_type == .Completion ) with
  | true => do pure true
  | false => do
    match trans.filter (·.trans_type == .Transition) with
    | [] => do pure false
    | one_or_more => do
      let dest_states := one_or_more.map (·.dest_state )
      -- dbg_trace s!"!!2"
      let dest_nodes ← dest_states.mapM (graph.node_from_name! · |>.throw_exception_nesting_msg s!"Error when checking if node ({node.current_state}) reaches complete")
      let updated_visited_nodes := visited_nodes ++ [node]
      let unvisited_dest_nodes := dest_nodes.filter (! updated_visited_nodes.contains ·)

      let dest_nodes_reach_complete ← unvisited_dest_nodes.mapM (·.is_node_reaches_complete graph inst_type? updated_visited_nodes)
      pure $ dest_nodes_reach_complete.any (· == true )

def CDFG.Transition.is_transition_reaches_complete (transition : Transition) (graph : Graph) (inst_type? : Option InstType) : Except String Bool := do
  let dest_node ← graph.node_from_name! transition.dest_state |>.throw_exception_nesting_msg s!"Error when checking if transition reaches complete"
  dest_node.is_node_reaches_complete graph inst_type? []

-- def CDFG.Node.is_node_transition_or_complete_pred_on_msg_from_ctrler : Node → Message → CtrlerName → Except String Bool
-- | node, msg, ctrler_name => do
--   -- check if any of the node's predicates are on the message's name
--   let trans_of_interest : Transitions := node.transitions.filter (·.trans_type != .Reset)
--   dbg_trace s!"curr ctrler/node: ({node.ctrler_name}, {node.current_state}) trans_of_interest: ({trans_of_interest.map (·.src_dest_states)})"
--   let all_conditions : List Condition := List.join $ trans_of_interest.map (·.predicate)
--   all_conditions.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)

-- AZ NOTE: Use later for pruning In Order Transformation queried states
-- This just breaks the constrains search on the pre-receive state graph, since it's missing a node
-- and this should be aware the node is missing and ignore it.
-- i.e. it should take a list of nodes to ignore

def CDFG.Transition.fully_qualified_dest_name (transition : Transition) (ctrler_name : CtrlerName) : String :=
  ctrler_name ++ "_" ++ transition.dest_state

abbrev NodeTransition := CDFG.Node × CDFG.Transition 

def NodeTransition.toString : NodeTransition → String
| (node, transition) => s!"({node.ctrler_name}, {node.current_state}) -> ({transition.if_expr_src_dest})"

instance : ToString NodeTransition := ⟨NodeTransition.toString⟩

def CDFG.Transitions.not_visited_transitions (transitions : Transitions) (visited_taken : List NodeTransition) /- (src_ctrler_name : CtrlerName) -/ : List Transition :=
  let taken_transitions := visited_taken.map (·.2)
  transitions.filter (! taken_transitions.contains ·)

def CDFG.Node.not_visitied_transitions (node : Node) (visited_taken : List NodeTransition) : List Transition :=
  let trans : Transitions := node.transitions.filter (·.trans_type == .Transition)
  trans.not_visited_transitions visited_taken /- node.ctrler_name -/

def CDFG.Node.not_visitied_transitions_completions (node : Node) (visited_taken : List NodeTransition) : List Transition :=
  let completions := node.transitions.filter (·.trans_type == .Completion)
  let trans : Transitions := node.transitions.filter (·.trans_type != .Reset) ++ completions
  trans.not_visited_transitions visited_taken /- node.ctrler_name -/

def CDFG.Node.is_node_reset_pred_on_msg_from_ctrler : Node → List TransitionType → Message → CtrlerName → Graph → Option InstType → Except String Bool
| node, trans_types, msg, ctrler_name, graph, inst_type? => do
  -- check if any of the node's predicates are on the message's name
  -- let trans := node.transitions.filter (trans_types.contains ·.trans_type)
  let trans := node.transitions.filter (·.trans_type == .Reset)
  -- let transitions_reaching_complete := ← trans.filterM (·.is_transition_reaches_complete graph inst_type? |>.throw_exception_nesting_msg s!"Error when checking if node's ({node.current_state}) transitions or completions are pred on msg: ({← msg.name}) to ({← msg.dest_ctrler})")
  let all_conditions : List Condition := List.join $ trans.map (·.predicate)
  -- dbg_trace s!"all_conditions: ({all_conditions})"
  let is_node_reaches_complete_and_awaits_msg := ← all_conditions.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)
  -- dbg_trace s!"is_reset_node_reaches_complete_and_awaits_msg: ({is_node_reaches_complete_and_awaits_msg})"
  pure is_node_reaches_complete_and_awaits_msg

def CDFG.Node.is_node_transition_or_complete_pred_on_msg_from_ctrler : Node → Message → CtrlerName → Graph → Option InstType → Except String Bool
| node, msg, ctrler_name, graph, inst_type? => do
  -- check if any of the node's predicates are on the message's name
  let transitions_reaching_complete := ← node.not_reset_transitions.filterM (·.is_transition_reaches_complete graph inst_type? |>.throw_exception_nesting_msg s!"Error when checking if node's ({node.current_state}) transitions or completions are pred on msg: ({← msg.name}) to ({← msg.dest_ctrler})")
  -- dbg_trace s!"1curr ctrler/node: ({node.ctrler_name}, {node.current_state}), inst-type: ({inst_type?}), transitions_reaching_complete: ({transitions_reaching_complete.map (·.if_expr_src_dest)})"
  let all_conditions : List Condition := List.join $ transitions_reaching_complete.map (·.predicate)
  -- dbg_trace s!"all_conditions: ({all_conditions})"
  let is_node_reaches_complete_and_awaits_msg := ← all_conditions.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)
  dbg_trace s!"is_node_reaches_complete_and_awaits_msg: ({is_node_reaches_complete_and_awaits_msg})"
  pure is_node_reaches_complete_and_awaits_msg

def CDFG.Transition.predicated_by_msg?
(transition : Transition) (msg : Message) (ctrler_name : CtrlerName)
(src_node : CDFG.Node)
-- (graph : Graph)
-- (inst_type? : Option InstType)
: Except String (Option NodeTransition) := do
  let is_pred_on_msg := ← transition.predicate.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)
  match is_pred_on_msg with
  | true => pure $ some (src_node, transition)
  | false => pure none

def CDFG.Node.node_transition_or_complete_pred_on_msg_from_ctrler : Node → Message → CtrlerName → Graph → Option InstType → Except String (List NodeTransition)
| node, msg, ctrler_name, graph, inst_type? => do
  -- check if any of the node's predicates are on the message's name
  let transitions_reaching_complete := ← node.not_reset_transitions.filterM (·.is_transition_reaches_complete graph inst_type? |>.throw_exception_nesting_msg s!"Error when checking if node's ({node.current_state}) transitions or completions are pred on msg: ({← msg.name}) to ({← msg.dest_ctrler})")
  -- dbg_trace s!"2curr ctrler/node: ({node.ctrler_name}, {node.current_state}), inst-type: ({inst_type?}), transitions_reaching_complete: ({transitions_reaching_complete.map (·.if_expr_src_dest)})"

  let node_transition_pred_on_msg? := ← transitions_reaching_complete.mapM (do ·.predicated_by_msg? msg ctrler_name node)
  let node_trans_pred_on_msg := node_transition_pred_on_msg?.filterMap id

  pure node_trans_pred_on_msg

  -- let all_conditions : List Condition := List.join $ transitions_reaching_complete.map (·.predicate)
  -- dbg_trace s!"all_conditions: ({all_conditions})"
  -- let is_node_reaches_complete_and_awaits_msg := ← all_conditions.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)
  -- dbg_trace s!"is_node_reaches_complete_and_awaits_msg: ({is_node_reaches_complete_and_awaits_msg})"
  -- pure is_node_reaches_complete_and_awaits_msg

-- Only to be used for finding the "instruction source" node of all insts
def CDFG.Message.findDestState (cdfg_nodes : List CDFG.Node) (msg : Message) (src_ctrler : String)
: Except String (List String) := do
  let (msg_dest, msg_name) := (← msg.dest_ctrler, ← msg.name)
  let trans_compl_listening_nodes := ← cdfg_nodes.filterM (·.is_node_transition_or_complete_pred_on_msg_from_ctrler msg src_ctrler {nodes := cdfg_nodes} none |>.throw_exception_nesting_msg s!"Error trying to find dest state of msg ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))")
  let reset_listening_nodes := ← cdfg_nodes.filterM (·.is_node_reset_pred_on_msg_from_ctrler [.Reset] msg src_ctrler {nodes := cdfg_nodes} none |>.throw_exception_nesting_msg s!"Error trying to find dest state of msg ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))")
  -- let all_listening_nodes := ← cdfg_nodes.filterM (·.is_node_reset_pred_on_msg_from_ctrler [.Reset, .Transition, .Completion] msg src_ctrler {nodes := cdfg_nodes} none |>.throw_exception_nesting_msg s!"Error trying to find dest state of msg ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))")
  match trans_compl_listening_nodes with
  | [] =>
    if (msg_dest, msg_name) ∈ API_dest_ctrlers_msg_names || msg_name ∈ API_msg_names then
      pure []
    else
      if reset_listening_nodes.length != 0 then
        -- check if the dest ctrlers are all simply 1 state 'resources' rather than controllers
        pure []
      else
        throw s!"Message: No node listening to msg from src_ctrler: ({src_ctrler}) of msg name: ({msg_name}) to msg dest: ({msg_dest}) in \n>> CDFG nodes: ({cdfg_nodes.map (·.current_state)})"
  | _ :: _ => -- NOTE: Should msgs be named uniquely?
    let nodes_of_ctrler_type := trans_compl_listening_nodes.filter (·.ctrler_name == msg_dest)
    let ret_names := nodes_of_ctrler_type.map (·.current_state)
    pure ret_names.eraseDups

structure Msg'dNodeTransition where
node : CDFG.Node
transition : CDFG.Transition

def CDFG.Message.findDestStateOfTypeReachingComplete (cdfg_nodes : List CDFG.Node) /- (original_graph : Graph) -/ (msg : Message) (src_ctrler : String) (inst_type : InstType)
: Except String (List NodeTransition) := do
  let (msg_dest, msg_name) := (← msg.dest_ctrler, ← msg.name)
  -- dbg_trace s!"Find dest state of msg name: ({msg_name}) to dest: ({msg_dest}) from src: ({src_ctrler})"
  let ret_nodes := List.join $ ← cdfg_nodes.mapM (
    ·.node_transition_or_complete_pred_on_msg_from_ctrler msg src_ctrler {nodes := cdfg_nodes} inst_type
      |>.throw_exception_nesting_msg s!"Error trying to find dest state of msg ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))"
  )
  match ret_nodes with
  | [] => do
    if (msg_dest, msg_name) ∈ API_dest_ctrlers_msg_names || msg_name ∈ API_msg_names then do
      pure []
    else do
      -- let nodes_pred_on_msg_from_original_graph ← original_graph.nodes.filterM (·.is_node_transition_or_complete_pred_on_msg_from_ctrler msg src_ctrler {nodes := cdfg_nodes} inst_type |>.throw_exception_nesting_msg s!"Error trying to find dest state in original state graph ({original_graph.node_names}) of msg_name ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))")
      -- match nodes_pred_on_msg_from_original_graph with
      -- | [] => do

        -- TODO: add a check to see if the ctrler has an inst instruction field
        -- it it doesn't can ignore, and return pure []
        pure []
        -- throw s!"Message: No node listening to msg from src_ctrler: ({src_ctrler}) of msg name: ({msg_name}) to msg dest: ({msg_dest}) in CDFG nodes: ({cdfg_nodes.qualified_state_names})"

      -- | _::_ => do
      --   pure []
  | _ :: _ => do -- NOTE: Should msgs be named uniquely?
    let nodes_of_ctrler_type := ret_nodes.filter (·.1.ctrler_name == msg_dest)
    pure nodes_of_ctrler_type

def CDFG.Message.findDestStateOfTypeReachingComplete? (cdfg_nodes : List CDFG.Node) (msg : Message) (src_ctrler : CtrlerName) (inst_type : InstType)
: Except String (Option (List StateName)) := do
  let (msg_dest, msg_name) := (← msg.dest_ctrler, ← msg.name)
  let ret_nodes := ← cdfg_nodes.filterM (·.is_node_transition_or_complete_pred_on_msg_from_ctrler msg src_ctrler {nodes := cdfg_nodes} inst_type |>.throw_exception_nesting_msg s!"Error trying to find Option dest state of msg ({msg_name}), to dest ({msg_dest}) from src ({src_ctrler}))")
  dbg_trace s!">>DestState? ({ret_nodes.map (·.current_state)}), msg: ({msg_name}), src_ctrler: ({src_ctrler})"
  match ret_nodes with
  | [] =>
    if (msg_dest, msg_name) ∈ API_dest_ctrlers_msg_names || msg_name ∈ API_msg_names then
      pure none -- more accuracte to return []?
    else 
      pure none
  | _ :: _ => -- NOTE: Should msgs be named uniquely?
    let nodes_of_ctrler_type := ret_nodes.filter (·.ctrler_name == msg_dest)
    let ret_names := nodes_of_ctrler_type.map (·.current_state)
    pure $ some ret_names.eraseDups

def CDFG.Transition.msg'd_nodes (transition : Transition)  (src_ctrler_name : CtrlerName) (graph : Graph) (inst_type : InstType) : Except String (List NodeTransition) := do
  let msg'd_node_names := List.join $ ← ( transition.messages.mapM ( do
    let msg := ·;
    let msg'd_nodes := ← msg.findDestStateOfTypeReachingComplete graph.nodes src_ctrler_name inst_type |>.throw_exception_nesting_msg s!"Error finding transition's msg'd_nodes: Transition: ({transition.src_dest_states})"
    -- dbg_trace s!">> msg'd_nodes/transitions of find dest state: ({msg'd_nodes}), Transition: ({transition.src_dest_states}), from Message: ({msg})"
    pure msg'd_nodes
    )
  )
  pure msg'd_node_names

-- find msg'd nodes
-- def CDFG.Transitions.msg'd_nodes (transitions : Transitions) (src_ctrler_name : CtrlerName) (graph : Graph) : Except String (List Node) := do
--   let nodes_list ← transitions.mapM (·.msg'd_nodes src_ctrler_name graph)
--   pure nodes_list.join

def CDFG.Transitions.msg'd_nodes_of_type_reaching_complete (transitions : Transitions) (src_ctrler_name : CtrlerName) (graph : Graph) (inst_type : InstType) : Except String (List NodeTransition) := do
  -- AZ NOTE: Was using these for debugging
  -- dbg_trace s!"COMPUTING: start finding nodes msg'd from ctrler: ({src_ctrler_name}) that reach complete"
  dbg_trace s!"%%LR? Length transitions: ({transitions.length})"
  let nodes_list : List NodeTransition := List.join $ ← transitions.mapM (·.msg'd_nodes src_ctrler_name graph inst_type)
  let nodes_list' := nodes_list.eraseDups
  -- dbg_trace s!"COMPUTING: msg'd_nodes_reaching_complete: ({nodes_list.map (let nt := ·; nt.1.current_state ++ nt.2.if_expr_src_dest)})"
    -- redundant, the find-dest-state function already filters out nodes that don't reach complete
  -- let filtered := ← nodes_list.filterM (·.1.is_node_reaches_complete graph inst_type [])
  -- dbg_trace s!"COMPUTING: filtered, only completion path nodes: ({filtered.map (·.current_state)})"
  -- pure nodes_list.join
  -- pure filtered
  pure nodes_list

def CDFG.Node.not_visited_transitions (node : Node) (visited_taken : List NodeTransition) : List Transition :=
  node.not_visitied_transitions visited_taken

def CDFG.Transitions.trans_not_pred_against_inst_type (transitions : Transitions) (inst_type : InstType) : Transitions :=
  transitions.filter (! ·.is_trans_pred_by_different_inst_type_than inst_type)

def CDFG.Node.not_visited_transitions_taken_by_inst_type (node : Node) (visited_taken : List NodeTransition) (inst_type : InstType) : List Transition :=
  -- dbg_trace s!"** Who called this?"
  let trans_don't_go_to_avoid_node := node.not_visited_transitions visited_taken
  -- dbg_trace s!"COMPUTING: not_visited_completions_that_don't_go_to_node_and_are_taken_by_inst_type: ({node.current_state})"
  -- dbg_trace s!"COMPUTING: Transition Dests: ({trans_don't_go_to_avoid_node.map (·.dest_state)})"
  let ret := trans_don't_go_to_avoid_node.filter (! ·.is_trans_pred_by_different_inst_type_than inst_type)
  -- dbg_trace s!"COMPUTING: trans dests not pred by other inst types: ({ret.map (·.dest_state)}) with messages: ({ret.map (·.messages)})"
  ret

def CDFG.Node.unique_trans'd_states_not_pred_by_other_insts : Node → InstType → List StateName
| node, desired_inst_type =>
  -- let ctrler_name : CtrlerName := node.ctrler_name
  -- let transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans : Transitions := node.not_visited_transitions_taken_by_inst_type [] desired_inst_type
  trans.map (·.dest_state)

def CDFG.Node.not_visited_transitions_completions (node : Node) (visited_taken : List NodeTransition) : List Transition :=
  node.not_visitied_transitions_completions visited_taken

-- def CDFG.Node.not_visited_transitions_completions_taken_by_inst_type (node : Node) (visited : List Node) (inst_type : InstType) : List Transition :=
--   dbg_trace s!"COMPUTING: all node transitions: ({node.transitions.map (·.src_dest_states)})"
--   dbg_trace s!"COMPUTING: visited states: ({visited.map (·.current_state)})"
--   let trans := node.not_visited_transitions_completions visited
--   dbg_trace s!"COMPUTING: not_visited_transitions_completions_that_don't_go_to_node_and_are_taken_by_inst_type: ({node.current_state})"
--   dbg_trace s!"COMPUTING: Transition & Completion Dests: ({trans.map (·.dest_state)})"
--   let ret := trans.filter (! ·.is_trans_pred_by_different_inst_type_than inst_type)
--   dbg_trace s!"COMPUTING: trans dests not pred by other inst types: ({ret.map (·.dest_state)}) with messages: ({ret.map (·.messages)})"
--   ret

def CDFG.Node.not_visited_transitions_all_completions_taken_by_inst_type (node : Node) (visited_taken : List NodeTransition) (inst_type : InstType) : List Transition :=
  -- dbg_trace s!"COMPUTING: (all_compls) all node transitions: ({node.transitions.map (·.src_dest_states)})"
  -- dbg_trace s!"COMPUTING: (all_compls) visited states/trans: ({visited_taken.map (let nt := ·; nt.1.current_state ++ nt.2.if_expr_src_dest)})"
  let trans := node.not_visited_transitions_completions visited_taken ++ node.completions
  -- dbg_trace s!"COMPUTING: (all_compls) not_visited_transitions_completions_that_don't_go_to_node_and_are_taken_by_inst_type: ({node.current_state})"
  -- dbg_trace s!"COMPUTING: (all_compls) Transition & Completion Dests: ({trans.map (·.dest_state)})"
  let ret := trans.filter (! ·.is_trans_pred_by_different_inst_type_than inst_type)
  -- dbg_trace s!"COMPUTING: (all_compls) trans dests not pred by other inst types: ({ret.map (·.dest_state)}) with messages: ({ret.map (·.messages)})"
  ret

def CDFG.Node.unique_msg'd_states_not_pred_by_other_insts : Node → Graph → InstType → Except String (List NodeTransition)
| node, graph, desired_inst_type => do
  let ctrler_name : CtrlerName := node.ctrler_name
  -- let transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans_compls : Transitions := node.not_visited_transitions_all_completions_taken_by_inst_type [] desired_inst_type
  let messages : Messages := List.join $ trans_compls.map (·.messages)
  let msg'd_states : List NodeTransition := (← messages.mapM (·.findDestStateOfTypeReachingComplete graph.nodes ctrler_name desired_inst_type |>.throw_exception_nesting_msg s!"Error while finding unique msg'd states not pred by other inst types") ).join

  pure msg'd_states

-- NOTE: Only use for finding the "start state" of instructions, or the "instruction source" of all insts..
def CDFG.Node.unique_all_msg'd_states : Node → Graph /- → InstType -/ → Except String (List StateName)
| node, graph /-, inst_type-/ => do
  -- NOTE: Know when to use this or the function above
  -- node.unique_msg'd_states_not_pred_by_other_insts graph inst_type |>.throw_exception_nesting_msg s!"Error while finding unique msg'd states"
  let ctrler_name : CtrlerName := node.ctrler_name
  let transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let messages : Messages := List.join $ transitions.map (·.messages)
  let msg'd_states! := messages.mapM (·.findDestState graph.nodes ctrler_name /- inst_type -/ |>.throw_exception_nesting_msg s!"Error finding unique msg'd states of node: ({node.current_state})")
  let msg'd_states : List StateName := List.join $ ← msg'd_states!.throw_exception_nesting_msg s!"Error finding unique msg'd states for node: ({node.current_state}). Msgs: ({messages})"

  let unique_msg'd_states : List StateName := msg'd_states.eraseDups
  pure unique_msg'd_states

def CDFG.Node.unique_trans'd_states : Node → (List StateName)
| node =>
  let transitions : Transitions := node.transitions.filter (·.trans_type == .Transition)
  let transitioned_to_states : List StateName := transitions.map (·.dest_state)
  
  transitioned_to_states

def CDFG.Condition.is_awaits_on_msg_from_nodes (condition : Condition) (nodes : List Node) : Except String Bool := do
  match condition with
  | .AwaitCondition await_stmt =>
    let (ctrler_name, msg_name) : CtrlerName × MsgName := (← await_stmt.await_when_ctrler_name_msg_name)
    let nodes_of_ctrler := nodes.filter (·.ctrler_name == ctrler_name)
    let nodes_of_ctrler's_transitions := List.join (nodes_of_ctrler.map (·.transitions))
    -- let nodes_of_ctrler's_transitions := List.join (nodes_of_ctrler.map (·.transitions.filter (·.messages.any (·.name == msg_name))))
    let transitions_that_send_msg ← 
      nodes_of_ctrler's_transitions.filterM (·.messages.anyM (
        let this_msg := ·.name
        match this_msg with
        | .ok msg => pure (msg == msg_name)
        | .error msg => throw msg)
        )
    pure (transitions_that_send_msg.length > 0)
  | _ => pure false

def Pipeline.QualifiedName.first : QualifiedName → Except String Identifier
| qual_name => do
  match qual_name.idents with
  | [] => throw "Error: QualifiedName has no idents"
  | [ident] => pure ident
  | ident :: _ => pure ident

-- #eval List.get 4 [1, 2, 3]
def Pipeline.QualifiedName.second : QualifiedName → Except String Identifier
| qual_name => do
  match qual_name.idents with
  | [] => throw "Error: QualifiedName has no idents"
  | [_] => throw "Error: QualifiedName has only one ident"
  | _ :: second :: _ => pure second

partial def Pipeline.Statement.matches_msg_from_ctrler (stmt : Statement) (msg : Message) (src_ctrler : CtrlerName) : Except String Bool := do
  -- dbg_trace s!"$$3 Match stmt: {stmt} with msg: {msg}, from ctrler: {src_ctrler}"
  match stmt with
  | .await (Option.none) stmts => do
    let when_stmt := ← match stmts with
      | [when_stmt] => pure when_stmt
      | _ => throw "Error: Await statement should have one statement"
    when_stmt.matches_msg_from_ctrler msg src_ctrler
  | .when qual_name /- idents -/ _ /- stmt -/ _ => do
    let ctrler_name_matches := ( ← qual_name.first ) == ( src_ctrler )
    let msg_name_matches := ( ← qual_name.second ) == ( ← msg.name )
    -- dbg_trace s!"$$4 qual_name: {qual_name}"
    -- dbg_trace s!"$$5 ctrler_name_matches: {ctrler_name_matches}, msg_name_matches: {msg_name_matches}"
    pure (ctrler_name_matches && msg_name_matches)
  | _ => pure false
  -- termination_by _ => stmt

def CDFG.Condition.is_awaits_on_msg (cond : Condition) (msg : Message) (src_ctrler : CtrlerName) : Except String Bool := do
  match cond with
  | .AwaitCondition await_stmt => do
    await_stmt.matches_msg_from_ctrler msg src_ctrler
  | _ => do pure false

def CDFG.Condition.is_awaits_on_any_msg (cond : Condition) : Bool :=
  match cond with
  | .AwaitCondition _ => true
  | _ => false
def CDFG.Transition.is_awaits_on_any_msg (transition : Transition) : Bool :=
  transition.predicate.any (·.is_awaits_on_any_msg)
def CDFG.Transitions.is_all_non_reset_awaits_on_any_msg (transitions : Transitions) : Bool :=
  transitions.all (·.is_awaits_on_any_msg)
def CDFG.Node.is_all_non_reset_awaits_on_msg (node : Node) : Bool :=
  node.not_reset_transitions.is_all_non_reset_awaits_on_any_msg

-- def CDFG.Condition.is_awaits_on_msg_from_reset_trans (cond : Condition) (graph : Graph) : Bool :=
--   match cond with
--   | .AwaitCondition await_stmt => true
--   | _ => false
-- def CDFG.Transition.is_awaits_on_msg_from_reset_trans (transition : Transition) (graph : Graph) : Bool :=
--   transition.predicate.any (·.is_awaits_on_msg_from_reset_trans graph)
-- def CDFG.Transitions.is_all_non_reset_awaits_on_msg_from_reset_trans (transitions : Transitions) (graph : Graph) : Bool :=
--   transitions.all (·.is_awaits_on_msg_from_reset_trans graph)
-- def CDFG.Node.is_all_non_reset_awaits_on_msg_from_reset_trans (node : Node) (graph : Graph) : Bool :=
--   node.not_reset_transitions.is_all_non_reset_awaits_on_msg_from_reset_trans graph

def CDFG.Message.is_awaited_by_node : Message → Node → CtrlerName → Except String Bool
| msg, node, src_ctrler => do
  let not_reset_trans := node.not_reset_transitions
  let await_preds := not_reset_trans.map (·.predicate.filter (·.is_await)) |>.join

  let awaits_that_await_for_msg := ← await_preds.filterM (·.is_awaits_on_msg msg src_ctrler)

  pure (awaits_that_await_for_msg.length > 0)

def CDFG.Node.unique_trans'd_states_not_pred_on : Node → List Node → Except String (List StateName)
| node, nodes => do
  let transitions : Transitions := node.transitions.filter (·.trans_type == .Transition)

  let transitions_not_pred_on_input_nodes := (←
    transitions.filterM (λ trans => do pure !(← trans.predicate.anyM (·.is_awaits_on_msg_from_nodes nodes)) ))

  let transitioned_to_states : List StateName := transitions_not_pred_on_input_nodes.map (·.dest_state)
  -- Filter out states pred on provided states

  -- match predicate with message belonging to a node from the post receive states
  
  pure transitioned_to_states

def CDFG.Graph.unique_msg'd_states_by_node : Graph → StateName → InstType → Except String (List NodeTransition)
| graph, state_name, inst_type => do
  -- Could try to get this one line implementation working,
  -- but it'll take some time to fully understand
  -- graph.map_nodeM state_name ( (·).unique_msg'd_states graph)

  let current_node? : Option Node := graph.node_from_name? state_name
  if let some current_node := current_node? then
    -- let msg_dest_node : List Node
    current_node.unique_msg'd_states_not_pred_by_other_insts graph inst_type |>.throw_exception_nesting_msg s!"Error finding unique msg'd states for node: ({state_name}), ctrler: ({current_node.ctrler_name})"
  else
    throw s!"Error: (graph, unique msg'd states) No node with name: ({state_name})\nGraph: ({graph})"

def CDFG.Graph.unique_transition'd_states_by_node : Graph → StateName → Except String (List StateName)
| graph, state_name => do
  let current_node? : Option Node := graph.node_from_name? state_name
  if let some current_node := current_node? then
    pure current_node.unique_trans'd_states
  else
    throw s!"Error: (graph, unique trans'd states) No node with name: ({state_name})"

-- Attempt to make the prev function more succinct
def CDFG.Graph.unique_transition'd_states_by_node' : Graph → StateName → Except String (List StateName)
| graph, state_name => do
  graph.node_map state_name (·.unique_trans'd_states)

-- TODO: transitioned to, and not predicated on states of an input list
def CDFG.Graph.unique_transition'd_states_by_node_not_pred_on : Graph → StateName → List Node → Except String (List StateName)
| graph, state_name, nodes => do
  -- dbg_trace s!">>> state_name : {state_name}"
  ← graph.node_mapM state_name (·.unique_trans'd_states_not_pred_on nodes)

def CDFG.Graph.all_msg'd_trans'd_states : Graph /- → InstType -/ → Except String (List StateName)
| graph /- , inst_type-/ => do
  let msg'd_states! := graph.nodes.mapM (·.unique_all_msg'd_states graph /- inst_type -/)
  let msg'd_states := List.join $ ← msg'd_states! |>.throw_exception_nesting_msg s!"Error finding all msg'd and trans'd to states"
  let trans'd_states := List.join $ graph.nodes.map (·.unique_trans'd_states)
  pure $ msg'd_states ++ trans'd_states

-- NOTE: Just use to find the "instruction source node" of a graph... could just replace with a keyword to simplify this...
def CDFG.Graph.not_trans'd_or_msg'd_node (graph : Graph) /- (inst_type : InstType) -/
: Except String Node := do
  -- Get list of state names that are transitioned to or messaged to
  -- Have each node check if they are in this list/set
  -- remaining node should be the "src" of where insts come from...
  let all_transitioned_or_messaged_states_list : List StateName ← graph.all_msg'd_trans'd_states /- inst_type -/
  let not_transitioned_or_messaged_states_list : List Node := graph.nodes.filter (λ node =>
    !(all_transitioned_or_messaged_states_list.any (· == node.current_state))
  )
  match not_transitioned_or_messaged_states_list with
  | [node] =>
    dbg_trace s!"## not_transitioned_to_node: {node}"
    pure node -- only one node, so return it
  | [] => -- empty
    let msg : String := "Error: No nodes which are not transitioned to? There should be 1?"
    throw msg
  | _ :: _ =>
    let msg : String := "Error: (1) More than one node is not transitioned to or messaged to.\n" ++
      s!"I'm only expecting one 'inst src' state: ({not_transitioned_or_messaged_states_list})"
    throw msg

abbrev Distance := Nat
-- TODO: Add a visited list
partial def CDFG.Graph.labelNodesByMessageDistance (start_node : Node) (message_distance : Distance) (graph : Graph) (inst_type : InstType)
(trans? : Option Transition)
: Except String (List (StateName × Distance)) := do
  -- dbg_trace s!"Msg Dist: ({message_distance})"
  -- dbg_trace s!"start_node: {start_node}"
  let unique_msg'd_states : List NodeTransition ← graph.unique_msg'd_states_by_node start_node.current_state inst_type
  let trans'd_states : List Node := ← match trans? with
    | some transition => do pure [ ← graph.node_from_name! transition.dest_state ]
    | none => do
      let unique_transitioned_to_states : List StateName := ← graph.unique_transition'd_states_by_node start_node.current_state
      unique_transitioned_to_states.mapM (do graph.node_from_name! ·)
  -- Look at unique messages
  -- recursive call to Messaged states/ctrlers (which increment counter)
  -- and to transitioned states (which don't increment counter)
  -- dbg_trace s!"**1 Recursive call on msg'd states: {unique_msg'd_states}"
  let states_traversed_from_message_list : List (List (StateName × Distance))  ←
    unique_msg'd_states.mapM (let nt := ·; labelNodesByMessageDistance nt.1 (message_distance + 1) graph inst_type ( some nt.2 ))
  let states_traversed_from_message : List (StateName × Distance) := states_traversed_from_message_list.join

  -- dbg_trace s!"**2 Recursive call on trans'd states: ({trans'd_states})"
  let states_traversed_from_transition_list : List (List (StateName × Distance))  ←
    trans'd_states.mapM (labelNodesByMessageDistance · message_distance graph inst_type (none) )
  let states_traversed_from_transition : List (StateName × Distance) := states_traversed_from_transition_list.join

  let this_state_and_dist : (StateName × Distance) := (start_node.current_state, message_distance)
  let states_traversed : List (StateName × Distance) := this_state_and_dist ::
    states_traversed_from_message ++ states_traversed_from_transition

  return states_traversed

def get_max_from_tuple_nat_list (lst : List (Node × Nat)) : (Node × Nat) :=
  match lst with
  | [] => (default,0)
  | h::t =>
    let (n, nat) := h
    let (n', nat') := (get_max_from_tuple_nat_list t)
    if (nat) >= nat' then (n,nat) else (n',nat')

def get_min_from_tuple_nat_list (lst : List (Node × Nat)) : (Node × Nat) :=
  match lst with
  | h::[] => h
  | h::t =>
    let (n, nat) := h
    let (n', nat') := (get_min_from_tuple_nat_list t)
    if (nat) <= nat' then (n,nat) else (n',nat')
  | [] => (default,0)

#eval get_max_from_tuple_nat_list [(default,2),(default,1)]
#check List.find?
#eval get_min_from_tuple_nat_list [(default,2),(default,1)]
-- AZ NOTE: This ignores states that "await" a message since they're pre-receive
partial def CDFG.Graph.findNodesReachableByTransitionAndMessage
(start : Node) (graph : Graph) (inst_type : InstType)
(trans_to_take? : Option Transitions)
: Except String (List Node) := do
  -- Find the reachable nodes
  -- Get message then dest states, & transition dest states
  -- dbg_trace s!"@1 start: {start}"
  let transitions := start.not_visited_transitions_all_completions_taken_by_inst_type [] inst_type

  -- dbg_trace s!">> node_name: {start}"
  -- dbg_trace s!">> transitions: {transitions}"
  let messages := List.join $ transitions.map Transition.messages
  -- dbg_trace s!">> messages: ({messages})"
  -- let msg_dest_node : List Node
  let ctrler_name : String := start.ctrler_name
  let dest_states! : Except String (List (List NodeTransition)) :=
      messages.mapM (λ message => do
        message.findDestStateOfTypeReachingComplete graph.nodes ctrler_name inst_type |>.throw_exception_nesting_msg s!"Error finding nodes reachable by trans & msgs. Node: ({start})"
      ) 
  let dest_states : List (List NodeTransition) := ← dest_states!.throw_exception_nesting_msg s!"Error: (Couldn't find dest_states for msgs: ({messages})"
  let msg'd_states : List NodeTransition := List.join dest_states
  -- dbg_trace s!"@2 msg'd_states: {msg'd_states}"
  -- dbg_trace s!">> msg'd_states: {msg'd_states}"
  
  -- transitions
  let transitions : Transitions := match trans_to_take? with
    | some transitions => transitions
    | none =>
      start.transitions.filter (λ transition =>
        transition.trans_type == .Transition)
  let transitioned_to_states : List StateName :=
    transitions.map (λ transition => transition.dest_state) |>.eraseDups

  let unique_msg'd_states : List NodeTransition := msg'd_states.eraseDups
  let unique_transitioned_to_states : List Node := ← transitioned_to_states.mapM (do graph.node_from_name! ·)
  -- dbg_trace s!"@3 unique_transitioned_to_states: {unique_transitioned_to_states}"
  
  -- Remove the unique_msg'd_states from the list of reachable nodes!
  -- This is because they're also technically pre-receive nodes as well...
  let reachable_nodes_by_message : (List Node) :=  (←
    unique_msg'd_states.mapM (do let nt := ·; graph.findNodesReachableByTransitionAndMessage nt.1 inst_type (some [nt.2]) )
  ).join
    -- NOTE: Commented out since i think i need these states just for their transitions in the
    -- unique constraint path finding function
  -- let reachable_nodes_by_message_without_await_states : List Node :=
  --   reachable_nodes_by_message.filter (λ node => !(unique_msg'd_states.contains node.current_state))

  let reachable_nodes_by_transition : List Node := (←
    unique_transitioned_to_states.mapM (do graph.findNodesReachableByTransitionAndMessage · inst_type none)
  ).join
  
  let reachable_nodes : List Node := start ::
    -- reachable_nodes_by_message_without_await_states ++ reachable_nodes_by_transition
    reachable_nodes_by_message ++ reachable_nodes_by_transition
  return reachable_nodes.eraseDups

partial def CDFG.Graph.preReceiveStates
(start : Node) (graph : Graph) (don't_visit : List StateName) (post_receive_nodes : List Node) (inst_type : InstType)
(transition_to_take? : Option Transition)
: Except String (List Node) := do
  -- dbg_trace s!">> pre start node: {start}"
  let msg'd_states : List NodeTransition ← graph.unique_msg'd_states_by_node start.current_state inst_type
  -- dbg_trace s!">> msg'd_states: {msg'd_states}"
  let trans_considered := ← match transition_to_take? with
    | some transition_to_take => do pure $ [← graph.node_from_name! transition_to_take.dest_state]
    | none => do
      let transitioned_to_states_not_pred_on_post_receive : List StateName
        ← graph.unique_transition'd_states_by_node_not_pred_on start.current_state post_receive_nodes
      -- dbg_trace s!">> trans'd to states: {transitioned_to_states_not_pred_on_post_receive}"

      transitioned_to_states_not_pred_on_post_receive.mapM (do graph.node_from_name! ·)


  -- if transition is predicated on a message from a post_receive_state then don't do the traversal
  let unique_msg'd_states : List NodeTransition := msg'd_states.eraseDups.filter (! don't_visit.contains ·.1.current_state)
  let unique_transitioned_to_states : List Node := trans_considered.eraseDups.filter (! don't_visit.contains ·.current_state)

  let msg'd_states_?trans := unique_msg'd_states.map (let nt := ·; (nt.1, some nt.2) )
  let trans'd_states_?trans := unique_transitioned_to_states.map (·, none)
  let next_states_to_visit := msg'd_states_?trans ++ trans'd_states_?trans
  
  -- dbg_trace s!"!!1"
  -- let curr_node ← graph.node_from_name! start |>.throw_exception_nesting_msg s!"Error when searching for pre-receive-states"
  let reachable_nodes : List Node := [ start ] ++ (←
    next_states_to_visit.mapM (let nt? := ·; graph.preReceiveStates nt?.1 don't_visit post_receive_nodes inst_type nt?.2)).join

  pure reachable_nodes.eraseDups


-- NOTE: This is causing overflow?
def CDFG.Graph.earliest_node_by_msg_dist (graph : Graph) (nodes : List Node) (inst_type : InstType)
: Except String Node := do
  let not_transitioned_or_messaged_state : Node ← graph.not_trans'd_or_msg'd_node /- inst_type -/ |>.throw_exception_nesting_msg s!"Error when searching for earliest node by msg dist in graph : ({graph.nodes.map (·.qualified_state_name)})"
  /- Then from here traverse the graph from the start node -/
  /- Produce Labels for each state of "Message Distance" and "State Distance" -/
  -- dbg_trace s!"not_transitioned_or_messaged_state: ({not_transitioned_or_messaged_state.qualified_state_name}) transitions: ({not_transitioned_or_messaged_state.transitions.map (·.if_expr_src_dest)})"
  let labelled_by_msg_distance : (List (StateName × Distance)) ←
    graph.labelNodesByMessageDistance not_transitioned_or_messaged_state 0 inst_type none
  -- dbg_trace s!"labelled_by_msg_distance: {labelled_by_msg_distance}"
  
  let receive_states_and_transitions_labelled : List (CDFG.Node × Nat) :=
    nodes.map (λ (node) =>
      let matching_nodes : List (String × Nat) :=
        labelled_by_msg_distance.filter (λ (state_name, /- msg_distance -/ _) =>
          state_name == node.current_state
        )
      let msg_distances : List Nat :=
        matching_nodes.map (λ (/- state_name -/ _, msg_distance) => msg_distance)
      let max_distance : Nat :=
        get_max_from_nat_list msg_distances
      (node, max_distance)
    )
    
  /- Sort by Message Distance. If multiple, sort by state distance -/
  -- Ignore for now. Just take the first one
  let (first_receive_node, _) :=
    get_min_from_tuple_nat_list receive_states_and_transitions_labelled
  -- dbg_trace s!"receive_states_and_transitions_labelled: {receive_states_and_transitions_labelled}"
  -- dbg_trace s!"first_receive_node: {first_receive_node}"

  pure first_receive_node

-- partial def CDFG.Node.is_node_reaches_complete : Node → Graph → Except String Bool
-- | node, graph => do
--   let trans := node.transitions;  
--   match trans.any (·.trans_type == .Completion ) with
--   | true => do pure true
--   | false =>
--     match trans.filter (·.trans_type == .Transition) with
--     | [] => pure false
--     | one_or_more =>
--       let dest_states := one_or_more.map (·.dest_state )
--       dbg_trace s!"!!2"
--       let dest_nodes ← dest_states.mapM (graph.node_from_name! ·)
--       let dest_nodes_reach_complete ← dest_nodes.mapM (·.is_node_reaches_complete graph)
--       pure $ dest_nodes_reach_complete.any (· == true )

def CDFG.Transition.has_commit_labelled_stmt (transition : Transition) : Bool :=
  transition.commit
  -- transition.stmts.any (·.is_commit_labelled)

def CDFG.Graph.commit_transition_state_ctrler (graph : Graph) : Except String Node := do
  let global_perform_nodes : List Node :=
    (graph.nodes.filter (·.transitions.any (·.has_commit_labelled_stmt)))

  match global_perform_nodes with
  | [] => throw "Error: No commit state found"
  | [node] => pure node
  | _ => throw "Error: More than one commit state found"

-- structure NodeTransition where
-- node : CDFG.Node
-- transition : CDFG.Transition
#check List.foldl
-- TODO: Stub
partial def CDFG.Graph.reachable_nodes_from_node_up_to_option_node
(depth := 0)
(graph : Graph) (node : Node) (avoid_node? : Option Node) (inst_type : InstType) (visited_taken : List NodeTransition)
(msg'd_trans : Option (Transitions))
: Except String ((List Node) × List NodeTransition) := do
  -- let visited := default;
  -- Look at 1. nodes that the current node transitions to
  --  Filter those by inst_type, if they are pred by the right type
  --  Filter by if it doesn't go to the option_node?
  -- Look at 2. nodes that the current node messages
  --  Can use the filter'd transitions from above, just check their messages they send, & the dest nodes of those messages
  --  One additional caveat, we consider completion transitions as well
  -- dbg_trace s!">> reachability comp: curr node: ({node.qualified_state_name})"

  let stop : Bool :=
    match avoid_node? with
    | some avoid_node => node == avoid_node
    | none => false

  if stop then do
    return ([node], visited_taken)
  else do
    -- dbg_trace s!"**11 Who called this?"
    dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: current node, inst_type ({inst_type}): ({node.current_state}). Type of inst: ({inst_type}). Current Ctrler: ({node.ctrler_name})"
    let trans_for_this_inst_type :=
      -- node.not_visited_transitions_taken_by_inst_type visited_taken inst_type
      node.not_visited_transitions_all_completions_taken_by_inst_type visited_taken inst_type -- Consider transitions as well for messages

    dbg_trace s!"Depth ({depth}) test1"
    let transitions_to_consider : Transitions :=
      match msg'd_trans with
      | none => trans_for_this_inst_type
      | some transitions => transitions.trans_not_pred_against_inst_type inst_type |>.not_visited_transitions visited_taken

    dbg_trace s!"Depth ({depth}) test2"
    let transitions_to_traverse : Transitions :=
      transitions_to_consider.basic_transitions

    -- dbg_trace s!"DEBUG: transitions_to_traverse: ({transitions_to_traverse.map (·.if_expr_src_dest)})"

    let visited_msg'd_node : List NodeTransition := match msg'd_trans with
    | some trans's => trans's.map ( (node, ·) )
    | none => []

    let trans_visited_taken : List NodeTransition :=
      transitions_to_traverse.map ((node, ·))

    let trans_dest_names : List StateName :=
      /- List.eraseDups $-/ transitions_to_traverse.map (·.dest_state)
    dbg_trace s!"Depth ({depth}) test3"
    let trans_dests : List Node :=
      ← trans_dest_names.mapM
        (graph.node_from_name! ·)
    dbg_trace s!"Depth ({depth}) test3.1"

    dbg_trace s!"Depth ({depth}) trans_visited_taken Length: ({trans_visited_taken.length})"
    -- let trans_visited_taken' :=
    --   trans_visited_taken.foldl
    --     (fun list_a node_trans =>
    --       if ! list_a.contains node_trans then list_a.concat node_trans else list_a)
    --     []
    -- dbg_trace s!"Depth ({depth}) trans_visited_taken' Length: ({trans_visited_taken'.length})"
    let all_trans'd_to_visited_taken' : List NodeTransition :=
      (trans_visited_taken) ++ visited_taken -- ++ visited_msg'd_node

    dbg_trace s!"Depth ({depth}) test3.2"
    let all_trans'd_to_visited_taken : List NodeTransition :=
      all_trans'd_to_visited_taken'
    dbg_trace s!"Depth ({depth}) test3.3"
    let dests : List Node :=
      List.eraseDups $
        trans_dests
    dbg_trace s!"Depth ({depth}) test3.4"

    dbg_trace s!"Depth ({depth}) test4"
    let trans'd_to : List (List Node × List NodeTransition) := ← dests.mapM (
        graph.reachable_nodes_from_node_up_to_option_node (depth + 1)
          · avoid_node? inst_type all_trans'd_to_visited_taken (none)
      )
    dbg_trace s!"Depth ({depth}) test5"

    let transition_reachable_nodes := /- List.eraseDups $ -/ List.join $ trans'd_to.map (·.1)
    let trans'd_to_visited_taken   := /- List.eraseDups $ -/ List.join $ trans'd_to.map (·.2)

    let transitions_to_check_msgs : Transitions :=
      transitions_to_consider.filter (·.trans_type != .Reset)
    -- Get dest nodes of msgs, ignore those sending to "API" nodes for now
    let trans_compls_for_this_inst_type : Transitions :=
      transitions_to_check_msgs
      -- node.not_visited_transitions_all_completions_taken_by_inst_type visited_taken inst_type -- Consider transitions as well for messages
    dbg_trace s!"Depth ({depth}) test5.1"
    dbg_trace s!"Depth ({depth}) Length trans_compls_for_this_inst_type: ({trans_compls_for_this_inst_type.length})"
    let msg'd_nodes_unfiltered : List NodeTransition :=
      ← trans_compls_for_this_inst_type.msg'd_nodes_of_type_reaching_complete node.ctrler_name graph inst_type
        |>.throw_exception_nesting_msg s!"Current Inst Type: ({inst_type})"

    dbg_trace s!"Depth ({depth}) test6"
    let msg'd_nodes := msg'd_nodes_unfiltered.filter (! trans'd_to_visited_taken.contains ·)

    -- dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: visited_taken node/trans: ({visited_taken})"
    -- dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: trans'd visited_taken node/trans: ({trans'd_to_visited_taken})"

    let msg'd_trans_visited_taken : List NodeTransition := -- List.eraseDups $
      trans'd_to_visited_taken ++ msg'd_nodes ++ visited_taken
    let msg'd_nodes_after_trans : List Node := List.eraseDups $
      ← msg'd_nodes.mapM (graph.node_from_name! ·.2.dest_state )
    -- AZ NOTE: won't carry the visited_taken between ctrlers, leaves the option of re-using ctrlers ope for later
    let msg'd_and_visited : List (List Node × List NodeTransition) :=
      ← msg'd_nodes_after_trans.mapM (do
        graph.reachable_nodes_from_node_up_to_option_node (depth + 1)
          · avoid_node? inst_type msg'd_trans_visited_taken (none) )
    let reachable_nodes_from_msgs : List Node := /- List.eraseDups $ -/ List.join $ msg'd_and_visited.map (·.1)
    let msg'd_visited : List NodeTransition := /- List.eraseDups $ -/ List.join $ msg'd_and_visited.map (·.2)
    dbg_trace s!"Depth ({depth}) test7"

    dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: trans & compl transitions, inst_type ({inst_type}): ({trans_compls_for_this_inst_type.map (·.if_expr_src_dest)})"
    dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: msg'd nodes, inst_type ({inst_type}): (current node: ({node.current_state})) msg'd_nodes: ({msg'd_nodes.map (·.toString)})"

    -- dbg_trace s!"Depth ({depth}) REACHABLE COMPUTATION: msg'd visited_taken node/trans: ({msg'd_visited})"

    return (
      /-List.eraseDups $-/ [node] ++ transition_reachable_nodes ++ reachable_nodes_from_msgs
      ,
      /-List.eraseDups $-/ msg'd_visited ++ msg'd_trans_visited_taken)

    -- NOTE: If we want to prove termination, show that the graph is a DAG, and that we only visit each node once, so we will eventually terminate
    -- Or show that the transition type doesn't have any cycles
    -- Or show that visited ensures we don't visit the same node twice
    -- termination_by CDFG.Graph.reachable_nodes_from_node_up_to_option_node graph node avoid_node? inst_type visited => graph

def CDFG.Graph.global_receive_node_of_inst_type (graph : Graph) (inst_type : InstType)
: Except String Node := do
  let receive_states_and_transitions : List Node ←
    graph.getReceiveStates inst_type

  let receive_state : Node ←
    match receive_states_and_transitions with
    | [state] => pure state
    | _ :: _ =>
      -- dbg_trace s!">> Multiple Nodes found for inst_type: ({inst_type}), nodes ({receive_states_and_transitions.qualified_state_names}), Try to find ones that reach completion first (i.e. not reset states)"
      let receive_states_that_reach_completion ← receive_states_and_transitions.filterM (·.is_node_reaches_complete graph inst_type [])
      -- dbg_trace s!">> Multiple Nodes: receive_states_that_reach_completion: ({receive_states_that_reach_completion.qualified_state_names})"

      -- Try to find ones that are speculative
      -- dbg_trace s!">> Multiple Nodes: Try to find speculative nodes"
      let commit_node := ← graph.commit_transition_state_ctrler |>.throw_exception_nesting_msg s!"Multiple Nodes: Error when searching for post commit nodes in graph : ({graph.nodes.map (·.qualified_state_name)})"
      -- dbg_trace s!">> Multiple Nodes: commit_node: ({commit_node.qualified_state_name}) Now find post-commit nodes"
      -- let post_commit_nodes := 
      let (post_commit_nodes', /- visited nodes -/_) := ← graph.reachable_nodes_from_node_up_to_option_node 0 commit_node none inst_type [] none
      let post_commit_nodes := post_commit_nodes'.eraseDups
      -- dbg_trace s!">> Multiple Nodes: post_commit_nodes: ({post_commit_nodes.qualified_state_names})"
      let speculative_receive_states := receive_states_that_reach_completion.filter ( ! post_commit_nodes.contains · )
      -- dbg_trace s!">> Multiple Nodes: speculative receive states: ({speculative_receive_states.qualified_state_names})"

      match speculative_receive_states with
      | [] =>
        throw <|
          String.intercalate "\n" [
            s!"Error: No speculative receive state found? Expected a speculative receive state?",
            s!" >> Receive States found: ({receive_states_and_transitions.qualified_state_names})",
            s!" >> Receive States that reach completion found: ({receive_states_that_reach_completion.qualified_state_names})",
            s!" >> Commit node: ({commit_node.qualified_state_name})",
            s!" >> Post commit nodes: ({post_commit_nodes.qualified_state_names})"
          ]
      | [state] => pure state
      | _ :: _ =>
        throw <|
          String.intercalate "\n" [
            s!"Error: Multiple speculative receive states found? Expected a single speculative receive state? ({speculative_receive_states.qualified_state_names})",
            s!" >> Receive States found: ({receive_states_and_transitions.qualified_state_names})",
            s!" >> Receive States that reach completion found: ({receive_states_that_reach_completion.qualified_state_names})",
            s!" >> Commit node: ({commit_node.qualified_state_name})",
            s!" >> Post commit nodes: ({post_commit_nodes.qualified_state_names})"
          ]

    | [] =>
      -- If none found, error!
      throw "Error: No receive state found"

def CDFG.Graph.global_complete_node_of_inst_type (graph : Graph) (inst_type : InstType)
: Except String Node := do
  match inst_type with
  | .memory_access access => do
    match access with
    | .load
    | .store => do
      graph.global_receive_node_of_inst_type inst_type
  | .memory_ordering ordering => do
    match ordering with
    | .mfence => do
      graph.commit_transition_state_ctrler

def common_nodes (nodes1 : List Node) (nodes2 : List Node) : List Node := nodes1.filter (nodes2.contains ·)

def remove_nodes_from_list (nodes : List Node) (nodes_to_remove : List Node) : List Node := nodes.filter (!nodes_to_remove.contains ·)

def remove_common_nodes (nodes1 : List Node) (nodes2 : List Node) : List Node := remove_nodes_from_list nodes1 (common_nodes nodes1 nodes2)

structure CtrlerPathConstraint where
ctrler : CtrlerName
path : List Node
constraints : List ConstraintInfo
deriving Inhabited, BEq
def CtrlerPathConstraint.toString : CtrlerPathConstraint → String
| ctrler_path_constraint =>
  let ctrler := ctrler_path_constraint.ctrler
  let path := ctrler_path_constraint.path
  let constraints := ctrler_path_constraint.constraints
  s!"===\nCtrler: ({ctrler})\nPath: ({path})\nConstraints: ({constraints})\n==="
instance : ToString CtrlerPathConstraint where toString := CtrlerPathConstraint.toString

def CtrlerPathConstraint.new_path_of_ctrler : CtrlerName → CtrlerPathConstraint
| ctrler_name => {ctrler := ctrler_name, path := [], constraints := []}

def CtrlerPathConstraint.add_constraints : CtrlerPathConstraint → List ConstraintInfo → CtrlerPathConstraint
| ctrler_path_constraint, constraints => {ctrler := ctrler_path_constraint.ctrler, path := ctrler_path_constraint.path, constraints := ctrler_path_constraint.constraints ++ constraints}

def CtrlerPathConstraint.add_path_node : CtrlerPathConstraint → Node → CtrlerPathConstraint
| ctrler_path_constraint, node => {ctrler := ctrler_path_constraint.ctrler, path := ctrler_path_constraint.path ++ [node], constraints := ctrler_path_constraint.constraints}

def CtrlerPathConstraint.new_path_of_ctrler_and_node : CtrlerName → Node → CtrlerPathConstraint
| ctrler_name, node => ( {ctrler := ctrler_name, path := [], constraints := []} : CtrlerPathConstraint ).add_path_node node

def CtrlerPathConstraint.add_constraints_and_path_node : CtrlerPathConstraint → List ConstraintInfo → Node → CtrlerPathConstraint
| ctrler_path_constraint, constraints, node =>
  (ctrler_path_constraint.add_constraints constraints).add_path_node node

-- def Pipeline.QualifiedName.idents : QualifiedName → List Identifier
-- | qual_name => match qual_name with
--   | .mk idents => idents

def CDFG.Condition.is_await_on_msg_from_ctrler (condition : Condition) (msg : Message) (src_ctrler : CtrlerName) : Except String Bool := do
  match condition with
  | .AwaitCondition await_stmt => do
    let bool := await_stmt.matches_msg_from_ctrler msg src_ctrler
    -- dbg_trace s!"$$6.6 bool: {bool}"
    bool
  | _ => pure false

def CDFG.Transitions.transitions_awaiting_on_msg_from_ctrler : Transitions → Message → CtrlerName  → Except String Transitions
| transitions, msg, src_ctrler => do
  let trans_awaiting_msg ← transitions.filterM (
    let bool := ·.predicate.anyM (
      let bool' := ·.is_await_on_msg_from_ctrler msg src_ctrler
      -- dbg_trace s!"$$6.5 bool': {bool'}"
      bool'
      );
    -- dbg_trace s!"$$6 bool: {bool}"
    -- dbg_trace s!"$$6.5 predicates: {·.predicate}"
    bool
    )
  -- dbg_trace s!"$$7 trans_awaiting_msg: ({trans_awaiting_msg})"
  pure trans_awaiting_msg

def CDFG.Transitions.transitions_awaiting_on_option_msg_from_ctrler : Transitions → Option ( Message × CtrlerName ) → Except String Transitions
| transitions, none =>
  -- dbg_trace s!"$$1 just return transitions"
  pure transitions
| transitions, some ( msg, src_ctrler ) => do
  -- dbg_trace s!"$$2 find transitions awaiting on msg: {msg}"
  let trans_awaiting_msg := ← transitions.transitions_awaiting_on_msg_from_ctrler msg src_ctrler
  -- dbg_trace s!"$$8 trans_awaiting_msg: ({trans_awaiting_msg})"
  pure trans_awaiting_msg

#eval [2,1,2,3].reverse.eraseDups.reverse
#eval [[1,2],[1,3]].map (λ x => List.join $ x.map (λ y => if [[1,2],[1,3]].all (λ z => z.contains y ) then [ y ] else [] ))

-- AZ NOTE: Not really used...
partial def CDFG.Graph.ctrler_trans_paths_and_constraints
(start : StateName) (graph : Graph) (path_constraints : CtrlerPathConstraint) (msg_trans_should_await : Option (Message × CtrlerName)) (inst_type : InstType)
: Except String (List CtrlerPathConstraint) := do
  -- dbg_trace s!"Ctrler Path Constraint, at: ({start}))"
  -- dbg_trace s!"Ctrler Path Constraint, path_constraints: ({path_constraints})"
  -- For this, I should get paths of nodes, and constraints
  --  but just per ctrler
  -- msgs start new paths

  let current_node? : Option Node := graph.nodes.find? (λ node =>
    node.current_state == start)
  if let some current_node :=  current_node? then
    -- Get messages
    let trans_transitions : Transitions := current_node.transitions.filter (·.trans_type == .Transition)
    let complete_transitions := current_node.transitions.filter (·.trans_type == .Completion)
    let trans_compl_transitions := trans_transitions ++ complete_transitions
    let msg'd_trans_compl_transitions := ← trans_compl_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await
    -- dbg_trace s!"^^msg'd_trans_compl_transitions : ({msg'd_trans_compl_transitions})"

    let messages := List.join $ msg'd_trans_compl_transitions.map (·.messages)
    -- dbg_trace s!"^^messages: ({messages})"
    let current_ctrler : String := current_node.ctrler_name
    let dest_states : List (List (NodeTransition × Message)) ← messages.mapM (λ msg => do
      let msg'd_states : List NodeTransition := (← msg.findDestStateOfTypeReachingComplete graph.nodes current_ctrler inst_type |>.throw_exception_nesting_msg s!"Error while finding ctrler paths & constraints")
      -- dbg_trace s!"msg'd_states: ({msg'd_states})"
      pure $ ZipWithList msg'd_states msg)
    -- dbg_trace s!"^^messages and dest_state: ({dest_states})"
    let msg'd_states : List ( NodeTransition × Message ) := List.join dest_states
  
    let unique_msg'd_states : List ( NodeTransition × Message ) := msg'd_states.eraseDups

    let paths_from_msg'd_states : List CtrlerPathConstraint := List.join $ ← unique_msg'd_states.mapM (λ (node_name, msg) => do
      -- dbg_trace s!"!!3"
      graph.ctrler_trans_paths_and_constraints node_name.1.current_state (CtrlerPathConstraint.new_path_of_ctrler_and_node (← msg.dest_ctrler ) (← graph.node_from_name! node_name.1.current_state |>.throw_exception_nesting_msg s!"Error when searching for ctrler transition paths and constraints"
      )) (Option.some (msg, current_ctrler)) inst_type )
    -- dbg_trace s!"Ctrler Path Constraint, paths_from_msg'd_states: ({paths_from_msg'd_states})"

    -- TODO: get the transition constraints from just those transitions that
    -- await on the msg 

    -- TODO: add to path_constraints for the current ctrler,
    -- Recursive call for each transition
    -- transitions

    -- dbg_trace s!"$$ transitions: ({current_node.transitions})"
    -- dbg_trace s!"$$ trans_transitions: ({trans_transitions})"
    -- dbg_trace s!"$$ msg we're awaiting on: ({msg_trans_should_await})"
    -- dbg_trace s!"^^ trans_transitions: ({trans_transitions})"
    let transitions_of_interest := ← trans_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await
    -- dbg_trace s!"^^transitions_of_interest: ({transitions_of_interest})"
    let transitioned_to_states : List StateName := transitions_of_interest.map (·.dest_state)
    -- group by dest_state × transitions
    let trans_to_dest_state : List (StateName × Transitions) := transitioned_to_states.map (λ state_name => (state_name, transitions_of_interest.filter (·.dest_state == state_name)))
    let dest_state_and_common_constraints : List (StateName × (List ConstraintInfo) ) :=
      trans_to_dest_state.map (λ (state_name, trans_to_same_state) =>
        -- Get constraints common to all transitions.
        -- Should probably later also predicate paths based on inst
        -- dbg_trace s!"~~3 trans_to_same_state: ({trans_to_same_state})"
        let common_constraints := List.join (trans_to_same_state.map (·.constraint_info.filter (λ constraint => trans_to_same_state.all (·.constraint_info.any (· == constraint) ) ) ) ) 
        -- dbg_trace s!"~~2 common_constraints: ({common_constraints})"
        let no_dup_common_constraints := common_constraints.reverse.eraseDups.reverse
        -- dbg_trace s!"~~4 no dup constraints: ({no_dup_common_constraints})"
        (state_name, no_dup_common_constraints))

    -- dbg_trace s!"~~1 path_constraints: ({path_constraints})"
    let ctrler_path_constraints_with_curr_node := path_constraints.add_path_node current_node
    let paths_to_dest_states_with_common_constraints : List CtrlerPathConstraint := List.join $ ← 
      dest_state_and_common_constraints.mapM (λ (state_name, common_constraints) => do
        -- dbg_trace s!"Did we error here1? state_name: {state_name}"
        -- dbg_trace s!"!!4"
        let node ← graph.node_from_name! state_name  
        let path_constraints_with_added_constraints := (ctrler_path_constraints_with_curr_node.add_constraints_and_path_node common_constraints node)
        -- dbg_trace s!"~~5 path with constraints: ({path_constraints_with_added_constraints})"
        (
          graph.ctrler_trans_paths_and_constraints state_name path_constraints_with_added_constraints none inst_type
        )
        )
    -- dbg_trace s!"Ctrler Path Constraint, paths_to_dest_states_with_common_constraints: ({paths_to_dest_states_with_common_constraints})"

    -- TODO: sth for completion type transitions
    -- TODO: join and return the paths that exist
    -- process results
    if !complete_transitions.isEmpty then
      let all_path_constraints := List.join $ List.join $ ctrler_path_constraints_with_curr_node.path.map (·.transitions.map (·.constraint_info) )
      let a_complete_path := ctrler_path_constraints_with_curr_node.add_constraints all_path_constraints
      let a_complete_path_and_others := pure $ ctrler_path_constraints_with_curr_node :: paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
      -- dbg_trace s!"@42.0 POST a_complete_path: ({a_complete_path})"
      a_complete_path_and_others
    else
      pure $ paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
  else
    throw "Node not found"

-- TODO: Implement a path constraint finder for pre-receive states
-- It should avoid going to nodes from the post-receive states
-- i.e. add input arg for nodes/graph to avoid

-- AZ NOTE: Not really used right now...
partial def CDFG.Graph.pre_receive_states_constraints
: StateName → Graph → CtrlerPathConstraint → Option (Message × CtrlerName) → Graph → InstType → Except String (List CtrlerPathConstraint) 
| start, pre_receive_states, ctrler's_path_constraints, msg_trans_should_await?, post_receive_states, inst_type => do
  -- dbg_trace s!"pre graph: ({pre_receive_states})"
  -- dbg_trace s!"post graph: ({post_receive_states})"
  -- dbg_trace s!"Did we error here2? state_name: {start}"
  -- dbg_trace s!"Pre-Receive State Fn: start: ({start})"
  -- dbg_trace s!"!!5"
  let current_node ← pre_receive_states.node_from_name! start

  let trans_transitions : Transitions := current_node.transitions.filter (·.trans_type == .Transition)
  let complete_transitions := current_node.transitions.filter (·.trans_type == .Completion)
  let trans_compl_transitions := trans_transitions ++ complete_transitions
  let msg'd_trans_compl_transitions := ← trans_compl_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await?

  let messages := List.join $ msg'd_trans_compl_transitions.map (·.messages)
  let ctrler_name : String := current_node.ctrler_name
  let dest_states : List (List (NodeTransition × Message)) ← messages.mapM (λ msg => do
    let msg'd_states : List NodeTransition := (← msg.findDestStateOfTypeReachingComplete pre_receive_states.nodes ctrler_name inst_type |>.throw_exception_nesting_msg s!"Error while finding pre receive state constraints")
    pure $ ZipWithList msg'd_states msg)
  let msg'd_states : List ( NodeTransition × Message ) := List.join dest_states
  
  let unique_msg'd_states : List ( NodeTransition × Message ) := msg'd_states.eraseDups
  -- dbg_trace s!"@@2.01 unique_msg'd_states: ({unique_msg'd_states})"

  let paths_from_msg'd_states : List CtrlerPathConstraint := List.join $ ← unique_msg'd_states.mapM (λ (node_name, msg) => do
    if post_receive_states.nodes.any (·.current_state == node_name.1.current_state) then
      return []
    else
      pre_receive_states.pre_receive_states_constraints node_name.1.current_state (CtrlerPathConstraint.new_path_of_ctrler (← msg.dest_ctrler)) (Option.some (msg, ctrler_name)) post_receive_states inst_type)
  -- dbg_trace s!"@@2.02 paths_from_msg'd_states: ({paths_from_msg'd_states})"

  -- TODO: get the transition constraints from just those transitions that
  -- await on the msg 

  -- TODO: add to path_constraints for the current ctrler,
  -- Recursive call for each transition
  -- transitions

  let transitions_of_interest := ← trans_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await?
  -- dbg_trace s!"## Transitions of interest: ({transitions_of_interest})"
  let transitioned_to_states : List StateName := transitions_of_interest.map (·.dest_state)
  -- group by dest_state × transitions
  let trans_to_dest_state : List (StateName × Transitions) := transitioned_to_states.map (λ state_name => (state_name, transitions_of_interest.filter (·.dest_state == state_name)))
  -- dbg_trace s!"## Trans'd to states: ({transitions_of_interest})"
  let trans_to_dest_state_not_in_post_receive_states := trans_to_dest_state.filter (λ (state_name, _) => post_receive_states.nodes.all (·.current_state != state_name) )

  let dest_state_and_common_constraints : List (StateName × (List ConstraintInfo) ) :=
    trans_to_dest_state_not_in_post_receive_states.map (λ (state_name, trans_to_same_state) =>
      -- Get constraints common to all transitions.
      -- Should probably later also predicate paths based on inst
      let common_constraints := List.join (trans_to_same_state.map (·.constraint_info.filter (λ constraint => trans_to_same_state.all (·.constraint_info.any (· == constraint) ) ) ) ) 
      let no_dup_common_constraints := common_constraints.reverse.eraseDups.reverse
      (state_name, no_dup_common_constraints))

  let ctrler_path_constraints_with_curr_node := ctrler's_path_constraints.add_path_node current_node
  let paths_to_dest_states_with_common_constraints : List CtrlerPathConstraint := List.join $ ← 
    dest_state_and_common_constraints.mapM (λ (state_name, common_constraints) => do
      -- dbg_trace s!"Did we error here3? state_name: {state_name}"
      -- dbg_trace s!"!!6"
      let node ← pre_receive_states.node_from_name! state_name  
      (
        pre_receive_states.pre_receive_states_constraints state_name
        (ctrler_path_constraints_with_curr_node.add_constraints_and_path_node common_constraints node) none post_receive_states inst_type
      )
      )

  -- TODO: sth for completion type transitions
  -- TODO: join and return the paths that exist
  -- process results
  if !complete_transitions.isEmpty || trans_to_dest_state_not_in_post_receive_states.isEmpty then
    let all_path_constraints := List.join $ List.join $ ctrler_path_constraints_with_curr_node.path.map (·.transitions.map (·.constraint_info) )
    let a_complete_path := ctrler_path_constraints_with_curr_node.add_constraints all_path_constraints
    let a_complete_path_and_others := pure $ ctrler_path_constraints_with_curr_node :: paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
    -- dbg_trace s!"@42.0 PRE a_complete_path: ({a_complete_path})"
    a_complete_path_and_others
  else 
    pure $ paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints

-- Rationale behind doing reverse -> eraseDups -> reverse & ordering of items... (an approximation)
#eval [1,2,5,1,2,3,5].eraseDups
#eval [1,2,5,1,2,3,5].reverse.eraseDups.reverse

def CanonStatePathsApprox : List CtrlerPathConstraint → List CtrlerPathConstraint
| potential_duplicate_state_paths =>
  -- dbg_trace s!"@@%% CanonStatePathsApprox: potential_duplicate_state_paths: {potential_duplicate_state_paths}"
  let ctrlers := potential_duplicate_state_paths.map (·.ctrler)
  let unique_ctrlers := ctrlers.eraseDups

  -- paths specific to each ctrler
  let ctrler_paths : List (CtrlerName × List CtrlerPathConstraint) := unique_ctrlers.map (λ ctrler => (ctrler, potential_duplicate_state_paths.filter (·.ctrler == ctrler) ) )

  -- ctrler paths -> path, with just common constraints
  let ctrler_path : List CtrlerPathConstraint := ctrler_paths.map (λ (ctrler, all_paths) =>
    let list_constraints : ( List ConstraintInfo ) := List.join $ all_paths.map (·.constraints)
      -- List.join $
      -- all_paths.map (λ path => path.constraints.filter ( λ constraint => all_paths.all (λ path' => path'.constraints.any (· == constraint) )))
    let no_dup_list_constraints := list_constraints.reverse.eraseDups.reverse

    let path_nodes : List Node := List.join $ all_paths.map (·.path)
    let no_dup_path_nodes := path_nodes.reverse.eraseDups.reverse

    {ctrler := ctrler, constraints := no_dup_list_constraints, path := no_dup_path_nodes}
  )
  ctrler_path

structure CtrlerConstraint where
  ctrler : CtrlerName
  constraints : List ConstraintInfo
  deriving Inhabited, BEq
def CtrlerConstraint.toString : CtrlerConstraint → String
| ctrler_constraint =>
  let ctrler := ctrler_constraint.ctrler
  let constraints := ctrler_constraint.constraints
  s!"== CtrlerConstraint ==\nctrler: ({ctrler})\nconstraints: ({constraints})\n====="
instance : ToString CtrlerConstraint where toString := CtrlerConstraint.toString

--#eval [1,2,3].find? (· == 2)
def PostReceivePathsUniqueConstraints : List CtrlerPathConstraint → List CtrlerPathConstraint → Except String (List CtrlerPathConstraint)
| post_ctrler_constraints, pre_ctrler_constraints => do
  let unique_post_ctrler_constraints : List CtrlerPathConstraint := List.join $ ←
    post_ctrler_constraints.mapM (λ post_ctrler_constraint => do
      let ctrler := post_ctrler_constraint.ctrler
      let pre_ctrler_constraint? : Option CtrlerPathConstraint := pre_ctrler_constraints.find? (·.ctrler == ctrler)
      if let some pre_ctrler_constraint := pre_ctrler_constraint? then
        let post_ctrler_constraints := post_ctrler_constraint.constraints
        let pre_ctrler_constraints := pre_ctrler_constraint.constraints
        -- dbg_trace s!"@@2.5 potential match of constraints: post_ctrler_constraints:({post_ctrler_constraints}) pre_ctrler_constraints:({pre_ctrler_constraints})"

        let path_nodes := post_ctrler_constraint.path
        let constraints_unique_to_post := post_ctrler_constraints.filter (!pre_ctrler_constraints.contains ·)
        pure [({ctrler := ctrler, path := path_nodes, constraints := constraints_unique_to_post} : CtrlerPathConstraint)]
      else
        -- let msg := s!"No pre-ctrler constraint for ctrler:({ctrler}) found in path_constraints:({pre_ctrler_constraints})"
        -- throw msg
        -- dbg_trace s!"@@2.4 No pre-ctrler constraint for ctrler:({ctrler})"
        pure []
    )
  let non_empty_unique_post_constraints := unique_post_ctrler_constraints.filter (·.constraints.length > 0)
  pure non_empty_unique_post_constraints

def CDFG.Node.non_reset_transitions (node : Node) : (Transitions) :=
  node.transitions.filter (·.trans_type != .Reset)

-- AZ TODO NOTE: make this recursively find predecessor states, and return those..
def CDFG.Graph.nodes_transitioning_to_node (graph : Graph) (node : Node)
: (List Node) :=
  graph.nodes.filter (·.basic_transitions.any (·.is_transition_to_state_name node.current_state))

def CDFG.Node.transitions_to_node (node : Node) (dest_node : Node) : Transitions :=
  node.non_reset_transitions.filter (·.is_transition_to_state_name dest_node.current_state)

#eval [[1,2],[2,3]].map (·.filter (let elem := ·; [[1,2],[2,3]].all (·.contains elem) )) |>.join

def List.get_common_elems_from_sub_list (list : List (List CDFG.Condition) ) /-(elem_accessor : α → β)-/ : List CDFG.Condition :=
 list.map (·.filter (let elem := ·; list.all (·.contains elem) )) |>.join

partial def CDFG.Graph.find_await_conds_on_this_node_path (graph : Graph) (node : Node) (visited : List Node)
: Except String (List CDFG.Condition) := do
  -- dbg_trace s!"This Node to find first msging node: ({node.current_state})"
  -- 1. Recursive back track through nodes until we find one not transitioned to
  -- get nodes transitioning to this one
  let nodes_transitioning_to_node : List Node := graph.nodes_transitioning_to_node node
  match nodes_transitioning_to_node with
  | [] => do
    -- dbg_trace s!"Reached end of this ctrler"
    pure $ []
  | _ => do
    -- dbg_trace s!"Still have nodes transitioning to this one: ({nodes_transitioning_to_node.map (·.current_state)})"
   -- if nodes_transitioning_to_node.length > 0 then
    -- recursive search
    -- shouldn't be any cycles in graph, but just in case...
    -- TODO
    let nodes_not_visited : List Node := nodes_transitioning_to_node.filter (λ node' => !(visited.contains node'))
    -- nodes_that_do_have_nodes_transitioning_to_it
    let nodes_that_are_not_the_first_node := nodes_not_visited.filter (graph.nodes_transitioning_to_node · |>.isNotEmpty)
    let first_msging_ctrler_node_list ← nodes_not_visited.mapM (graph.find_await_conds_on_this_node_path · (visited.concat node))

    -- Get all trans to this node
    let trans_to_this_node := nodes_that_are_not_the_first_node.map (·.transitions_to_node node) |>.join
    let await_cond_preds_of_trans := trans_to_this_node.map (·.predicate.filter (·.is_await)) |>.join |>.eraseDups
    -- dbg_trace s!"@@(node: ({node.current_state})) await_cond_preds_of_trans: ({await_cond_preds_of_trans})"
    -- let await_stmts ← await_cond_preds_of_trans.mapM (·.await_stmt)
    -- TODO NOTE: Collect the awaits, return them as 2nd ret val

    -- check to confirm all paths lead to the same node
    match H : first_msging_ctrler_node_list with
    | [] =>
      -- dbg_trace s!">>node: ({node})"
      -- dbg_trace s!">>visited: ({visited})"
      -- dbg_trace s!">>nodes_transitioning_to_node: ({nodes_transitioning_to_node})"
      -- dbg_trace s!">>nodes_not_visited: ({nodes_not_visited})"
      -- dbg_trace s!">>nodes_that_are_not_the_first_node: ({nodes_that_are_not_the_first_node})"
      -- dbg_trace s!">>first_msging_ctrler_node_list: ({first_msging_ctrler_node_list})"
      throw s!"Error: No first msging ctrler node found. Node: ({node}).\nNodes transitioning to node: ({nodes_transitioning_to_node})"
    | a::as => do
      have one_or_more : 0 < first_msging_ctrler_node_list.length := by simp[Nat.zero_lt_succ, H]
      let all_same_node := first_msging_ctrler_node_list.all (· == (first_msging_ctrler_node_list[0]'one_or_more))
      match all_same_node with
      | true =>
        -- want to ret all common await conditions
        let list_await_cond_list : List (List CDFG.Condition) := first_msging_ctrler_node_list.map (·)
        let common_await_conds := list_await_cond_list.get_common_elems_from_sub_list
        pure (await_cond_preds_of_trans ++ common_await_conds)
      | false => throw "Error: Not all paths lead to the same node"

partial def CDFG.Graph.first_msging_ctrler_node_from_node (graph : Graph) (node : Node) (visited : List Node)
: Except String (Node × List CDFG.Condition) := do
  -- dbg_trace s!"This Node to find first msging node: ({node.current_state})"
  -- 1. Recursive back track through nodes until we find one not transitioned to
  -- get nodes transitioning to this one
  let nodes_transitioning_to_node : List Node := graph.nodes_transitioning_to_node node
  match nodes_transitioning_to_node with
  | [] => do
    -- dbg_trace s!"Reached end of this ctrler"
    -- finished search, and can call fn to get other ctrler msging this one
    -- get when predicates, find msg from other ctrler
    let basic_transitions := node.transitions.filter (·.trans_type == .Transition)
    let await_predicates := List.join $ basic_transitions.map (·.predicate.filter (match · with | .AwaitCondition _ => true | _ => false))
    -- match await_predicate to sending ctrler node
    -- return the node
    match H : await_predicates with
    | [] => do
      throw s!"Error: No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
    | a::as => do
      -- the first message pass to the node is the "first" message that starts this ctrler's state machine
      -- assuming I'm adding to the predicate list in the order of the stmts
      have one_or_more : await_predicates.length > 0 := by simp[Nat.zero_lt_succ, H]
      let await_pred := await_predicates[0]'one_or_more
      let found_node_that_msgs_this? : Option Node := ← await_pred.await_pred's_sending_node graph |>.throw_exception_nesting_msg s!"Error in finding the first msging ctrler node from node ({node.current_state})"
      if let some found_node_that_msgs_this := found_node_that_msgs_this? then
        pure (found_node_that_msgs_this, [])
      else
        throw s!"Error: Couldn't find a node that msgs this 'first' node of a ctrler: ({node.current_state})"
  | _ => do
    -- dbg_trace s!"Still have nodes transitioning to this one: ({nodes_transitioning_to_node.map (·.current_state)})"
   -- if nodes_transitioning_to_node.length > 0 then
    -- recursive search
    -- shouldn't be any cycles in graph, but just in case...
    -- TODO
    let nodes_not_visited : List Node := nodes_transitioning_to_node.filter (λ node' => !(visited.contains node'))
    -- nodes_that_do_have_nodes_transitioning_to_it
    let nodes_that_are_not_the_first_node := nodes_not_visited.filter (graph.nodes_transitioning_to_node · |>.isNotEmpty)
    let first_msging_ctrler_node_list ← nodes_not_visited.mapM (graph.first_msging_ctrler_node_from_node · (visited.concat node))

    -- Get all trans to this node
    let trans_to_this_node := nodes_that_are_not_the_first_node.map (·.transitions_to_node node) |>.join
    let await_cond_preds_of_trans := trans_to_this_node.map (·.predicate.filter (·.is_await)) |>.join |>.eraseDups
    -- dbg_trace s!"@@(node: ({node.current_state})) await_cond_preds_of_trans: ({await_cond_preds_of_trans})"
    -- let await_stmts ← await_cond_preds_of_trans.mapM (·.await_stmt)
    -- TODO NOTE: Collect the awaits, return them as 2nd ret val

    -- check to confirm all paths lead to the same node
    match H : first_msging_ctrler_node_list with
    | [] =>
      -- dbg_trace s!">>node: ({node})"
      -- dbg_trace s!">>visited: ({visited})"
      -- dbg_trace s!">>nodes_transitioning_to_node: ({nodes_transitioning_to_node})"
      -- dbg_trace s!">>nodes_not_visited: ({nodes_not_visited})"
      -- dbg_trace s!">>nodes_that_are_not_the_first_node: ({nodes_that_are_not_the_first_node})"
      -- dbg_trace s!">>first_msging_ctrler_node_list: ({first_msging_ctrler_node_list})"
      throw s!"Error: No first msging ctrler node found. Node: ({node}).\nNodes transitioning to node: ({nodes_transitioning_to_node})"
    | a::as => do
      have one_or_more : 0 < first_msging_ctrler_node_list.length := by simp[Nat.zero_lt_succ, H]
      let all_same_node := first_msging_ctrler_node_list.all (·.1 == (first_msging_ctrler_node_list[0]'one_or_more).1)
      match all_same_node with
      | true =>
        -- want to ret all common await conditions
        let list_await_cond_list : List (List CDFG.Condition) := first_msging_ctrler_node_list.map (·.2)
        let common_await_conds := list_await_cond_list.get_common_elems_from_sub_list
        pure ((first_msging_ctrler_node_list[0]'one_or_more).1, await_cond_preds_of_trans ++ common_await_conds)
      | false => throw "Error: Not all paths lead to the same node"

partial def CDFG.Graph.first_msging_ctrler_node_from_node? (graph : Graph) (node : Node) (visited : List Node)
: Except String (Option (Node × List CDFG.Condition)) := do
  -- 1. Recursive back track through nodes until we find one not transitioned to
  -- get nodes transitioning to this one
  let nodes_transitioning_to_node : List Node := graph.nodes_transitioning_to_node node
  match nodes_transitioning_to_node with
  | [] => do
    -- finished search, and can call fn to get other ctrler msging this one
    -- get when predicates, find msg from other ctrler
    let basic_transitions := node.transitions.filter (·.trans_type == .Transition)
    let await_predicates := List.join $ basic_transitions.map (·.predicate.filter (match · with | .AwaitCondition _ => true | _ => false))
    -- match await_predicate to sending ctrler node
    -- return the node
    match H : await_predicates with
    | [] => do
      -- throw s!"Error: No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
      -- dbg_trace s!"No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
      pure Option.none
    | a::as => do
      -- the first message pass to the node is the "first" message that starts this ctrler's state machine
      -- assuming I'm adding to the predicate list in the order of the stmts
      have one_or_more : await_predicates.length > 0 := by simp[Nat.zero_lt_succ, H]
      let await_pred := await_predicates[0]'one_or_more
      let found_node_that_msgs_this? : Option Node := ← await_pred.await_pred's_sending_node graph |>.throw_exception_nesting_msg s!"Error in finding the first msging ctrler node from node ({node.current_state})"
      if let some found_node_that_msgs_this := found_node_that_msgs_this? then
        pure $ some (found_node_that_msgs_this, [])
      else
        throw s!"Error: Couldn't find a node that msgs this 'first' node of a ctrler: ({node.current_state})"
  | _ => do
   -- if nodes_transitioning_to_node.length > 0 then
    -- recursive search
    -- shouldn't be any cycles in graph, but just in case...
    -- TODO
    let nodes_not_visited : List Node := nodes_transitioning_to_node.filter (λ node' => !(visited.contains node'))
    let first_msging_ctrler_node_list ← nodes_not_visited.mapM (graph.first_msging_ctrler_node_from_node · (visited.concat node))

    let trans_to_this_node := nodes_not_visited.map (·.transitions_to_node node) |>.join
    let await_cond_preds_of_trans := trans_to_this_node.map (·.predicate.filter (·.is_await)) |>.join |>.eraseDups
    -- check to confirm all paths lead to the same node
    match first_msging_ctrler_node_list with
    | [] =>
      -- dbg_trace s!">>node: ({node})"
      -- dbg_trace s!">>visited: ({visited})"
      -- dbg_trace s!">>nodes_transitioning_to_node: ({nodes_transitioning_to_node})"
      -- dbg_trace s!">>nodes_not_visited: ({nodes_not_visited})"
      -- dbg_trace s!">>first_msging_ctrler_node_list: ({first_msging_ctrler_node_list})"
      throw s!"Error: No first msging ctrler node found. Node: ({node}).\nNodes transitioning to node: ({nodes_transitioning_to_node})"
    | _ => do
      let all_same_node := first_msging_ctrler_node_list.all (·.1 == first_msging_ctrler_node_list[0]!.1)
      match all_same_node with
      | true =>
        let list_await_cond_list : List (List CDFG.Condition) := first_msging_ctrler_node_list.map (·.2)
        let common_await_conds := list_await_cond_list.get_common_elems_from_sub_list
        pure $ some (first_msging_ctrler_node_list[0]!.1, await_cond_preds_of_trans ++ common_await_conds)
      | false => throw "Error: Not all paths lead to the same node"

def CDFG.Condition.is_predicated_by_is_head_api (cond : Condition) : Bool :=
  match cond with
  | .DSLExpr cond_expr => -- recursive search for if there's a function call in any term
    -- dbg_trace s!"Checking if cond_expr is pred head: ({cond_expr})"
    cond_expr.is_contains_is_head_api
  | _ => false

-- NOTE: This is specifically for transitions that send a message to a given ctrler_name
def CDFG.Node.is_complete_trans_pred_is_head (node : Node ) (ctrler_name : CtrlerName)
: Except String Bool := do
  let transitions_completes : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans_msging_ctrler := ← transitions_completes.filterM (·.messages.anyM (·.is_dest_equals ctrler_name))
  -- dbg_trace s!"Trans_msging_ctrler: ({trans_msging_ctrler}), Ctrler: ({ctrler_name})"
  pure $ trans_msging_ctrler.all (·.predicate.any CDFG.Condition.is_predicated_by_is_head_api)

def CDFG.Transitions.is_all_pred_is_head (transitions : Transitions) : Bool :=
  transitions.all (·.predicate.any CDFG.Condition.is_predicated_by_is_head_api)

def CDFG.Node.is_compls_trans_pred_is_head (node : Node ) : Bool :=
  let transitions := node.transitions.filter (·.trans_type == .Transition)
  let completions := node.transitions.filter (·.trans_type == .Completion)
  let compls_trans : Transitions := transitions ++ completions
  compls_trans.is_all_pred_is_head

def CDFG.Node.ctrler_of_node (node : Node) (ctrlers : List controller_info) : Except String controller_info :=
  get_ctrler_from_ctrlers_list node.ctrler_name ctrlers

-- Generalised
mutual
partial def CDFG.Node.is_trans_to_state_name_pred_by_provided_cond
(node : Node) (state_name : StateName) (graph : Graph) (cond_check : Condition → Bool)
: Bool :=
  let trans_to_given_state := node.transitions.filter (·.dest_state == state_name)
  let is_pred_is_head := trans_to_given_state.all (·.predicate.any (cond_check ·))
  if is_pred_is_head then
    true
  else
    node.is_all_paths_to_node_predicated_by_provided_cond graph cond_check

partial def CDFG.Node.is_all_paths_to_node_predicated_by_provided_cond (node : Node) (graph : Graph) (cond_check : Condition → Bool)
: Bool :=
  -- 1. Get all nodes that transition to this one
  let nodes_transitioning_to_node : List Node := graph.nodes_transitioning_to_node node
  -- 2. Check if all paths to this node are predicated is_head
  match nodes_transitioning_to_node with
  | [] => false
  | _ =>
    nodes_transitioning_to_node.all (·.is_trans_to_state_name_pred_by_provided_cond node.current_state graph cond_check)
end

def CDFG.Node.is_msg_in_order (node : Node) (graph : Graph) (ctrlers : List controller_info)
: Except String Bool := do
  -- 1. Check ctrler info
  let ctrler ← node.ctrler_of_node ctrlers
  let ctrler_type ← ctrler.type
  -- If just ctrler, return false
  -- If FIFO, do a recursive check if transitions
  -- If Unordered queue, check if search on seq_num less than given seq_num (if true then return true, if false ret false)
  match ctrler_type with
  | .BasicCtrler => pure false
  | .FIFO => -- trace back and see if all transition paths are predicated is_head
    pure $ node.is_all_paths_to_node_predicated_by_provided_cond graph CDFG.Condition.is_predicated_by_is_head_api
  | .Unordered => do
    -- pure $ node.is_all_paths_to_node_predicated_by_provided_cond graph
    -- NOTE: Skip for now. Not needed for the LSQs right now.
    -- Would need t owrite something like: is_self_search_older_seq_num_success fn in AnalysisHelpers
    pure false

-- just a copy for referece..
-- def CDFG.Node.is_complete_trans_pred_is_head (node : Node ) (ctrler_name : CtrlerName)
-- : Except String Bool := do
--   let transitions_completes : Transitions := node.transitions.filter (·.trans_type != .Reset)
--   let trans_msging_ctrler := ← transitions_completes.filterM (·.messages.anyM (·.is_dest_equals ctrler_name))
--   pure $ trans_msging_ctrler.all (·.predicate.any CDFG.Condition.is_predicated_by_is_head_api)

def CDFG.Graph.nodes_not_reset_trans_to_node (graph : Graph) (node : Node) : (List Node) :=
  let nodes_trans_to_this := graph.nodes.filter (·.not_reset_transitions.any (·.dest_state == node.current_state))
  nodes_trans_to_this

--NOTE: i.e. transition awaits on msg from another ctrler in PO
def CDFG.Node.is_trans_with_msg_to_node_also_pred_PO_by_await (node : Node) (msg_dest_node : Node)
: Except String Bool := do
  let transitions_completes : Transitions := node.non_reset_transitions
  let trans_that_send_msg_to_node ← transitions_completes.filterM (·.messages.anyM (·.is_awaited_by_node msg_dest_node node.ctrler_name))

  pure (trans_that_send_msg_to_node.length > 0)

-- -- i.e. Check if nodes transitioning to this node are pred is_head. There must be .  recursively
-- def CDFG.Node.is_recursive_pred_is_head_PO (node : Node) (graph : Graph) : Except String Bool := do
--   -- 1. recursively check if nodes transitioning to this node (and the transitions to this node specifically)
--   -- are pred is_head
--   let nodes_trans_not_reset_to_this_node := graph.nodes_not_reset_trans_to_node node
--   -- if not empty
--   -- Get transitions from nodes that trans to this node, check if all are is_head
--   -- ret true
--   -- if empty
--   -- ret false

--   -- 2. Also check if this node is pred by await-when msgs that are from FIFO ctrlers that are pred is_head
--   -- i.e. recursively do what is_node_inserted_to_in_PO does in step 1.
--   -- let 

--   -- This is an OR relationship; so long as one path forces this node to progress in PO, then it's PO
--   -- copilot is dumb
  
--   -- remember to remove
--   return default

def CDFG.Node.is_non_reset_trans_pred_is_head (node : Node) (prev_node : StateName) : Except String Bool := do
  let transitions_completes : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans_msging_ctrler := transitions_completes.filter (·.dest_state == prev_node)
  -- dbg_trace s!"finding transitions that are pred is_head: ({trans_msging_ctrler}), from: ({prev_node}) to: ({node.current_state})"
  pure $ trans_msging_ctrler.all (·.predicate.any CDFG.Condition.is_predicated_by_is_head_api)

partial def CDFG.Graph.is_transition_pred_is_head (graph : Graph) (node : Node) (prev_node : StateName) : Except String Bool := do
  let is_node_trans_to_prev_node_pred_is_head : Bool ← node.is_non_reset_trans_pred_is_head prev_node 

  match is_node_trans_to_prev_node_pred_is_head with
  | true => do pure true
  | false => do
    let nodes_trans_to_this := graph.nodes_transitioning_to_node node
    let any_of_those_nodes_pred_is_head := nodes_trans_to_this.anyM (graph.is_transition_pred_is_head · node.current_state)
    any_of_those_nodes_pred_is_head

partial def CDFG.Graph.is_complete_trans_pred_is_head (graph : Graph) (node : Node) (ctrler_name : CtrlerName)
: Except String Bool := do
  -- dbg_trace s!"Start check for if node: ({node.current_state}) is pred is_head"
  let is_node_pred_is_head : Bool ← node.is_complete_trans_pred_is_head ctrler_name

  match is_node_pred_is_head with
  | true => do
    -- dbg_trace s!"node: ({node.current_state}) node was pred head?"
    pure true
  | false => do
    -- dbg_trace s!"node: ({node.current_state}) node was not pred head, checking if any of the nodes transitioning to this node are pred head"
    let nodes_trans_to_this := graph.nodes_transitioning_to_node node
    let any_of_those_nodes_pred_is_head := nodes_trans_to_this.anyM (graph.is_transition_pred_is_head · node.current_state)
    any_of_those_nodes_pred_is_head
  -- termination_by _ =>  

#eval [some 1, some 2].filterMap id

partial def CDFG.Graph.is_node_inserted_to_in_PO (graph : Graph) (node : Node) (ctrlers : List controller_info)
: Except String Bool := do
  -- dbg_trace s!"<<1 heuristic search"
  -- dbg_trace s!"node_name,1: ({node.current_state})"
  -- 1. Recursive back track through nodes until we find one not transitioned to, get node that msgs it
  let msging_node_of_input_node? : Option (Node × List CDFG.Condition) ← graph.first_msging_ctrler_node_from_node? node []

  -- let 
  -- sorry
  -- 2. Check transitions predicated on msg from other ctrler
    -- AZ NOTE: Heuristic should check there's no Unordered queue in the path, and at least 1 ordered FIFO pred by is_head
  if let some (msging_node_of_input_node, await_conds) := msging_node_of_input_node? then
    -- NOTE: Use OR between these two conditions: either recursively the node is pred is_head, or the node has a transition that msgs this node that is pred is_head
    -- let is_'insert'_msging_node_trans_pred_is_head : Bool ← msging_node_of_input_node.is_complete_trans_pred_is_head node.ctrler_name
    let is_'insert'_msging_node_trans_pred_is_head : Bool ← graph.is_complete_trans_pred_is_head msging_node_of_input_node node.ctrler_name

    -- Check if the nodes that msg this ctrler from this path are pred by is_head
    -- Make a recursive call using this function on each node
    -- Take the awaits and find the nodes that msg them
    -- use those nodes for the recursive call

    -- NOTE: These nodes of course are from other ctrler(s)..
    let nodes_that_send_msg_to_the_await_conds? : List (Option Node) ← await_conds.mapM (·.await_pred's_sending_node graph |>.throw_exception_nesting_msg s!"Error while trying to check if a node ({node.current_state}) is inserted in PO")
    let nodes_that_send_msg_to_the_await_conds := nodes_that_send_msg_to_the_await_conds?.filterMap id
    -- Because the other nodes are pred into being PO, this is PO as well if this is pred on them
    -- by transitivity
    let msging_nodes_are_pred_is_head_PO : Bool ← nodes_that_send_msg_to_the_await_conds.anyM (graph.is_node_inserted_to_in_PO · ctrlers)
    -- the below check is okay, due to the above msg from other ctrler check.
    let is_'insert'_msging_node_ctrler_node_pred_is_head : Bool ← graph.is_node_inserted_to_in_PO msging_node_of_input_node ctrlers

    -- Next thing is to fix this..
    let is_node_transition_path_PO : Bool ← msging_node_of_input_node.is_msg_in_order graph ctrlers
    -- 3. Do a recursive back track through nodes, checking for transitions that are pred by is_head
    -- dbg_trace s!"!!1 The node that 'inserts' into this ctrler: ({msging_node_of_input_node})"
    -- dbg_trace s!"!!1 Is msging 'insert' node pred is_head(): ({is_'insert'_msging_node_trans_pred_is_head})"
    -- dbg_trace s!"!!1 Is node transition path PO: ({is_node_transition_path_PO})"
    -- dbg_trace s!"!!1 Is there a msging node that's on a PO path: ({msging_nodes_are_pred_is_head_PO})"

    -- Case 1. This current ctrler is pred is_head / PO. Look back recursively and ensure all ctrlers on the path are is_head / PO
    if is_'insert'_msging_node_trans_pred_is_head || is_node_transition_path_PO then
      let prev_nodes_are_also_inserted_to_in_PO : Bool ← graph.is_node_inserted_to_in_PO msging_node_of_input_node ctrlers
      pure (prev_nodes_are_also_inserted_to_in_PO)
    else if msging_nodes_are_pred_is_head_PO || is_'insert'_msging_node_ctrler_node_pred_is_head then -- Case 2. a node that msgs this node's path is pred is_head and is PO, thus by transitivity this node is also PO
      -- continued: Or, the 'inserting' node is msg'd and that trail is pred is_head / PO
      pure true
    else -- Case 3.
      -- recursive search
      pure false
  else
    pure true

def CDFG.Transitions.transitioned_to_states_within_graph : Transitions → Graph → List Node 
| transitions, graph =>
  let transitioned_to_states : List StateName := transitions.map (·.dest_state)
  let trans'd_to_nodes := graph.nodes.filter (·.current_state ∈ transitioned_to_states)
  trans'd_to_nodes

#eval 1 ∈ [3,2,1]  

partial def CDFG.Graph.downstream_messaged_states : Graph → Graph → Node → InstType → Except String (List Node)
| total_graph, post_receive_graph, curr_node, inst_type => do
  let unique_msg'd_states ← curr_node.unique_msg'd_states_not_pred_by_other_insts total_graph inst_type |>.throw_exception_nesting_msg s!"Error finding downstream messaged states. Current Node: ({curr_node.current_state})) Ctrler: ({curr_node.ctrler_name})"
  let msg'd_states := unique_msg'd_states.map (·.1.current_state)
  let msg'd_states_in_post_receive_graph : List Node := post_receive_graph.nodes.filter (·.current_state ∈ msg'd_states)
  -- pure msg'd_states_in_post_receive_graph
  let basic_transitions : Transitions := curr_node.transitions.filter (·.trans_type == .Transition)
  let trans'd_states := basic_transitions.transitioned_to_states_within_graph post_receive_graph

  let downstream_msg'd_states := List.join $ ← (trans'd_states.mapM (total_graph.downstream_messaged_states post_receive_graph · inst_type))
  let all_downstream_msg'd_states := msg'd_states_in_post_receive_graph ++ downstream_msg'd_states
  pure all_downstream_msg'd_states

def get_min_from_tuple_nat_list_CtrlerStates (lst : List (CtrlerStates × Nat)) : (CtrlerStates × Nat) :=
  match lst with
  | h::[] => h
  | h::t =>
    let (n, nat) := h
    let (n', nat') := (get_min_from_tuple_nat_list_CtrlerStates t)
    if (nat) <= nat' then (n,nat) else (n',nat')
  | [] => (default,0)

-- NOTE: Don't need pre-receive states...
partial def CDFGCtrlerStatesToStallOnThatAreSeparateFromPreReceiveStates
(post_receive_graph : Graph) (pre_receive_graph : Graph) (total_graph : Graph) (receive_mem_resp_or_later_state : StateName) (ctrlers : Ctrlers)
(dist : Distance) (inst_type : InstType)
: Except String (Option (CtrlerStates × Distance)) := do
  -- Check the state we're starting at, if:
  -- 1. if the ctrler is a queue
  -- 2. if it's a queue, is it inserted into in PO
  -- 3. will it be already in this queue while younger insts are at the stall state i.e. 'point a'?

  -- dbg_trace s!"!!7"
  let node ← post_receive_graph.node_from_name! receive_mem_resp_or_later_state
  -- 1.
  let ctrler ← ctrlers.ctrler_from_name node.ctrler_name
  let is_a_queue : Bool := (← ctrler.type).is_a_queue

  -- 2.
  let node_is_inserted_into_in_PO ← total_graph.is_node_inserted_to_in_PO node ctrlers
  -- dbg_trace s!"!!Is node ({node.current_state}) inserted into in PO? ({node_is_inserted_into_in_PO})"

  -- 3.
  -- let ctrler_states := total_graph.nodes.filter (·.ctrler_name == node.ctrler_name)
  let ctrler_post_states := post_receive_graph.nodes.filter (·.ctrler_name == node.ctrler_name)
  let ctrler_pre_states := pre_receive_graph.nodes.filter (·.ctrler_name == node.ctrler_name)

  -- let ctrler_states_are_not_all_pre : Bool := ctrler_states.any (! ctrler_pre_states.contains ·)
  let there_are_post_states : Bool := ctrler_post_states.length > 0
  let there_are_pre_states : Bool := ctrler_pre_states.length > 0

  let node_is_already_in_queue_pre_states_approximation := /- ctrler_states_are_not_all_pre && -/ there_are_post_states && there_are_pre_states

  -- if yes, return the post states not in the pre states
  if is_a_queue && node_is_inserted_into_in_PO && node_is_already_in_queue_pre_states_approximation then
    -- let ctrler_post_states_not_in_pre_states := ctrler_post_states.filter (! ctrler_pre_states.contains ·)
    -- let this_ctrler_and_states := { ctrler := ctrler.name, states := ctrler_post_states_not_in_pre_states.map (·.current_state)}
    let this_ctrler_and_states := { ctrler := ctrler.name, states := ctrler_post_states.map (·.current_state)}
    pure $ some (this_ctrler_and_states, dist)
  else
  -- if no, then do a recursive search on msg'd states, and recursive search on all of them
    let states_msg'd_by_this_ctrler's_post_receive_states ← total_graph.downstream_messaged_states post_receive_graph node inst_type
    let states_msg'd := states_msg'd_by_this_ctrler's_post_receive_states.map (·.current_state)
    let downstream_ctrlers ← states_msg'd.mapM (CDFGCtrlerStatesToStallOnThatAreSeparateFromPreReceiveStates post_receive_graph pre_receive_graph total_graph · ctrlers (dist + 1) inst_type)
    let just_downstream_ctrlers := downstream_ctrlers.filter (·.isSome)
    match just_downstream_ctrlers with
    | [] =>
      -- if is_a_queue then
      --   pure $ some (⟨node.ctrler_name, []⟩,0)
      --   -- Could probably do a 3rd case; If basic ctrler, then 
      -- else
        pure none
    | [one] => pure (one)
    | list => -- choose the closest one from
      let ctrler_states_dist := list.map (·.get!)
      let min := get_min_from_tuple_nat_list_CtrlerStates ctrler_states_dist
      pure $ some min

def nat_from_node_nat (node_nat : Node × Nat) : Nat := node_nat.2

#eval [1,2].maximum?

def get_min_from_α_nat_list {α : Type } (lst : List (α × Nat)) : Except String (α × Nat) := do
  match lst with
  | h::[] => pure h
  | h::t =>
    let (n, nat) := h
    let (n', nat') ← (get_min_from_α_nat_list t)
    if (nat) <= nat' then pure (n,nat) else pure (n',nat')
  | [] => throw "Error: Empty List"

def CDFG.Graph.queue_ctrler_distance_from_node (graph : Graph) (start : Node) (ctrlers : Ctrlers) (inst_type : InstType)
: Except String (List (CtrlerName × Distance)) := do
  let nodes_labelled_by_msg_distance : List (StateName × Distance) ← graph.labelNodesByMessageDistance start 0 inst_type none
  -- dbg_trace s!"!!8"
  let node_nodes_by_dist : List (Node × Distance) ← nodes_labelled_by_msg_distance.mapM (λ (ctrler_name, dist) => do pure (← graph.node_from_name! ctrler_name , dist))
  let ctrler_names := graph.nodes.map (·.ctrler_name) |> List.eraseDups

  -- filter away ctrler_names that are not queues
  let queue_ctrler_names : List CtrlerName := ← ctrler_names.filterM (λ ctrler_name => do pure $ (← (← ctrlers.ctrler_from_name ctrler_name).type) != .BasicCtrler)

  let ctrler_node_distances : List (CtrlerName × List (Node × Distance)) := queue_ctrler_names.map ( λ ctrler_name => (ctrler_name, node_nodes_by_dist.filter (·.1.ctrler_name == ctrler_name)) )
  let ctrler_distances? := ctrler_node_distances.map (λ (ctrler, node_dist) => (ctrler, node_dist.map (nat_from_node_nat) |> List.maximum? ))
  let all_ctrlers_have_dist := (ctrler_distances?.all (λ (_,dist?) => dist?.isSome) )
  if all_ctrlers_have_dist then
    let ctrler_distances : List ( CtrlerName × Distance ) := ← ctrler_distances?.mapM (λ (ctrler, dist?) => do
      let dist ← match dist? with | some dist => pure dist | none => do throw "Error, Ctrler should have a distance Nat."
      pure (ctrler, dist)
      )
    pure ctrler_distances
  else
    throw "Error, all ctrlers should have a distance Nat. This is because the ctrler_names are obtained from Nodes × Dist, which should have a dist "

def min_dist_ctrler (ctrler_dist : List (CtrlerName × Distance)) (ctrler_names : List CtrlerName) : Except String CtrlerName := do
  let relevant_ctrler_dist := ctrler_dist.filter (ctrler_names.contains ·.1)
  let (min_dist_ctrler, /- dist -/ _) ← get_min_from_α_nat_list relevant_ctrler_dist 
  pure min_dist_ctrler

-- def CDFG.Graph.earliest_node_by_msg_dist_from_node (graph : Graph) (start_node : Node) (nodes : List Node)
-- : Except String Node := do
--   /- Then from here traverse the graph from the start node -/
--   /- Produce Labels for each state of "Message Distance" and "State Distance" -/
--   let labelled_by_msg_distance : (List (StateName × Distance)) ←
--     graph.labelNodesByMessageDistance start_node.current_state 0
--   dbg_trace s!"labelled_by_msg_distance: {labelled_by_msg_distance}"
  
--   let receive_states_and_transitions_labelled : List (CDFG.Node × Nat) :=
--     nodes.map (λ (node) =>
--       let matching_nodes : List (String × Nat) :=
--         labelled_by_msg_distance.filter (λ (state_name, /- msg_distance -/ _) =>
--           state_name == node.current_state
--         )
--       let msg_distances : List Nat :=
--         matching_nodes.map (λ (/- state_name -/ _, msg_distance) => msg_distance)
--       let max_distance : Nat :=
--         get_max_from_nat_list msg_distances
--       (node, max_distance)
--     )
    
--   /- Sort by Message Distance. If multiple, sort by state distance -/
--   -- Ignore for now. Just take the first one
--   let (first_receive_node, _) :=
--     get_min_from_tuple_nat_list receive_states_and_transitions_labelled
--   dbg_trace s!"receive_states_and_transitions_labelled: {receive_states_and_transitions_labelled}"
--   dbg_trace s!"first_receive_node: {first_receive_node}"

--   pure first_receive_node

def CtrlerConstraintsToCtrlerStateConstraintExpr
(post_receive_graph : CDFG.Graph)
(ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states : List CtrlerConstraint)
: Except String CtrlerStateExpr := do
  -- NOTE: Use this in the func for creating the ctrler/state/constraint info for the constraint cases above
  -- dbg_trace s!"All ctrler constraints unique to post receive: ({ctrler_constraints_unique_to_post_receive})"
  -- Simplification on item 5, assume if ctrler has states in both post & pre receive, consider it for query
  let ctrler_constraints : CtrlerConstraint := ← 
    match ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states with
    | [] =>
      -- dbg_trace s!"Unique post-receive constraints: ({ctrler_constraints_unique_to_post_receive})"
      throw "Trying to implement this algorithm and only use constraints unique to post receive states."
    | ctrler_constraints :: _ => pure ctrler_constraints

  let ctrler_constraint_node := post_receive_graph.nodes.find? (·.transitions.any (·.constraint_info.any (ctrler_constraints.constraints.contains ·) ) )
  -- return the ctrler & constraints to stall on,
  -- assuming the search on the ctrler will for any older inst of the right type
  if let some node := ctrler_constraint_node then
    pure $ ({ctrler := ctrler_constraints.ctrler, state := node.current_state, constraints := (← ctrler_constraints.constraints.mapM ConstraintToBool ) } : CtrlerStateExpr)
  else
    throw "Couldn't find ctrler constraint node with the unique constraint?"

def CDFG.Transitions.progression_transitions_are_any_await_transitions : Transitions → Bool
| transitions =>
  let progression_transitions := transitions.filter (·.trans_type != .Reset )
  -- NOTE: TODO later: filter out handle transitions
  -- let non_handle_transitions := progression_transitions.filter (· )
  progression_transitions.any (·.predicate.any (·.is_await))

def CDFG.Node.is_await_state : Node → Bool
| node =>
  node.transitions.progression_transitions_are_any_await_transitions

def CDFG.Graph.nodes_basic_transitioning_to_node (graph : Graph) (node : Node)
: (List Node) :=
  graph.nodes.filter (λ node' => (node'.transitions.filter (·.trans_type == .Transition)).any (·.is_transition_to_state_name node.current_state))

def CDFG.Node.do_other_states_basic_transition_to_this_state (node : Node) (graph : Graph) : Bool :=
  let nodes_basic_transitioning_to_node := graph.nodes_basic_transitioning_to_node node
  match nodes_basic_transitioning_to_node with
  | [] => false
  | _ => true

def CtrlerStates.prune_pre_receive_await_states (ctrler_states : CtrlerStates) (post_receive_graph : Graph)
: Except String CtrlerStates := do
  -- 1. Identify "await" states in the ctrler_states
  -- 2. Check if any other states basic-transition to them
  -- 3. If so, remove them from the ctrler_states

  -- 1.
  let ctrler_nodes : List CDFG.Node := post_receive_graph.nodes.filter (·.ctrler_name == ctrler_states.ctrler )
  let ctrler_graph : Graph := { nodes := ctrler_nodes }
  -- dbg_trace s!"!!9"
  let states ← ctrler_states.states.mapM (ctrler_graph.node_from_name! ·)
  let await_states := states.filter (·.is_await_state)
  -- 2. & 3.
  let non_trans'd_to_await_states := await_states.filter (! ·.do_other_states_basic_transition_to_this_state ctrler_graph)
  let non_trans'd_to_await_state_names := non_trans'd_to_await_states.map (·.current_state)

  let pruned_state_names := ctrler_states.states.filter (! non_trans'd_to_await_state_names.contains ·)
  pure {ctrler := ctrler_states.ctrler, states := pruned_state_names}

def CtrlerStates.prune_states_with_no_complete_path (ctrler_states : CtrlerStates) (post_receive_graph : Graph) (inst_type : InstType)
: Except String CtrlerStates := do
  -- 1.
  -- dbg_trace s!"States Before pruning non-complete-path states: {ctrler_states.states}"
  let ctrler_nodes : List CDFG.Node := post_receive_graph.nodes.filter (·.ctrler_name == ctrler_states.ctrler )
  let ctrler_nodes_on_path_that_reach_complete ← ctrler_nodes.filterM (·.is_node_reaches_complete post_receive_graph inst_type [])
  let ctrler_node_names_that_reach_complete := ctrler_nodes_on_path_that_reach_complete.map (·.current_state)
  -- dbg_trace s!"States after pruning non-complete-path states: {ctrler_node_names_that_reach_complete}"
  pure {ctrler := ctrler_states.ctrler, states := ctrler_node_names_that_reach_complete}

def CtrlerStates.prune_pre_receive_await_and_no_complete_path_states (ctrler_states : CtrlerStates) (post_receive_graph : Graph) (inst_type : InstType)
: Except String CtrlerStates := do
  (← ctrler_states.prune_states_with_no_complete_path post_receive_graph inst_type).prune_pre_receive_await_states post_receive_graph
  -- let pruned_no_pre_await_states ← ctrler_states.prune_pre_receive_await_states post_receive_graph

def CtrlerPathConstraint.ctrler_constraints : CtrlerPathConstraint → CtrlerConstraint
| .mk ctrler /- path -/ _ constraints =>
  {ctrler := ctrler, constraints := constraints}

-- TODO: finish
-- One of the Main funcs.
def find_ctrler_or_state_to_query_for_stall (graph : Graph) (inst_to_check_completion : InstType) (ctrlers : List controller_info) (inst_type : InstType)
: Except String (Sum CtrlerStates CtrlerStateExpr) := do
  -- dbg_trace s!"<< Starting find_ctrler_or_state_to_query_for_stall"
  let receive_state_to_search_from : Node ←
    graph.global_complete_node_of_inst_type inst_to_check_completion

  /- Use res, find all post-'receive' states -/
  let post_receive_states_with_dups : (List Node) :=
      (← (graph.findNodesReachableByTransitionAndMessage receive_state_to_search_from inst_type none)).concat receive_state_to_search_from
  let post_receive_states : (List Node) := post_receive_states_with_dups.eraseDups
  -- dbg_trace s!">> post_receive_states: ({post_receive_states})"

  /- Use post-'receive' states, find all pre-'receive' states -/
  -- let pre_receive_states : (List Node) :=
  -- NOTE: This is the "starting point" state
  let not_transitioned_or_messaged_state : Node ← graph.not_trans'd_or_msg'd_node /- inst_type -/ |>.throw_exception_nesting_msg s!"Error when finding ctrler/state to query for stall in graph: ({graph.nodes.map (·.qualified_state_name)})"
  let pre_receive_states : (List Node) ← 
        (graph.preReceiveStates not_transitioned_or_messaged_state [receive_state_to_search_from.current_state] post_receive_states inst_type none)

  let post_receive_graph : Graph := { nodes := post_receive_states }
  -- dbg_trace s!"@@@-1 post_receive_graph: ({post_receive_graph})"
  let receive_state_name : StateName := receive_state_to_search_from.current_state
-- : StateName → Graph → CtrlerPathConstraint → Option Message → Graph → Except String (List CtrlerPathConstraint) 
  -- dbg_trace s!"@@@0 receive_state_name: ({receive_state_name})"
  let post_receive_states_paths : List CtrlerPathConstraint ← 
    post_receive_graph.ctrler_trans_paths_and_constraints receive_state_name ( CtrlerPathConstraint.new_path_of_ctrler receive_state_name ) none inst_type
  -- dbg_trace s!"@@@1 post_receive_paths: ({post_receive_states_paths})"
  let canonized_post_receive_paths : List CtrlerPathConstraint := CanonStatePathsApprox post_receive_states_paths
  -- dbg_trace s!"@@@1.1 canon post_receive_paths: ({canonized_post_receive_paths})"


  -- dbg_trace s!">> pre_receive_states: ({pre_receive_states})"
  let pre_receive_graph : Graph := { nodes := pre_receive_states }
  -- dbg_trace s!"@@@1.5 pre_receive_graph: ({pre_receive_graph.node_names})"
  let pre_receive_states_paths : List CtrlerPathConstraint ← 
    pre_receive_graph.pre_receive_states_constraints not_transitioned_or_messaged_state.current_state ( CtrlerPathConstraint.new_path_of_ctrler receive_state_name ) none post_receive_graph inst_type
  -- dbg_trace s!"@@@2 pre_receive_paths: ({pre_receive_states_paths})"
  let canonized_pre_receive_paths : List CtrlerPathConstraint := CanonStatePathsApprox pre_receive_states_paths
  -- dbg_trace s!"@@@2.1 canonized_pre_receive_paths: ({canonized_pre_receive_paths})"

  -- use canonized post & pre receive graphs, check for constraints unique to post receive path
  let ctrler_constraints_unique_to_post_receive : List CtrlerPathConstraint ← PostReceivePathsUniqueConstraints canonized_post_receive_paths canonized_pre_receive_paths
  -- dbg_trace s!"@@@3 post-receive unique constraints: ({ctrler_constraints_unique_to_post_receive})"

  -- USE POST RECEIVE STATES?
  let ctrler_states_and_dist ← CDFGCtrlerStatesToStallOnThatAreSeparateFromPreReceiveStates post_receive_graph pre_receive_graph graph receive_state_to_search_from.current_state ctrlers 0 inst_type

  -- USE CONTRAINTS?
  let ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states : List CtrlerPathConstraint :=
    ctrler_constraints_unique_to_post_receive.filter (λ ctrler_constraint => 
      pre_receive_states.any (λ node => node.ctrler_name == ctrler_constraint.ctrler)
    )

  -- NOTE: New thing i'm trying
  -- Ctrler states can return an empty list of states, and in this case
  -- don't gen an if stmt, just use the await search API
  match ctrler_states_and_dist, ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states with
  | some (ctrler_states, /- dist -/ _), ctrler_path_constraints :: _ =>
    -- check dist to each, choose shortest dist from receive state
    match ctrler_states.states, ctrler_path_constraints.path with
    | /- state_name -/ _ :: _ , /- path_node -/ _ :: _ =>
      let ctrlers_dist_labels : List (CtrlerName × Distance) ← post_receive_graph.queue_ctrler_distance_from_node receive_state_to_search_from ctrlers inst_type
      let ctrler_names := [ctrler_states.ctrler, ctrler_path_constraints.ctrler]

      let closer_ctrler_to_receive_state ← min_dist_ctrler ctrlers_dist_labels ctrler_names
      if closer_ctrler_to_receive_state == ctrler_states.ctrler then
        -- use ctrler_states
        -- TODO: Create a function for this:
        -- Pass this info in a structure
        let ctrler_states_pruned_awaits ← ctrler_states.prune_pre_receive_await_and_no_complete_path_states post_receive_graph inst_type
        pure $ Sum.inl ctrler_states_pruned_awaits

        -- NOTE: This will be for later in the stall state gen:
        -- 1. Remove the "await" state in the states if there is one... i.e. the "await receive global response" since it's also pre-receive
        -- 2. If the states list is now empty, then don't gen an if stmt, just use the await search API
        -- If the states list if not empty, then gen the if stmt that transitions or stalls
      else
        -- use ctrler_path_constraints
        -- TODO:
        -- Create a fn from the old code below
        pure $ Sum.inr $ ← CtrlerConstraintsToCtrlerStateConstraintExpr post_receive_graph (ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states.map (·.ctrler_constraints))
    | _, _ => 
      throw "Error: There should be states and path nodes"
  | some (ctrler_states, /- dist -/ _), [] =>
    -- use ctrler_states
    let ctrler_states_pruned_awaits ← ctrler_states.prune_pre_receive_await_and_no_complete_path_states post_receive_graph inst_type
    pure $ Sum.inl ctrler_states_pruned_awaits
  | none, /- ctrler_path_constraints -/ _ :: _ =>
    -- use ctrler_path_constraints
    pure $ Sum.inr $ ← CtrlerConstraintsToCtrlerStateConstraintExpr post_receive_graph (ctrler_constraints_unique_to_post_receive_and_have_pre_receive_states.map (·.ctrler_constraints))
  | none, [] =>
    -- throw "No ctrler states or constraints to use for query"
    throw "No ctrler states or constraints to use for stall query"

  -- generate stall state, add it to the right controller_info obj, 

  -- remove common states from post-receive states
  -- NOTE: Only need this if we query specific states the ctrler is in
  -- let pre_receive_states_no_overlap : List Node := remove_common_nodes pre_receive_states post_receive_states

  /- Look at the state sets per ctrler, check for (1) states unique to post-'receive' -/
  /- and (2) Variable Constraints that are unique to post-'receive' -/
  -- TODO
  -- 1. Find the pre-receive states' variable constraints, keep in a structure {ctrler_name, variable constraints}
  -- 2. Traverse post-receive states with this info, return all traces
  -- 3. Group together canonical traces, and check which unique var constraints are consistent 
  -- 4. Consistent var constraints are chosen
  -- 5. Verify consistent var constraint's ctrler will have the inst while younger inst is stalled
  -- Check if unique state/variable constraints is present in queue when inst2 reaches the "stall state"

  -- Get traversal graphs of both inst1 and inst 2
  -- check if rules of inst2's path mean that inst1 will be at a state of the same ctrler as the unique state

  -- 6. Should be 1, if more, choose any, or choose earliest from the receive-state

  /- Return the Ctrler/State/Variable to stall on info -/


-- #eval [[2],[1]].filter (·.any (· > 1))

def CDFG.Graph.commit_state_ctrler (graph : CDFG.Graph) : Except String CDFG.Node := do
  -- Find commit state & ctrler
  let commit_state! : Except String Node := graph.commit_transition_state_ctrler

  commit_state!.throw_exception_nesting_msg "Error while finding where the Commit State is"

-- -- Trying something
-- theorem global_perform_of_memory_access (inst_type : InstType) : inst_type ∈ [.load, .store] := by
--   induction inst_type with
--   | load => 
--   | store => exact inst_type ∈ [.load, .store],
--   | mfence => 

open Pipeline in
def CDFG.Node.load_req_address_seq_num (node : Node) : Except String (Expr × Expr) := do
  let unique_msgs := node.non_reset_transitions.map (·.messages) |>.eraseDups |>.join
  let perform_msgs ← unique_msgs.filterM (do ·.is_global_perform_of_type load)
  match perform_msgs with
  | [perform_msg] => do
    let addr_seq_num? ← perform_msg.load_req_address_seq_num!? |>.throw_exception_nesting_msg "Error: Couldn't get Load Perform Msg's addr/seq_num?"
    match addr_seq_num? with
    | some addr_seq_num => do pure addr_seq_num
    | none => do throw "Error: Load Perform Message should have an address sequence number"
  | [] => throw "Error: There should be a perform message"
  | _::_ => throw "Error: Don't know how to handle multiple perform messages"

open Pipeline in
def CDFG.Transition.is_has_var_assigned
(transition : Transition)
(var_name : VarName)
(only_consider_var_assign : Bool)
(is_perform_load_transition : Bool)
: /- Except String -/ (Option Statement) :=
  let stmts_to_search :=
    match is_perform_load_transition with
    | true =>
      -- ignore stmts after the send_load_request API
      transition.stmts.remove_post_send_load_stmts
    | false =>
      transition.stmts

  let reversed_stmts := stmts_to_search.reverse
  reversed_stmts.first_var_assign_to_var var_name only_consider_var_assign

def CDFG.Condition.is_await_when_with_arg
(cond : Condition)
(arg_name : VarName)
: Except String Bool := do
  match cond with
  | .AwaitCondition await_stmt
  | .APICondition await_stmt => do
    let when_blks ← await_stmt.get_await_when_blocks
    when_blks.anyM (·.is_ident_in_when_args arg_name)
  | _ => pure false

def CDFG.Transition.is_has_await_when_with_arg
(transition : Transition)
(arg_name : VarName)
: Except String Bool :=
  transition.predicate.anyM (·.is_await_when_with_arg arg_name)

def CDFG.Transition.is_has_perform_of_inst_type
(transition : Transition)
(inst_type : InstType)
: Except String Bool :=
  transition.messages.anyM (·.is_global_perform_of_type inst_type)

def CDFG.Graph.global_perform_node_of_memory_access (graph : Graph) (inst_type : InstType) : Except String Node := do
  let global_perform_nodes! : Except String (List Node) := do
    graph.nodes.filterM (do ·.transitions.anyM (do ·.messages.anyM (do ·.is_global_perform_of_type inst_type)))
  let global_perform_nodes ← global_perform_nodes!
    |>.throw_exception_nesting_msg "Error finding the Global Perform Node for inst_type: ({inst_type}). Graph: ({graph.node_names})"
  match global_perform_nodes with
  | [global_perform_node] => do pure global_perform_node
  | _::_ => do 
    graph.earliest_node_by_msg_dist global_perform_nodes inst_type
      |>.throw_exception_nesting_msg s!"Error finding the earliest Global Perform Node for inst_type: ({inst_type}). Graph: ({graph.node_names})"
  | [] => do
    -- dbg_trace s!"Graph Nodes: ({graph.node_names})"
    throw s!"Error: No global perform node found in graph: ({graph.node_names})"

def CDFG.Graph.global_perform_node_of_inst_type (graph : Graph) (inst_type : InstType) : Except String ( Node ) := do
  match inst_type with
  | .memory_access access => do
    match access with
    | .load
    | .store => do
      graph.global_perform_node_of_memory_access inst_type
  | .memory_ordering ordering => do
    match ordering with
    | .mfence => do
      -- Replace, filter for node with commit label
      -- Use this function
      -- CDFG.Graph.commit_transition_state_ctrler
      let commit_node : Node := ← graph.commit_transition_state_ctrler
        |>.throw_exception_nesting_msg s!"Error finding the Commit State Node for inst_type: ({inst_type}). Graph: ({graph.node_names})"
      pure commit_node

-- def CDFG.Condition.is_predicated_by_search_older_seq_num (cond : Condition) : Bool :=
--   match cond with
--   | .APICondition await_stmt => -- recursive search for if there's a function call in any term
--     await_stmt.is_self_search_older_seq_num_success
--   | _ => false


partial def CDFG.Graph.PO_inserted_ctrler_node_from_node (graph : Graph) (node : Node) (ctrlers : List controller_info)
: Except String Node := do
  -- dbg_trace s!"<<2 heuristic search"
  -- dbg_trace s!"node_name: ({node.current_state})"
  -- sorry
  -- 1. Recursive back track through nodes until we find one not transitioned to, get node that msgs it
  let (msging_node_of_input_node, /-await_conds-/ _) : Node × (List CDFG.Condition) := ← graph.first_msging_ctrler_node_from_node node []
  -- dbg_trace s!"msging_node_of_input_node: ({msging_node_of_input_node.current_state})"
  -- 2. Check transitions predicated on msg from other ctrler
    -- AZ NOTE: Heuristic should check there's no Unordered queue in the path, and at least 1 ordered FIFO pred by is_head

  -- dbg_trace s!">>START check if insert node is pred is head"
  let is_'insert'_msging_node_trans_pred_is_head : Bool ← graph.is_complete_trans_pred_is_head msging_node_of_input_node node.ctrler_name
  -- dbg_trace s!">>END check if insert node is pred is head"
  let is_node_transition_path_PO : Bool ← msging_node_of_input_node.is_msg_in_order graph ctrlers

  let await_conds_from_'inserting'_ctrler_node ← graph.find_await_conds_on_this_node_path msging_node_of_input_node []
  let nodes_that_send_msg_to_the_await_conds? : List (Option Node) ← await_conds_from_'inserting'_ctrler_node.mapM (·.await_pred's_sending_node graph |>.throw_exception_nesting_msg s!"Error while finding a node that inserts into this node ({node.current_state}) in PO")
  let nodes_that_send_msg_to_the_await_conds := nodes_that_send_msg_to_the_await_conds?.filterMap id
  -- dbg_trace s!"nodes_that_send_msg_to_the_await_conds: ({nodes_that_send_msg_to_the_await_conds.map (·.current_state)})"
  -- Because the other nodes are pred into being PO, this is PO as well if this is pred on them
  -- by transitivity
  let msging_nodes_are_pred_is_head_PO : Bool := ← nodes_that_send_msg_to_the_await_conds.anyM (do
    let list_node := ·;
    let node's_predecessors_are_PO : Bool ← list_node.is_msg_in_order graph ctrlers;
    let node's_transitions_are_PO : Bool ← graph.is_complete_trans_pred_is_head list_node node.ctrler_name
    -- dbg_trace s!"list_node: ({list_node.current_state})"
    -- dbg_trace s!"list_node's_predecessors_are_PO: ({node's_predecessors_are_PO})"
    -- dbg_trace s!"list_node's_transitions_are_PO: ({node's_transitions_are_PO})"

    pure $ node's_predecessors_are_PO || node's_transitions_are_PO
  )
  -- let is_'insert'_msging_node_ctrler_node_pred_is_head : Bool ← graph.is_node_inserted_to_in_PO msging_node_of_input_node ctrlers

  -- 3. Do a recursive back track through nodes, checking for transitions that are pred by is_head
  -- dbg_trace s!"Trying to find the stall point at node ({node.current_state}) by a heurstic"
  -- dbg_trace s!"is the inserting msging node transition pred is head: ({is_'insert'_msging_node_trans_pred_is_head})"
  -- dbg_trace s!"is the node transition path PO: ({is_node_transition_path_PO})"
  -- dbg_trace s!"are the msging nodes pred is head PO: ({msging_nodes_are_pred_is_head_PO})"
  if is_'insert'_msging_node_trans_pred_is_head || is_node_transition_path_PO || msging_nodes_are_pred_is_head_PO then
    pure node
  else
    -- recursive search
    graph.PO_inserted_ctrler_node_from_node msging_node_of_input_node ctrlers

def CDFG.Graph.find_stall_point_heuristic (graph : Graph) (inst_type : InstType) (ctrlers : List controller_info)
: Except String CtrlerState := do
  -- First, find the first state, which is the state that sends the global perform msg
  -- dbg_trace s! ">> Get global perform node"
  let global_perform_node : Node ← graph.global_perform_node_of_inst_type inst_type

  -- >Second, check if the ctrler is inserted into in PO order (check is_head and insert)
  -- >Can achieve this by iterating backwards to a node which is not transition'd to, but is
  -- only messaged to
  -- >At this node,
  -- >>check predicate (should be when), find the sender,
  -- >>>If sender is a FIFO, see if the sender is predicated is_head
  -- >>>>If it is, then it's ordered. If not, then it's unordered
  -- >>>If queue is unordered, then see if it's predicated on await self search msg when
  -- >>>>If it is, then it's ordered. If not, then it's unordered
  -- >>>If it's a ctrler, then it's unordered

  -- if not, back track to the previous ctrler to repeat
  -- if ordered, return this node
  -- let PO_inserted_ctrler_node : Node ← graph.PO_inserted_ctrler_node_from_node global_perform_node ctrlers
  -- dbg_trace s! ">> before starting the recursive search"
  let stall_node ← graph.PO_inserted_ctrler_node_from_node global_perform_node ctrlers
  pure ({ctrler := stall_node.ctrler_name, state := stall_node.current_state} : CtrlerState)

-- import Pipeline.DSLtoCDFG

-- def CDFGInOrderTfsm (ctrlers : List controller_info) (inst_to_stall_on_type : InstType) (inst_to_stall_type : InstType)
-- : Except String (List controller_info) := do
--   dbg_trace "<< Starting CDFGInOrderTfsm"
--   let graph_nodes ← DSLtoCDFG ctrlers
--   dbg_trace "<< Got graph nodes translated"
--   let graph := {nodes := graph_nodes}
--   let stall_point ← find_stall_point_heuristic graph inst_to_stall_type ctrlers
--   dbg_trace s!"<< Found stall point from heuristic: ({stall_point})"
--   let ctrler_state_to_stall_on : StateOrConstraintToStallOn ← find_ctrler_or_state_to_query_for_stall graph inst_to_stall_on_type ctrlers
--   dbg_trace "<< Found ctrler/state to stall at"

--   let stall_node ← CreateStallNode stall_point ctrler_state_to_stall_on ctrlers inst_to_stall_on_type inst_to_stall_type

--   let new_state_name := stall_point.ctrler ++ "_stall_" ++ stall_point.state
--   let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_state_name stall_node stall_point.state

--   pure updated_ctrlers

def CDFG.Transition.is_has_result_write_labelled (transition : Transition)
: (Option Pipeline.Statement) :=
  let result_write_stmts := transition.stmts.find? (·.is_result_write_from_stmts)
  result_write_stmts
  -- match result_write_stmts with
  -- | [] => pure none
  -- | [result_write] => pure result_write
  -- | _ => throw s!"More than one result write in transition: ({transition})"

def CDFG.Node.is_not_reset_trans_result_write_labelled (node : Node)
: Except String (Option Pipeline.Statement) := do
  let not_reset_transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let result_write_stmts? : List (Option Pipeline.Statement) := not_reset_transitions.map (·.is_has_result_write_labelled)
  let no_none_stmts? := result_write_stmts?.filter (·.isSome)
  let no_dup_result_write_stmts? := no_none_stmts?.eraseDups
  match no_dup_result_write_stmts? with
  | [] => pure none
  | [result_write?] => pure $ result_write?
  | _ => throw s!"More than one result write: ({no_dup_result_write_stmts?}) in node: ({node})"

structure NodeLabelledStmt where
  node : Node
  labelled_stmt : Pipeline.Statement
deriving Inhabited, BEq

#eval [reg_file, write].to_qual_name == (Pipeline.QualifiedName.mk [reg_file, write])

def NodeLabelledStmt.stmts_to_read_the_old_value
(node_labelled_stmt : NodeLabelledStmt) (old_var_dest : Identifier)
-- NOTE: Maybe also take as input the following stmts after reading the old value,
-- If the old value is stored in a Queue, the queue needs to be searched...
: Except String (Pipeline.Statement) := do
  let labelled_stmt := node_labelled_stmt.labelled_stmt
  let stmt ← labelled_stmt.labelled_stmt's_stmt
  match stmt with
  | .stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name arg_exprs)) => do
    if qual_name == [reg_file, write].to_qual_name && arg_exprs.length == 2 then do
      -- return a register file read
      -- Should check copy the original register file read call in?
      if let some written_reg_expr := arg_exprs[1]? then do
        pure $ CreateDSLVarAssignmentStmt old_var_dest (CreateDSLRefFileReadExpr reg_file read [written_reg_expr])
      else do
        throw s!"Error, shouldn't get here, since this scope is only reached if arg_exprs.length == 2"
    else do
      throw s!"Error while accessing Statement of a load's result write, got an unexpected function call: ({stmt})"
  | .variable_assignment /- qual_name -/ _ /- expr -/ _ => do
    -- TODO:
    -- Access the variable assigned the old value;
    -- Verify it IS a state var, i.e. it's persistent
    -- Check the ctrler type this thing belongs in
    -- If it's a queue, return a search API call,
    -- If it's a ctrler, need to access it's contents somehow.... maybe just error for now
    -- For now: just throw an error
    throw s!"Error while accessing the old load value's Statement. We're not handling variable assignment just yet"
  | _ => do throw s!"Error while accessing the old load value's Statement. Got an unexpected Statement: ({stmt})"

def NodeLabelledStmt.stmts_to_write_replay_value_to_result_write_location
(node_labelled_stmt : NodeLabelledStmt) (replay_var : Identifier)
: Except String Pipeline.Statement := do
  let labelled_stmt := node_labelled_stmt.labelled_stmt
  let stmt ← labelled_stmt.labelled_stmt's_stmt
  match stmt with
  | .stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name arg_exprs)) => do
    if qual_name == [reg_file, write].to_qual_name && arg_exprs.length == 2 then do
      -- return a register file read
      -- Should check copy the original register file read call in?
      -- let written_reg_expr : Pipeline.Expr :=
      --   if H : arg_exprs.length = 2 then
      --     have h : 1 < arg_exprs.length := by simp[H];
      --     arg_exprs[1]'h
      --   else
      --     default
      if let some written_reg_expr := arg_exprs[1]? then do
        pure <| CreateDSLMsgCall reg_file write [replay_var.to_dsl_var_expr, written_reg_expr]
      else do
        throw s!"Error, shouldn't get here, since this scope is only reached if arg_exprs.length == 2"
    else do
      throw s!"Error while accessing Statement of a load's result write, got an unexpected function call: ({stmt})"
  | .variable_assignment /- qual_name -/ _ /- expr -/ _ => do
    -- TODO:
    -- Access the variable assigned the old value;
    -- Verify it IS a state var, i.e. it's persistent
    -- Check the ctrler type this thing belongs in
    -- If it's a queue, return a search API call,
    -- If it's a ctrler, need to access it's contents somehow.... maybe just error for now
    -- For now: just throw an error
    throw s!"Error while trying to write replay val to the old val location. We're not handling variable assignment just yet"
  | _ => do throw s!"Error while trying to write replay val to the old val location. Got an unexpected Statement: ({stmt})"

-- AZ NOTE: This can be found from analysis, simply finding
-- 1. IF there are states that are
-- predicated on msgs from "commit",
-- then look at the set of state variables that are set by the await-when receive load mem response
-- if they're still live, then they have the old load value
-- Or, the other case:
-- 2. IF the "LQ" is not predicated on "commit" this means
-- we just track where the await load mem response value is passed to (static analysis)
-- and is still "Live" in the "commit" states
-- i.e. it goes to a ctrler and is not potentially overwritten by another inst
-- Also note the RF is a special case where we need to reason that the RF entry is not
-- overwritten by another inst
def CDFG.Graph.getResultWriteStateAndStmt (graph : Graph)
: Except String NodeLabelledStmt := do
  let nodes_with_labelled_stmt : List (List NodeLabelledStmt) := ←
    (graph.nodes.mapM ( λ node => do
      let result_write_stmt? := node.is_not_reset_trans_result_write_labelled;
      match ← result_write_stmt? with
      | none => pure []
      | some labelled_stmt => pure [ ⟨ node, labelled_stmt ⟩ ]
      ))
  let node_with_labelled_stmt := List.join nodes_with_labelled_stmt
  match node_with_labelled_stmt with
  | [] => throw "Error: No result write state found"
  | [node_labelled_stmt] => pure node_labelled_stmt
  | _ => throw "Error: More than one result write state found"

def CDFG.Graph.old_load_value_node (graph : Graph)
: Except String NodeLabelledStmt := do
  -- Find old load value state & stmt that the old load value is written to
  let result_write_states_and_stmt! : Except String NodeLabelledStmt := graph.getResultWriteStateAndStmt

  result_write_states_and_stmt!.throw_exception_nesting_msg "Error while finding where the Old Load Value is"

def CDFG.Graph.old_load_value_stmt_state_ctrler (graph : CDFG.Graph) : Except String NodeLabelledStmt := do
  graph.old_load_value_node

def CDFG.Graph.await_load_receive_state_ctrler (graph : CDFG.Graph) : Except String CDFG.Node /- CtrlerName do i want an explicit ctrler_name? -/ := do
  -- Find load receive global response state & ctrler
  let receive_state_to_search_from! : Except String Node := graph.global_complete_node_of_inst_type load

  receive_state_to_search_from!.throw_exception_nesting_msg "Error while finding where the Await Load Receive State is"

def CDFG.Graph.load_global_perform_state_ctrler (graph : CDFG.Graph) : Except String CDFG.Node := do
  -- Find global perform state & ctrler
  let global_perform_node! : Except String Node := graph.global_perform_node_of_inst_type load

  global_perform_node!.throw_exception_nesting_msg "Error while finding where the Load Global Perform State is"

-- TODO: Do something like this to find the "commit state"
  -- TODO: Add a "commit" keyword to the DSL to mark the commit state...
-- def CDFG.Graph.global_perform_node_of_inst_type (graph : Graph) (inst_type : InstType)
-- : Except String Node := do
--   let global_perform_nodes : List Node :=
--     (← graph.nodes.filterM (·.transitions.anyM (·.messages.anyM (·.is_global_perform_of_type inst_type))))
--   if global_perform_nodes.length == 1 then
--     pure global_perform_nodes[0]!
--   else if global_perform_nodes.length > 1 then
--     -- Label by msg distance, take shortest one
--     graph.earliest_node_by_msg_dist global_perform_nodes
--   else
--     dbg_trace s!"Graph Nodes: ({graph.nodes.map (·.current_state)})"
--     throw "Error: No global perform node found"

-- NOTE: This function won't do what it's supposed to,
-- Because commit labels are not stored in transition.stmts, just recorded as a bool flag
-- def Pipeline.Statement.is_commit_labelled (stmt : Pipeline.Statement) : Bool :=
--   match stmt with
--   | Pipeline.Statement.labelled_statement label /-stmt-/ _ =>
--     match label with
--     | .commit => true
--     | _ => false
--   | _ => false

-- def CDFG.Transition.has_commit_labelled_effect (transition : Transition) : Bool :=
--   transition.effects.any (·.is_commit_labelled)

def CDFG.Node.is_node_transition_or_complete_pred_on_msg_from_state : Node → Node → Except String Bool
| this_node, predicating_node => do
  let pred_node_trans := predicating_node.transitions.filter (·.trans_type != .Reset)
  -- Only consider transitions that commit
  let pred_node_commit_trans := pred_node_trans.filter (·.has_commit_labelled_stmt)
  let pred_node_msgs := List.join $ pred_node_commit_trans.map (·.messages) -- NOTE: What if there are multiple commit transitions?

  -- dbg_trace s!"$$Pred Node Msgs: ({pred_node_msgs})"
  -- Find any messages the predicating node sends to this node that are from the "Committing Transition(s)"
  let msgs_from_predicating_node_to_this_node ← pred_node_msgs.filterM (λ msg => do
    let dest_ctrler := ← msg.dest_ctrler;
    -- dbg_trace s!"$$Dest Ctrler: ({dest_ctrler})"
    -- dbg_trace s!"$$This Ctrler: ({this_node.ctrler_name})"
    pure $ dest_ctrler == this_node.ctrler_name
    )
  -- dbg_trace s!"$$msgs_from_predicating_node_to_this_node: ({msgs_from_predicating_node_to_this_node})"
  
  let this_node_transitions := this_node.transitions.filter (·.trans_type != .Reset)
  let this_node_preds := List.join $ this_node_transitions.map (·.predicate)
  let this_node_await_preds := this_node_preds.filter (·.is_await)
  -- Match the msgs with the awaits here
  let msgs_from_pred_that_this_node_awaits := ← msgs_from_predicating_node_to_this_node.filterM (λ msg => do -- writing this more explicitly, lean's parser couldn't work this out...
    -- let msg_name ← msg.name
    let pred_ctrler_name := predicating_node.ctrler_name
    this_node_await_preds.anyM (CDFG.Condition.is_await_on_msg_from_ctrler · msg pred_ctrler_name)
  )

  -- TODO: If there are msgs from pred node that this node awaits, then this node is pred on the commit state
  -- ret true.
  match msgs_from_pred_that_this_node_awaits with
  | [] => pure false
  | _ => pure true

def CDFG.Graph.ctrler_completion_pred_on_commit_states (graph : Graph) (ctrler_name : CtrlerName) (commit_node : CDFG.Node) : Except String (List Node) := do
  let ctrler_nodes := graph.nodes.filter (·.is_from_ctrler ctrler_name)

  let ctrler_nodes_pred_on_msg_from_commit! : Except String (List Node) := ctrler_nodes.filterM (·.is_node_transition_or_complete_pred_on_msg_from_state commit_node)
  let ctrler_nodes_pred_on_msg_from_commit : List Node ← ctrler_nodes_pred_on_msg_from_commit!.throw_exception_nesting_msg "Error while checking if ctrler is predicated on commit state"


  pure ctrler_nodes_pred_on_msg_from_commit
  -- let is_ctrler_pred_on_msg_from_commit := ctrler_nodes_pred_on_msg_from_commit.length > 0
  -- pure is_ctrler_pred_on_msg_from_commit

def CDFG.Node.global_perform_load_msg (node : Node) : Except String Message := do
  let not_reset_transitions := node.transitions.filter (·.trans_type != .Reset)
  let messages := not_reset_transitions.map (·.messages) |>.join

  let global_perform_msgs! := messages.filterM (·.is_global_perform_of_type load)

  let global_perform_msgs ← global_perform_msgs!.throw_exception_nesting_msg "Error while finding global load perform stmts"
  let global_perform_msgs_no_dups := global_perform_msgs.eraseDups

  match global_perform_msgs_no_dups with
  | [] => throw "Error: No global load perform stmts found"
  | [msg] => pure msg
  | _ => throw "Error: More than one global load perform stmts found"
    
def CDFG.Node.global_perform_load_stmt (node : Node) : Except String Pipeline.Statement := do
  pure (← node.global_perform_load_msg ).to_stmt

-- AZ NOTE UNUSED: This is unused at the moment... 
-- def CDFG.Node.result_write_stmt (node : Node) : Except String Pipeline.Statement := do
--   let not_reset_trans := node.not_reset_transitions
--   let stmts := not_reset_trans.map (·.stmts) |>.join |>.eraseDups
--   let result_write_stmts := stmts.filter (·.is_result_write_from_stmts)

--   match result_write_stmts with
--   | [] => throw "Error: No result write stmts found"
--   | [stmt] => pure stmt
--   | _ => throw "Error: More than one result write stmts found"

def CDFG.Condition.is_pred_inst_not_load (cond : Condition) : Bool :=
  match cond with
  | .DSLExpr expr => expr.is_contains_instruction_not_eq_ld
  | _ => false

def CDFG.Node.is_pred_by_instruction_not_load (node : Node) (dest_node_name : StateName) : Bool :=
  let trans := node.non_reset_transitions
  let trans_to_dest := trans.filter (·.dest_state == dest_node_name)
  let preds := trans_to_dest.map (·.predicate) |>.join |>.eraseDups

  let exists_pred_on_not_load := preds.any (·.is_pred_inst_not_load)
  exists_pred_on_not_load

def CDFG.Graph.filter_is_node_for_ld (graph : Graph) (node : Node) : Bool :=
  dbg_trace s!"%%LR? is node for load?"
  let nodes_transitioning_to_node := graph.nodes_transitioning_to_node node
  let is_any_node_trans_to_this_node_not_for_load : Bool := nodes_transitioning_to_node.any (·.is_pred_by_instruction_not_load node.current_state)

  match is_any_node_trans_to_this_node_not_for_load with
  | true => false -- then this state path is not for load, i.e. is pred on instruction.op != ld
  | false => true -- then loads may take this path

def CDFG.Graph.filter_input_nodes_only_ld (graph : Graph) (nodes : List Node) : List Node :=
  nodes.filter ( graph.filter_is_node_for_ld · )

def CDFG.Node.listen_handle_stmt? (node : Node) (ctrlers : Ctrlers) : Except String (Option Pipeline.Statement) := do
  let ctrler ← ctrlers.ctrler_from_name node.ctrler_name 
  let state ← ctrler.state_from_name node.current_state
  let stmt ← state.listen_handle_stmt? 

  pure stmt

def CDFG.Node.wrap_stmt_with_node's_listen_handle_if_exists (node : CDFG.Node) (stmt : Pipeline.Statement) (ctrlers : Ctrlers) : Except String Pipeline.Statement := do
  let listen_handle? ← node.listen_handle_stmt? ctrlers
  match listen_handle? with
  | none => do pure stmt
  | some listen_handle => do listen_handle.replace_listen_handle_stmts [stmt]

def CDFG.Graph.is_not_transitioned_to (graph : Graph) (node : Node) : Bool :=
  graph.nodes_transitioning_to_node node == []

-- def CDFG.Graph.is_not_basic_trans'd_to (graph : Graph) (node : Node) : Bool :=
--   graph.nodes_transitioning_to_node node == []

def CDFG.Node.is_not_msg'd (node : Node) : Bool :=
  ! node.is_all_non_reset_awaits_on_msg

-- def CDFG.Node.is_not_msg'd_by_transitions (node : Node) (graph : Graph) : Bool :=
--   ! node.is_all_non_reset_awaits_on_msg_from_reset_trans graph



def CDFG.Transition.is_inst_source_transition (transition : Transition) : Bool :=
  transition.stmts.any (·.is_inst_source_stmt)

def CDFG.Node.is_inst_source_node (node : Node) : Bool :=
  let not_reset_trans := node.not_reset_transitions
  let is_inst_source_labelled := not_reset_trans.any (·.is_inst_source_transition)

  is_inst_source_labelled
  
-- A place holder, for the finding the instruction source node of a graph
def CDFG.Graph.inst_source_node (graph : Graph) : Except String Node :=
  -- let not_basic_trans'd_to_nodes := graph.nodes.filter (graph.is_not_basic_trans'd_to ·)
  -- let not_msg'd_or_transitioned_to := not_basic_trans'd_to_nodes.filter (·.is_not_msg'd)
  let inst_source_nodes := graph.nodes.filter (·.is_inst_source_node)

  match inst_source_nodes with
  | [] => throw "Error: No instruction source node found"
  | [node] => pure node
  | _ => throw s!"Error: More than one instruction source node found: ({inst_source_nodes.map (·.current_state)})"

-- TODO: Stub
def CDFG.Graph.states_an_inst_of_type_can_be_in (graph : Graph) (inst_source_node : Node) (must_stall_at_node? : Option Node) (inst_type : InstType) : Except String Graph := do
  let (nodes', _) ← graph.reachable_nodes_from_node_up_to_option_node 0 inst_source_node must_stall_at_node? inst_type [] none
  let nodes := nodes'.eraseDups
  pure { nodes := nodes }

def CDFG.Node.is_match_ctrler_and_state_names (node : Node) (ctrler_name : CtrlerName) (state_name : StateName) : Bool :=
  node.ctrler_name == ctrler_name && node.current_state == state_name

def CDFG.Graph.nodes_from_ctrler_and_state (graph : Graph) (ctrler_name : CtrlerName) (state_name : StateName) : List Node :=
  graph.nodes.filter (·.is_match_ctrler_and_state_names ctrler_name state_name)

def CDFG.Graph.node_from_ctrler_and_state! (graph : Graph) (ctrler_name : CtrlerName) (state_name : StateName) : Except String Node := do
  let nodes_matching_ctrler_and_state_name := graph.nodes_from_ctrler_and_state ctrler_name state_name
  match nodes_matching_ctrler_and_state_name with
  | [node] => do pure node
  | [] => do throw s!"No nodes found for ctrler_name: ({ctrler_name}) and state_name: ({state_name})"
  | _ => do throw s!"Multiple nodes found for ctrler_name: ({ctrler_name}) and state_name: ({state_name})"

def CDFG.Graph.node_from_ctrler_and_state? (graph : Graph) (ctrler_name : CtrlerName) (state_name : StateName) : Except String (Option Node) := do
  let nodes_matching_ctrler_and_state_name := graph.nodes_from_ctrler_and_state ctrler_name state_name
  match nodes_matching_ctrler_and_state_name with
  | [node] => do pure $ some node
  | [] => do pure none
  | _::_ => do throw s!"Multiple nodes found for ctrler_name: ({ctrler_name}) and state_name: ({state_name})"

def CDFG.Node.unique_msg'd_states_not_from_trans_compl_pred_by_other_insts : Node → Graph → InstType → Except String (List NodeTransition)
| node, graph, desired_inst_type => do
  let ctrler_name : CtrlerName := node.ctrler_name
  -- let transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans_compls : Transitions := node.not_visited_transitions_all_completions_taken_by_inst_type [] desired_inst_type
  let messages : Messages := List.join $ trans_compls.map (·.messages)
  let msg'd_states : List NodeTransition := (← messages.mapM (·.findDestStateOfTypeReachingComplete graph.nodes ctrler_name desired_inst_type |>.throw_exception_nesting_msg s!"Error finding unique msg'd states from trans & compl transitions not pred by other inst types") ).join

  let unique_msg'd_states : List NodeTransition := msg'd_states.eraseDups
  pure unique_msg'd_states

abbrev VisitedNodesOfCurrentCtrler := List Node
abbrev NodesOlderPOInstsCan'tBeOn := List Node

-- Not really used right now....
-- AZ NOTE: should ignore is_head checks if graph passes unordered queue(s). should do by passing a bool flag
partial def CDFG.Graph.not_allowable_nodes_for_older_PO_insts_constrained_by_inst_graph_and_node
(constraining_graph : Graph) (constraining_graph_inst_type : InstType) (current_node : Node) (node_inst_is_stalled_on : Node)
(ctrlers : Ctrlers) (visited : VisitedNodesOfCurrentCtrler) (nodes_older_PO_insts_can't_be_on : NodesOlderPOInstsCan'tBeOn)
: Except String (NodesOlderPOInstsCan'tBeOn) := do
  -- Check insts that are allowed to be in this graph

  -- dbg_trace s!"current_node: ({current_node.current_state}), node_inst_is_stalled_on: ({node_inst_is_stalled_on.current_state})"
  -- Check if the current node is the node_inst_is_stalled_on
  -- if it is, then just stop and return the nodes_older_PO_insts_can't_be_on
  match current_node == node_inst_is_stalled_on with
  | true =>
    -- dbg_trace s!"STOP: found stalled node: ({current_node.current_state}). returning nodes_older_PO_insts_can't_be_on: ({nodes_older_PO_insts_can't_be_on.map (·.current_state)})"
    pure nodes_older_PO_insts_can't_be_on
  | false => 
    -- look at constraining graph, observe how to reach the node_inst_is_stalled_on, and look at constraints imposed by the constraining graph upto the starting node
    let current_ctrler ← ctrlers.ctrler_from_name current_node.ctrler_name |>.throw_exception_nesting_msg s!"Couldn't get ctrler of name: ({current_node.ctrler_name}) while pruning an inst graph's allowable nodes/states"
    let ctrler_type ← current_ctrler.type |>.throw_exception_nesting_msg s!"Couldn't get type of ctrler: ({current_ctrler}) while pruning an inst graph's allowable nodes/states" 

    -- if the ctrler is FIFO, remember to check if the transitions are pred is_head
    -- if pred is_head, then the first inst cannot be in states upto this state of this ctrler that have been visited so far
    let is_pred_po : Bool :=
      match ctrler_type with
      | .FIFO => -- do a check on non-reset transitions for if pred is_head
        current_node.is_compls_trans_pred_is_head
      | _ => -- is either BasicCtrler or Unordered, no check for pred is_head -- not bothering with this so far.
        -- could also do the same check for PO by checking for stalling if "older" insts exist in Unordered queue
        false
  
    let (after_po_check_visited_nodes, after_po_check_nodes_older_po_insts_can't_be_on) :=
      match is_pred_po with
      | true => 
        -- dbg_trace s!"is_pred_po: ({is_pred_po})."
        ([], visited ++ nodes_older_PO_insts_can't_be_on ++ [current_node])
      | false =>
        (visited ++ [current_node], nodes_older_PO_insts_can't_be_on)
  
    -- dbg_trace s!"COLLECTED NODES: ({after_po_check_nodes_older_po_insts_can't_be_on.map (·.current_state)})."

    -- get msg'd & transition'd to nodes, handle accordingly
    let msg'd_state_names : List NodeTransition := ← current_node.unique_msg'd_states_not_pred_by_other_insts constraining_graph constraining_graph_inst_type
      |>.throw_exception_nesting_msg s!"Couldn't get msg'd states of node: ({current_node.current_state}) while pruning an inst graph's allowable nodes/states"
    let msg'd_nodes := ← msg'd_state_names.mapM (constraining_graph.node_from_name! ·.1.current_state)
    -- recurse with a fresh visited list, and po-check-not-allowed-list
    let msg'd_constraining_nodes := ← msg'd_nodes.mapM (
      constraining_graph.not_allowable_nodes_for_older_PO_insts_constrained_by_inst_graph_and_node constraining_graph_inst_type · node_inst_is_stalled_on ctrlers [] after_po_check_nodes_older_po_insts_can't_be_on)

    let trans'd_state_names : List StateName := current_node.unique_trans'd_states_not_pred_by_other_insts constraining_graph_inst_type
    let trans'd_nodes := ← trans'd_state_names.mapM (constraining_graph.node_from_name! ·)
    -- recurse with the same visited list, and po-check-not-allowed-list
    let trans'd_constraining_nodes := ← trans'd_nodes.mapM (
      constraining_graph.not_allowable_nodes_for_older_PO_insts_constrained_by_inst_graph_and_node constraining_graph_inst_type · node_inst_is_stalled_on ctrlers after_po_check_visited_nodes after_po_check_nodes_older_po_insts_can't_be_on) 

    let all_nodes_older_po_insts_can't_be_on := List.join $ msg'd_constraining_nodes ++ trans'd_constraining_nodes

    -- match trans'd_nodes with
    -- | [] =>
    --   dbg_trace s!"Don't have any transitions or msgs from node: ({current_node.current_state})"
    --   pure [] -- didn't encounter the stall node, so this is not a path constrained by the stall node!
    -- | _ =>
    --   dbg_trace s!"Have transitions or msgs from node: ({current_node.current_state}), trans'd msg'd ({msg'd_nodes.map (·.current_state) ++ trans'd_nodes.map (·.current_state)})"
    -- dbg_trace s!"RETURN: all_nodes_older_po_insts_can't_be_on: ({all_nodes_older_po_insts_can't_be_on.map (·.current_state)})"
    return all_nodes_older_po_insts_can't_be_on
    -- termination_by _ => constraining_graph

def CDFG.Graph.remove_nodes_from_graph (graph : Graph) (nodes : List Node) : Graph :=
  {nodes := graph.nodes.filter (! nodes.contains ·)}

def CDFG.Graph.prune_allowable_nodes_by_inst_graph_and_node
(first_'to_stall_on'_inst_graph : Graph) (second_'to_stall'_inst_graph : Graph) (second_'to_stall'_inst_type : InstType)
(inst_source_node : Node) (node_inst_is_stalled_on : Node) (ctrlers : Ctrlers)
: Except String Graph := do
  let nodes_older_po_insts_can't_be_on := ←
    second_'to_stall'_inst_graph.not_allowable_nodes_for_older_PO_insts_constrained_by_inst_graph_and_node second_'to_stall'_inst_type inst_source_node node_inst_is_stalled_on ctrlers [] []
    |>.throw_exception_nesting_msg s!"Hit Error while finding states that older PO insts cannot be in."
  
  -- dbg_trace s!"======= BEGIN nodes_older_po_insts_can't_be_on ========"
  -- dbg_trace s!"BEGIN nodes_older_po_insts_can't_be_on: ({nodes_older_po_insts_can't_be_on.map (·.current_state)})\nEND nodes_older_po_insts_can't_be_on"
  -- dbg_trace s!"======= END nodes_older_po_insts_can't_be_on ========"

  -- remove these nodes from the to stall graph.
  let constrained_first_inst_graph := first_'to_stall_on'_inst_graph.remove_nodes_from_graph nodes_older_po_insts_can't_be_on
  -- dbg_trace s!"======= BEGIN constrained_first_inst_graph ========"
  -- dbg_trace s!"BEGIN constrained_first_inst_graph: ({constrained_first_inst_graph.nodes.map (·.current_state)})\nEND constrained_first_inst_graph"
  -- dbg_trace s!"======= END constrained_first_inst_graph ========"
  return constrained_first_inst_graph

def CDFG.Graph.states_the_'to_stall_on'_node_can_be_in
(graph : Graph)
(first_'to_stall_on'_inst_type : InstType)
(second_'to_stall'_inst_type : InstType)
(stall_node_of_second_inst_type : CtrlerState)
(ctrlers : Ctrlers)
: Except String Graph := do
  let inst_source_node ← graph.inst_source_node
  -- dbg_trace s!"======= BEGIN inst_source_node ========"
  -- dbg_trace s!"BEGIN inst_source_node: ({inst_source_node.current_state})\nEND inst_source_node"
  -- dbg_trace s!"======= END inst_source_node ========"

  -- 1. Find all nodes that can be reached from the inst_source_node for the inst types
  let to_stall_on_inst_nodes : Graph ← graph.states_an_inst_of_type_can_be_in inst_source_node none first_'to_stall_on'_inst_type 
  -- dbg_trace s!"======= BEGIN to_stall_on_inst_nodes ========"
  -- dbg_trace s!"BEGIN to_stall_on_inst_nodes: ({to_stall_on_inst_nodes.nodes.map (·.current_state)})\nEND to_stall_on_inst_nodes"
  -- dbg_trace s!"======= END to_stall_on_inst_nodes ========"

  let stall_node := ← graph.node_from_ctrler_and_state! stall_node_of_second_inst_type.ctrler stall_node_of_second_inst_type.state 
  -- dbg_trace s!"======= BEGIN stall_node ========"
  -- dbg_trace s!"BEGIN stall_node: ({stall_node.current_state})\nEND stall_node"
  -- dbg_trace s!"======= END stall_node ========"
  let stall_node? : Option Node := some stall_node
  let stalled_inst_nodes : Graph ← graph.states_an_inst_of_type_can_be_in inst_source_node stall_node? second_'to_stall'_inst_type 
  -- dbg_trace s!"======= BEGIN stalled_inst_nodes ========"
  -- dbg_trace s!"BEGIN stalled_inst_nodes: ({stalled_inst_nodes.nodes.map (·.current_state)})\nEND stalled_inst_nodes"
  -- dbg_trace s!"======= END stalled_inst_nodes ========"

  -- 2. Based on the nodes the 2nd inst type can be in, prune the allowable states of the 1st inst type
  -- Prune based on:
  -- > if "is_head()"
  -- > uhh.... anything else?
  -- let 
  let allowable_'to_stall_on'_node_states_when_inst_stalled := ←
    to_stall_on_inst_nodes.prune_allowable_nodes_by_inst_graph_and_node stalled_inst_nodes second_'to_stall'_inst_type inst_source_node stall_node ctrlers
  -- dbg_trace s!"======= BEGIN allowable_'to_stall_on'_node_states_when_inst_stalled ========"
  -- dbg_trace s!"BEGIN allowable_'to_stall_on'_node_states_when_inst_stalled: ({allowable_'to_stall_on'_node_states_when_inst_stalled.nodes.map (·.current_state)})\nEND allowable_'to_stall_on'_node_states_when_inst_stalled"
  -- dbg_trace s!"======= END allowable_'to_stall_on'_node_states_when_inst_stalled ========"

  return allowable_'to_stall_on'_node_states_when_inst_stalled

def CDFG.Graph.ctrler_names_of_graph (graph : Graph) : List String :=
  graph.nodes.map (·.ctrler_name) |>.eraseDup

def CDFG.Graph.ctrler_and_nodes_pairs (graph : Graph) : List (CtrlerName × List Node) :=
  let ctrler_names := graph.ctrler_names_of_graph
  ctrler_names.map (
    let ctrler_name := ·;
    (ctrler_name, graph.nodes.filter (·.ctrler_name == ctrler_name))
  )

structure CtrlerNameNodes where
ctrler_name : CtrlerName
nodes : List Node

def CDFG.Transition.is_trans_msging_all_these_nodes (trans : Transition) (nodes : List Node) (src_ctrler : CtrlerName) (inst_type : InstType) : Except String Bool := do
  let msg_dests? := ← trans.messages.mapM (·.findDestStateOfTypeReachingComplete? nodes src_ctrler inst_type);
  let msg_dests := msg_dests?.filterMap id |>.join.eraseDups
  -- dbg_trace s!">>is_trans_msging_all_these_nodes: Nodes: ({nodes.map (·.current_state)}), Msg Dests: ({msg_dests}), Trans Dests: ({trans.dest_state})"
  pure $ nodes.all (·.current_state ∈ msg_dests)

def CDFG.Node.is_node_msging_these_nodes (node : Node) (nodes : List Node) (inst_type : InstType) : Except String Bool :=
  -- dbg_trace s!">>is_node_msging_these_nodes: Node: ({node.current_state}), Nodes: ({nodes.map (·.current_state)})"
  node.not_reset_transitions.anyM (·.is_trans_msging_all_these_nodes nodes node.ctrler_name inst_type)

def CDFG.Graph.nodes_msging_these_nodes (graph : Graph) (nodes : List Node) (inst_type : InstType) : Except String (List Node) :=
  graph.nodes.filterM (·.is_node_msging_these_nodes nodes inst_type)

def CDFG.Graph.node_msging_node (graph : Graph) (node : Node) (inst_type : InstType) : Except String (Option Node) := do
  let nodes_msging_node : List Node := ← graph.nodes_msging_these_nodes [node] inst_type
  match nodes_msging_node with
  | [] => pure none
  | [node] => pure $ some node
  | _ => throw s!"More than one node msging node ({node.current_state})"

-- Could probably de-couple this function...
-- def CtrlerNameNodes.starts_from_same_transition_as_other (this_ctrler_name_nodes : CtrlerNameNodes) (other_ctrler_name_nodes : CtrlerNameNodes) (graph : Graph)
-- : Except String Bool := do
--   let this_ctrler_graph : Graph := {nodes := this_ctrler_name_nodes.nodes}
--   let other_ctrler_graph : Graph := {nodes := other_ctrler_name_nodes.nodes}

--   let this_ctrler_first_node : Node := ← this_ctrler_graph.findNotTransitionedToState
--   let other_ctrler_first_node : Node := ← other_ctrler_graph.findNotTransitionedToState

--   let nodes_which_message_both_these_nodes : List Node := ← graph.nodes_msging_these_nodes [this_ctrler_first_node, other_ctrler_first_node]
--   let is_nodes_that_msg_both_nodes : Bool :=
--     match nodes_which_message_both_these_nodes with
--     | [] => false
--     | _ => true
--   pure is_nodes_that_msg_both_nodes

def CDFG.Graph.all_trans'd_states : Graph → List StateName
| graph =>
  List.join $ graph.nodes.map (·.unique_trans'd_states)

def CDFG.Graph.not_trans'd_node (graph : Graph)
: Except String Node := do
  -- Get list of state names that are transitioned to or messaged to
  -- Have each node check if they are in this list/set
  -- remaining node should be the "src" of where insts come from...
  let all_transitioned_states_list : List StateName := graph.all_trans'd_states
  let not_transitioned_states_list : List Node := graph.nodes.filter (λ node =>
    !(all_transitioned_states_list.any (· == node.current_state))
  )
  match not_transitioned_states_list with
  | [node] =>
    -- dbg_trace s!"## not_transitioned_to_node: {node}"
    pure node -- only one node, so return it
  | [] => -- empty
    let msg : String := "Error: No nodes which are not transitioned to? There should be 1?"
    throw msg
  | _ :: _ =>
    let msg : String := "Error: More than one node is not transitioned to.\n" ++
      s!"I'm only expecting one 'inst src' state: ({not_transitioned_states_list})"
    throw msg

def CtrlerNameNodes.starts_at_same_or_later_than_given_ctrler
(this_ctrler_name_nodes : CtrlerNameNodes) (other_ctrler_name_nodes : CtrlerNameNodes) (graph : Graph) (inst_type : InstType)
: Except String ( Bool × Option Node ) := do
  let this_ctrler_graph : Graph := {nodes := this_ctrler_name_nodes.nodes}
  let other_ctrler_graph : Graph := {nodes := other_ctrler_name_nodes.nodes}

  let this_ctrler_first_node : Node := ← this_ctrler_graph.not_trans'd_node |>.throw_exception_nesting_msg s!"Coudln't find not transitioned to state of this graph: ({this_ctrler_graph.nodes.map (·.current_state)})"
  let other_ctrler_first_node : Node := ← other_ctrler_graph.not_trans'd_node |>.throw_exception_nesting_msg s!"Coudln't find not transitioned to state of other graph: ({this_ctrler_graph.nodes.map (·.current_state)})"


  let nodes_which_message_both_these_nodes : List Node := ← graph.nodes_msging_these_nodes [this_ctrler_first_node, other_ctrler_first_node] inst_type
  let is_nodes_that_msg_both_nodes : Bool :=
    match nodes_which_message_both_these_nodes with
    | [] => false
    | _ => true

  -- Other condition is if the other ctrler messages this ctrler, i.e. this ctrler is pred on any msg from other ctrler's nodes
  let is_this_ctrler_msg'd_by_other_ctrler? : Option Node := ← other_ctrler_graph.node_msging_node this_ctrler_first_node inst_type

  pure (is_nodes_that_msg_both_nodes, is_this_ctrler_msg'd_by_other_ctrler?)

def CDFG.Node.is_transitions_to_node (node : Node) (dest_node_to_check : Node) : Bool :=
  node.basic_transitions.any (·.dest_state == dest_node_to_check.current_state)

def CDFG.Node.is_untransitioned (node : Node) : Bool :=
  -- dbg_trace s!"## is_untransitioned_to_state| node: ({node.current_state}), graph: ({graph.nodes.map (·.current_state)})"
  -- let nodes_not_this_node : List Node := graph.nodes.filter (· != node)
  -- let nodes_transitioning_to_this_node := nodes_not_this_node.filter (·.is_transitions_to_node node)
  -- match nodes_transitioning_to_this_node with
  -- | [] => true
  -- | _ => false
  node.basic_transitions.length == 0

def CDFG.Graph.last_'untransitioned'_state (graph : Graph) : Except String Node := do
  let untransitioned_to_states : List Node := graph.nodes.filter (·.is_untransitioned)
  -- dbg_trace s!"## untransitioned_to_states: ({untransitioned_to_states.map (·.current_state)})"
  match untransitioned_to_states with
  | [] => throw s!"No untransitioned to states in graph: ({graph.nodes.map (·.current_state)})"
  | [node] => pure node
  | _ => throw s!"More than one untransitioned to state in graph: ({untransitioned_to_states.map (·.current_state)})"

def CDFG.Node.msg'd_by_nodes_of_graph? (node : Node) (graph : Graph) (inst_type : InstType) : Except String (Option Node) := do
  let nodes_msging_node : List Node := ← graph.nodes_msging_these_nodes [node] inst_type
  match nodes_msging_node with
  | [] => pure none
  | [node] => pure $ some node
  | _ => throw s!"More than one node msging node ({node.current_state})"

def CDFG.Node.is_msgs_nodes_of_graph (node: Node) (graph : Graph) (inst_type : InstType) : Except String Bool := do
  let node_graph : Graph := {nodes := [node]}
  let node_msging_graph_nodes : List Node := ← node_graph.nodes_msging_these_nodes graph.nodes inst_type

  match node_msging_graph_nodes with
  | [] => pure false
  | _ => pure true

def CtrlerNameNodes.finishes_at_same_or_earlier_node_than_given_ctrler
(this_ctrler_name_nodes : CtrlerNameNodes) (other_ctrler_name_nodes : CtrlerNameNodes) (inst_type : InstType)-- (graph : Graph)
: Except String ( Bool × Option Node ) := do
  -- TODO:
  -- implement this high-level overview:
  -- 1. get last node of each ctrler nodes
  -- 2. Check if the last node of this ctrler msgs the any node of the other ctrler
  let this_ctrler_graph : Graph := {nodes := this_ctrler_name_nodes.nodes}
  let other_ctrler_graph : Graph := {nodes := other_ctrler_name_nodes.nodes}

  -- one case: Last node is "completed" by the other ctrler
  let this_ctrler_last_node : Node := ← this_ctrler_graph.last_'untransitioned'_state
  let is_this_last_node_msg'd_by_other? : Option Node := ← this_ctrler_last_node.msg'd_by_nodes_of_graph? other_ctrler_graph inst_type

  -- another case: Last node "completes" by msging the other ctrler
  let is_this_last_node_msgs_other : Bool := ← this_ctrler_last_node.is_msgs_nodes_of_graph other_ctrler_graph inst_type

  -- Should try to check if there was a node returned by the previous analysis,
  -- and see if it reaches this found node (if a node was found, that is)

  return (is_this_last_node_msgs_other, is_this_last_node_msg'd_by_other?)

partial def CDFG.Node.is_path_to_node (node : Node) (dest_node : Node) (nodes : List Node) : Bool :=
  let dests := node.basic_transitions.map (·.dest_state)
  match dests with
  | [] => false
  | _ => -- check if dest is in dests
    let dest_node_in_dests : Bool := dests.any (· == dest_node.current_state)
    match dest_node_in_dests with
    | true => true
    | false => -- If it's not, recurse in nodes
      dests.map (let dest_name := ·; nodes.filter (·.current_state == dest_name))
        |>.join |>.any (·.is_path_to_node dest_node nodes)


def CtrlerNameNodes.is_there_a_total_order_btn_nodes (ctrler_name_nodes : CtrlerNameNodes) (upper_node : Node) (lower_node : Node) : Bool :=
  upper_node.is_path_to_node lower_node ctrler_name_nodes.nodes

def CtrlerNameNodes.is_a_live_subset_of
(this_ctrler_name_nodes : CtrlerNameNodes) (other_ctrler_name_nodes : CtrlerNameNodes) (graph : Graph) (inst_type : InstType)
: Except String Bool := do
  -- Check if this_ctrler_name and nodes is a subset of the other ctrler_name and nodes

  -- a few steps to check:
  -- 1. if the first node of this ctrler "starts" at the same time as the first of the other ctrler, or at one of the later nodes of the other ctrler
  -- 2. if the last node of this ctrler "completes" at the same time as the last of the other ctrler, or at one of the earlier nodes of the other ctrler
  -- dbg_trace s!"==== is_a_live_subset_of ===="
  -- dbg_trace s!"this_ctrler_name_nodes: ({this_ctrler_name_nodes.ctrler_name})"
  -- dbg_trace s!"other_ctrler_name_nodes: ({other_ctrler_name_nodes.ctrler_name})"
  let (is_nodes_that_msg_both_nodes, is_this_ctrler_msg'd_by_other_ctrler?) := ← this_ctrler_name_nodes.starts_at_same_or_later_than_given_ctrler other_ctrler_name_nodes graph inst_type
  -- dbg_trace s!">>is_nodes_that_msg_both_nodes: ({is_nodes_that_msg_both_nodes}), This ({this_ctrler_name_nodes.ctrler_name}) Other ({other_ctrler_name_nodes.ctrler_name})})"
  -- dbg_trace s!">>is_this_ctrler_msg'd_by_other_ctrler?: ({is_this_ctrler_msg'd_by_other_ctrler?}) This ({this_ctrler_name_nodes.ctrler_name}) Other ({other_ctrler_name_nodes.ctrler_name})"
  let (is_this_last_node_msgs_other, is_this_last_node_msg'd_by_other?) := ← this_ctrler_name_nodes.finishes_at_same_or_earlier_node_than_given_ctrler other_ctrler_name_nodes inst_type-- graph
  -- dbg_trace s!">>is_this_last_node_msgs_other: ({is_this_last_node_msgs_other}) This ({this_ctrler_name_nodes.ctrler_name}) Other ({other_ctrler_name_nodes.ctrler_name})"
  -- dbg_trace s!">>is_this_last_node_msg'd_by_other?: ({is_this_last_node_msg'd_by_other?}) This ({this_ctrler_name_nodes.ctrler_name}) Other ({other_ctrler_name_nodes.ctrler_name})"

  let is_this_last_node_msg'd_by_other? : Option Node := ←
    match is_this_last_node_msg'd_by_other? with
    | none => do pure none
    | some node => do
      let other_ctrler_graph : Graph := {nodes := other_ctrler_name_nodes.nodes}
      let other_ctrler_last_node : Node := ← other_ctrler_graph.last_'untransitioned'_state
      -- AZ NOTE: this is to treat the msging ctrler as the main ctrler
      -- since the comparison in List.prune_ctrler_nodes_against_others_that is done
      -- by taking the ctrler of interest (ex. ROB) and comparing other ctrlers as "this" against it "other"
      -- and I want the ctrler of interest, i.e. "other", to be kept
      match other_ctrler_last_node == node with
      | true => do pure none
      | false => do pure is_this_last_node_msg'd_by_other?

  match is_nodes_that_msg_both_nodes, is_this_ctrler_msg'd_by_other_ctrler?, is_this_last_node_msgs_other, is_this_last_node_msg'd_by_other? with
  | true, _, true, _ => do
    pure true
  | true, _, _, some _ => do
    pure true
  | _, some _, true, _ => do
    pure true
  | _, some node, _, some node' => do
    pure $ other_ctrler_name_nodes.is_there_a_total_order_btn_nodes node node'
  | _, _, _, _ => do pure false
  -- 1.
  -- let this_ctrler_starts_comparable_to_other_ctrler : Bool := 

def List.prune_ctrler_nodes_against_others_that_are_live_longer (list_ctrler_and_nodes : List (CtrlerNameNodes)) (graph : Graph) (inst_type : InstType) : Except String (List CtrlerNameNodes) := do
  list_ctrler_and_nodes.filterM ( do
    let ctrler_name_nodes := ·;
    let other_ctrlers_and_their_nodes : List CtrlerNameNodes := list_ctrler_and_nodes.filter (·.ctrler_name != ctrler_name_nodes.ctrler_name);
    let is_ctrler_live_subset_of_others : Bool := ← other_ctrlers_and_their_nodes.anyM (ctrler_name_nodes.is_a_live_subset_of · graph inst_type)
    pure <| ! is_ctrler_live_subset_of_others -- Flip bool, to filter out ctrler + states that are subsets of others
  )

def List.to_graph (list_ctrler_and_nodes : List (CtrlerNameNodes)) : Graph := {
  nodes := list_ctrler_and_nodes.map (·.nodes) |>.join
}

-- Filter out nodes that are live for a subset of another node
def CDFG.Graph.prune_ctrler_nodes_that_are_live_for_a_subset_of_another_ctrler
(graph : Graph) (first_'to_stall_on'_inst_type : InstType) (inst_type : InstType)
: Except String Graph := do
  let inst_source_node ← graph.inst_source_node
  let to_stall_on_inst_nodes : Graph ← graph.states_an_inst_of_type_can_be_in inst_source_node none first_'to_stall_on'_inst_type 
  -- dbg_trace s!">>to_stall_on_inst_nodes: of type:({first_'to_stall_on'_inst_type}) ({to_stall_on_inst_nodes.nodes.map (·.current_state)})"

  -- group nodes by ctrlers
  -- check ctrler node set liveness between eachother. Check if each is a subset of any other, then prune it
  let list_ctrler_and_nodes := to_stall_on_inst_nodes.ctrler_and_nodes_pairs
  let ctrler_name_and_nodes_list : List CtrlerNameNodes := list_ctrler_and_nodes.map (let (ctrler_name, nodes) := ·; {ctrler_name := ctrler_name, nodes := nodes})

  let pruned_ctrler_name_and_nodes_list : (List CtrlerNameNodes) := ← ctrler_name_and_nodes_list.prune_ctrler_nodes_against_others_that_are_live_longer to_stall_on_inst_nodes inst_type
    |>.throw_exception_nesting_msg s!"Error pruning ctrler nodes that are live for a subset of another ctrler. Inst Graph: ({to_stall_on_inst_nodes})"
  
  pure pruned_ctrler_name_and_nodes_list.to_graph

-- def CDFG.Graph.completion_state_of_inst_type (graph : Graph) (inst_type : InstType)
-- : Except String Node := do
--   match inst_type with
--   | .load
--   | .store
--   | .mfence => do
--     graph.global_complete_node_of_inst_type inst_type

def CDFG.Graph.find_pre_receive_stall_states
(graph : Graph) (first_'to_stall_on'_inst_type : InstType)
: Except String (List Node) := do
  let inst_source_node ← graph.inst_source_node

  -- dbg_trace s!">> start INST GRAPH for inst type: ({first_'to_stall_on'_inst_type})"
    -- AZ NOTE: Current Issue: reachable nodes function doesn't end
  let (inst_graph', _) := ← graph.reachable_nodes_from_node_up_to_option_node 0 inst_source_node none first_'to_stall_on'_inst_type [] none
  let inst_graph := inst_graph'.eraseDups
  -- dbg_trace s!">> inst_type: ({first_'to_stall_on'_inst_type}) inst_graph: ({inst_graph.qualified_state_names})"
  -- dbg_trace s!">> end INST GRAPH for inst type: ({first_'to_stall_on'_inst_type})"

  let receive_response_node : Node ← graph.global_complete_node_of_inst_type first_'to_stall_on'_inst_type |>.throw_exception_nesting_msg s!"Error finding 'complete node' of inst_type: ({first_'to_stall_on'_inst_type}), in graph: ({graph.node_names})"
  -- dbg_trace s!">> receive_response_node: ({receive_response_node.qualified_state_name})"
  let (post_receive_inst_graph_with_receive_node', _) := ← graph.reachable_nodes_from_node_up_to_option_node 0 receive_response_node none first_'to_stall_on'_inst_type [] none
  let post_receive_inst_graph_with_receive_node := post_receive_inst_graph_with_receive_node'.eraseDups
  -- dbg_trace s!">> post_receive_inst_graph_with_receive_node: ({post_receive_inst_graph_with_receive_node.qualified_state_names})"
  let post_receive_inst_graph := post_receive_inst_graph_with_receive_node.filter (·.current_state != receive_response_node.current_state)
  -- dbg_trace s!">> inst_type: ({first_'to_stall_on'_inst_type}) post_receive_inst_graph: ({post_receive_inst_graph.qualified_state_names})"

  let pre_receive_inst_graph := inst_graph.filter (·.current_state ∉ post_receive_inst_graph.map (·.current_state) )
  -- dbg_trace s!">> inst_graph: ({inst_graph.qualified_state_names})"
  -- dbg_trace s!">> receive_response_node: ({receive_response_node.qualified_state_name})"
  -- dbg_trace s!">> post_receive_inst_graph: ({post_receive_inst_graph.qualified_state_names})"
  -- dbg_trace s!">> pre_receive_inst_graph: ({pre_receive_inst_graph.qualified_state_names})"

  return pre_receive_inst_graph

def List.to_ctrler_states (nodes : List Node) : List CtrlerStates :=
  let ctrler_names := nodes.map (·.ctrler_name) |>.eraseDups
  let ctrler_states : List CtrlerStates :=
    ctrler_names.map (
      let ctrler_name := ·;
      let state_names := nodes.filter (·.ctrler_name == ctrler_name);
      { ctrler := ctrler_name, states :=  state_names.map (·.current_state) }
    )
  ctrler_states

namespace LoadAddress

-- def CDFG.Graph.node_where_load_addr_state_var_assigned
-- (graph : Graph)
-- (current_node : Node)
-- (addr_name : VarName)
-- -- (is_addr_a_state_var : Bool)
-- : Except String Node :=

--   default

-- def CDFG.Graph.node_where_load_addr_obtained
-- (graph : Graph)
-- (current_node : Node)
-- (addr_name : VarName)
-- -- (is_addr_a_state_var : Bool)
-- : Except String Node :=
--   default

  open Pipeline in
  partial def CDFG.Graph.update_ctrlers_at_node_where_load_addr_obtained_search
  (graph : Graph)
  (current_node : Node)
  (addr_name : VarName)
  (is_addr_a_state_var : Bool)
  -- Use ctrlers if we're going to return the updated list of ctrlers.
  (ctrlers : Ctrlers)
  -- (stmts_to_inject : List Statement) -- don't need, the add insert to lat should gen this
  (visited : List Node)
  -- LAT stuff
  (lat_name : CtrlerName)
  (load_req_address : Expr)
  (load_req_seq_num : Expr)
  : Except String (Bool × Ctrlers) := do
    -- Check if this node is the node that produces the address variable
    -- (A) If the addr is a state var, then check if this is where it is assigned in it's stmts
    --   > then what we do is look at the stmts, and see if there's any var assign stmts to the var_name
    -- (B) If the addr is not a state var, then it's either
    -- (1) a local variable
    --   > then search for the first variable_assignment or value_declaration before the send_load_request api
    -- (2) input from a message
    --   > then search if there are await-when pairs with the var_name in the when identifier args
    -- (3) from an await-when API search
    --   > then search if there is an await-when API stmt that the send_load_request transition is predicated by

    -- if no transitions non-reset are perform load, then
    -- only consider non-reset transitions
    let some_transitions_are_perform_load : Bool ← current_node.non_reset_transitions.anyM (·.is_has_perform_of_inst_type load)

    let transitions_considered ←
      match some_transitions_are_perform_load with
      | true => current_node.non_reset_transitions.filterM (·.is_has_perform_of_inst_type load)
      | false => pure current_node.non_reset_transitions

    let addr_from_await_when : Bool := ←
      match is_addr_a_state_var with
      | false =>
        -- check the send_load_request transitions
        transitions_considered.anyM (·.is_has_perform_of_inst_type load)
      | true => pure false

    match addr_from_await_when with
    | true =>
      -- Add the stmt to the state in the ctrlers..
      -- TODO
      -- Go to the ctrler/state, search stmts up to the stmt that is the await/when that has the identifier matching the addr_name var name
      -- then add the stmts after it.
      let updated_ctrlers ←
        ctrlers.AddInsertToLATWhenPerform
        lat_name
        current_node.ctrler_name
        current_node.current_state
        load_req_address
        load_req_seq_num
        none
        (some addr_name)
        List.inject_stmts_in_when_matching_arg_at_ctrler_state

      pure (true, updated_ctrlers)
    | false =>
      -- Do the search
      -- transitions.find_variable_assignment or find value_declaration based on is_addr_a_state_var
      let addr_var_stmts? : List (Option Statement) := transitions_considered.map (·.is_has_var_assigned addr_name is_addr_a_state_var some_transitions_are_perform_load)
      let addr_var_stmts := addr_var_stmts?.filterMap id |>.eraseDups
      let addr_var_stmt? :=
        match addr_var_stmts with
        | h::_ =>
          -- dbg_trace s!"NOTE: in Load-Replay? Just taking the first stmt that assigns to var: ({addr_name}). Stmts: ({addr_var_stmts})"
          some h
        | [] => none

      match addr_var_stmt? with
      | some addr_var_stmt =>
        -- TODO: Write the inject_stmts_after_stmt_at_ctrler_state function
        let updated_ctrlers ←
          ctrlers.AddInsertToLATWhenPerform
            lat_name
            current_node.ctrler_name
            current_node.current_state
            load_req_address
            load_req_seq_num
            (some addr_var_stmt)
            none
            List.inject_stmts_after_stmt_at_ctrler_state

        pure (true, updated_ctrlers)
      | none =>
        -- get nodes transitioning to this one
        let nodes_trans_to_this := graph.nodes_transitioning_to_node current_node |>.eraseDups
        -- get the not-visited nodes, traverse them 1 by 1, stop if this function returns "found it"
        let not_visited_nodes := nodes_trans_to_this.filter (! visited.contains ·)
        let check_unvisited : List (Bool × Ctrlers) :=
          ← not_visited_nodes.mapM (LoadAddress.CDFG.Graph.update_ctrlers_at_node_where_load_addr_obtained_search graph · addr_name is_addr_a_state_var ctrlers (visited ++ [current_node]) lat_name load_req_address load_req_seq_num)
        let addr_re_write? := check_unvisited.find? (let (bool, _) := ·; bool)
        match addr_re_write? with
        | some addr_re_write => pure addr_re_write
        | none => throw s!"Error: wasn't able to add statements just after the phys_addr generating stmt."

    -- TODO: Check if addr_from_await_when is
    -- true => then add the stmt at that node..
    -- false => then do the below
    -- start, for each transition,
    -- if case (B),
    -- Check if there is an await-when in the predicate, and if there is a identifier == addr_name in the arguments
    -- if yes, then this is the state to add the stmt to, and it can be added anywhere in the when-scope
    -- Else, and for case (A)
    -- look at stmts, truncate after send_load_request stmt
    -- reverse list of stmts
    -- search for first stmt that meets the criteria

end LoadAddress


