
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
  | .await (none) lst_stmts =>
    let there_is_a_receive_complete : List Bool := (←
      lst_stmts.mapM (
        match · with
        | .when qual_name /- args -/ _ /- stmt -/ _ =>
          let src_ctrler : Except String CtrlerName := qual_name.ctrler_name
          let msg_name : Except String MsgName := qual_name.msg_name
          let completion_msg_name : String := inst_to_check_completion.completion_msg_name
          match src_ctrler with
          | .ok src_ctrler' =>
            match msg_name with
            | .ok msg_name' =>
              if src_ctrler' == memory_interface && msg_name' == completion_msg_name then
                pure [true]
              else
                pure []
            | .error msg => throw s!"Couldn't check when msg_name: ({msg})"
          | .error msg => throw s!"Couldn't check when src_ctrler_name: ({msg})"
        | _ => pure []
        )).join
    if there_is_a_receive_complete.length > 0 then
      pure true
    else
      pure false
  | _ => pure false
  
def CDFG.Node.has_transition_or_complete_with_msg_name (node : Node) (msg_name : MsgName) : Except String Bool := do
  let transition_or_complete_trans := node.transitions.filter (·.trans_type != .Reset)
  transition_or_complete_trans.anyM (·.messages.anyM (·.is_name_equals msg_name))

def Pipeline.Statement.await_when's_sending_node (stmt : Pipeline.Statement) (graph : Graph)
: Except String (Node) := do
  let (ctrler_name, msg_name) := ← stmt.await_when_ctrler_name_msg_name
  -- TODO:
  -- Filter graph nodes by ctrler_name
  let ctrler_nodes := graph.nodes.filter (·.ctrler_name == ctrler_name)
  -- Filter ctrler_nodes by ones that have transitions w/ message w/ msg_name
  let msg_nodes ← ctrler_nodes.filterM (·.has_transition_or_complete_with_msg_name msg_name)
  -- If more than one node, or 0, throw error
  match msg_nodes with
  | [node] => pure node
  | _ => throw s!"Statement: More than one node in ctrler: ({ctrler_name}) with msg: ({msg_name})"

def CDFG.Condition.await_pred's_sending_node (condition : Condition) (graph : Graph)
: Except String Node := do
  match condition with
  | .AwaitCondition await_stmt => await_stmt.await_when's_sending_node graph
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
  dbg_trace s!"cond is_pred_on_msg_from_ctrler: src_ctrler: ({ctrler_name}) && msg: ({← msg.name})"
  match cond with
  | .AwaitCondition await_stmt =>
    let (ctrler_name', msg_name) := ← await_stmt.await_when_ctrler_name_msg_name
    dbg_trace s!"await_cond from ctrler: ({ctrler_name'}) && msg: ({msg_name})"
    pure $ (msg_name == (← msg.name)) && (ctrler_name' == ctrler_name)
  | .HandleCondition handle_blk =>
    let (ctrler_name', msg_name) := ← handle_blk.ctrler_msg_names
    pure $ (msg_name == (← msg.name)) && (ctrler_name' == ctrler_name)
  | _ => pure false
    
  -- | .HandleCondition handle_stmt =>

def CDFG.Node.is_node_transition_or_complete_pred_on_msg_from_ctrler : Node → Message → CtrlerName → Except String Bool
| node, msg, ctrler_name => do
  -- check if any of the node's predicates are on the message's name
  let trans_of_interest : Transitions := node.transitions.filter (·.trans_type != .Reset)
  dbg_trace s!"trans_of_interest: ({trans_of_interest})"
  let all_conditions : List Condition := List.join $ trans_of_interest.map (·.predicate)
  all_conditions.anyM (·.is_pred_on_msg_from_ctrler msg ctrler_name)

def CDFG.Message.findDestState (cdfg_nodes : List CDFG.Node) (msg : Message) (src_ctrler : String)
: Except String (List String) := do
  -- let dest_ctrler ← msg.dest_ctrler
  -- let msg_name ← msg.name
  -- let dest_nodes : List Node := cdfg_nodes.filter (λ node =>
  --   node.ctrler_name == dest_ctrler
  -- )
  -- let dest_nodes_waiting_on_msg : List Node := dest_nodes.filter (
  --   λ node =>
  --     -- check if node awaits a message with name msg_name 
  --     -- could have also just checked the list of predicates
  --     let transition's_preds_await_msg : List Bool :=
  --       node.transitions.map (λ transition =>
  --         match transition.trans_type with
  --         -- TODO: Change to include completion
  --         | .Completion => false
  --         | .Transition =>
  --           -- Check predicates
  --           let conds_that_await_msg : List Condition :=
  --             transition.predicate.filter (λ (cond : Condition) =>
  --               -- check for await stmts
  --               match cond with
  --               | .AwaitCondition await_stmt =>
  --                 match await_stmt with
  --                 | .await (none) stmts =>
  --                   let there_is_a_matching_when : List Bool :=
  --                     stmts.map (λ stmt =>
  --                       match stmt with
  --                       | .when qual_name /- args -/ _ /- stmt -/ _ =>
  --                         match qual_name with
  --                         | .mk lst_ident =>
  --                           let src_ctrler' : String := lst_ident[0]!
  --                           let msg_name' : String := lst_ident[1]!
  --                           if src_ctrler' == src_ctrler && msg_name == msg_name' then
  --                             true
  --                           else
  --                             false
  --                       | _ => false
  --                       )
  --                   there_is_a_matching_when.any (λ bool => bool == true)
  --                 | _ => false
  --               -- Also check listen-handle
  --               | .HandleCondition handle_stmt =>
  --                 match handle_stmt with
  --                 | .mk qual_name /- args -/ _ /- stmt -/ _ =>
  --                   match qual_name with
  --                   | .mk lst_ident =>
  --                     let src_ctrler' : String := lst_ident[0]!
  --                     let msg_name' : String := lst_ident[1]!
  --                     if src_ctrler' == src_ctrler && msg_name == msg_name' then
  --                       true
  --                     else
  --                       false
  --               | _ => false
  --             )
  --           conds_that_await_msg.length > 0
  --         | .Reset => false
  --       )
  --     transition's_preds_await_msg.any (λ bool => bool == true)
  -- )
  -- let ret := dest_nodes_waiting_on_msg.map (λ node =>
  --   node.current_state)
  -- dbg_trace s!">> states awaiting on this msg: {ret}"
  -- let ctrler_nodes := cdfg_nodes.filter (·.ctrler_name == src_ctrler )
  -- dbg_trace s!"ctrler_nodes: {ctrler_nodes}"
  let ret_nodes := ← cdfg_nodes.filterM (·.is_node_transition_or_complete_pred_on_msg_from_ctrler msg src_ctrler)
  let ret_names := ret_nodes.map (·.current_state)
  pure ret_names

def CDFG.Node.unique_msg'd_states : Node → Graph → Except String (List StateName)
| node, graph => do
  let ctrler_name : CtrlerName := node.ctrler_name
  let transitions : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let messages : Messages := List.join $ transitions.map (·.messages)
  let msg'd_states : List StateName := (← messages.mapM (·.findDestState graph.nodes ctrler_name) ).join

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

def CDFG.Node.unique_trans'd_states_not_pred_on : Node → List Node → Except String (List StateName)
| node, nodes => do
  let transitions : Transitions := node.transitions.filter (·.trans_type == .Transition)

  let transitions_not_pred_on_input_nodes := (←
    transitions.filterM (λ trans => do pure !(← trans.predicate.anyM (·.is_awaits_on_msg_from_nodes nodes)) ))

  let transitioned_to_states : List StateName := transitions_not_pred_on_input_nodes.map (·.dest_state)
  -- Filter out states pred on provided states

  -- match predicate with message belonging to a node from the post receive states
  
  pure transitioned_to_states

def CDFG.Graph.unique_msg'd_states_by_node : Graph → StateName → Except String (List StateName)
| graph, state_name => do
  -- Could try to get this one line implementation working,
  -- but it'll take some time to fully understand
  -- graph.map_nodeM state_name ( (·).unique_msg'd_states graph)

  let current_node? : Option Node := graph.node_from_name? state_name
  if let some current_node := current_node? then
    -- let msg_dest_node : List Node
    current_node.unique_msg'd_states graph
  else
    throw s!"Error: (graph, unique msg'd states) No node with name: ({state_name})"

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
  dbg_trace s!">>> state_name : {state_name}"
  ← graph.node_mapM state_name (·.unique_trans'd_states_not_pred_on nodes)

def CDFG.Graph.all_msg'd_trans'd_states : Graph → Except String (List StateName)
| graph => do
  let msg'd_states := (← graph.nodes.mapM (·.unique_msg'd_states graph)).join
  let trans'd_states := List.join $ graph.nodes.map (·.unique_trans'd_states)
  pure $ msg'd_states ++ trans'd_states

def CDFG.Graph.findNotTransitionedToState (graph : Graph)
: Except String Node := do
  -- Get list of state names that are transitioned to or messaged to
  -- Have each node check if they are in this list/set
  -- remaining node should be the "src" of where insts come from...
  let all_transitioned_or_messaged_states_list : List StateName ← graph.all_msg'd_trans'd_states
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
    let msg : String := "Error: More than one node is not transitioned to or messaged to." ++
      s!"I'm only expecting one 'inst src' state: ({not_transitioned_or_messaged_states_list})"
    throw msg

abbrev Distance := Nat
-- TODO: Add a visited list
partial def CDFG.Graph.labelNodesByMessageDistance (start_node : StateName) (message_distance : Distance) (graph : Graph)
: Except String (List (StateName × Distance)) := do
  dbg_trace s!"Msg Dist: ({message_distance})"
  dbg_trace s!"start_node: {start_node}"
  let unique_msg'd_states : List StateName ←  graph.unique_msg'd_states_by_node start_node
  let unique_transitioned_to_states : List StateName ← graph.unique_transition'd_states_by_node start_node
  -- Look at unique messages
  -- recursive call to Messaged states/ctrlers (which increment counter)
  -- and to transitioned states (which don't increment counter)
  dbg_trace s!"**1 Recursive call on msg'd states: {unique_msg'd_states}"
  let states_traversed_from_message_list : List (List (StateName × Distance))  ←
    unique_msg'd_states.mapM (labelNodesByMessageDistance · (message_distance + 1) graph)
  let states_traversed_from_message : List (StateName × Distance) := states_traversed_from_message_list.join

  dbg_trace s!"**2 Recursive call on trans'd states: {unique_transitioned_to_states}"
  let states_traversed_from_transition_list : List (List (StateName × Distance))  ←
    unique_transitioned_to_states.mapM (labelNodesByMessageDistance · message_distance graph)
  let states_traversed_from_transition : List (StateName × Distance) := states_traversed_from_transition_list.join

  let this_state_and_dist : (StateName × Distance) := (start_node, message_distance)
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
partial def CDFG.Graph.findNodesReachableByTransitionAndMessage (start : String) (graph : Graph)
: Except String (List Node) := do
  let current_node? : Option Node := graph.nodes.find? (λ node =>
    node.current_state == start)
  if let some current_node :=  current_node? then
    -- Find the reachable nodes
    -- Get message then dest states, & transition dest states
    dbg_trace s!"@1 start: {start}"
    let transitions := current_node.transitions.filter (λ transition =>
      transition.trans_type != .Reset
      )
    -- dbg_trace s!">> node_name: {start}"
    -- dbg_trace s!">> transitions: {transitions}"
    let messages := List.join $ transitions.map Transition.messages
    -- dbg_trace s!">> messages: ({messages})"
  -- let msg_dest_node : List Node
    let ctrler_name : String := current_node.ctrler_name
    let dest_states : List (List String) ←
        messages.mapM (λ message =>
          message.findDestState graph.nodes ctrler_name
        )
    let msg'd_states : List String := List.join dest_states
    dbg_trace s!"@2 msg'd_states: {msg'd_states}"
    -- dbg_trace s!">> msg'd_states: {msg'd_states}"
  
    -- transitions
    let transitions : Transitions :=
      current_node.transitions.filter (λ transition =>
        transition.trans_type == .Transition)
    let transitioned_to_states : List StateName :=
      transitions.map (λ transition => transition.dest_state)

    let unique_msg'd_states : List String := msg'd_states.eraseDups
    let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
    dbg_trace s!"@3 unique_transitioned_to_states: {unique_transitioned_to_states}"
  
  -- Remove the unique_msg'd_states from the list of reachable nodes!
  -- This is because they're also technically pre-receive nodes as well...
    let reachable_nodes_by_message : (List Node) :=  (←
      unique_msg'd_states.mapM (λ state_name => graph.findNodesReachableByTransitionAndMessage state_name)).join
      -- NOTE: Commented out since i think i need these states just for their transitions in the
      -- unique constraint path finding function
    -- let reachable_nodes_by_message_without_await_states : List Node :=
    --   reachable_nodes_by_message.filter (λ node => !(unique_msg'd_states.contains node.current_state))

    let reachable_nodes_by_transition : List Node := (←
      unique_transitioned_to_states.mapM (λ state_name => graph.findNodesReachableByTransitionAndMessage state_name)).join
  
    let reachable_nodes : List Node := current_node ::
      -- reachable_nodes_by_message_without_await_states ++ reachable_nodes_by_transition
      reachable_nodes_by_message ++ reachable_nodes_by_transition
    return reachable_nodes.eraseDups
  else
    throw "Node not found"

partial def CDFG.Graph.preReceiveStates
(start : StateName) (graph : Graph) (don't_visit : List StateName) (post_receive_nodes : List Node)
: Except String (List Node) := do
  dbg_trace s!">> pre start node: {start}"
  let msg'd_states : List StateName ← graph.unique_msg'd_states_by_node start
  dbg_trace s!">> msg'd_states: {msg'd_states}"
  let transitioned_to_states_not_pred_on_post_receive : List StateName
    ← graph.unique_transition'd_states_by_node_not_pred_on start post_receive_nodes
  dbg_trace s!">> trans'd to states: {transitioned_to_states_not_pred_on_post_receive}"

  -- if transition is predicated on a message from a post_receive_state then don't do the traversal
  let unique_msg'd_states : List String := msg'd_states.eraseDups
  let unique_transitioned_to_states : List String := transitioned_to_states_not_pred_on_post_receive.eraseDups
  let next_unique_states_to_visit : List String := unique_msg'd_states ++ unique_transitioned_to_states
  let next_states_to_visit : List String := next_unique_states_to_visit.filter (λ state => !(don't_visit.contains state))
  
  dbg_trace s!"!!1"
  let curr_node ← graph.node_from_name! start 
  let reachable_nodes : List Node := [ curr_node ] ++ (←
    next_states_to_visit.mapM (graph.preReceiveStates · don't_visit post_receive_nodes) ).join

  pure reachable_nodes.eraseDups


-- NOTE: This is causing overflow?
def CDFG.Graph.earliest_node_by_msg_dist (graph : Graph) (nodes : List Node)
: Except String Node := do
  let not_transitioned_or_messaged_state : Node ← graph.findNotTransitionedToState
  /- Then from here traverse the graph from the start node -/
  /- Produce Labels for each state of "Message Distance" and "State Distance" -/
  dbg_trace s!"not_transitioned_or_messaged_state: {not_transitioned_or_messaged_state}"
  let labelled_by_msg_distance : (List (StateName × Distance)) ←
    graph.labelNodesByMessageDistance not_transitioned_or_messaged_state.current_state 0
  dbg_trace s!"labelled_by_msg_distance: {labelled_by_msg_distance}"
  
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
  dbg_trace s!"receive_states_and_transitions_labelled: {receive_states_and_transitions_labelled}"
  dbg_trace s!"first_receive_node: {first_receive_node}"

  pure first_receive_node

partial def CDFG.Node.is_node_reaches_complete : Node → Graph → Except String Bool
| node, graph => do
  let trans := node.transitions;  
  match trans.any (·.trans_type == .Completion ) with
  | true => do pure true
  | false =>
    match trans.filter (·.trans_type == .Transition) with
    | [] => pure false
    | one_or_more =>
      let dest_states := one_or_more.map (·.dest_state )
      dbg_trace s!"!!2"
      let dest_nodes ← dest_states.mapM (graph.node_from_name! ·)
      let dest_nodes_reach_complete ← dest_nodes.mapM (·.is_node_reaches_complete graph)
      pure $ dest_nodes_reach_complete.any (· == true )

def CDFG.Graph.global_receive_node_of_inst_type (graph : Graph) (inst_type : InstType)
: Except String Node := do
  let receive_states_and_transitions : List Node ←
    graph.getReceiveStates inst_type

  let receive_state : Node ←
    match receive_states_and_transitions with
    | [state] => pure state
    | _ :: _ =>
      let receive_states_that_reach_completion ← receive_states_and_transitions.filterM (·.is_node_reaches_complete graph)
      let earliest_receive_states_that_reach_completion ← graph.earliest_node_by_msg_dist receive_states_that_reach_completion
      pure earliest_receive_states_that_reach_completion
    | [] =>
      -- If none found, error!
      throw "Error: No receive state found"

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
  dbg_trace s!"$$3 Match stmt: {stmt} with msg: {msg}, from ctrler: {src_ctrler}"
  match stmt with
  | .await (Option.none) stmts => do
    let when_stmt := ← match stmts with
      | [when_stmt] => pure when_stmt
      | _ => throw "Error: Await statement should have one statement"
    when_stmt.matches_msg_from_ctrler msg src_ctrler
  | .when qual_name /- idents -/ _ /- stmt -/ _ => do
    let ctrler_name_matches := ( ← qual_name.first ) == ( src_ctrler )
    let msg_name_matches := ( ← qual_name.second ) == ( ← msg.name )
    dbg_trace s!"$$4 qual_name: {qual_name}"
    dbg_trace s!"$$5 ctrler_name_matches: {ctrler_name_matches}, msg_name_matches: {msg_name_matches}"
    pure (ctrler_name_matches && msg_name_matches)
  | _ => pure false

def CDFG.Condition.is_await_on_msg_from_ctrler (condition : Condition) (msg : Message) (src_ctrler : CtrlerName) : Except String Bool := do
  match condition with
  | .AwaitCondition await_stmt => do
    let bool := await_stmt.matches_msg_from_ctrler msg src_ctrler
    dbg_trace s!"$$6.6 bool: {bool}"
    bool
  | _ => pure false

def CDFG.Transitions.transitions_awaiting_on_msg_from_ctrler : Transitions → Message → CtrlerName  → Except String Transitions
| transitions, msg, src_ctrler => do
  let trans_awaiting_msg ← transitions.filterM (
    let bool := ·.predicate.anyM (
      let bool' := ·.is_await_on_msg_from_ctrler msg src_ctrler
      dbg_trace s!"$$6.5 bool': {bool'}"
      bool'
      );
    dbg_trace s!"$$6 bool: {bool}"
    -- dbg_trace s!"$$6.5 predicates: {·.predicate}"
    bool
    )
  dbg_trace s!"$$7 trans_awaiting_msg: ({trans_awaiting_msg})"
  pure trans_awaiting_msg

def CDFG.Transitions.transitions_awaiting_on_option_msg_from_ctrler : Transitions → Option ( Message × CtrlerName ) → Except String Transitions
| transitions, none =>
  dbg_trace s!"$$1 just return transitions"
  pure transitions
| transitions, some ( msg, src_ctrler ) => do
  dbg_trace s!"$$2 find transitions awaiting on msg: {msg}"
  let trans_awaiting_msg := ← transitions.transitions_awaiting_on_msg_from_ctrler msg src_ctrler
  -- dbg_trace s!"$$8 trans_awaiting_msg: ({trans_awaiting_msg})"
  pure trans_awaiting_msg

#eval [2,1,2,3].reverse.eraseDups.reverse
#eval [[1,2],[1,3]].map (λ x => List.join $ x.map (λ y => if [[1,2],[1,3]].all (λ z => z.contains y ) then [ y ] else [] ))

partial def CDFG.Graph.ctrler_trans_paths_and_constraints
(start : StateName) (graph : Graph) (path_constraints : CtrlerPathConstraint) (msg_trans_should_await : Option (Message × CtrlerName))
: Except String (List CtrlerPathConstraint) := do
  dbg_trace s!"Ctrler Path Constraint, at: ({start}))"
  dbg_trace s!"Ctrler Path Constraint, path_constraints: ({path_constraints})"
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
    dbg_trace s!"^^msg'd_trans_compl_transitions : ({msg'd_trans_compl_transitions})"

    let messages := List.join $ msg'd_trans_compl_transitions.map (·.messages)
    dbg_trace s!"^^messages: ({messages})"
    let current_ctrler : String := current_node.ctrler_name
    let dest_states : List (List (StateName × Message)) ← messages.mapM (λ msg => do
      let msg'd_states : List String := (← msg.findDestState graph.nodes current_ctrler)
      dbg_trace s!"msg'd_states: ({msg'd_states})"
      pure $ ZipWithList msg'd_states msg)
    dbg_trace s!"^^messages and dest_state: ({dest_states})"
    let msg'd_states : List ( StateName × Message ) := List.join dest_states
  
    let unique_msg'd_states : List ( StateName × Message ) := msg'd_states.eraseDups

    let paths_from_msg'd_states : List CtrlerPathConstraint := List.join $ ← unique_msg'd_states.mapM (λ (node_name, msg) => do
      dbg_trace s!"!!3"
      graph.ctrler_trans_paths_and_constraints node_name (CtrlerPathConstraint.new_path_of_ctrler_and_node (← msg.dest_ctrler ) (← graph.node_from_name! node_name)) (Option.some (msg, current_ctrler)))
    dbg_trace s!"Ctrler Path Constraint, paths_from_msg'd_states: ({paths_from_msg'd_states})"

    -- TODO: get the transition constraints from just those transitions that
    -- await on the msg 

    -- TODO: add to path_constraints for the current ctrler,
    -- Recursive call for each transition
    -- transitions

    dbg_trace s!"$$ transitions: ({current_node.transitions})"
    dbg_trace s!"$$ trans_transitions: ({trans_transitions})"
    dbg_trace s!"$$ msg we're awaiting on: ({msg_trans_should_await})"
    dbg_trace s!"^^ trans_transitions: ({trans_transitions})"
    let transitions_of_interest := ← trans_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await
    dbg_trace s!"^^transitions_of_interest: ({transitions_of_interest})"
    let transitioned_to_states : List StateName := transitions_of_interest.map (·.dest_state)
    -- group by dest_state × transitions
    let trans_to_dest_state : List (StateName × Transitions) := transitioned_to_states.map (λ state_name => (state_name, transitions_of_interest.filter (·.dest_state == state_name)))
    let dest_state_and_common_constraints : List (StateName × (List ConstraintInfo) ) :=
      trans_to_dest_state.map (λ (state_name, trans_to_same_state) =>
        -- Get constraints common to all transitions.
        -- Should probably later also predicate paths based on inst
        dbg_trace s!"~~3 trans_to_same_state: ({trans_to_same_state})"
        let common_constraints := List.join (trans_to_same_state.map (·.constraint_info.filter (λ constraint => trans_to_same_state.all (·.constraint_info.any (· == constraint) ) ) ) ) 
        dbg_trace s!"~~2 common_constraints: ({common_constraints})"
        let no_dup_common_constraints := common_constraints.reverse.eraseDups.reverse
        dbg_trace s!"~~4 no dup constraints: ({no_dup_common_constraints})"
        (state_name, no_dup_common_constraints))

    dbg_trace s!"~~1 path_constraints: ({path_constraints})"
    let ctrler_path_constraints_with_curr_node := path_constraints.add_path_node current_node
    let paths_to_dest_states_with_common_constraints : List CtrlerPathConstraint := List.join $ ← 
      dest_state_and_common_constraints.mapM (λ (state_name, common_constraints) => do
        dbg_trace s!"Did we error here1? state_name: {state_name}"
        dbg_trace s!"!!4"
        let node ← graph.node_from_name! state_name  
        let path_constraints_with_added_constraints := (ctrler_path_constraints_with_curr_node.add_constraints_and_path_node common_constraints node)
        dbg_trace s!"~~5 path with constraints: ({path_constraints_with_added_constraints})"
        (
          graph.ctrler_trans_paths_and_constraints state_name path_constraints_with_added_constraints none 
        )
        )
    dbg_trace s!"Ctrler Path Constraint, paths_to_dest_states_with_common_constraints: ({paths_to_dest_states_with_common_constraints})"

    -- TODO: sth for completion type transitions
    -- TODO: join and return the paths that exist
    -- process results
    if !complete_transitions.isEmpty then
      let all_path_constraints := List.join $ List.join $ ctrler_path_constraints_with_curr_node.path.map (·.transitions.map (·.constraint_info) )
      let a_complete_path := ctrler_path_constraints_with_curr_node.add_constraints all_path_constraints
      let a_complete_path_and_others := pure $ ctrler_path_constraints_with_curr_node :: paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
      dbg_trace s!"@42.0 POST a_complete_path: ({a_complete_path})"
      a_complete_path_and_others
    else
      pure $ paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
  else
    throw "Node not found"

-- TODO: Implement a path constraint finder for pre-receive states
-- It should avoid going to nodes from the post-receive states
-- i.e. add input arg for nodes/graph to avoid

partial def CDFG.Graph.pre_receive_states_constraints
: StateName → Graph → CtrlerPathConstraint → Option (Message × CtrlerName) → Graph → Except String (List CtrlerPathConstraint) 
| start, pre_receive_states, ctrler's_path_constraints, msg_trans_should_await?, post_receive_states => do
  -- dbg_trace s!"pre graph: ({pre_receive_states})"
  -- dbg_trace s!"post graph: ({post_receive_states})"
  dbg_trace s!"Did we error here2? state_name: {start}"
  dbg_trace s!"Pre-Receive State Fn: start: ({start})"
  dbg_trace s!"!!5"
  let current_node ← pre_receive_states.node_from_name! start

  let trans_transitions : Transitions := current_node.transitions.filter (·.trans_type == .Transition)
  let complete_transitions := current_node.transitions.filter (·.trans_type == .Completion)
  let trans_compl_transitions := trans_transitions ++ complete_transitions
  let msg'd_trans_compl_transitions := ← trans_compl_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await?

  let messages := List.join $ msg'd_trans_compl_transitions.map (·.messages)
  let ctrler_name : String := current_node.ctrler_name
  let dest_states : List (List (StateName × Message)) ← messages.mapM (λ msg => do
    let msg'd_states : List String := (← msg.findDestState pre_receive_states.nodes ctrler_name)
    pure $ ZipWithList msg'd_states msg)
  let msg'd_states : List ( StateName × Message ) := List.join dest_states
  
  let unique_msg'd_states : List ( StateName × Message ) := msg'd_states.eraseDups
  dbg_trace s!"@@2.01 unique_msg'd_states: ({unique_msg'd_states})"

  let paths_from_msg'd_states : List CtrlerPathConstraint := List.join $ ← unique_msg'd_states.mapM (λ (node_name, msg) => do
    if post_receive_states.nodes.any (·.current_state == node_name) then
      return []
    else
      pre_receive_states.pre_receive_states_constraints node_name (CtrlerPathConstraint.new_path_of_ctrler (← msg.dest_ctrler)) (Option.some (msg, ctrler_name)) post_receive_states)
  dbg_trace s!"@@2.02 paths_from_msg'd_states: ({paths_from_msg'd_states})"

  -- TODO: get the transition constraints from just those transitions that
  -- await on the msg 

  -- TODO: add to path_constraints for the current ctrler,
  -- Recursive call for each transition
  -- transitions

  let transitions_of_interest := ← trans_transitions.transitions_awaiting_on_option_msg_from_ctrler msg_trans_should_await?
  dbg_trace s!"## Transitions of interest: ({transitions_of_interest})"
  let transitioned_to_states : List StateName := transitions_of_interest.map (·.dest_state)
  -- group by dest_state × transitions
  let trans_to_dest_state : List (StateName × Transitions) := transitioned_to_states.map (λ state_name => (state_name, transitions_of_interest.filter (·.dest_state == state_name)))
  dbg_trace s!"## Trans'd to states: ({transitions_of_interest})"
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
      dbg_trace s!"Did we error here3? state_name: {state_name}"
      dbg_trace s!"!!6"
      let node ← pre_receive_states.node_from_name! state_name  
      (
        pre_receive_states.pre_receive_states_constraints state_name
        (ctrler_path_constraints_with_curr_node.add_constraints_and_path_node common_constraints node) none post_receive_states
      )
      )

  -- TODO: sth for completion type transitions
  -- TODO: join and return the paths that exist
  -- process results
  if !complete_transitions.isEmpty || trans_to_dest_state_not_in_post_receive_states.isEmpty then
    let all_path_constraints := List.join $ List.join $ ctrler_path_constraints_with_curr_node.path.map (·.transitions.map (·.constraint_info) )
    let a_complete_path := ctrler_path_constraints_with_curr_node.add_constraints all_path_constraints
    let a_complete_path_and_others := pure $ ctrler_path_constraints_with_curr_node :: paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints
    dbg_trace s!"@42.0 PRE a_complete_path: ({a_complete_path})"
    a_complete_path_and_others
  else 
    pure $ paths_from_msg'd_states ++ paths_to_dest_states_with_common_constraints

-- Rationale behind doing reverse -> eraseDups -> reverse & ordering of items... (an approximation)
#eval [1,2,5,1,2,3,5].eraseDups
#eval [1,2,5,1,2,3,5].reverse.eraseDups.reverse

def CanonStatePathsApprox : List CtrlerPathConstraint → List CtrlerPathConstraint
| potential_duplicate_state_paths =>
  dbg_trace s!"@@%% CanonStatePathsApprox: potential_duplicate_state_paths: {potential_duplicate_state_paths}"
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
        dbg_trace s!"@@2.5 potential match of constraints: post_ctrler_constraints:({post_ctrler_constraints}) pre_ctrler_constraints:({pre_ctrler_constraints})"

        let path_nodes := post_ctrler_constraint.path
        let constraints_unique_to_post := post_ctrler_constraints.filter (!pre_ctrler_constraints.contains ·)
        pure [({ctrler := ctrler, path := path_nodes, constraints := constraints_unique_to_post} : CtrlerPathConstraint)]
      else
        -- let msg := s!"No pre-ctrler constraint for ctrler:({ctrler}) found in path_constraints:({pre_ctrler_constraints})"
        -- throw msg
        dbg_trace s!"@@2.4 No pre-ctrler constraint for ctrler:({ctrler})"
        pure []
    )
  let non_empty_unique_post_constraints := unique_post_ctrler_constraints.filter (·.constraints.length > 0)
  pure non_empty_unique_post_constraints

def CDFG.Graph.nodes_transitioning_to_node (graph : Graph) (node : Node)
: (List Node) :=
  graph.nodes.filter (·.transitions.any (·.is_transition_to_state_name node.current_state))

partial def CDFG.Graph.first_msging_ctrler_node_from_node (graph : Graph) (node : Node) (visited : List Node)
: Except String Node := do
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
    match await_predicates with
    | [] => do
      throw s!"Error: No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
    | _ => do
      -- the first message pass to the node is the "first" message that starts this ctrler's state machine
      -- assuming I'm adding to the predicate list in the order of the stmts
      let await_pred := await_predicates[0]!
      await_pred.await_pred's_sending_node graph
  | _ => do
   -- if nodes_transitioning_to_node.length > 0 then
    -- recursive search
    -- shouldn't be any cycles in graph, but just in case...
    -- TODO
    let nodes_not_visited : List Node := nodes_transitioning_to_node.filter (λ node' => !(visited.contains node'))
    let first_msging_ctrler_node_list ← nodes_not_visited.mapM (graph.first_msging_ctrler_node_from_node · (visited.concat node))
    -- check to confirm all paths lead to the same node
    match first_msging_ctrler_node_list with
    | [] =>
      dbg_trace s!">>node: ({node})"
      dbg_trace s!">>visited: ({visited})"
      dbg_trace s!">>nodes_transitioning_to_node: ({nodes_transitioning_to_node})"
      dbg_trace s!">>nodes_not_visited: ({nodes_not_visited})"
      dbg_trace s!">>first_msging_ctrler_node_list: ({first_msging_ctrler_node_list})"
      throw s!"Error: No first msging ctrler node found. Node: ({node}).\nNodes transitioning to node: ({nodes_transitioning_to_node})"
    | _ => do
      let all_same_node := first_msging_ctrler_node_list.all (· == first_msging_ctrler_node_list[0]!)
      match all_same_node with
      | true => pure first_msging_ctrler_node_list[0]!
      | false => throw "Error: Not all paths lead to the same node"

partial def CDFG.Graph.first_msging_ctrler_node_from_node? (graph : Graph) (node : Node) (visited : List Node)
: Except String (Option Node) := do
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
    match await_predicates with
    | [] => do
      -- throw s!"Error: No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
      dbg_trace s!"No await predicate found. Node: ({node}). Transitions: ({basic_transitions})"
      pure Option.none
    | _ => do
      -- the first message pass to the node is the "first" message that starts this ctrler's state machine
      -- assuming I'm adding to the predicate list in the order of the stmts
      let await_pred := await_predicates[0]!
      await_pred.await_pred's_sending_node graph
  | _ => do
   -- if nodes_transitioning_to_node.length > 0 then
    -- recursive search
    -- shouldn't be any cycles in graph, but just in case...
    -- TODO
    let nodes_not_visited : List Node := nodes_transitioning_to_node.filter (λ node' => !(visited.contains node'))
    let first_msging_ctrler_node_list ← nodes_not_visited.mapM (graph.first_msging_ctrler_node_from_node · (visited.concat node))
    -- check to confirm all paths lead to the same node
    match first_msging_ctrler_node_list with
    | [] =>
      dbg_trace s!">>node: ({node})"
      dbg_trace s!">>visited: ({visited})"
      dbg_trace s!">>nodes_transitioning_to_node: ({nodes_transitioning_to_node})"
      dbg_trace s!">>nodes_not_visited: ({nodes_not_visited})"
      dbg_trace s!">>first_msging_ctrler_node_list: ({first_msging_ctrler_node_list})"
      throw s!"Error: No first msging ctrler node found. Node: ({node}).\nNodes transitioning to node: ({nodes_transitioning_to_node})"
    | _ => do
      let all_same_node := first_msging_ctrler_node_list.all (· == first_msging_ctrler_node_list[0]!)
      match all_same_node with
      | true => pure (Option.some first_msging_ctrler_node_list[0]!)
      | false => throw "Error: Not all paths lead to the same node"

def CDFG.Condition.is_predicated_by_is_head_api (cond : Condition) : Bool :=
  match cond with
  | .DSLExpr cond_expr => -- recursive search for if there's a function call in any term
    cond_expr.is_contains_is_head_api
  | _ => false

def CDFG.Node.is_complete_trans_pred_is_head (node : Node ) (ctrler_name : CtrlerName)
: Except String Bool := do
  let transitions_completes : Transitions := node.transitions.filter (·.trans_type != .Reset)
  let trans_msging_ctrler := ← transitions_completes.filterM (·.messages.anyM (·.is_dest_equals ctrler_name))
  pure $ trans_msging_ctrler.all (·.predicate.any CDFG.Condition.is_predicated_by_is_head_api)

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

partial def CDFG.Graph.is_node_inserted_to_in_PO (graph : Graph) (node : Node) (ctrlers : List controller_info)
: Except String Bool := do
  dbg_trace s!"<< heuristic search"
  dbg_trace s!"node_name: ({node.current_state})"
  -- 1. Recursive back track through nodes until we find one not transitioned to, get node that msgs it
  let msging_node_of_input_node? : Option Node ← graph.first_msging_ctrler_node_from_node? node []
  -- 2. Check transitions predicated on msg from other ctrler
    -- AZ NOTE: Heuristic should check there's no Unordered queue in the path, and at least 1 ordered FIFO pred by is_head
  if let some msging_node_of_input_node := msging_node_of_input_node? then
    let is_msging_node_trans_pred_is_head : Bool ← msging_node_of_input_node.is_complete_trans_pred_is_head node.ctrler_name
    let is_node_transition_path_PO : Bool ← msging_node_of_input_node.is_msg_in_order graph ctrlers
    -- 3. Do a recursive back track through nodes, checking for transitions that are pred by is_head
    if is_msging_node_trans_pred_is_head || is_node_transition_path_PO then
      let prev_nodes_are_also_inserted_to_in_PO : Bool ← graph.is_node_inserted_to_in_PO msging_node_of_input_node ctrlers
      pure (prev_nodes_are_also_inserted_to_in_PO)
    else
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

partial def CDFG.Graph.downstream_messaged_states : Graph → Graph → Node → Except String (List Node)
| total_graph, post_receive_graph, curr_node => do
  let unique_msg'd_states ← curr_node.unique_msg'd_states total_graph
  let msg'd_states_in_post_receive_graph : List Node := post_receive_graph.nodes.filter (·.current_state ∈ unique_msg'd_states)
  -- pure msg'd_states_in_post_receive_graph
  let basic_transitions : Transitions := curr_node.transitions.filter (·.trans_type == .Transition)
  let trans'd_states := basic_transitions.transitioned_to_states_within_graph post_receive_graph

  let downstream_msg'd_states := List.join $ ← (trans'd_states.mapM (total_graph.downstream_messaged_states post_receive_graph ·))
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
(post_receive_graph : Graph) (pre_receive_graph : Graph) (total_graph : Graph) (receive_mem_resp_or_later_state : StateName) (ctrlers : List controller_info)
(dist : Distance)
: Except String (Option (CtrlerStates × Distance)) := do
  -- Check the state we're starting at, if:
  -- 1. if the ctrler is a queue
  -- 2. if it's a queue, is it inserted into in PO
  -- 3. will it be already in this queue while younger insts are at the stall state i.e. 'point a'?

  dbg_trace s!"!!7"
  let node ← post_receive_graph.node_from_name! receive_mem_resp_or_later_state
  -- 1.
  let ctrler ← CtrlerMatchingName ctrlers node.ctrler_name
  let is_a_queue : Bool := (← ctrler.type).is_a_queue

  -- 2.
  let node_is_inserted_into_in_PO ← total_graph.is_node_inserted_to_in_PO node ctrlers

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
    let states_msg'd_by_this_ctrler's_post_receive_states ← total_graph.downstream_messaged_states post_receive_graph node 
    let states_msg'd := states_msg'd_by_this_ctrler's_post_receive_states.map (·.current_state)
    let downstream_ctrlers ← states_msg'd.mapM (CDFGCtrlerStatesToStallOnThatAreSeparateFromPreReceiveStates post_receive_graph pre_receive_graph total_graph · ctrlers (dist + 1))
    let just_downstream_ctrlers := downstream_ctrlers.filter (·.isSome)
    match just_downstream_ctrlers with
    | [] => pure none
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

def CDFG.Graph.queue_ctrler_distance_from_node (graph : Graph) (start_name : StateName) (ctrlers : List controller_info)
: Except String (List (CtrlerName × Distance)) := do
  let nodes_labelled_by_msg_distance : List (StateName × Distance) ← graph.labelNodesByMessageDistance start_name 0
  dbg_trace s!"!!8"
  let node_nodes_by_dist : List (Node × Distance) ← nodes_labelled_by_msg_distance.mapM (λ (ctrler_name, dist) => do pure (← graph.node_from_name! ctrler_name , dist))
  let ctrler_names := graph.nodes.map (·.ctrler_name) |> List.eraseDups

  -- filter away ctrler_names that are not queues
  let queue_ctrler_names : List CtrlerName := ← ctrler_names.filterM (λ ctrler_name => do pure $ (← (← CtrlerMatchingName ctrlers ctrler_name).type) != .BasicCtrler)

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

def CDFG.Condition.is_await (condition : Condition) : Bool :=
  match condition with
  | .AwaitCondition /- await_stmt -/ _ => true
  | _ => false

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
  dbg_trace s!"!!9"
  let states ← ctrler_states.states.mapM (ctrler_graph.node_from_name! ·)
  let await_states := states.filter (·.is_await_state)
  -- 2. & 3.
  let non_trans'd_to_await_states := await_states.filter (! ·.do_other_states_basic_transition_to_this_state ctrler_graph)
  let non_trans'd_to_await_state_names := non_trans'd_to_await_states.map (·.current_state)

  let pruned_state_names := ctrler_states.states.filter (! non_trans'd_to_await_state_names.contains ·)
  pure {ctrler := ctrler_states.ctrler, states := pruned_state_names}

def CtrlerStates.prune_states_with_no_complete_path (ctrler_states : CtrlerStates) (post_receive_graph : Graph)
: Except String CtrlerStates := do
  -- 1.
  dbg_trace s!"States Before pruning non-complete-path states: {ctrler_states.states}"
  let ctrler_nodes : List CDFG.Node := post_receive_graph.nodes.filter (·.ctrler_name == ctrler_states.ctrler )
  let ctrler_nodes_on_path_that_reach_complete ← ctrler_nodes.filterM (·.is_node_reaches_complete post_receive_graph)
  let ctrler_node_names_that_reach_complete := ctrler_nodes_on_path_that_reach_complete.map (·.current_state)
  dbg_trace s!"States after pruning non-complete-path states: {ctrler_node_names_that_reach_complete}"
  pure {ctrler := ctrler_states.ctrler, states := ctrler_node_names_that_reach_complete}

def CtrlerStates.prune_pre_receive_await_and_no_complete_path_states (ctrler_states : CtrlerStates) (post_receive_graph : Graph)
: Except String CtrlerStates := do
  (← ctrler_states.prune_states_with_no_complete_path post_receive_graph).prune_pre_receive_await_states post_receive_graph
  -- let pruned_no_pre_await_states ← ctrler_states.prune_pre_receive_await_states post_receive_graph

def CtrlerPathConstraint.ctrler_constraints : CtrlerPathConstraint → CtrlerConstraint
| .mk ctrler /- path -/ _ constraints =>
  {ctrler := ctrler, constraints := constraints}

-- TODO: finish
-- One of the Main funcs.
def find_ctrler_or_state_to_query_for_stall (graph : Graph) (inst_to_check_completion : InstType) (ctrlers : List controller_info)
: Except String (Sum CtrlerStates CtrlerStateExpr) := do
  dbg_trace s!"<< Starting find_ctrler_or_state_to_query_for_stall"
  let receive_state_to_search_from : Node ←
    graph.global_receive_node_of_inst_type inst_to_check_completion

  /- Use res, find all post-'receive' states -/
  let post_receive_states_with_dups : (List Node) :=
      (← (graph.findNodesReachableByTransitionAndMessage receive_state_to_search_from.current_state)).concat receive_state_to_search_from
  let post_receive_states : (List Node) := post_receive_states_with_dups.eraseDups
  -- dbg_trace s!">> post_receive_states: ({post_receive_states})"

  /- Use post-'receive' states, find all pre-'receive' states -/
  -- let pre_receive_states : (List Node) :=
  -- NOTE: This is the "starting point" state
  let not_transitioned_or_messaged_state : Node ← graph.findNotTransitionedToState
  let pre_receive_states : (List Node) ← 
        (graph.preReceiveStates not_transitioned_or_messaged_state.current_state [receive_state_to_search_from.current_state] post_receive_states)

  let post_receive_graph : Graph := { nodes := post_receive_states }
  dbg_trace s!"@@@-1 post_receive_graph: ({post_receive_graph})"
  let receive_state_name : StateName := receive_state_to_search_from.current_state
-- : StateName → Graph → CtrlerPathConstraint → Option Message → Graph → Except String (List CtrlerPathConstraint) 
  dbg_trace s!"@@@0 receive_state_name: ({receive_state_name})"
  let post_receive_states_paths : List CtrlerPathConstraint ← 
    post_receive_graph.ctrler_trans_paths_and_constraints receive_state_name ( CtrlerPathConstraint.new_path_of_ctrler receive_state_name ) none
  dbg_trace s!"@@@1 post_receive_paths: ({post_receive_states_paths})"
  let canonized_post_receive_paths : List CtrlerPathConstraint := CanonStatePathsApprox post_receive_states_paths
  dbg_trace s!"@@@1.1 canon post_receive_paths: ({canonized_post_receive_paths})"


  -- dbg_trace s!">> pre_receive_states: ({pre_receive_states})"
  let pre_receive_graph : Graph := { nodes := pre_receive_states }
  dbg_trace s!"@@@1.5 pre_receive_graph: ({pre_receive_graph})"
  let pre_receive_states_paths : List CtrlerPathConstraint ← 
    pre_receive_graph.pre_receive_states_constraints not_transitioned_or_messaged_state.current_state ( CtrlerPathConstraint.new_path_of_ctrler receive_state_name ) none post_receive_graph
  dbg_trace s!"@@@2 pre_receive_paths: ({pre_receive_states_paths})"
  let canonized_pre_receive_paths : List CtrlerPathConstraint := CanonStatePathsApprox pre_receive_states_paths
  dbg_trace s!"@@@2.1 canonized_pre_receive_paths: ({canonized_pre_receive_paths})"

  -- use canonized post & pre receive graphs, check for constraints unique to post receive path
  let ctrler_constraints_unique_to_post_receive : List CtrlerPathConstraint ← PostReceivePathsUniqueConstraints canonized_post_receive_paths canonized_pre_receive_paths
  dbg_trace s!"@@@3 post-receive unique constraints: ({ctrler_constraints_unique_to_post_receive})"

  -- USE POST RECEIVE STATES?
  let ctrler_states_and_dist ← CDFGCtrlerStatesToStallOnThatAreSeparateFromPreReceiveStates post_receive_graph pre_receive_graph graph receive_state_to_search_from.current_state ctrlers 0

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
      let ctrlers_dist_labels : List (CtrlerName × Distance) ← post_receive_graph.queue_ctrler_distance_from_node receive_state_to_search_from.current_state ctrlers
      let ctrler_names := [ctrler_states.ctrler, ctrler_path_constraints.ctrler]

      let closer_ctrler_to_receive_state ← min_dist_ctrler ctrlers_dist_labels ctrler_names
      if closer_ctrler_to_receive_state == ctrler_states.ctrler then
        -- use ctrler_states
        -- TODO: Create a function for this:
        -- Pass this info in a structure
        let ctrler_states_pruned_awaits ← ctrler_states.prune_pre_receive_await_and_no_complete_path_states post_receive_graph
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
    let ctrler_states_pruned_awaits ← ctrler_states.prune_pre_receive_await_and_no_complete_path_states post_receive_graph
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

def CDFG.Graph.global_perform_node_of_inst_type (graph : Graph) (inst_type : InstType)
: Except String Node := do
  let global_perform_nodes : List Node :=
    (← graph.nodes.filterM (·.transitions.anyM (·.messages.anyM (·.is_global_perform_of_type inst_type))))
  if global_perform_nodes.length == 1 then
    pure global_perform_nodes[0]!
  else if global_perform_nodes.length > 1 then
    -- Label by msg distance, take shortest one
    graph.earliest_node_by_msg_dist global_perform_nodes
  else
    dbg_trace s!"Graph Nodes: ({graph.nodes.map (·.current_state)})"
    throw "Error: No global perform node found"

-- def CDFG.Condition.is_predicated_by_search_older_seq_num (cond : Condition) : Bool :=
--   match cond with
--   | .APICondition await_stmt => -- recursive search for if there's a function call in any term
--     await_stmt.is_self_search_older_seq_num_success
--   | _ => false


partial def CDFG.Graph.PO_inserted_ctrler_node_from_node (graph : Graph) (node : Node) (ctrlers : List controller_info)
: Except String Node := do
  dbg_trace s!"<< heuristic search"
  dbg_trace s!"node_name: ({node.current_state})"
  -- 1. Recursive back track through nodes until we find one not transitioned to, get node that msgs it
  let msging_node_of_input_node : Node ← graph.first_msging_ctrler_node_from_node node []
  -- 2. Check transitions predicated on msg from other ctrler
    -- AZ NOTE: Heuristic should check there's no Unordered queue in the path, and at least 1 ordered FIFO pred by is_head
  let is_msging_node_trans_pred_is_head : Bool ← msging_node_of_input_node.is_complete_trans_pred_is_head node.ctrler_name
  let is_node_transition_path_PO : Bool ← msging_node_of_input_node.is_msg_in_order graph ctrlers
  -- 3. Do a recursive back track through nodes, checking for transitions that are pred by is_head
  if is_msging_node_trans_pred_is_head || is_node_transition_path_PO then
    pure node
  else
    -- recursive search
    graph.PO_inserted_ctrler_node_from_node msging_node_of_input_node ctrlers

def find_stall_point_heuristic (graph : Graph) (inst_type : InstType) (ctrlers : List controller_info)
: Except String CtrlerState := do
  -- First, find the first state, which is the state that sends the global perform msg
  dbg_trace s! ">> Get global perform node"
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
  dbg_trace s! ">> before starting the recursive search"
  let stall_node ← graph.PO_inserted_ctrler_node_from_node global_perform_node ctrlers
  pure ({ctrler := stall_node.ctrler_name, state := stall_node.current_state} : CtrlerState)

-- import Pipeline.DSLtoCDFG

def CDFGInOrderTfsm (ctrlers : List controller_info) (inst_to_stall_type : InstType) (inst_to_stall_on_type : InstType)
: Except String (List controller_info) := do
  dbg_trace "<< Starting CDFGInOrderTfsm"
  let graph_nodes ← DSLtoCDFG ctrlers
  dbg_trace "<< Got graph nodes translated"
  let graph := {nodes := graph_nodes}
  let stall_point ← find_stall_point_heuristic graph inst_to_stall_on_type ctrlers
  dbg_trace s!"<< Found stall point from heuristic: ({stall_point})"
  let ctrler_state_to_stall_on : StateOrConstraintToStallOn ← find_ctrler_or_state_to_query_for_stall graph inst_to_stall_type ctrlers
  dbg_trace "<< Found ctrler/state to stall at"

  let stall_node ← CreateStallNode stall_point ctrler_state_to_stall_on ctrlers inst_to_stall_on_type 

  let new_state_name := stall_point.ctrler ++ "_stall_" ++ stall_point.state
  let updated_ctrlers ← UpdateCtrlerWithNode ctrlers stall_point.ctrler new_state_name stall_node stall_point.state

  pure updated_ctrlers

def CDFG.Transition.is_has_result_write_labelled (transition : Transition)
: (Option Pipeline.Statement) :=
  let result_write_stmts := transition.effects.find? (·.is_result_write_from_effects)
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
  let stmt := node_labelled_stmt.labelled_stmt
  match stmt with
  | .stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name arg_exprs)) => do
    if qual_name == [reg_file, write].to_qual_name && arg_exprs.length == 2 then
      -- return a register file read
      -- Should check copy the original register file read call in?
      if let some written_reg_expr := arg_exprs[1]? then
        pure $ CreateDSLVarAssignmentStmt old_var_dest (CreateDSLRefFileReadExpr reg_file read [written_reg_expr])
      else
        throw s!"Error, shouldn't get here, since this scope is only reached if arg_exprs.length == 2"
    else
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
  | _ => throw s!"Error while accessing the old load value's Statement. Got an unexpected Statement: ({stmt})"

def NodeLabelledStmt.stmts_to_write_replay_value_to_result_write_location
(node_labelled_stmt : NodeLabelledStmt) (replay_var : Identifier)
: Except String Pipeline.Statement := do
  let stmt := node_labelled_stmt.labelled_stmt
  match stmt with
  | .stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call qual_name arg_exprs)) => do
    if qual_name == [reg_file, write].to_qual_name && arg_exprs.length == 2 then
      -- return a register file read
      -- Should check copy the original register file read call in?
      -- let written_reg_expr : Pipeline.Expr :=
      --   if H : arg_exprs.length = 2 then
      --     have h : 1 < arg_exprs.length := by simp[H];
      --     arg_exprs[1]'h
      --   else
      --     default
      if let some written_reg_expr := arg_exprs[1]? then
        pure <| CreateDSLMsgCall reg_file write [replay_var.to_dsl_var_expr, written_reg_expr]
      else
        throw s!"Error, shouldn't get here, since this scope is only reached if arg_exprs.length == 2"
    else
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
  | _ => throw s!"Error while accessing the old load value's Statement. Got an unexpected Statement: ({stmt})"
  
  default

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
  let receive_state_to_search_from! : Except String Node := graph.global_receive_node_of_inst_type load

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

def Pipeline.Statement.is_commit_labelled (stmt : Pipeline.Statement) : Bool :=
  match stmt with
  | Pipeline.Statement.labelled_statement label /-stmt-/ _ =>
    match label with
    | .commit => true
    | _ => false
  | _ => false

-- def CDFG.Transition.has_commit_labelled_effect (transition : Transition) : Bool :=
--   transition.effects.any (·.is_commit_labelled)

def CDFG.Transition.has_commit_labelled_stmt (transition : Transition) : Bool :=
  transition.effects.any (·.is_commit_labelled)

def CDFG.Graph.commit_transition_state_ctrler (graph : Graph) : Except String Node := do
  let global_perform_nodes : List Node :=
    (graph.nodes.filter (·.transitions.any (·.has_commit_labelled_stmt)))

  match global_perform_nodes with
  | [] => throw "Error: No commit state found"
  | [node] => pure node
  | _ => throw "Error: More than one commit state found"
  

def CDFG.Graph.commit_state_ctrler (graph : CDFG.Graph) : Except String CDFG.Node := do
  -- Find commit state & ctrler
  let commit_state! : Except String Node := graph.commit_transition_state_ctrler

  commit_state!.throw_exception_nesting_msg "Error while finding where the Commit State is"

def CDFG.Node.is_node_transition_or_complete_pred_on_msg_from_state : Node → Node → Except String Bool
| this_node, predicating_node => do
  let pred_node_trans := predicating_node.transitions.filter (·.trans_type != .Reset)
  -- Only consider transitions that commit
  let pred_node_commit_trans := pred_node_trans.filter (·.has_commit_labelled_stmt)
  let pred_node_msgs := List.join $ pred_node_commit_trans.map (·.messages) -- NOTE: What if there are multiple commit transitions?
  -- Find any messages the predicating node sends to this node that are from the "Committing Transition(s)"
  let msgs_from_predicating_node_to_this_node ← pred_node_msgs.filterM (λ msg => do
    let dest_ctrler := ← msg.dest_ctrler;
    pure $ dest_ctrler == this_node.ctrler_name
    )
  
  let this_node_transitions := this_node.transitions.filter (·.trans_type != .Reset)
  let this_node_preds := List.join $ this_node_transitions.map (·.predicate)
  let this_node_await_preds := this_node_preds.filter (·.is_await)
  -- Match the msgs with the awaits here
  let msgs_from_pred_that_this_node_awaits := ← msgs_from_predicating_node_to_this_node.filterM (λ msg => do -- writing this more explicitly, lean's parser couldn't work this out...
    -- let msg_name ← msg.name
    let ctrler_name ← msg.dest_ctrler
    this_node_await_preds.anyM (CDFG.Condition.is_await_on_msg_from_ctrler · msg ctrler_name)
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

def CDFG.Node.not_reset_transitions (node : Node) : Transitions := node.transitions.filter (·.trans_type != .Reset)

def CDFG.Node.result_write_stmt (node : Node) : Except String Pipeline.Statement := do
  let not_reset_trans := node.not_reset_transitions
  let effects := not_reset_trans.map (·.effects) |>.join |>.eraseDups
  let result_write_stmts := effects.filter (·.is_result_write_from_effects)

  match result_write_stmts with
  | [] => throw "Error: No result write stmts found"
  | [stmt] => pure stmt
  | _ => throw "Error: More than one result write stmts found"

