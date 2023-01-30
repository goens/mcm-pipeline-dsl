
import PipelineDsl.AST
import PipelineDsl.CDFG

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

abbrev MsgName := String
def Pipeline.QualifiedName.msg_name (qual_name : Pipeline.QualifiedName) : Except String MsgName := do
  match qual_name with
  | .mk lst_ident =>
    match lst_ident[1]? with
    | some ident => pure ident
    | none => throw "QualifiedName: Couldn't get 1st element of list of identifiers"

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

def CDFG.Message.findDestState (cdfg_nodes : List CDFG.Node) (msg : Message) (src_ctrler : String)
: Except String (List String) := do
  let dest_ctrler ← msg.dest_ctrler
  let msg_name ← msg.name
  let dest_nodes : List Node := cdfg_nodes.filter (λ node =>
    node.ctrler_name == dest_ctrler
  )
  let dest_nodes_waiting_on_msg : List Node := dest_nodes.filter (
    λ node =>
      -- check if node awaits a message with name msg_name 
      -- could have also just checked the list of predicates
      let transition's_preds_await_msg : List Bool :=
        node.transitions.map (λ transition =>
          match transition.trans_type with
          -- TODO: Change to include completion
          | .Completion => false
          | .Transition =>
            -- Check predicates
            let conds_that_await_msg : List Condition :=
              transition.predicate.filter (λ (cond : Condition) =>
                -- check for await stmts
                match cond with
                | .AwaitCondition await_stmt =>
                  match await_stmt with
                  | .await (none) stmts =>
                    let there_is_a_matching_when : List Bool :=
                      stmts.map (λ stmt =>
                        match stmt with
                        | .when qual_name /- args -/ _ /- stmt -/ _ =>
                          match qual_name with
                          | .mk lst_ident =>
                            let src_ctrler' : String := lst_ident[0]!
                            let msg_name' : String := lst_ident[1]!
                            if src_ctrler' == src_ctrler && msg_name == msg_name' then
                              true
                            else
                              false
                        | _ => false
                        )
                    there_is_a_matching_when.any (λ bool => bool == true)
                  | _ => false
                -- Also check listen-handle
                | .HandleCondition handle_stmt =>
                  match handle_stmt with
                  | .mk qual_name /- args -/ _ /- stmt -/ _ =>
                    match qual_name with
                    | .mk lst_ident =>
                      let src_ctrler' : String := lst_ident[0]!
                      let msg_name' : String := lst_ident[1]!
                      if src_ctrler' == src_ctrler && msg_name == msg_name' then
                        true
                      else
                        false
                | _ => false
              )
            conds_that_await_msg.length > 0
          | .Reset => false
        )
      transition's_preds_await_msg.any (λ bool => bool == true)
  )
  return dest_nodes_waiting_on_msg.map (λ node =>
    node.current_state)

def CDFG.Node.unique_msg'd_states : Node → Graph → Except String (List StateName)
| node, graph => do
  let current_node_name : StateName := node.ctrler_name
  let messages : Messages := List.join $ node.transitions.map (·.messages)
  let msg'd_states : List StateName := (← messages.mapM (·.findDestState graph.nodes current_node_name) ).join

  let unique_msg'd_states : List StateName := msg'd_states.eraseDups
  pure unique_msg'd_states

def CDFG.Node.unique_trans'd_states : Node → (List StateName)
| node =>
  let transitions : Transitions := node.transitions.filter (·.trans_type == .Transition)
  let transitioned_to_states : List StateName := transitions.map (·.dest_state)
  
  transitioned_to_states

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
    throw "Error: No node with name: ({start})"

def CDFG.Graph.unique_transition'd_states_by_node : Graph → StateName → Except String (List StateName)
| graph, state_name => do
  let current_node? : Option Node := graph.node_from_name? state_name
  if let some current_node := current_node? then
    pure current_node.unique_trans'd_states
  else
    throw "Error: No node with name: ({start})"

-- Attempt to make the prev function more succinct
def CDFG.Graph.unique_transition'd_states_by_node' : Graph → StateName → Except String (List StateName)
| graph, state_name => do
  graph.node_map state_name (·.unique_trans'd_states)

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
  | [node] => pure node -- only one node, so return it
  | [] => -- empty
    let msg : String := "Error: No nodes which are not transitioned to? There should be 1?"
    throw msg
  | _ :: _ =>
    let msg : String := "Error: More than one node is not transitioned to or messaged to." ++
      s!"I'm only expecting one 'inst src' state: ({not_transitioned_or_messaged_states_list})"
    throw msg

abbrev Distance := Nat
partial def CDFG.Graph.labelNodesByMessageDistance (start_node : StateName) (message_distance : Distance) (graph : Graph)
: Except String (List (StateName × Distance)) := do
    let unique_msg'd_states : List StateName ←  graph.unique_msg'd_states_by_node start_node
    let unique_transitioned_to_states : List StateName ← graph.unique_transition'd_states_by_node start_node
    -- Look at unique messages
    -- recursive call to Messaged states/ctrlers (which increment counter)
    -- and to transitioned states (which don't increment counter)
    let states_traversed_from_message_list : List (List (StateName × Distance))  ←
      unique_msg'd_states.mapM (labelNodesByMessageDistance · (message_distance + 1) graph)
    let states_traversed_from_message : List (StateName × Distance) := states_traversed_from_message_list.join

    let states_traversed_from_transition_list : List (List (StateName × Distance))  ←
      unique_transitioned_to_states.mapM (labelNodesByMessageDistance · message_distance graph)
    let states_traversed_from_transition : List (StateName × Distance) := states_traversed_from_transition_list.join

    let states_traversed : List (StateName × Distance) :=
      states_traversed_from_message ++ states_traversed_from_transition

    return states_traversed

def get_max_from_tuple_nat_list (lst : List (Node × Nat)) : (Node × Nat) :=
  match lst with
  | [] => (default,0)
  | h::t =>
    let (n, nat) := h
    let (n', nat') := (get_max_from_tuple_nat_list t)
    if (nat) >= nat' then (n,nat) else (n',nat')

#eval get_max_from_tuple_nat_list [(default,2),(default,1)]
#check List.find?
-- AZ NOTE: This ignores states that "await" a message since they're pre-receive
partial def CDFG.Graph.findNodesReachableByTransitionAndMessage (start : String) (graph : Graph)
: Except String (List Node) := do
  let current_node? : Option Node := graph.nodes.find? (λ node =>
    node.current_state == start)
  if let some current_node :=  current_node? then
    -- Find the reachable nodes
    -- Get message then dest states, & transition dest states
    let transitions := current_node.transitions.filter (λ transition =>
      transition.trans_type != .Reset
      )
    let messages := List.join $ transitions.map Transition.messages
  -- let msg_dest_node : List Node
    let current_node_name : String := current_node.current_state
    let dest_states : List (List String) ←
        messages.mapM (λ message =>
          message.findDestState graph.nodes current_node_name
        )
    let msg'd_states : List String := List.join dest_states
  
    -- transitions
    let transitions : Transitions :=
      current_node.transitions.filter (λ transition =>
        transition.trans_type == .Transition)
    let transitioned_to_states : List StateName :=
      transitions.map (λ transition => transition.dest_state)

    let unique_msg'd_states : List String := msg'd_states.eraseDups
    let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
  
    let reachable_nodes_by_message : (List Node) :=  (←
      unique_msg'd_states.mapM (λ state_name => graph.findNodesReachableByTransitionAndMessage state_name)).join
    let reachable_nodes_by_message_without_await_states : List Node :=
      reachable_nodes_by_message.filter (λ node => !(unique_msg'd_states.contains node.current_state))

    let reachable_nodes_by_transition : List Node := (←
      unique_transitioned_to_states.mapM (λ state_name => graph.findNodesReachableByTransitionAndMessage state_name)).join
  
    let reachable_nodes : List Node :=
      reachable_nodes_by_message_without_await_states ++ reachable_nodes_by_transition
    return reachable_nodes.eraseDups
  else
    throw "Node not found"

partial def CDFG.Graph.findNodesReachableByTransitionAndMessageNotInList (start : String) (graph : Graph) (don't_visit : List String)
: Except String (List Node) := do
  let msg'd_states : List StateName ← graph.unique_msg'd_states_by_node start
  let transitioned_to_states : List StateName ← graph.unique_transition'd_states_by_node start

  -- TODO AZ: Remove the unique_msg'd_states from the list of reachable nodes!
  -- This is because they're also technically pre-receive nodes as well...
  let unique_msg'd_states : List String := msg'd_states.eraseDups
  let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
  let next_unique_states_to_visit : List String := unique_msg'd_states ++ unique_transitioned_to_states
  let next_states_to_visit : List String := next_unique_states_to_visit.filter (λ state => !(don't_visit.contains state))
  
  let reachable_nodes : List Node := (←
    next_states_to_visit.mapM (graph.findNodesReachableByTransitionAndMessage ·) ).join

  pure reachable_nodes.eraseDups

def getPostReceiveStates (graph : Graph) (inst_to_check_completion : InstType)
: Except String (List CDFG.Node) := do
  let receive_states_and_transitions : List Node ←
    graph.getReceiveStates inst_to_check_completion

  let receive_state_to_search_from : Except String Node :=
    if receive_states_and_transitions.length == 1 then
      pure receive_states_and_transitions[0]!
    else if receive_states_and_transitions.length > 1 then
      -- Find the earliest receive state
      -- Fewest (1) messages traversed is the "first"
      /- Try to progress by starting from the head node... -/
      /- Get list of all states which have a transition pointing to it -/
      -- i.e. transition dests
      /- And all states which have a message sent to them -/
      -- i.e. match message name and dest_ctrler to dest_ctrler's state
      /- Then filter the Nodes, if they're not in the list of transitioned to & messaged states -/
      let not_transitioned_or_messaged_state : Except String Node := graph.findNotTransitionedToState
      let node_name : Except String Node :=
        match not_transitioned_or_messaged_state with
        | .ok node => 
          /- Then from here traverse the graph from the start node -/
          /- Produce Labels for each state of "Message Distance" and "State Distance" -/
          let labelled_by_msg_distance : Except String (List (StateName × Distance)) :=
            graph.labelNodesByMessageDistance node.current_state 0
          
          let nodes_and_msg_dist : Except String (Node) :=
            match labelled_by_msg_distance with
            | .ok names_labels =>
              let receive_states_and_transitions_labelled : List (CDFG.Node × Nat) :=
                receive_states_and_transitions.map (λ (node) =>
                  let matching_nodes : List (String × Nat) :=
                    names_labels.filter (λ (state_name, /- msg_distance -/ _) =>
                      state_name == node.ctrler_name
                    )
                  let msg_distances : List Nat :=
                    matching_nodes.map (λ (/- state_name -/ _, msg_distance) => msg_distance)
                  let max_distance : Nat :=
                    get_max_from_nat_list msg_distances
                  (node, max_distance)
                )
                
              let (first_receive_node, _) :=
                get_max_from_tuple_nat_list receive_states_and_transitions_labelled
              
              pure (first_receive_node)
            | .error msg => throw msg
          nodes_and_msg_dist
        | .error msg => throw msg


      /- Sort by Message Distance. If multiple, sort by state distance -/
      -- Ignore for now. Just take the first one
      node_name
    else
      -- If none found, error!
      throw "Error: No receive state found"

  /- Use res, find all post-'receive' states -/
  let post_receive_states : (List Node) ←
    match receive_state_to_search_from with
    | .ok node =>
      pure (graph.findNodesReachableByTransitionAndMessage node.current_state)
    | .error msg => throw msg

  /- Use post-'receive' states, find all pre-'receive' states -/
  -- let pre_receive_states : (List Node) :=
  let not_transitioned_or_messaged_state : Except String Node := graph.findNotTransitionedToState
  let pre_receive_states : (List Node) ← 
    match not_transitioned_or_messaged_state with
    | .ok node =>
      match receive_state_to_search_from with
      | .ok receive_node =>
        -- TODO Make sure the function doesn't reach post-receive states
        -- Need to find a way to use the transitions' guard to not
        -- traverse post-receive states
        -- Check if transition fwd requires msg from post-receive state? This could work
        -- AZ TODO: Update the function do work like this signature suggests..
        pure (graph.findNodesReachableByTransitionAndMessageWithoutNeedingPostReceiveMessagesAlsoNotInList node.current_state [receive_node.current_state] post_receive_states)
      | .error msg => throw msg
    | .error msg => throw msg
  -- pre_receive_states

  /- Look at the state sets per ctrler, check for (1) states unique to post-'receive' -/
  /- and (2) Variable Constraints that are unique to post-'receive' -/
  -- let 

  /- Return the Ctrler/State/Variable to stall on info -/

  return []