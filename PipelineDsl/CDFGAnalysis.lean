
import PipelineDsl.AST
import PipelineDsl.CDFG

/-
1. Take a CDFG Graph and find states after the receive state
2. Use post receive states to find pre-receive states
3. Identify unique states between the two that remains until the inst is committed
-/

-- #eval [].

open CDFG
-- 1. Take a CDFG Graph and find states after the receive state
-- Get receive states and transitions that receive the desired memory response
def getReceiveStatesAndTransitions (cdfg_nodes : List CDFG.Node) (inst_to_check_completion : InstType)
: List (CDFG.Node × Transitions) :=
  /- i. from state nodes find receive state -/
  /- Check node transitions for receiving the memory response -/
  let receive_states_and_transitions : List (CDFG.Node × Transitions) :=
    List.join (
    cdfg_nodes.map (λ node =>
    let receive_complete_transitions : Transitions :=
      node.transitions.filter (λ transition =>
        match transition.trans_type with
        | .Transition =>
          let receive_complete_transitions :=
          transition.predicate.filter (λ predicate =>
            match predicate with
            | .AwaitCondition await_stmt =>
              -- let the_stmt : Pipeline.Statement := await_stmt
              match await_stmt with
              | .await (none) lst_stmts =>
                let there_is_a_receive_complete : List Bool := List.join (
                  lst_stmts.map (λ stmt =>
                    match stmt with
                    | .when qual_name /- args -/ _ /- stmt -/ _ =>
                      match qual_name with
                      | .mk lst_ident =>
                        let src_ctrler : String := lst_ident[0]!
                        let msg_name : String := lst_ident[1]!
                        let completion_msg_name : String := match inst_to_check_completion with
                          | .load => load_completed
                          | .store => store_completed
                        if src_ctrler == memory_interface && msg_name == completion_msg_name then
                          [true]
                        else
                          []
                    | _ => []
                    ))
                if there_is_a_receive_complete.length > 0 then
                  true
                else
                  false
              | _ => false
            | _ => false
            )
          if receive_complete_transitions.length > 0 then
            true
          else
            false
        | _ => false
      )
    if receive_complete_transitions.length > 0 then
      [(node, receive_complete_transitions)]
    else
      []
    ))
  receive_states_and_transitions

def Message.findDestState (cdfg_nodes : List CDFG.Node) (msg : Message) (src_ctrler : String)
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
  dest_nodes_waiting_on_msg.map (λ node =>
    node.current_state)

def getMessagedOrTransitionedToStates (cdfg_nodes : List CDFG.Node)
: /- Except String -/ (List String) := --do
  let all_transitioned_or_messaged_states_lists := List.join (
    cdfg_nodes.map (λ node =>
      -- get node transition.Transition dests & messages
      let transition_dests : List String := List.join ( node.transitions.map (λ transition =>
        match transition.trans_type with
        | .Transition => [ transition.dest_state ]
        | .Completion => []
        | .Reset => []
        -- transition.dest_state
      ))
      let messaged_states_list : (List String) := List.join (
        node.transitions.map (λ transition =>
          let messaged_states : List String := List.join (
            transition.messages.map (λ (message : Message) =>
              -- Find message dest, use message name and dest_ctrler to find dest_ctrler's state
              let message_term : Pipeline.Term := match message with
                | .mk term => term
              let dest_ctrler_msg : Option (String × String) := 
                match message_term with
                | .function_call qual_name /- exprs -/ _ => 
                  match qual_name with
                  | .mk idents =>
                    let dest_ctrler : String := idents[0]!
                    let msg_name : String := idents[1]!
                    Option.some (dest_ctrler, msg_name)
                | _ => none
              match dest_ctrler_msg with
              | .some (dest_ctrler_name, sent_msg_name) =>
                let msg'd_states : List String :=
                  findDestStateFromMsg cdfg_nodes dest_ctrler_name node.ctrler_name sent_msg_name
                msg'd_states
              | .none => []
            )
          )
          messaged_states
        )
      )
      let all_subsequent_states : List String := transition_dests ++ messaged_states_list
      all_subsequent_states
    )
  )
  -- return all_transitioned_or_messaged_states_lists
  all_transitioned_or_messaged_states_lists

def findNotTransitionedToState (cdfg_nodes : List CDFG.Node)
: Except String Node :=
  -- Get list of state names that are transitioned to or messaged to
  -- Have each node check if they are in this list/set
  -- remaining node should be the "src" of where insts come from...
  let all_transitioned_or_messaged_states_lists : List String := getMessagedOrTransitionedToStates cdfg_nodes
  let not_transitioned_or_messaged_states_list : List Node := cdfg_nodes.filter (λ node =>
    let state_is_a_transitioned_or_messaged_state : Bool :=
      all_transitioned_or_messaged_states_lists.any (λ transitioned_or_messaged_state_name =>
        node.current_state == transitioned_or_messaged_state_name
      )
    !state_is_a_transitioned_or_messaged_state
  )
  match not_transitioned_or_messaged_states_list with
  | [node] => -- only one node, so return it
    pure node
  | [] =>
    let msg : String := "Error: No nodes which are not transitioned to? There should be 1?"
    throw msg
  | _ :: _ =>
    let msg : String := "Error: More than one node is not transitioned to or messaged to." ++
      s!"I'm only expecting one 'inst src' state: ({not_transitioned_or_messaged_states_list})"
    throw msg

partial def labelNodesByMessageDistance (start : String) (message_distance : Nat) (cdfg_nodes : List CDFG.Node)
: Except String (List (String × Nat)) := do
  -- Get transition messages' dest(s)
  -- just get the unique dests
  let current_node_list : List Node := cdfg_nodes.filter (λ node =>
    node.current_state == start)
  -- let current_node : Except String Node :=
  --   match current_node_list with
  --   | [node] => pure node
  --   | [] =>
  --     let msg : String := s!"Error: No nodes with current state: ({start})"
  --     throw msg
  --   | _ :: _ =>
  --     let msg : String := s!"Error: More than one node with current state: ({start}) ({current_node_list})"
  --     throw msg
  let messages : List (String × String) :=
    List.join (
      current_node_list.map (λ node =>
        -- get node transition messages  
        let node_transition_msgs : List (String × String) := List.join (
          node.transitions.map (λ transition =>
            let get_msgs : Bool :=
              match transition.trans_type with
              | .Transition => true
              | .Completion => true
              | .Reset => false
            let transition_msgs : List (String × String) :=
              if get_msgs then
                List.join (
                  transition.messages.map (λ message =>
                    let msg_term :=
                      match message with
                      | .mk term => term
                    
                    let dest_ctrler_msg_list : List (String × String) :=
                      match msg_term with
                      | .function_call qual_name /- exprs -/ _ =>
                        match qual_name with
                        | .mk idents =>
                          let dest_ctrler : String := idents[0]!
                          let msg_name : String := idents[1]!
                          [(dest_ctrler, msg_name)]
                      | _ => []
                    
                    dest_ctrler_msg_list
                  )
                )
              else []
            transition_msgs
          )
        )
        node_transition_msgs
      )
    )
  -- let msg_dest_node : List Node
  let current_node_name : String := current_node_list[0]!.ctrler_name
  let msg'd_states : List String :=
    List.join (
      messages.map (λ (dest_ctrler, msg_name) =>
        findDestStateFromMsg cdfg_nodes dest_ctrler current_node_name msg_name
      )
    )
  
  -- transitions
  let transitioned_to_states : List String :=
    List.join (
      current_node_list.map (λ node =>
        List.join (
          node.transitions.map (λ transition =>
            match transition.trans_type with
            | .Transition => [ transition.dest_state ]
            | .Reset => []
            | .Completion => []
          )
        )
      )
    )

  let unique_msg'd_states : List String := msg'd_states.eraseDups
  let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
  -- Look at unique messages
  -- recursive call to Messaged states/ctrlers (which increment counter)
  -- and to transitioned states (which don't increment counter)
  let states_traversed_from_message_list : List (List (String × Nat))  ←
    unique_msg'd_states.mapM (λ msg'd_state =>
      labelNodesByMessageDistance msg'd_state (message_distance + 1) cdfg_nodes
    )
  let states_traversed_from_message : List (String × Nat) := states_traversed_from_message_list.join

  let states_traversed_from_transition_list : List (List (String × Nat))  ←
    unique_transitioned_to_states.mapM (λ transitioned_to_state =>
      labelNodesByMessageDistance transitioned_to_state message_distance cdfg_nodes
    )
  let states_traversed_from_transition : List (String × Nat) := states_traversed_from_transition_list.join

  let states_traversed : List (String × Nat) :=
    states_traversed_from_message ++ states_traversed_from_transition

  return states_traversed

def get_max_from_tuple_nat_list (lst : List (Node × Transitions × Nat)) : (Node × Transitions × Nat) :=
  match lst with
  | [] => (default,default,0)
  | h::t =>
    let (n, trans, nat) := h
    let (n', trans', nat') := (get_max_from_tuple_nat_list t)
    if (nat) >= nat' then (n,trans,nat) else (n',trans',nat')

#eval get_max_from_tuple_nat_list [(default,[],2),(default,[],1)]
#check List.find?
-- AZ NOTE: This ignores states that "await" a message since they're pre-receive
partial def findNodesReachableByTransitionAndMessage (start : String) (cdfg_nodes : List CDFG.Node)
: List Node := Id.run do
  let current_node? : Option Node := cdfg_nodes.find? (λ node =>
    node.current_state == start)
  if let some current_node :=  current_node? then
    -- Find the reachable nodes
    -- Get message then dest states, & transition dest states
    let transitions := current_node.transitions.filter (λ transition =>
      transition.trans_type != .Reset
      )
    let messages := List.join $ transitions.map Transition.messages
  -- let msg_dest_node : List Node
  let current_node_name : String := current_node_list[0]!.ctrler_name
  let msg'd_states : List String :=
    List.join (
      messages.map (λ (dest_ctrler, msg_name) =>
        findDestStateFromMsg cdfg_nodes dest_ctrler current_node_name msg_name
      )
    )
  
  -- transitions
  let transitioned_to_states : List String :=
    List.join (
      current_node_list.map (λ node =>
        List.join (
          node.transitions.map (λ transition =>
            match transition.trans_type with
            | .Transition => [ transition.dest_state ]
            | .Reset => []
            | .Completion => []
          )
        )
      )
    )

  let unique_msg'd_states : List String := msg'd_states.eraseDups
  let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
  
  let reachable_nodes_by_message : List String := List.join (
    unique_msg'd_states.map (λ state_name => findNodesReachableByTransitionAndMessage state_name cdfg_nodes))
  let reachable_nodes_by_message_without_await_states : List String :=
    reachable_nodes_by_message.filter (λ state_name => !(unique_msg'd_states.contains state_name))

  let reachable_nodes_by_transition : List String := List.join (
    unique_transitioned_to_states.map (λ state_name => findNodesReachableByTransitionAndMessage state_name cdfg_nodes))
  
  let reachable_nodes : List String :=
    reachable_nodes_by_message_without_await_states ++ reachable_nodes_by_transition
  reachable_nodes.eraseDups

partial def findNodesReachableByTransitionAndMessageNotInList (start : String) (cdfg_nodes : List CDFG.Node) (don't_visit : List String)
: List String :=
  let current_node_list : List Node := cdfg_nodes.filter (λ node =>
    node.current_state == start)
  -- Find the reachable nodes
  -- Get message then dest states, & transition dest states
  let messages : List (String × String) :=
    List.join (
      current_node_list.map (λ node =>
        -- get node transition messages  
        let node_transition_msgs : List (String × String) := List.join (
          node.transitions.map (λ transition =>
            let get_msgs : Bool :=
              match transition.trans_type with
              | .Transition => true
              | .Completion => true
              | .Reset => false
            let transition_msgs : List (String × String) :=
              if get_msgs then
                List.join (
                  transition.messages.map (λ message =>
                    let msg_term :=
                      match message with
                      | .mk term => term
                    
                    let dest_ctrler_msg_list : List (String × String) :=
                      match msg_term with
                      | .function_call qual_name /- exprs -/ _ =>
                        match qual_name with
                        | .mk idents =>
                          let dest_ctrler : String := idents[0]!
                          let msg_name : String := idents[1]!
                          [(dest_ctrler, msg_name)]
                      | _ => []
                    
                    dest_ctrler_msg_list
                  )
                )
              else []
            transition_msgs
          )
        )
        node_transition_msgs
      )
    )
  -- let msg_dest_node : List Node
  let current_node_name : String := current_node_list[0]!.ctrler_name
  let msg'd_states : List String :=
    List.join (
      messages.map (λ (dest_ctrler, msg_name) =>
        findDestStateFromMsg cdfg_nodes dest_ctrler current_node_name msg_name
      )
    )
  
  -- transitions
  let transitioned_to_states : List String :=
    List.join (
      current_node_list.map (λ node =>
        List.join (
          node.transitions.map (λ transition =>
            match transition.trans_type with
            | .Transition => [ transition.dest_state ]
            | .Reset => []
            | .Completion => []
          )
        )
      )
    )

  -- TODO AZ: Remove the unique_msg'd_states from the list of reachable nodes!
  -- This is because they're also technically pre-receive nodes as well...
  let unique_msg'd_states : List String := msg'd_states.eraseDups
  let unique_transitioned_to_states : List String := transitioned_to_states.eraseDups
  let next_unique_states_to_visit : List String := unique_msg'd_states ++ unique_transitioned_to_states
  let next_states_to_visit : List String := next_unique_states_to_visit.filter (λ state => !(don't_visit.contains state))
  
  let reachable_nodes : List String := List.join (
    next_states_to_visit.map (λ state_name => findNodesReachableByTransitionAndMessage state_name cdfg_nodes)
  )

  reachable_nodes.eraseDups

def getPostReceiveStates (cdfg_nodes : List CDFG.Node) (inst_to_check_completion : InstType)
: Except String (List CDFG.Node) := do
  let receive_states_and_transitions : List (CDFG.Node × Transitions) :=
    getReceiveStatesAndTransitions cdfg_nodes inst_to_check_completion

  let receive_state_to_search_from : Except String (Node × Transitions) :=
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
      let not_transitioned_or_messaged_state : Except String Node := findNotTransitionedToState cdfg_nodes
      let node_name : Except String (Node × Transitions) :=
        match not_transitioned_or_messaged_state with
        | .ok node => 
          /- Then from here traverse the graph from the start node -/
          /- Produce Labels for each state of "Message Distance" and "State Distance" -/
          let labelled_by_msg_distance : Except String (List (String × Nat)) :=
            labelNodesByMessageDistance node.current_state 0 cdfg_nodes
          
          let nodes_and_msg_dist : Except String (Node × Transitions) :=
            match labelled_by_msg_distance with
            | .ok names_labels =>
              let receive_states_and_transitions_labelled : List (CDFG.Node × Transitions × Nat) :=
                receive_states_and_transitions.map (λ (node, transitions) =>
                  let matching_nodes : List (String × Nat) :=
                    names_labels.filter (λ (state_name, /- msg_distance -/ _) =>
                      state_name == node.ctrler_name
                    )
                  let msg_distances : List Nat :=
                    matching_nodes.map (λ (/- state_name -/ _, msg_distance) => msg_distance)
                  let max_distance : Nat :=
                    get_max_from_nat_list msg_distances
                  (node, transitions, max_distance)
                )
                
              let (first_receive_node, first's_transitions, _) :=
                get_max_from_tuple_nat_list receive_states_and_transitions_labelled
              
              pure (first_receive_node, first's_transitions)
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
  let post_receive_states : Except String (List String) :=
    match receive_state_to_search_from with
    | .ok (node, /- transitions -/ _) =>
      pure (findNodesReachableByTransitionAndMessage node.current_state cdfg_nodes)
    | .error msg => throw msg

  /- Use post-'receive' states, find all pre-'receive' states -/
  let pre_receive_states : Except String (List String) :=
    match post_receive_states with
    | .ok post_receive_state_names =>
      let not_transitioned_or_messaged_state : Except String Node := findNotTransitionedToState cdfg_nodes
      let pre_receive_states : Except String (List String) :=
        match not_transitioned_or_messaged_state with
        | .ok node => pure (findNodesReachableByTransitionAndMessageNotInList node.current_state cdfg_nodes post_receive_state_names)
        | .error msg => throw msg
      pre_receive_states
    | .error msg => throw msg

  /- Look at the state sets per ctrler, check for (1) states unique to post-'receive' -/
  /- and (2) Variable Constraints that are unique to post-'receive' -/
  -- let 

  /- Return the Ctrler/State/Variable to stall on info -/

  return []