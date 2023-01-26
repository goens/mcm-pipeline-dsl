
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

def findDestStateFromMsg (cdfg_nodes : List CDFG.Node) (dest_ctrler : String) (src_ctrler : String) (msg_name : String)
: List String :=
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
          | .Completion => false
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
      let transition_dests : List String := node.transitions.map (λ transition =>
        transition.dest_state
      )
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

def labelNodesByMessageDistance (start : String) (cdfg_nodes : List CDFG.Node)
: Except String (String × Nat) :=
  -- Get transition messages' dest(s)
  -- just get the unique dests
  let current_node_list : List Node := cdfg_nodes.filter (λ node =>
    node.current_state == start)
  let messages : List (String × String) :=
    List.join (
      current_node_list.map (λ node =>
        -- get node transition messages  
        let node_transition_msgs : List (String × String) := List.join (
          node.transitions.map (λ transition =>
            let transition_msgs : List (String × String) := List.join (
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
            transition_msgs
          )
        )
        node_transition_msgs
      )
    )
  
  -- Look at unique messages
  -- recursive call to Messaged states/ctrlers (which increment counter)
  -- and to transitioned states (which don't increment counter)

  -- let current_node : Node :=
  --   match current_node_list with
  --   | [node] => pure node
  --   | [] =>
  --     let msg : String := s!"Error: No nodes with current state: ({start})"
  --     throw msg
  --   | _ :: _ =>
  --     let msg : String := s!"Error: More than one node with current state: ({start}) ({current_node_list})"
  --     throw msg
  return ("",0)

def getPostReceiveStates (cdfg_nodes : List CDFG.Node) (inst_to_check_completion : InstType)
: Except String (List CDFG.Node) := do
  let receive_states_and_transitions : List (CDFG.Node × Transitions) :=
    getReceiveStatesAndTransitions cdfg_nodes inst_to_check_completion

  let receive_state_to_search_from : Node :=
    if receive_states_and_transitions.length == 1 then
      receive_states_and_transitions[0]!
    else if receive_states_and_transitions.length > 1 then
      -- Find the earliest receive state
      -- Fewest (1) messages traversed is the "first"
      /- Try to progress by starting from the head node... -/
      /- Get list of all states which have a transition pointing to it -/
      -- i.e. transition dests
      /- And all states which have a message sent to them -/
      -- i.e. match message name and dest_ctrler to dest_ctrler's state
      /- Then filter the Nodes, if they're not in the list of transitioned to & messaged states -/
      let not_transitioned_or_messaged_state : Node := findNotTransitionedToState cdfg_nodes

      /- Then from here traverse the graph from the start node -/
      /- Produce Labels for each state of "Message Distance" and "State Distance" -/

      /- Sort by Message Distance. If multiple, sort by state distance -/
    else
      -- If none found, error!
      throw "Error: No receive state found"

  /- Use res, find all post-'receive' states -/

  /- Use post-'receive' states, find all pre-'receive' states -/

  /- Look at the state sets per ctrler, check for (1) states unique to post-'receive' -/
  /- and (2) Variable Constraints that are unique to post-'receive' -/

  /- Return the Ctrler/State/Variable to stall on info -/

  return []