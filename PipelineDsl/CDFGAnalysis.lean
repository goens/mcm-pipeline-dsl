
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