
import PipelineDsl.AST
import PipelineDsl.CDFG

import PipelineDsl.DSLtoCDFG

import PipelineDsl.CDFGAnalysis

def Ctrlers.replay_in_load_api_ctrler_state (ctrlers : Ctrlers) (graph : Graph) (load_api_state : CDFG.Node) : Except String Ctrlers := do
  -- create a replay load state, simply sends the load request, then transitions to the next state
  default

-- def CreateAwaitLoadResponseState (state_name : StateName) (transition_dest : StateName) (stmts : List Pipeline.Statement)

-- I don't think we need multiple transitions (i.e. within the stmts, like in if-stmts or await-when stmts) just yet...
structure SingleTransitionState where
dest : StateName
stmts : List Pipeline.Statement
name : StateName
deriving Inhabited, BEq

def CDFG.Graph.AddLoadReplayToCtrlers (graph : Graph) (ctrlers : Ctrlers) : Except String (Ctrlers) := do
  -- Get the relevant 4 states & ctrlers
  let commit_node ← graph.commit_state_ctrler
  let global_perform_load_node ← graph.load_global_perform_state_ctrler
  let global_complete_load_node ← graph.await_load_receive_state_ctrler
  let old_load_value_node ← graph.old_load_value_stmt_state_ctrler

  /- =================== Item A ===================== -/
  -- Item A. from comments in the top level function below
  -- For our LSQs we're modeling, the commit & load API nodes are in different ctrlers
  -- don't bother with this check...

  -- If the Load API nodes is in a Queue vs Ctrler

  -- Load API Node's Ctrler .is_predicated_on_commit_ctrler:
  -- Then we can add the Load Replay right before the state where it is predicated on the commit ctrler

  -- OTHERWISE, if the ctrler is a basic Ctrler or an unordered queue,
  -- we add it to the loadAPI ctrler as another exec path

  let load_api_ctrler_pred_on_commit : List Node := (← graph.ctrler_completion_pred_on_commit_states global_perform_load_node.ctrler_name commit_node).eraseDups
  let is_load_api_ctrler_pred_on_commit : Bool := load_api_ctrler_pred_on_commit.length > 0

  let load_api_ctrler : Ctrler ← ctrlers.ctrler_from_name global_perform_load_node.ctrler_name
  let load_api_ctrler_type : CtrlerType ← load_api_ctrler.type

  -- Helper function to check if Ctrler is pred on commit, and Ctrler type
  -- Goal is to add Replay in the right way (at commit pred state, or as alternative exec path) (and know if we need to msg this ctrler)
  -- Do the actual adding of the replay state later, since it also may need to signal to a specific ctrler/state
  let load_api_stmt ← global_perform_load_node.global_perform_load_stmt
  

  /- =================== Item B ===================== -/
  -- Item B. Check the "await mem response" ctrler
  -- Overlap with Item A. If they're in the same ctrler, then the Replay State from A can transition to a generated "await mem response" state
  -- Otherwise, transition to the state it's added at.

  let is_load_api_and_await_load_in_same_ctrler : Bool := global_perform_load_node.ctrler_name == global_complete_load_node.ctrler_name

  let await_replay_mem_response_state_name := global_complete_load_node.ctrler_name ++ "_await_load_replay_mem_response_"

  -- NOTE TODO: Also find the place to add this replay API state
  let replay_api_transition_dest : StateName := ←
    match is_load_api_ctrler_pred_on_commit with
    | true => do -- i.e. this entry lives for the duration of the "perform load" actions
      match is_load_api_and_await_load_in_same_ctrler with
      | true => do -- transition to the added await mem response state
        pure await_replay_mem_response_state_name
      | false => do
        match load_api_ctrler_pred_on_commit with
        | [node_pred_on_commit] => pure node_pred_on_commit.current_state
        | node_pred_on_commit :: _ => pure node_pred_on_commit.current_state
        | [] => throw "Error: () Load API Ctrler is predicated on commit, but no commit state found"
    | false => do -- this is the "create an alternate exec path" scenario, since loads pass through here.
      match is_load_api_and_await_load_in_same_ctrler with
      | true => do -- transition to the added await mem response state
        pure await_replay_mem_response_state_name
      | false => do -- back to first state
        load_api_ctrler.init_trans_dest

  -- 
  -- ** Awaiting the Memory Response
  -- B.
  -- if load API ctrler == await mem response ctrler,
  --   add await mem response to this ctrler
  --   (just Need the 1 state then.)
  --   Then see C. for adding comparison
  -- if load API ctrler != await mem response ctrler,
  --   Murphi NOTE: This assumes I can uniquely know which ctrler is awaiting this specific mem response, thru the seq_num, so ensure seq_nums are reset after complete...
  --   Need 2 states: 1. for moving to a state that specifically awaits for load-replay's response, 2. for receiving the response.
  --   Then see C. for adding comparison
  match is_load_api_and_await_load_in_same_ctrler with
  | true => -- create a state to await the mem response
    let when_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [load_value] compare_and_transition_stmt_blk
    let await_stmt := Pipeline.Statement.await none when_stmt
  | false =>

  -- let await_replay_mem_states : List Pipeline.Description :=
  --   if is_load_api_and_await_load_in_same_ctrler then
  --     -- create a simple await mem response state
  --     -- Dest: the load API state that is predicated on commit
  --   else

      -- create 1. a state that awaits a msg name (uniquely named "<src_ctrler>_await_load_replay_mem_response_<dest_ctrler>, or something like this")
      -- Dest: to 2.
      -- Stmts: await for the unique msg name from the load API ctrler
      -- Name: (this just uses the existing "start" state, so I don't need to make a name, just a when-case)
      -- create 2. a state to await the mem response
      -- Dest: to the "first state"
      -- Stmts: await for mem response, do the comparison C.
      -- Name: <dest_ctrler>_awaiting_load_replay_mem_response_<src_ctrler>

  /- =================== Item C ===================== -/
  -- C. Compare the old load value to the new load value

  -- check if the old load value marked by the label is in the same ctrler as the await_mem_response ctrler
  -- it should be, otherwise throw an error, I haven't modeled this case so far in the LSQs

  -- Create stmts to get the old value (inductive, if the old value is in a state var, or in the rf.write API)

  -- Create comparison, if old==new, then msg Commit ctrler to transition to next state
  -- i.e. Commit ctrler needs an await replay complete state
  -- if old!=new, then update write the new value, just reusing the labelled msg name
  -- also add a Stmt <commit_ctrler>.squash()
  -- Don't need separate transition dests for these cases, just use Part B's transition dests

  let inst_dest_reg_expr := Pipeline.Expr.some_term (Pipeline.Term.qualified_var ["instruction", "dest_reg"].to_qual_name)

  let old_load_value_labelled_stmt := old_load_value_node.labelled_stmt.stmt_of_labelled_stmt
  let old_value_var_name := "old_load_value_load_replay"
  let old_value_stmt := ←
    match old_load_value_labelled_stmt.is_rf_write with
    | true => do
      let var' := Pipeline.TypedIdentifier.mk "u32" old_value_var_name
      let rf_read_expr := Pipeline.Expr.some_term (Pipeline.Term.function_call [rf, read].to_qual_name [inst_dest_reg_expr])
      pure $ Pipeline.Statement.value_declaration var' rf_read_expr
    | false => do throw "Error: Only handling 'result_write' that's rf.write() for now"
  let compare_expr := Pipeline.Expr.equal (Pipeline.Term.var old_value_var_name) (Pipeline.Term.var load_value)

  -- do we message the commit ctrler to transition to the next state?
  let is_await_complete_and_commit_same_ctrler := global_complete_load_node.ctrler_name == commit_node.ctrler_name
  let load_replay_complete_msg_name := "load_replay_complete"
  let msg_commit := ← match is_await_complete_and_commit_same_ctrler with
    | true => do throw "Error: Not handling this case yet, where the commit ctrler is the same ctrler as the await complete ctrler"
    | false =>
      let msg_stmt := Pipeline.Statement.stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call [commit_node.ctrler_name, load_replay_complete_msg_name].to_qual_name []))
      pure msg_stmt

  let load_response_value_var_expr := Pipeline.Expr.some_term (Pipeline.Term.var load_value)
  let write_new_val := Pipeline.Statement.stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call [rf, write].to_qual_name [inst_dest_reg_expr, load_response_value_var_expr]))

  let inst_seq_num := Pipeline.Expr.some_term (Pipeline.Term.qualified_var ["instruction", "seq_num"].to_qual_name)
  let squash_stmt := Pipeline.Statement.stray_expr (Pipeline.Expr.some_term (Pipeline.Term.function_call [commit_node.ctrler_name, "squash"].to_qual_name [inst_seq_num]))

  let compare_equal_blk := Pipeline.Statement.block [msg_commit]
  let compare_not_eq_blk := Pipeline.Statement.block [write_new_val, squash_stmt, msg_commit]

  let compare_squash_conditional := Pipeline.Conditional.if_else_statement compare_expr compare_equal_blk compare_not_eq_blk

  -- Use this as C, in part B.
  -- Maybe move to another helper function to make the higher level idea of this function clearer
  let compare_replay_val_stmt : Pipeline.Statement := Pipeline.Statement.conditional_stmt compare_squash_conditional


  default

def Ctrlers.CDFGLoadReplayTfsm (ctrlers : Ctrlers)
: Except String (List controller_info) := do
  let graph_nodes ← DSLtoCDFG ctrlers
  let graph : CDFG.Graph := {nodes := graph_nodes}

  -- The function should do as the comments below describe.
  let graph_with_load_replay! := graph.AddLoadReplayToCtrlers ctrlers

  graph_with_load_replay!.throw_exception_nesting_msg "Error while adding Load-Replay to Ctrlers!"
  -- ** Get info about ctrlers for load replay
  -- 1. Get the "Commit" Ctrler
  -- 2. Search for where load API is called
  -- 2.5 which ctrler awaits the load response
  -- 3. Search for where the old load value is marked

  -- ** Do the "start replay load"
  -- A. If "Commit" and Load API are in the same ctrler then don't need to generate a msging line of code added.

  -- If they are not in the same ctrler,

  -- IF Load API is in a Queue:
  -- IF the queue entries have a complete transition predicated by the Commit ctrler
  -- place this await state before the state that the ctrler is in waiting for the commit ctrler...
  -- add a msging line of code in the commit ctrler

  -- If the queue entries do not have a complete transition predicated by the Commit ctrler
  -- Throw an error, this is not a handled case

  -- IF Load API is in a Ctrler:
  -- Add an alternative await when case to the Ctrler to do the "replay load" actions

  -- ** Awaiting the Memory Response
  -- B.
  -- if load API ctrler == await mem response ctrler,
  --   add await mem response to this ctrler
  --   Then see C. for adding comparison
  -- if load API ctrler != await mem response ctrler,
  --   add await mem response to the await mem response ctrler
  --   NOTE: This assumes I can uniquely know which ctrler is awaiting this specific mem response, thru the seq_num, so ensure seq_nums are reset after complete...
  --   But this should also go into a "replay state" so it knows to go to the replay states
  --   Thus: add an await when in the ctrler that transitions to another state, to transition to a "replay" version that awaits the mem response
  --   Then see C. for adding comparison

  -- ** Comparison with Old Load Value
  -- C.
  --     if old load value is in the same ctrler as the await mem response,
  --       add a comparison state or if expr here as well
  --     if it's not in the same ctrler,
  --       msg the old load value ctrler and add a comparison state there
  --
  --  If comparison is equal, signal to commit ctrler (if in a different ctrler than commit), if it's in the commit ctrler, just transition to next state (commit)
  --  If comparison is not equal, signal to commit ctrler to

  -- ** Squash
  -- D.
  -- Msg Commit ctrler to squash if the comparison fails