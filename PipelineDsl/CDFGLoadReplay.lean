
import PipelineDsl.AST
import PipelineDsl.CDFG

import PipelineDsl.DSLtoCDFG

import PipelineDsl.CDFGAnalysis

import PipelineDsl.LoadReplayHelpers
import PipelineDsl.CreateDSLHelpers

-- namespace LoadReplay

-- The 4 nodes
open CDFG in
structure CommitIssueAwaitValueStmtNodes where
commit_node : Node
global_perform_load_node : Node
global_complete_load_node : Node
old_load_value_node : NodeLabelledStmt

def replay_generated_prefix : String := "replay_generated_"

open CDFG in
def ReplayAwaitStateName (global_complete_load_node : Node) : StateName :=
  let await_response_state_name := global_complete_load_node.current_state
  let replay_await_state_name := replay_generated_prefix ++ await_response_state_name

  replay_await_state_name

def await_replay_start := "await_replay_start"

def StartReplayAwaitMsgName (await_ctrler_name : CtrlerName) : String :=
  await_replay_start ++ "_" ++ await_ctrler_name

def replay_issue_load_to_mem := "replay_issue_load_to_mem"

def CreateReplayIssueLoadState
(is_issue_ctrler_and_await_response_ctrler_same : Bool)
(is_issue_ctrler_pred_on_commit : Bool)
-- (replay_await_state_name : StateName)
(four_nodes : CommitIssueAwaitValueStmtNodes)
(issue_ctrler_node_pred_on_commit? : Option CDFG.Node)
(ctrlers : Ctrlers)
: Except String (Pipeline.Description) := do
  -- NOTE: Temporary way to get a "replay" load stmt
  -- Proper way should read the load API usage, and copy any
  -- dependent stmts
  -- This would also then be a list of statments
  let replay_load_stmt : Pipeline.Statement := CreateDSLMsgCall memory_interface load_perform []

  let (transition_stmt, additional_stmts) : Pipeline.Statement × (List Pipeline.Statement) := ← 
    if is_issue_ctrler_and_await_response_ctrler_same then
      -- transition to the generated replay_await_response
      let replay_await_state_name := ReplayAwaitStateName four_nodes.global_complete_load_node
      let transition_to_replay_await : Pipeline.Statement := Pipeline.Statement.transition replay_await_state_name

      pure (transition_to_replay_await, [])
    else
      -- Add msg stmt to the ctrler the await response is in
      let await_ctrler_name := four_nodes.global_complete_load_node.ctrler_name
      let start_replay_await_msg_name := StartReplayAwaitMsgName await_ctrler_name

      let start_replay_await_msg := CreateDSLMsgCall await_ctrler_name start_replay_await_msg_name []

      -- Generate the transition to the dest state
      if is_issue_ctrler_pred_on_commit then -- && !is_issue_ctrler_and_await_response_ctrler_same
        -- transition to existing await commit state in the issue load ctrler
        if let some issue_ctrler_node_pred_on_commit := issue_ctrler_node_pred_on_commit? then
          let issue_ctrler_state_that's_pred_on_commit := issue_ctrler_node_pred_on_commit.current_state
          let transition_to_issue_ctrler_state := Pipeline.Statement.transition issue_ctrler_state_that's_pred_on_commit

          pure (transition_to_issue_ctrler_state, [start_replay_await_msg])
        else
          throw "While CreateReplayIssueLoadState, couldn't find the state issue is predicated by commit on"
      else do -- !is_issue_ctrler_pred_on_commit && !is_issue_ctrler_and_await_response_ctrler_same
        -- transition to the first state in the issue ctrler
        let issue_ctrler : Ctrler ← ctrlers.ctrler_from_name four_nodes.global_perform_load_node.ctrler_name
        let issue_ctrler_first_state : StateName ← issue_ctrler.init_trans_dest
        let transition_to_first_state := Pipeline.Statement.transition issue_ctrler_first_state

        pure (transition_to_first_state, [start_replay_await_msg])
  
  let replay_issue_stmts_blk := Pipeline.Statement.block ([replay_load_stmt] ++ additional_stmts ++ [transition_stmt])
  let replay_issue_name := replay_issue_load_to_mem
  let replay_state := Pipeline.Description.state replay_issue_name replay_issue_stmts_blk

  pure replay_state

def GenAwaitReplayLoadStmts (ctrlers : Ctrlers) (four_nodes : CommitIssueAwaitValueStmtNodes)
: Except String (List Pipeline.Statement) := do
  let await_load_resp_ctrler : Ctrler ← ctrlers.ctrler_from_name four_nodes.global_perform_load_node.ctrler_name
  let await_ctrler_type ← await_load_resp_ctrler.type
  let additional_stmts ← match await_ctrler_type with
    | .Unordered => pure [CreateDSLFuncCallStmt remove []]
    | .BasicCtrler => pure []
    | .FIFO => throw "Error while adding await Replay response: FIFO not supported. Due to head & tail ptrs"
  pure additional_stmts

def CreateReplayAwaitLoadState
(is_issue_ctrler_and_await_response_ctrler_same : Bool)
(is_issue_ctrler_pred_on_commit : Bool)
(four_nodes : CommitIssueAwaitValueStmtNodes)
(issue_ctrler_node_pred_on_commit? : Option CDFG.Node)
(ctrlers : Ctrlers)
: Except String Pipeline.Description := do
  -- ===== required stmts: =====
  -- 1. await the load response
  -- Just simply use an await stmt for now
  -- Should try to copy the original await stmt and handling code

  -- 2. read the old value that was marked
  -- look at the old value stmt, if it's from the reg file, then read from the reg file
  -- if it's from a state var, then this is hard to say... maybe we read from the start var
  let old_load_value_stmt : Pipeline.Statement := ←
    four_nodes.old_load_value_node.stmts_to_read_the_old_value old_load_value

  -- 3. compare, handle 2 cases
  --   a. old val == new val: no extra stmts
  --   b. old val != new val: write to where the value is stored
  --   NOTE: This may be more complicated, if the user marks a state var assignment, but
  --   the real place to write to is the register file...

  -- Stmts to
  -- (a) write the correct replay value to the result write location
  -- (b) send squash msg to the commit ctrler

  -- 3.(a)
  let write_correct_replay_value := ←
    four_nodes.old_load_value_node.stmts_to_write_replay_value_to_result_write_location replay_value

  let violating_seq_num_decl_assign := CreateDSLDeclAssignExpr seq_num violating_seq_num inst_seq_num_expr
  -- 3.(b)
  let commit_ctrler_squash := CreateDSLMsgCall four_nodes.commit_node.ctrler_name squash [violating_seq_num.to_dsl_var_expr]

  -- the if cond: old_val != replay_val
  let old_val_not_equal_replay_val_expr : Pipeline.Expr := CreateDSLBoolNotEqualExpr old_load_value replay_value 
  -- the stmts
  let if_old_not_equal_replay_stmts :=
    Pipeline.Statement.block [write_correct_replay_value, violating_seq_num_decl_assign, commit_ctrler_squash]

  -- the if stmt to handle the mispeculated case
  let if_old_not_equal_replay_expr := CreateDSLIfStmt old_val_not_equal_replay_val_expr if_old_not_equal_replay_stmts

  -- 4. signal to ROB that replay is complete
  let replay_complete_msg := CreateDSLMsgCall four_nodes.commit_node.ctrler_name replay_complete_msg_to_commit []

  -- 5. ===== Then handle the transition at the end =====
  let (transition_stmt, additional_stmts) : Pipeline.Statement × (List Pipeline.Statement) := ←
    if is_issue_ctrler_and_await_response_ctrler_same && is_issue_ctrler_pred_on_commit then do
      -- transition to the issue load's state that's pred on commit
      if let some issue_ctrler_node_pred_on_commit := issue_ctrler_node_pred_on_commit? then do
        let issue_ctrler_state_that's_pred_on_commit := issue_ctrler_node_pred_on_commit.current_state
        let transition_to_issue_ctrler_state := Pipeline.Statement.transition issue_ctrler_state_that's_pred_on_commit

        pure (transition_to_issue_ctrler_state, [])
      else do
        throw ""
    else if is_issue_ctrler_and_await_response_ctrler_same then do
      -- transition to the (issue load / await response) ctrler's first state
      let trans_to_first_state ← ctrlers.transition_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      pure (trans_to_first_state, additional_stmts)
    else do
      -- transition to the await load ctrler's first state
      let trans_to_first_state ← ctrlers.transition_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      pure (trans_to_first_state, additional_stmts)
      

  -- Combined from 2. 3. & 4.
  -- NOTE: We add 2. here, but if the ctrler with the old value isn't a "reg_file.read(dest_reg)"
  -- then it's a queue, and we access a queue with a search API where we need to be in scope
  -- But that case is not handled yet..
  let check_mispeculation_and_replay_complete_stmts := Pipeline.Statement.block $
    [old_load_value_stmt, if_old_not_equal_replay_expr, replay_complete_msg] ++ additional_stmts ++ [transition_stmt]

  -- from 1.
  let when_load_response_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [] check_mispeculation_and_replay_complete_stmts
  -- All of the await-replay-load stmts
  let await_load_response := Pipeline.Statement.await none [when_load_response_stmt]

  -- The name of the new await replay state
  let await_replay_state_name := ReplayAwaitStateName four_nodes.global_complete_load_node

  -- Return: The await replay state.
  let await_replay_state := Pipeline.Description.state await_replay_state_name await_load_response.to_block

  return await_replay_state

def await_replay_start_state_name := "LoadReplay_await_replay_start"
def start_replay_msg_name := "start_replay"

def CDFG.Node.create_when_replay_start_msg_state (commit_node : CDFG.Node) : Pipeline.Statement :=
  let transition_to_issue_replay := Pipeline.Statement.transition replay_issue_load_to_mem
  let commit_ctrler := commit_node.ctrler_name
  let when_commit_start_msg := Pipeline.Statement.when [commit_ctrler, start_replay_msg_name].to_qual_name [] transition_to_issue_replay.to_block
  when_commit_start_msg

def CreateReplayAwaitCommitStartMsgState
(four_nodes : CommitIssueAwaitValueStmtNodes)
: (Pipeline.Description) :=
  -- 1. Create an await stmt to wait for the commit start msg
  -- 2. Transition to the right state
  let when_commit_start_msg := four_nodes.commit_node.create_when_replay_start_msg_state
  let await_when := Pipeline.Statement.await none [when_commit_start_msg]

  Pipeline.Description.state await_replay_start_state_name await_when.to_block

def insert := "insert"
def UpdateIssueCtrlerFirstStateToAwaitReplayCommit
(four_nodes : CommitIssueAwaitValueStmtNodes)
(ctrlers : Ctrlers)
: Except String (Pipeline.Description) := do
  -- 1. Get the issue ctrler's first state
  -- 2. Get it's stmts
  -- 3. find the await stmt
  -- 4. add a when stmt to the await stmt

  -- 1.
  let issue_ctrler_name := four_nodes.global_perform_load_node.ctrler_name
  let issue_ctrler ← ctrlers.ctrler_from_name issue_ctrler_name
  let issue_ctrler_first_state ← issue_ctrler.init_trans_dest
  let first_state ← issue_ctrler.state_from_name issue_ctrler_first_state

  -- 2.
  let when_commit_start_msg := ←
    match ← issue_ctrler.type with
    | .BasicCtrler => pure $ four_nodes.commit_node.create_when_replay_start_msg_state
    | .Unordered =>
      let trans_to_await_replay_start := Pipeline.Statement.transition await_replay_start_state_name
      pure $ Pipeline.Statement.when [four_nodes.commit_node.ctrler_name, insert].to_qual_name [] trans_to_await_replay_start.to_block
    | .FIFO => throw "Error while gen await commit start replay msg: Got FIFO ctrler (can't handle due to head/tail ptrs)"
  let first_state ← first_state.append_when_case_to_state's_await_stmt when_commit_start_msg

  pure first_state

def CDFG.Graph.AddLoadReplayToCtrlers (graph : Graph) (ctrlers : Ctrlers) : Except String (Ctrlers) := do
  -- Get the relevant 4 states & ctrlers
  let commit_node ← graph.commit_state_ctrler
  let global_perform_load_node ← graph.load_global_perform_state_ctrler
  let global_complete_load_node ← graph.await_load_receive_state_ctrler
  let old_load_value_node ← graph.old_load_value_stmt_state_ctrler

  let four_nodes := ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩

  let issue_ctrler_type ← (← ctrlers.ctrler_from_name global_perform_load_node.ctrler_name ).type

  let issue_ctrler_pred_on_commit : List Node := (← graph.ctrler_completion_pred_on_commit_states global_perform_load_node.ctrler_name commit_node).eraseDups
  let is_issue_ctrler_pred_on_commit : Bool := issue_ctrler_pred_on_commit.length > 0
  let issue_ctrler_node_pred_on_commit? : Option Node := if is_issue_ctrler_pred_on_commit then issue_ctrler_pred_on_commit.head? else none
  let is_issue_ctrler_and_await_response_ctrler_same := global_perform_load_node.ctrler_name == global_complete_load_node.ctrler_name

  /- =================== Await Commit "Start Replay" Msg State ===================== -/
  -- 
  let (await_replay_start_state, unordered_insert_to_await_state?) : Pipeline.Description × (Option Pipeline.Description) := ←
    if is_issue_ctrler_pred_on_commit then do
      pure (CreateReplayAwaitCommitStartMsgState four_nodes, none)
    else if issue_ctrler_type == .BasicCtrler then do
      pure (← UpdateIssueCtrlerFirstStateToAwaitReplayCommit four_nodes ctrlers, none)
    else if issue_ctrler_type == .Unordered then do
      pure (CreateReplayAwaitCommitStartMsgState four_nodes, ← UpdateIssueCtrlerFirstStateToAwaitReplayCommit four_nodes ctrlers)
    else do
      throw "Error while handling issue ctrler type cases to add await start from commit: Got FIFO ctrler (can't handle due to head/tail ptrs)"
  -- AZ NOTE: Must insert or replace the state accordingly, based on the cases above...

  /- =================== New Issue Replay State ===================== -/
  -- Issue the Replay Memory Request
  let new_issue_replay_state ← CreateReplayIssueLoadState is_issue_ctrler_and_await_response_ctrler_same
    is_issue_ctrler_pred_on_commit ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩  
    issue_ctrler_node_pred_on_commit? ctrlers 

  /- =================== New Await Replay State ===================== -/
  -- ** Awaiting the Replay Memory Response
  let new_await_replay_state ← CreateReplayAwaitLoadState is_issue_ctrler_and_await_response_ctrler_same
    is_issue_ctrler_pred_on_commit ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩  
    issue_ctrler_node_pred_on_commit? ctrlers


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