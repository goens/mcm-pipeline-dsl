
import PipelineDsl.AST
import PipelineDsl.CDFG

import PipelineDsl.DSLtoCDFG

import PipelineDsl.CDFGAnalysis

import PipelineDsl.LoadReplayHelpers

import PipelineDsl.CDFGInOrderTfsm

import PipelineDsl.InstructionHelpers

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

def GenAwaitReplayLoadStmts (ctrlers : Ctrlers) (four_nodes : CommitIssueAwaitValueStmtNodes)
: Except String (List Pipeline.Statement) := do
  let await_load_resp_ctrler : Ctrler ← ctrlers.ctrler_from_name four_nodes.global_perform_load_node.ctrler_name
  let await_ctrler_type ← await_load_resp_ctrler.type
  let additional_stmts ← match await_ctrler_type with
    | .Unordered => pure [CreateDSLFuncCallStmt remove []]
    | .BasicCtrler => pure []
    | .FIFO => throw "Error while adding await Replay response: FIFO not supported. Due to head & tail ptrs"
  pure additional_stmts

open Pipeline in
def CreateReplayVerifyCheckAndSquash
(is_issue_ctrler_and_await_response_ctrler_same : Bool)
(is_issue_ctrler_pred_on_commit : Bool)
(four_nodes : CommitIssueAwaitValueStmtNodes)
(issue_ctrler_node_pred_on_commit? : Option CDFG.Node)
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(replay_value_var : VarName) -- from replay or fwding store
: Except String (List Statement) := do
  -- ===== required stmts: =====
  -- 1. await the load response
  -- Just simply use an await stmt for now
  -- Should try to copy the original await stmt and handling code

  -- 2. read the old value that was marked
  -- look at the old value stmt, if it's from the reg file, then read from the reg file
  -- if it's from a state var, then this is hard to say... maybe we read from the start var
  let old_load_value_decl : Statement := Statement.variable_declaration (TypedIdentifier.mk u32 old_load_value)
  let old_load_value_stmt : Statement ←
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
  let write_correct_replay_value ←
    four_nodes.old_load_value_node.stmts_to_write_replay_value_to_result_write_location replay_value_var

  let violating_seq_num_decl_assign := CreateDSLDeclAssignExpr seq_num violating_seq_num $ ← inst_seq_num_expr
  -- 3.(b)
  -- let commit_ctrler_squash := CreateDSLMsgCall four_nodes.commit_node.ctrler_name squash [violating_seq_num.to_dsl_var_expr]
  let squash_lat_msg := CreateDSLMsgCall lat_name squash [violating_seq_num.to_dsl_var_expr]
  -- AZ NOTE: Need some kind of fix here, make the load controller squash the load without too much human intervention
  -- let load_ctrler_squash := CreateDSLMsgCall four_nodes.global_complete_load_node.ctrler_name squash [violating_seq_num.to_dsl_var_expr]

  -- the if cond: old_val != replay_val
  let old_val_not_equal_replay_val_expr : Pipeline.Expr := CreateDSLBoolNotEqualExpr old_load_value replay_value_var
  -- the stmts
  let if_old_not_equal_replay_stmts :=
    Pipeline.Statement.block [write_correct_replay_value, violating_seq_num_decl_assign, squash_lat_msg /-, commit_ctrler_squash, load_ctrler_squash-/ ]

  -- the if stmt to handle the mispeculated case
  let if_old_not_equal_replay_expr := CreateDSLIfStmt old_val_not_equal_replay_val_expr if_old_not_equal_replay_stmts

  -- 5. ===== Then handle the transition at the end =====
  dbg_trace s!"%%St-Fwd LR transition 0"
  let (complete_stmt, additional_stmts) : Pipeline.Statement × (List Pipeline.Statement) := ←
    if is_issue_ctrler_and_await_response_ctrler_same && is_issue_ctrler_pred_on_commit then do
      -- transition to the issue load's state that's pred on commit
      if let some issue_ctrler_node_pred_on_commit := issue_ctrler_node_pred_on_commit? then do
        let issue_ctrler_state_that's_pred_on_commit := issue_ctrler_node_pred_on_commit.current_state
        let transition_to_issue_ctrler_state := Pipeline.Statement.transition issue_ctrler_state_that's_pred_on_commit

        dbg_trace s!"%%St-Fwd LR transition 1: transition_to_issue_ctrler_state: ({transition_to_issue_ctrler_state})"
        pure (transition_to_issue_ctrler_state, [])
      else do
        throw "Error, issue ctrler node should be available.."
    else if is_issue_ctrler_and_await_response_ctrler_same then do
      -- transition to the (issue load / await response) ctrler's first state
      let compl_to_first_state ← ctrlers.complete_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      dbg_trace s!"%%St-Fwd LR transition 2: compl_to_first_state: ({compl_to_first_state})"
      pure (compl_to_first_state, additional_stmts)
    else do
      -- TODO: handle case of issue ctrler != await response ctrler; should msg the ctrler?

      throw s!"Error: not handling the case of 'issue ctrler' != 'await response ctrler' yet, in Replay St-Fwding check."
      -- -- transition to the await load ctrler's first state
      -- let compl_to_first_state ← ctrlers.complete_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      -- let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      -- pure (compl_to_first_state, additional_stmts)

  -- 4. signal to ROB that replay is complete
  let replay_complete_func_call_msg := CreateDSLMsgCall four_nodes.commit_node.ctrler_name replay_complete_msg_to_commit []
  let commit_ctrler ← ctrlers.ctrler_from_name four_nodes.commit_node.ctrler_name
  let replay_complete_msg ← commit_ctrler.queue_search_api_to_send_msg replay_complete_func_call_msg (additional_stmts ++ [complete_stmt])


  -- Combined from 2. 3. & 4.
  -- NOTE: We add 2. here, but if the ctrler with the old value isn't a "reg_file.read(dest_reg)"
  -- then it's a queue, and we access a queue with a search API where we need to be in scope
  -- But that case is not handled yet..

  let check_mispeculation_and_replay_complete_stmts := -- Pipeline.Statement.block $
    [old_load_value_decl, old_load_value_stmt, if_old_not_equal_replay_expr, replay_complete_msg]

  pure check_mispeculation_and_replay_complete_stmts

  -- -- from 1.
  -- -- let when_load_response_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [] check_mispeculation_and_replay_complete_stmts
  -- let trans_to_compare_and_squash := Pipeline.Statement.transition "replay_compare_and_check_state"
  -- let when_load_response_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [] trans_to_compare_and_squash.to_block
  -- -- All of the await-replay-load stmts
  -- let await_load_response := Pipeline.Statement.await none [when_load_response_stmt]

  -- -- The name of the new await replay state
  -- let await_replay_state_name := ReplayAwaitStateName four_nodes.global_complete_load_node

  -- let listen_handle_wrapped_await_replay ← four_nodes.global_complete_load_node.wrap_stmt_with_node's_listen_handle_if_exists await_load_response.to_block ctrlers

  -- -- Return: The await replay state.
  -- let await_replay_state := Pipeline.Description.state await_replay_state_name listen_handle_wrapped_await_replay.to_block

  -- let compare_and_check_wrapped_stmts : Pipeline.Statement ←
  --   if is_issue_ctrler_and_await_response_ctrler_same then do
  --     -- match issue_ctrler_node_pred_on_commit? with
  --     -- | .some /- issue_ctrler_node_pred_on_commit -/ _ => do
  --       four_nodes.global_complete_load_node.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block ctrlers
  --     -- | .none =>
  --       -- four_nodes.global_perform_load_node.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block ctrlers
  --   else do
  --     let await_load_ctrler_node := four_nodes.global_perform_load_node
  --     let await_load_ctrler ← ctrlers.ctrler_from_name await_load_ctrler_node.ctrler_name
  --     let await_load_ctrler_first_state ← await_load_ctrler.init_trans_dest_state

  --     await_load_ctrler_first_state.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block

  -- let compare_and_check_state := Pipeline.Description.state "replay_compare_and_check_state" compare_and_check_wrapped_stmts.to_block

  -- dbg_trace "##Sanity 2"
  -- return (await_replay_state, compare_and_check_state)
  -- default
  -- sorry

open Pipeline in
def CreateReplayIssueLoadState
(is_issue_ctrler_and_await_response_ctrler_same : Bool)
(is_issue_ctrler_pred_on_commit : Bool)
-- (replay_await_state_name : StateName)
(four_nodes : CommitIssueAwaitValueStmtNodes)
(issue_ctrler_node_pred_on_commit? : Option CDFG.Node)
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
(lat_seq_num_var lat_address_var : VarName)
(post_commit_store_send_node? : Option CDFG.Node)
: Except String (Pipeline.Description) := do
  -- NOTE: Temporary way to get a "replay" load stmt
  -- Proper way should read the load API usage, and copy any
  -- dependent stmts
  -- This would also then be a list of statments

  let (transition_stmt, additional_stmts) : Pipeline.Statement × (List Pipeline.Statement) := ←
    if is_issue_ctrler_and_await_response_ctrler_same then
      -- transition to the generated replay_await_response
      let replay_await_state_name := ReplayAwaitStateName four_nodes.global_complete_load_node
      let transition_to_replay_await : Pipeline.Statement := Pipeline.Statement.transition replay_await_state_name

      pure (transition_to_replay_await, [])
    else do
      -- Add msg stmt to the ctrler the await response is in
      let await_ctrler_name := four_nodes.global_complete_load_node.ctrler_name
      let start_replay_await_msg_name := StartReplayAwaitMsgName await_ctrler_name

      let start_replay_await_msg := CreateDSLMsgCall await_ctrler_name start_replay_await_msg_name []
      let await_load_ctrler : Ctrler := ← four_nodes.global_complete_load_node.ctrler_of_node ctrlers
      let search_api_to_send_start_replay_await_msg := ← await_load_ctrler.queue_search_api_to_send_msg start_replay_await_msg []

      -- Generate the transition to the dest state
      if is_issue_ctrler_pred_on_commit then -- && !is_issue_ctrler_and_await_response_ctrler_same
        -- transition to existing await commit state in the issue load ctrler
        if let some issue_ctrler_node_pred_on_commit := issue_ctrler_node_pred_on_commit? then
          let issue_ctrler_state_that's_pred_on_commit := issue_ctrler_node_pred_on_commit.current_state
          let transition_to_issue_ctrler_state := Pipeline.Statement.transition issue_ctrler_state_that's_pred_on_commit

          pure (transition_to_issue_ctrler_state, [search_api_to_send_start_replay_await_msg])
        else
          throw "While CreateReplayIssueLoadState, couldn't find the state issue is predicated by commit on"
      else do -- !is_issue_ctrler_pred_on_commit && !is_issue_ctrler_and_await_response_ctrler_same
        -- (complete!) transition to the first state in the issue ctrler
        let issue_ctrler : Ctrler ← ctrlers.ctrler_from_name four_nodes.global_perform_load_node.ctrler_name
        let issue_ctrler_first_state : StateName ← issue_ctrler.init_trans_dest
        let complete_to_first_state := Pipeline.Statement.complete issue_ctrler_first_state

        pure (complete_to_first_state, [search_api_to_send_start_replay_await_msg])

  -- TODO:
  -- Make the replay load stmt use:
  -- 1. The LAT's address
  -- 2. The instruction's seq_num

  -- Get the instruction from the perform load node
  -- four_nodes.global_perform_load_node.ctrler_name
  let entry's_lat_addr_var := qual_var_expr [entry, lat_address_var]
  let inst_seq_num := qual_var_expr [instruction, seq_num]
  let replay_load_stmt : Pipeline.Statement := CreateDSLMsgCall memory_interface load_perform [entry's_lat_addr_var, inst_seq_num]

  -- TODO: Generate the seq_num, either check if this ctrler has an instruction state_var, if it does, use it!
  -- Or make an assumption that the commit ctrler has the instruction
  let replay_issue_stmts_blk := ([replay_load_stmt] ++ additional_stmts ++ [transition_stmt])

  -- ====== Now create the post-commit store ctrler search if one exists ======
  -- and use search_for_load_seq_num if we find no fwding store.
  let fwd_check_pc_store_or_replay : Statement ←
    match post_commit_store_send_node? with
    | none => pure replay_issue_stmts_blk.to_block -- no post-commit st ctrler? then just replay the load
    | some pc_st_ctrler_n => do -- there's a post-commit store ctrler. Check if there's a fwding store
      dbg_trace s!"%%St-Fwd LR 5: going to add the fwding check, pc_st_ctrler_n: ({pc_st_ctrler_n})"
      -- TODO load-replay fwding search: -- Try to finish this on Sunday...
      -- a helper function to search the pc_st_ctrler
      -- Get the post-commit store send node's write (1) val & (2) addr, variable Identifiers
      -- >>> Use this fn to get (1) and (2): get_perform_store_msg_value_addr_vars from CDFGAnalysis.lean
      let (w_val_id, addr_val_id) ← pc_st_ctrler_n.get_perform_store_msg_value_addr_vars
      -- >>> Generalize the search fun: CtrlerStates.query_older_insts in AnalysisHelpers.lean

      -- Search for:
      --   (a) an older store (instruction.seq_num > entry.instruction.seq_num)
      --   (b) with a matching addr..
      --      (i) need both the Store Ctrler's addr, and the Load Ctrler's addr.
      --          Must first search the LAT for the load address ( create an addr var to hold this )
      --          AND then the store controller (check this created local addr var == entry.addr)
      --   (c) (depending on if the ctrler is unordered) min the constraint
      let def_load_addr_name := "replay_load_addr_var"
      -- Stmt.1
      let def_lat_addr_var_stmt := Statement.variable_declaration (TypedIdentifier.mk address def_load_addr_name)
      -- Stmt.2
      let get_lat_load_addr_stmt := AssignSrcToDestVar def_load_addr_name entry's_lat_addr_var -- NOTE: This is the load's addr

      let pc_st_c_type ← ctrlers.ctrler_from_name pc_st_ctrler_n.ctrler_name >>= (·.type)
      -- pc-st-ctrler.Cond.1 -- store is older
      let older_store_cond_e := InstructionEntryIsOlder pc_st_ctrler_n.ctrler_name pc_st_c_type
      -- pc-st-ctrler.Cond.2 -- store addr matches load addr
      let store_with_matching_addr_cond_e ← CtrlerEntryFieldMatches pc_st_ctrler_n.ctrler_name pc_st_c_type [def_load_addr_name] [addr_val_id]
      -- pc-st-ctrler.Cond.3 (Optional) -- youngest older store
      let youngest_older_store_cond_e ← MinDifferenceBetween [instruction, seq_num] [instruction, seq_num] false
      let youngest_older_store_cond_e? := match pc_st_c_type with
        | .Unordered => some youngest_older_store_cond_e
        | .FIFO | .BasicCtrler => none
      -- pc-st-ctrler.Cond.4 -- Check that it's a store!
      let entry_is_a_store_cond_e ← CtrlerEntryIsStore pc_st_ctrler_n.ctrler_name pc_st_c_type

      -- pc-st-ctrler Condition.
      let fwding_store_search_cond ← AndExprs
        [older_store_cond_e, store_with_matching_addr_cond_e, entry_is_a_store_cond_e]

      -- >>> The replay-verification code is probably here: CreateReplayAwaitLoadState in this file
      let fwding_replay_verif_stmts ← CreateReplayVerifyCheckAndSquash
        is_issue_ctrler_and_await_response_ctrler_same
        is_issue_ctrler_pred_on_commit
        four_nodes
        issue_ctrler_node_pred_on_commit?
        ctrlers
        lat_name
        w_val_id

      let no_fwding_st_found_bool__ := "no_fwding_st_found_bool__"
      -- Stmt.3.a
      let no_fwding_st_found_bool___decl_stmt :=
        Statement.value_declaration
          (TypedIdentifier.mk "bool" no_fwding_st_found_bool__)
          (term_expr $ Term.const $ str_lit "false")
      -- Stmt.3.b
      let set_no_fwding_st_found_bool__stmt := AssignSrcToDestVar no_fwding_st_found_bool__ (term_expr $ Term.const $ str_lit "true") -- NOTE: This is the load's addr

      -- If there's a matching entry:
      --   Already got the post-commit store send node's write (1) val & (2) addr
      --   Use the (1) val to do the replay-verification:
      --     Will need to take the existing replay-verification code
      --     and generalize it to accept a value var name as something to compare against..?

      -- If there's no fwding store:
      --   just use the search_for_load_seq_num statement.

      -- Stmt.3
      let query_pc_st_c_stmt ←
        QueryCtrlerWithConstraints
          -- Query Search Conditions
          fwding_store_search_cond youngest_older_store_cond_e?
            -- If found or not found case stmt blks
            fwding_replay_verif_stmts.to_block
            [set_no_fwding_st_found_bool__stmt].to_block
            -- NOTE: don't place the replay_issue_stmts_block here!
            -- place it after this block instead...?
            -- replay_issue_stmts_blk -- already a block
          -- Queried Ctrler's Type
          pc_st_c_type
          -- Queried Ctrler's Name
          pc_st_ctrler_n.ctrler_name

      -- Stmt.3.c
      let only_replay_if_no_fwding_found_stmt : Statement :=
        Pipeline.Statement.conditional_stmt $ if_statement
          (var_expr no_fwding_st_found_bool__)
          (Pipeline.Statement.block replay_issue_stmts_blk)

      let set_lat_addr_and_query_pc_st_c_stmts : Statement :=
        Pipeline.Statement.block $
          [def_lat_addr_var_stmt, get_lat_load_addr_stmt, no_fwding_st_found_bool___decl_stmt, query_pc_st_c_stmt, only_replay_if_no_fwding_found_stmt]
      pure set_lat_addr_and_query_pc_st_c_stmts

  -- then replace replay_issue_stmts_blk with search_for_load_seq_num
  -- NOTE: Must think of way to reset the LAT upon mis-match
  -- Generate some handle block to listen to squash... from the commit node
  -- NOTE: let lat_not_found_error := Statement.error "Couldn't find the lat entry!"
  let search_for_load_seq_num : SearchStatement ← lat_name.unordered_query_match
    fwd_check_pc_store_or_replay [].to_block
    [ lat_seq_num_var ] [instruction, seq_num]

  let replay_issue_in_listen_handle ←
    four_nodes.global_perform_load_node.wrap_stmt_with_node's_listen_handle_if_exists
      search_for_load_seq_num ctrlers

  let replay_issue_name := replay_issue_load_to_mem
  let replay_state := Pipeline.Description.state replay_issue_name replay_issue_in_listen_handle.to_block

  pure replay_state

open Pipeline in
def CreateReplayAwaitLoadState
(is_issue_ctrler_and_await_response_ctrler_same : Bool)
(is_issue_ctrler_pred_on_commit : Bool)
(four_nodes : CommitIssueAwaitValueStmtNodes)
(issue_ctrler_node_pred_on_commit? : Option CDFG.Node)
(ctrlers : Ctrlers)
(lat_name : CtrlerName)
: Except String (Pipeline.Description × Pipeline.Description) := do
  -- ===== required stmts: =====
  -- 1. await the load response
  -- Just simply use an await stmt for now
  -- Should try to copy the original await stmt and handling code

  -- 2. read the old value that was marked
  -- look at the old value stmt, if it's from the reg file, then read from the reg file
  -- if it's from a state var, then this is hard to say... maybe we read from the start var
  let old_load_value_decl : Statement := Statement.variable_declaration (TypedIdentifier.mk u32 old_load_value)
  let old_load_value_stmt : Statement ←
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
  let write_correct_replay_value ←
    four_nodes.old_load_value_node.stmts_to_write_replay_value_to_result_write_location replay_value

  let violating_seq_num_decl_assign := CreateDSLDeclAssignExpr seq_num violating_seq_num $ ← inst_seq_num_expr
  -- 3.(b)
  let commit_ctrler_squash := CreateDSLMsgCall four_nodes.commit_node.ctrler_name squash [violating_seq_num.to_dsl_var_expr]
  let squash_lat_msg := CreateDSLMsgCall lat_name squash [violating_seq_num.to_dsl_var_expr]
  -- AZ NOTE: Need some kind of fix here, make the load controller squash the load without too much human intervention
  let load_ctrler_squash := CreateDSLMsgCall four_nodes.global_complete_load_node.ctrler_name squash [violating_seq_num.to_dsl_var_expr]

  -- the if cond: old_val != replay_val
  let old_val_not_equal_replay_val_expr : Pipeline.Expr := CreateDSLBoolNotEqualExpr old_load_value replay_value
  -- the stmts
  let if_old_not_equal_replay_stmts :=
    Pipeline.Statement.block [write_correct_replay_value, violating_seq_num_decl_assign, squash_lat_msg /-, commit_ctrler_squash, load_ctrler_squash-/ ]

  -- the if stmt to handle the mispeculated case
  let if_old_not_equal_replay_expr := CreateDSLIfStmt old_val_not_equal_replay_val_expr if_old_not_equal_replay_stmts

  -- 5. ===== Then handle the transition at the end =====
  let (complete_stmt, additional_stmts) : Pipeline.Statement × (List Pipeline.Statement) := ←
    if is_issue_ctrler_and_await_response_ctrler_same && is_issue_ctrler_pred_on_commit then do
      -- transition to the issue load's state that's pred on commit
      if let some issue_ctrler_node_pred_on_commit := issue_ctrler_node_pred_on_commit? then do
        let issue_ctrler_state_that's_pred_on_commit := issue_ctrler_node_pred_on_commit.current_state
        let transition_to_issue_ctrler_state := Pipeline.Statement.transition issue_ctrler_state_that's_pred_on_commit

        pure (transition_to_issue_ctrler_state, [])
      else do
        throw "Error, issue ctrler node should be available.."
    else if is_issue_ctrler_and_await_response_ctrler_same then do
      -- transition to the (issue load / await response) ctrler's first state
      let compl_to_first_state ← ctrlers.complete_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      pure (compl_to_first_state, additional_stmts)
    else do
      -- transition to the await load ctrler's first state
      let compl_to_first_state ← ctrlers.complete_to_ctrler's_first_state_stmt four_nodes.global_perform_load_node.ctrler_name

      -- add a remove() func_call if the ctrler is a queue, don't for basic ctrlers, error for FIFOs
      let additional_stmts ← GenAwaitReplayLoadStmts ctrlers four_nodes

      pure (compl_to_first_state, additional_stmts)

  -- 4. signal to ROB that replay is complete
  let replay_complete_func_call_msg := CreateDSLMsgCall four_nodes.commit_node.ctrler_name replay_complete_msg_to_commit []
  let commit_ctrler ← ctrlers.ctrler_from_name four_nodes.commit_node.ctrler_name
  let replay_complete_msg ← commit_ctrler.queue_search_api_to_send_msg replay_complete_func_call_msg (additional_stmts ++ [complete_stmt])


  -- Combined from 2. 3. & 4.
  -- NOTE: We add 2. here, but if the ctrler with the old value isn't a "reg_file.read(dest_reg)"
  -- then it's a queue, and we access a queue with a search API where we need to be in scope
  -- But that case is not handled yet..

  let check_mispeculation_and_replay_complete_stmts := Pipeline.Statement.block $
    [old_load_value_decl, old_load_value_stmt, if_old_not_equal_replay_expr, replay_complete_msg]

  -- from 1.
  -- let when_load_response_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [] check_mispeculation_and_replay_complete_stmts
  let trans_to_compare_and_squash := Pipeline.Statement.transition "replay_compare_and_check_state"
  let when_load_response_stmt := Pipeline.Statement.when [memory_interface, load_completed].to_qual_name [] trans_to_compare_and_squash.to_block
  -- All of the await-replay-load stmts
  let await_load_response := Pipeline.Statement.await none [when_load_response_stmt]

  -- The name of the new await replay state
  let await_replay_state_name := ReplayAwaitStateName four_nodes.global_complete_load_node

  let listen_handle_wrapped_await_replay ← four_nodes.global_complete_load_node.wrap_stmt_with_node's_listen_handle_if_exists await_load_response.to_block ctrlers

  -- Return: The await replay state.
  let await_replay_state := Pipeline.Description.state await_replay_state_name listen_handle_wrapped_await_replay.to_block

  let compare_and_check_wrapped_stmts : Pipeline.Statement ←
    if is_issue_ctrler_and_await_response_ctrler_same then do
      -- match issue_ctrler_node_pred_on_commit? with
      -- | .some /- issue_ctrler_node_pred_on_commit -/ _ => do
        four_nodes.global_complete_load_node.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block ctrlers
      -- | .none =>
        -- four_nodes.global_perform_load_node.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block ctrlers
    else do
      let await_load_ctrler_node := four_nodes.global_perform_load_node
      let await_load_ctrler ← ctrlers.ctrler_from_name await_load_ctrler_node.ctrler_name
      let await_load_ctrler_first_state ← await_load_ctrler.init_trans_dest_state

      await_load_ctrler_first_state.wrap_stmt_with_node's_listen_handle_if_exists check_mispeculation_and_replay_complete_stmts.to_block

  let compare_and_check_state := Pipeline.Description.state "replay_compare_and_check_state" compare_and_check_wrapped_stmts.to_block

  dbg_trace "##Sanity 2"
  return (await_replay_state, compare_and_check_state)

open Pipeline in
def CDFG.Node.create_when_msg_stmt_that_trans_to_given_dest
(node : CDFG.Node) (msg_name : MsgName) (given_dest : StateName)
(when's_args : List Identifier) (when's_stmts : List Statement)
: Pipeline.Statement :=
  let transition_to_issue_replay := when's_stmts ++ [ transition given_dest ]
  let ctrler := node.ctrler_name
  let when_commit_start_msg :=
    Statement.when [ctrler, msg_name].to_qual_name when's_args transition_to_issue_replay.to_block
  when_commit_start_msg

def CDFG.Node.UpdateCtrlerFirstStateToAwaitMsgFrom (node : CDFG.Node) (replay_msg_prefix : String) (replay_state_prefix : String) (ctrlers : Ctrlers)
: Except String Pipeline.Description := do
  -- 1. Get the issue ctrler's first state
  -- 2. Get it's stmts
  -- 3. find the await stmt
  -- 4. add a when stmt to the await stmt

  let ctrler_name := node.ctrler_name
  let state_to_clone_replay_version := node.current_state

  -- 1.
  let ctrler ← ctrlers.ctrler_from_name ctrler_name
  let ctrler_first_state ← ctrler.init_trans_dest
  let first_state ← ctrler.state_from_name ctrler_first_state

  let await_replay_state_name := replay_state_prefix ++ "_" ++ state_to_clone_replay_version

  let start_replay_await_msg_name := replay_msg_prefix ++ "_" ++ ctrler_name

  -- 2.
  let when_commit_start_msg : Pipeline.Statement := ←
    match ← ctrler.type with
    | .BasicCtrler => do
      let inst_state_var ← ctrler.instruction_var
      let receive_instruction := var_asn_var [inst_state_var] instruction
      pure $
        node.create_when_msg_stmt_that_trans_to_given_dest
        start_replay_await_msg_name await_replay_state_name
        [instruction] [receive_instruction]
    | .Unordered => do
      let trans_to_await_replay_start := Pipeline.Statement.transition await_replay_state_name
      pure $ Pipeline.Statement.when [node.ctrler_name, insert].to_qual_name [] trans_to_await_replay_start.to_block
    | .FIFO => do throw s!"Error while gen updated first state to await a msg ({start_replay_await_msg_name}): Got FIFO ctrler (can't handle due to head/tail ptrs)"
  let first_state ← first_state.append_when_case_to_state's_await_stmt when_commit_start_msg

  pure first_state

def UpdateCtrlerFirstStateToAwaitReplayFromIssue
(four_nodes : CommitIssueAwaitValueStmtNodes)
(ctrlers : Ctrlers)
: Except String (Pipeline.Description) := do
  -- 1. Get the issue ctrler's first state
  -- 2. Get it's stmts
  -- 3. find the await stmt
  -- 4. add a when stmt to the await stmt
  four_nodes.global_perform_load_node.UpdateCtrlerFirstStateToAwaitMsgFrom await_replay_start replay_generated_prefix ctrlers

def await_replay_start_state_name := "LoadReplay_await_replay_start"
def start_replay_msg_name := "start_replay"

open Pipeline in
def CDFG.Node.create_when_replay_start_msg_state
(commit_node : Node)
(issue_node : Node)
(ctrlers : Ctrlers)
: Except String Pipeline.Statement := do
  let issue_ctrler ← ctrlers.ctrler_from_name issue_node.ctrler_name
  let inst_state_var ← issue_ctrler.instruction_var
  let receive_instruction := var_asn_var [inst_state_var] instruction

  let transition_to_issue_replay := Pipeline.Statement.transition replay_issue_load_to_mem

  let get_inst_and_trans_to_issue := [receive_instruction, transition_to_issue_replay]

  let commit_ctrler := commit_node.ctrler_name
  let when_commit_start_msg := Statement.when
    [commit_ctrler, start_replay_msg_name].to_qual_name [instruction]
    get_inst_and_trans_to_issue.to_block
  pure when_commit_start_msg

def CreateReplayAwaitCommitStartMsgState
(four_nodes : CommitIssueAwaitValueStmtNodes)
(listen_handle_blk : Option Pipeline.Statement)
(ctrlers : Ctrlers)
: Except String (Pipeline.Description) := do
  -- 1. Create an await stmt to wait for the commit start msg
  -- 2. Transition to the right state
  let when_commit_start_msg := four_nodes.commit_node.create_when_replay_start_msg_state four_nodes.global_perform_load_node ctrlers
  let await_when := await none [← when_commit_start_msg]
  match listen_handle_blk with
  | none => do pure $ Pipeline.Description.state await_replay_start_state_name await_when.to_block
  | some listen_handle_blk => do pure $ Pipeline.Description.state await_replay_start_state_name (← listen_handle_blk.replace_listen_handle_stmts [await_when]).to_block

-- This is for the receive ctrler, if it's separate from the issue ctrler,
-- to specifically transition or do actions for replay, rather than it's normal actions

-- NOTE: Don't need this function?
-- def CreateSeparateReceiveReplayAwaitLoadState
-- (four_nodes : CommitIssueAwaitValueStmtNodes)
-- (ctrlers : Ctrlers)
-- : Except String ( Pipeline.Description × Option Pipeline.Description ) := do
--   -- create the message name that this state would awailt
--   let await_ctrler_name := four_nodes.global_complete_load_node.ctrler_name
--   -- let start_replay_await_msg_name := StartReplayAwaitMsgName await_ctrler_name

--   let await_ctrler ← ctrlers.ctrler_from_name await_ctrler_name
--   let await_ctrler_type ← await_ctrler.type

--   if await_ctrler_type == .BasicCtrler then do -- NOTE: Case where we update the issue ctrler's first state to this returned msg
--     pure (← UpdateCtrlerFirstStateToAwaitReplayFromIssue four_nodes ctrlers, none)
--   else if await_ctrler_type == .Unordered then do -- NOTE: Case where we update the issue ctrler's first state to the optional state, and add the await-commit-start state
--     pure (CreateReplayAwaitCommitStartMsgState four_nodes, some $ ← UpdateCtrlerFirstStateToAwaitReplayFromIssue four_nodes ctrlers)
--   else do
--     throw "Error while handling issue ctrler type cases to add await start from commit: Got FIFO ctrler (can't handle due to head/tail ptrs)"

-- def insert := "insert"
open Pipeline in
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
  let when_commit_start_msg : Statement := ←
    match ← issue_ctrler.type with
    | .BasicCtrler => four_nodes.commit_node.create_when_replay_start_msg_state four_nodes.global_perform_load_node ctrlers
    | .Unordered => do
      let replay_issue_name := replay_issue_load_to_mem

      let inst_state_var ← issue_ctrler.instruction_var
      let receive_instruction := var_asn_var [inst_state_var] instruction

      let trans_to_await_replay_start := Pipeline.Statement.transition replay_issue_name

      let get_inst_and_trans_to_issue := [receive_instruction, trans_to_await_replay_start]
      pure $
        Statement.when
          [four_nodes.commit_node.ctrler_name, insert].to_qual_name [instruction]
          get_inst_and_trans_to_issue.to_block
    | .FIFO => throw "Error while gen await commit start replay msg: Got FIFO ctrler (can't handle due to head/tail ptrs)"
  let first_state ← first_state.append_when_case_to_state's_await_stmt when_commit_start_msg

  pure first_state

def commit_await_replay_complete := "await_replay_complete"

def CommitAwaitReplayCompleteStateName (commit_ctrler_name : CtrlerName) : StateName :=
  commit_ctrler_name ++ "_" ++ commit_await_replay_complete

def UpdateCommitCtrlerToStartReplayLoad
(four_nodes : CommitIssueAwaitValueStmtNodes)
(ctrlers : Ctrlers)
(is_issue_ctrler_pred_on_commit : Bool)
-- 2 returns: the commit state that starts replay & the original commit code
: Except String (Pipeline.Description × Pipeline.Description) := do
  -- Get the commit node.
  -- Get the commit "Description state"
  let commit_ctrler ← ctrlers.ctrler_from_name four_nodes.commit_node.ctrler_name
  let commit_state ← commit_ctrler.state_from_name four_nodes.commit_node.current_state

  let issue_ctrler_name := four_nodes.global_perform_load_node.ctrler_name
  let issue_ctrler ← ctrlers.ctrler_from_name issue_ctrler_name

  -- prepare the msg issue ctrler msg
  --   Should be a defined name
  let commit_ctrler_inst_var_expr := var_expr $ ← commit_ctrler.instruction_var
  let start_replay_func_call_msg : Pipeline.Statement :=
    CreateDSLMsgCall issue_ctrler_name start_replay_msg_name [commit_ctrler_inst_var_expr]
  let start_replay_msg : Pipeline.Statement ← issue_ctrler.queue_search_api_to_send_msg start_replay_func_call_msg []

  -- prepare the commit ctrlerawait replay completion state name & transition stmt to it
  let commit_await_replay_complete_state_name := CommitAwaitReplayCompleteStateName four_nodes.commit_node.ctrler_name
  let trans_to_commit_await_replay_complete := Pipeline.Statement.transition commit_await_replay_complete_state_name

  -- Use a helper function to search up to the commit label
  -- At the commit label:
  -- 1. Add the start replay msg there
  -- 2. Take the <commit>'s <stmt> and the <remaining stmts>
  --    to place in a new state called "original_commit_" + state_name
  -- i.e.
  --   if (cond) {}
  --     <commit> <stmt>
  --     <remaining stmts>
  --   } else {
  --     ...
  --   }

  -- AZ TODO: find some way to communicate with
  -- BasicCtrler? for NoSQ
  -- issue: the msg requires the ctrler be on a specific state,
  -- but the ctrler can be on a different state while being used by other ctrlers

  let issue_ctrler_type ← issue_ctrler.type
  let issue_ctrler_first_state ← issue_ctrler.init_trans_dest

  let need_dsl_msg_check? : Option StateName :=
    if is_issue_ctrler_pred_on_commit then
      none
      -- don't need to do anything, expect the ctrler
      -- to be on the right state and await the msg.
      -- NOTE: If it isn't, the msg will error and we
      -- can deal with this case in the future
    else
      match issue_ctrler_type with
      | .BasicCtrler =>
        some issue_ctrler_first_state
        -- Then the ctrler needs to be on the
        -- first state. Thus we check and stall
      | .Unordered =>
        none
        -- Ctrler will be inserted into if an entry is available.
      | .FIFO =>
        none
        -- Can't handle FIFO case yet. Since FIFOs have
        -- a head/tail ptr, we can't insert an entry that will be soon removed
        -- Or perhaps we could, but just cause other entries to be waiting on this one

  let start_replay_dsl_code := [start_replay_msg, trans_to_commit_await_replay_complete]
  let start_replay_if_ready :=
    match need_dsl_msg_check? with
    | .some state_name =>
      -- check if BasicCtrler is on state name
      let state_check := equal (qual_var_term [issue_ctrler_name, DSLKeyword.state]) (var_term state_name)
      let trans_to_self := reset four_nodes.commit_node.current_state
      conditional_stmt
        $ if_else_statement
          state_check
            start_replay_dsl_code.to_block
            trans_to_self.to_block
    | .none =>
      start_replay_dsl_code.to_block

  -- Create another if stmt: if (instruction.op == ld) {start_replay_msg, trans_to_commit_await_replay_complete}
  -- else {trans_to_original_commit_code_state}
  let trans_to_original_commit_code_state :=
    let original_commit_code_state_name := original_commit_code_prefix.append "_" |>.append four_nodes.commit_node.current_state
    Pipeline.Statement.transition original_commit_code_state_name
  let do_replay_if_inst_is_load_if_stmt := Pipeline.Statement.conditional_stmt $
    let if_instruction_is_ld_expr : Pipeline.Expr :=
      IsInstructionAnyLoad
      -- equal (qual_var_term [instruction, op]) (var_term ld)
    Pipeline.Conditional.if_else_statement
      if_instruction_is_ld_expr
        /- if case -/ start_replay_if_ready.to_block
        /- else    -/ trans_to_original_commit_code_state

  let (updated_start_replay_commit_po_state, original_commit_actions_state) ← commit_state.split_off_stmts_at_commit_and_inject_stmts [do_replay_if_inst_is_load_if_stmt]

  return (updated_start_replay_commit_po_state, original_commit_actions_state)

-- def original_commit_code_prefix := "original_commit"
-- def commit_await_replay_complete := "await_replay_complete"

def CreateCommitAwaitReplayCompleteState
(four_nodes : CommitIssueAwaitValueStmtNodes)
(ctrlers : Ctrlers)
: Except String Pipeline.Description := do
  -- prepare a transition to "original_commit_" + state_name
  let commit_ctrler_name := four_nodes.commit_node.ctrler_name
  let original_commit_code_state_name := original_commit_code_prefix ++ "_" ++ four_nodes.commit_node.current_state
  let trans_to_original_commit_code := Pipeline.Statement.transition original_commit_code_state_name

  -- prepare the await replay issue await stmt
  --   Need this Msg name & the Ctrler that has the Await response
  let await_resp_ctrler := four_nodes.global_complete_load_node.ctrler_name
  let when_replay_resp := Pipeline.Statement.when
    [await_resp_ctrler, replay_complete_msg_to_commit].to_qual_name [] trans_to_original_commit_code.to_block
  let await_replay_stmt := Pipeline.Statement.await none [when_replay_resp]

  let await_replay_stmt_wrapped_in_listen_handle ← four_nodes.commit_node.wrap_stmt_with_node's_listen_handle_if_exists await_replay_stmt.to_block ctrlers
  dbg_trace s!"<<< await_replay_stmt_wrapped_in_listen_handle: ({await_replay_stmt_wrapped_in_listen_handle})"
  -- Create this await replay response state
  --   Need the state name & above await stmt
  let await_replay_state_name := commit_ctrler_name ++ "_" ++ commit_await_replay_complete
  let await_replay_state := Pipeline.Description.state await_replay_state_name await_replay_stmt_wrapped_in_listen_handle.to_block

  -- return the await replay response state
  pure await_replay_state

-- TODO: not for the 3 LSQs, but for future designs...
-- def UpdateAwaitCtrlerFirstStateToAwaitReplay

-- Check for a post-commit store controller
def CDFG.Graph.exists_post_commit_store_ctrler?
: Graph → Except String (Option Node)
| uarch_graph => do
  -- (1) Get post commit states
  let commit_node ← uarch_graph.commit_state_ctrler
  -- these are post-commit states of a store
  dbg_trace s!"%%St-Fwd LR 0: find pc store states"
  let pc_store_ns_ts ← uarch_graph.reachable_nodes_from_node_up_to_option_node 0 commit_node none store [] none
  let pc_store_ns := pc_store_ns_ts.1.nodes_to_graph
  dbg_trace s!"%%St-Fwd LR 1: pc_store_ns: ({pc_store_ns})"

  -- (2) Check if there are post-commit store states
  let pc_store_send_n? ← pc_store_ns.store_global_perform_state_ctrler?
  -- NOTE: May exist a case where we should check the uarch_graph
  --   for the store-receive node; may not get it from if in separate ctrler, but what design would do this?
  let pc_store_receive_n? ← pc_store_ns.store_receive_perform_state_ctrler?

  dbg_trace s!"%%St-Fwd LR 2: pc_store_send_n?: ({pc_store_send_n?})"
  dbg_trace s!"%%St-Fwd LR 3: pc_store_receive_n?: ({pc_store_receive_n?})"
  match pc_store_send_n? with
  | none => pure none -- Return false, if there is no post-commit store send request node
  | some pc_store_send_n /- pc store send request node -/ =>
    match pc_store_receive_n? with
    | none => throw s!"Post Commit Store Check: There's a post-commit store send node, but no post-commit store receive?"
    | some pc_store_receive_n =>
      -- (3) Get post-commit store states
      -- these are post-commit states just within the commit controller

      let post_pc_store_receive_ns_ts ← uarch_graph.reachable_nodes_from_node_up_to_option_node 0 pc_store_receive_n none store [] none

      -- (4) Check if post-commit nodes are predicated on post-commit store post-receive response nodes

      -- (4.a) Could check if post-commit nodes in the commit structure are predicated by the post-commit store
      let pc_ctrler_ns := (uarch_graph.transitioned_to_states commit_node |>.append [commit_node] |>.eraseDups )
      -- if pc_ctrler_ns.is_states_pred_by_msg_from_other_states post_pc_store_receive_ns_ts.1 then
      --   pure pc_store_receive_n.ctrler_name
      -- else pure none

      -- (4.b) Could also just check if the receive node ctrler's reaching set of states
      -- reaches back to the commit controller
      let pc_commit_ctrler_store_receive_ns := post_pc_store_receive_ns_ts.1.filter (pc_ctrler_ns.contains ·)
      match pc_commit_ctrler_store_receive_ns.filter (·.ctrler_name == commit_node.ctrler_name) with
      | _ :: _ => pure none -- If the post-commit-store messages back the commit controller before it completes
      | [] =>
        dbg_trace s!"%%St-Fwd LR 4: pc_commit_ctrler_store_receive_ns: ({pc_commit_ctrler_store_receive_ns})"
        pure pc_store_send_n -- If the post-commit-store doesn't message the commit controller
        -- Would we want to return the receive node as well?


open Pipeline in
def CDFG.Graph.AddLoadReplayToCtrlers
(graph : Graph) (ctrlers : Ctrlers)
(lat_name : CtrlerName) (lat_seq_num_var lat_address_var : VarName)
: Except String (Ctrlers × CtrlerState × CtrlerName)
:= do
  dbg_trace s!"%%LR Get 4 Nodes to add Replay to Ctrlers"
  -- Get the relevant 4 states & ctrlers
  let commit_node ← graph.commit_state_ctrler
  let global_perform_load_node ← graph.load_global_perform_state_ctrler
  let global_complete_load_node ← graph.await_load_receive_state_ctrler
  let old_load_value_node ← graph.old_load_value_stmt_state_ctrler

  let four_nodes := ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩

  dbg_trace s!"%%LR Add Replay value var declaration"
  let replay_value_decl : TypedIdentifier := (TypedIdentifier.mk u32 replay_value)
  let ctrlers : Ctrlers ←
    ctrlers.add_var_decl_to_ctrler four_nodes.global_perform_load_node.ctrler_name replay_value_decl

  dbg_trace s!"%%LR Get Load Issue Ctrler"
  -- Issue Ctrler / State info
  let issue_ctrler ← ctrlers.ctrler_from_name global_perform_load_node.ctrler_name
  let issue_ctrler_type ← issue_ctrler.type
  -- Particularly if the Issue Ctrler's state(s) are predicated on Commit
  dbg_trace s!"%%LR Check if Issue Ctrler is pred by post commit nodes"
  let issue_ctrler_pred_on_commit_nodes : List Node := (← graph.ctrler_completion_pred_on_commit_states global_perform_load_node.ctrler_name commit_node).eraseDups
  dbg_trace s!"%%LR Filter Issue Ctrler nodes pred by post commit nodes for Load Nodes"
  let issue_ctrler_pred_on_commit_nodes_for_load := graph.filter_input_nodes_only_ld issue_ctrler_pred_on_commit_nodes
  dbg_trace s!"$$issue_ctrler_pred_on_commit_nodes_for_load: ({issue_ctrler_pred_on_commit_nodes_for_load})"
  let ( is_issue_ctrler_pred_on_commit, issue_ctrler_node_pred_on_commit?, issue_ctrler_node_pred_on_commit_listen_handle_stmt? )
    : Bool × Option Node × Option Pipeline.Statement := ← do
    if H_non_zero : 0 < issue_ctrler_pred_on_commit_nodes_for_load.length then do
      let issue_ctrler_node_pred_on_commit := issue_ctrler_pred_on_commit_nodes_for_load[0]'(by simp[H_non_zero])
      let listen_handle_of_node? : Option Pipeline.Statement ← issue_ctrler_node_pred_on_commit.listen_handle_stmt? ctrlers
      pure (true, some issue_ctrler_node_pred_on_commit, listen_handle_of_node?)
    else do
      pure (false, none, none)
  dbg_trace s!"$$is_issue_ctrler_pred_on_commit: ({is_issue_ctrler_pred_on_commit})"

  let is_issue_ctrler_and_await_response_ctrler_same := global_perform_load_node.ctrler_name == global_complete_load_node.ctrler_name
  dbg_trace s!"global_perform_load_node: ({global_perform_load_node})"
  dbg_trace s!"global_complete_load_node: ({global_complete_load_node})"
  dbg_trace s!"is_issue_ctrler_and_await_response_ctrler_same: ({is_issue_ctrler_and_await_response_ctrler_same})"

  /- =================== Await Commit "Start Replay" Msg State ===================== -/
  --
  dbg_trace s!"$$sanity1"
  let await_replay_start_state : Pipeline.Description := ←
    if is_issue_ctrler_pred_on_commit then do -- NOTE: Case where we add this state to the issue ctrler, and make other states transition to this one
      dbg_trace s!"$$1is_issue_ctrler_pred_on_commit: ({is_issue_ctrler_pred_on_commit})"
      CreateReplayAwaitCommitStartMsgState four_nodes issue_ctrler_node_pred_on_commit_listen_handle_stmt? ctrlers
    else do
      match issue_ctrler_type with
      | .BasicCtrler => do -- NOTE: Case where we update the issue ctrler's first state to this returned msg
        UpdateIssueCtrlerFirstStateToAwaitReplayCommit four_nodes ctrlers
      | .Unordered => do -- NOTE: Case where we update the issue ctrler's first state to the optional state, and add the await-commit-start state
        UpdateIssueCtrlerFirstStateToAwaitReplayCommit four_nodes ctrlers
      | .FIFO => do throw "Error while handling issue ctrler type cases to add await start from commit: Got FIFO ctrler (can't handle due to head/tail ptrs)"
  -- AZ NOTE: Must insert or replace the state accordingly, based on the cases above...

  dbg_trace s!"await_replay_start_state: ({await_replay_start_state})"

  dbg_trace s!"$$sanity2"
  /- =================== New Issue Replay State ===================== -/
  let post_commit_store_ctrler_name? ← graph.exists_post_commit_store_ctrler?
  -- Issue the Replay Memory Request
  let new_issue_replay_state ← CreateReplayIssueLoadState is_issue_ctrler_and_await_response_ctrler_same
    is_issue_ctrler_pred_on_commit ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩
    issue_ctrler_node_pred_on_commit? ctrlers lat_name lat_seq_num_var lat_address_var
    post_commit_store_ctrler_name?

  dbg_trace s!"new_issue_replay_state: ({new_issue_replay_state})"

  dbg_trace s!"$$sanity3"
  dbg_trace s!"is_issue_ctrler_and_await_response_ctrler_same: ({is_issue_ctrler_and_await_response_ctrler_same})"
  /- ============= For completeness, I should also update the await-load-response ctrler if there is one separate from issue-load-ctrler =============== -/
  let (updated_replay_await_load_issued_state?) : Option Pipeline.Description := ←
    if is_issue_ctrler_and_await_response_ctrler_same then do
      dbg_trace s!"$$sanity3.1"
      pure none
    else do -- If ctrlers are different, we need to msg the await-load-response ctrler to specifically go to the 'replay' state after getting the load mem response
      dbg_trace s!"$$sanity3.2"
      pure $ some $ ← UpdateCtrlerFirstStateToAwaitReplayFromIssue four_nodes ctrlers
      -- throw "If ctrlers are different, we need to msg the await-load-response ctrler to specifically go to the 'replay' state after getting the load mem response"
  dbg_trace s!"updated_replay_await_load_issued_state?: ({updated_replay_await_load_issued_state?})"

  dbg_trace s!"$$sanity4"
  /- =================== New Await Replay State ===================== -/
  -- ** Awaiting the Replay Memory Response

  let (new_await_replay_state, new_compare_and_squash_state) ←
    CreateReplayAwaitLoadState is_issue_ctrler_and_await_response_ctrler_same
    is_issue_ctrler_pred_on_commit ⟨commit_node, global_perform_load_node, global_complete_load_node, old_load_value_node⟩
    issue_ctrler_node_pred_on_commit? ctrlers lat_name

  dbg_trace s!"$$sanity5"
  /- === Add Msg passing to coordinate the Load Replay process === -/
  -- Given the 3 (or 4) States, the Commit Ctrler msg the Issue Ctrler to Start replay
  -- Also generate a generated option state that awaits the receive complete
  let (update_commit_start_replay_state, original_commit_actions_state) ←
    -- If issue & commit are in different ctrlers, Update the state to send the start replay msg
    -- Update Commit state;
    if global_perform_load_node.ctrler_name != commit_node.ctrler_name then
      -- NOTE: ficticious function, TODO: implement
      let update_state ← UpdateCommitCtrlerToStartReplayLoad four_nodes ctrlers is_issue_ctrler_pred_on_commit
      pure update_state
    else -- Else just transition to the new issue replay state
      throw "Error while adding Load-Replay to Ctrlers: Commit & Issue Load Ctrlers are the same. Not handling this just yet"

  dbg_trace s!"$$sanity6"
  /- ================= Commit Ctrler: await load replay completion ================ -/
  let commit_await_replay_complete_state ← CreateCommitAwaitReplayCompleteState four_nodes ctrlers

  /- =============== add the states to the ctrlers ==================-/

  /- ===== Start by adding Commit states =====-/
  let ctrlers_with_commit_start_replay_state ← ctrlers.update_ctrler_state commit_node.ctrler_name update_commit_start_replay_state
  let ctrlers_with_commit_start_and_finish_replay_state ←
    ctrlers_with_commit_start_replay_state.add_ctrler_states commit_node.ctrler_name [commit_await_replay_complete_state, original_commit_actions_state]

  /- ===== Add issue replay states ===== -/
  let ctrlers_with_issue_replay_state : Ctrlers := ←
    if is_issue_ctrler_pred_on_commit then do -- NOTE: Case where we add this state to the issue ctrler, and make other states transition to this one
      -- if issue is pred on commit make the states pointing to commit, to point to the await-replay-start state
      -- AZ NOTE: TODO: this should first update the transition destinations,
      -- and then add the new states. Fixed. Oddly was correct in the other if branches...
      let ctrler_with_states_trans_to_given_state_updated := ←
        ctrlers_with_commit_start_and_finish_replay_state.update_ctrler_states_trans_to_specific_state
          global_perform_load_node.ctrler_name issue_ctrler_node_pred_on_commit?.get!.current_state (← await_replay_start_state.state_name)
          (some load) -- should be for transitions taken by a load
      let states_added_to_ctrler_in_ctrlers := ←
        ctrler_with_states_trans_to_given_state_updated.add_ctrler_states global_perform_load_node.ctrler_name [await_replay_start_state, new_issue_replay_state]
      pure states_added_to_ctrler_in_ctrlers
    else
      match issue_ctrler_type with
      | .BasicCtrler => do -- NOTE: Case where we update the issue ctrler's first state to this returned msg
        let ctrlers_with_issue_await_replay_start_state ← ctrlers_with_commit_start_and_finish_replay_state.update_ctrler_state global_perform_load_node.ctrler_name await_replay_start_state
        ctrlers_with_issue_await_replay_start_state.add_ctrler_states global_perform_load_node.ctrler_name [new_issue_replay_state]
      | .Unordered => do -- NOTE: Case where we update the issue ctrler's first state to the optional state, and add the await-commit-start state
        let ctrlers_with_issue_await_replay_start_state ← ctrlers_with_commit_start_and_finish_replay_state.update_ctrler_state global_perform_load_node.ctrler_name await_replay_start_state
        ctrlers_with_issue_await_replay_start_state.add_ctrler_states global_perform_load_node.ctrler_name [new_issue_replay_state]
      | .FIFO =>
        throw "Error while adding created replay issue states to ctrlers: Shouldn't reach here, we aren't ready to handle .FIFO"

  let await_resp_ctrler_type ← (← ctrlers.ctrler_from_name global_complete_load_node.ctrler_name ).type
  /- ===== Add await replay states ===== -/
  let ctrlers_with_await_replay_response_state : Ctrlers := ←
    if is_issue_ctrler_and_await_response_ctrler_same then -- NOTE: Case where we add this state to the issue ctrler, and make other states transition to this one
      ctrlers_with_issue_replay_state.add_ctrler_states global_complete_load_node.ctrler_name [new_await_replay_state, new_compare_and_squash_state]
    else
      match updated_replay_await_load_issued_state? with
      | some updated_replay_await_load_issued_state =>
        match await_resp_ctrler_type with
        | .BasicCtrler => do -- NOTE: Case where we update the issue ctrler's first state to this returned msg
          let ctrlers_with_await_replay_start_state ← ctrlers_with_issue_replay_state.update_ctrler_state global_perform_load_node.ctrler_name updated_replay_await_load_issued_state
          ctrlers_with_await_replay_start_state.add_ctrler_states global_complete_load_node.ctrler_name [new_await_replay_state, new_compare_and_squash_state]
        | .Unordered => do -- NOTE: Case where we update the issue ctrler's first state to the optional state, and add the await-commit-start state
          let ctrlers_with_await_replay_start_state ← ctrlers_with_issue_replay_state.update_ctrler_state global_perform_load_node.ctrler_name updated_replay_await_load_issued_state
          ctrlers_with_await_replay_start_state.add_ctrler_states global_complete_load_node.ctrler_name [new_await_replay_state, new_compare_and_squash_state]
        | .FIFO =>
          throw "Error while adding created await replay response states to ctrlers: Shouldn't reach here, we aren't ready to handle .FIFO"
      | none =>
        throw "Error while adding created await replay response states to ctrlers: Shouldn't reach here, based on the if condition, this should have been set to Option.some"

  let commit_start_replay_state_name := ← update_commit_start_replay_state.state_name
  return (ctrlers_with_await_replay_response_state, ⟨commit_node.ctrler_name, commit_start_replay_state_name⟩, ← original_commit_actions_state.state_name)

def Ctrlers.CDFGLoadReplayTfsm (ctrlers : Ctrlers) (mcm_ordering? : Option MCMOrdering)
: Except String (List controller_info) := do
  let graph_nodes ← DSLtoCDFG ctrlers
  let graph : CDFG.Graph := {nodes := graph_nodes}
  dbg_trace s!"$$LoadReplay Start"
  -- dbg_trace "$$LoadReplay ctrlers: {ctrlers}"
  -- dbg_trace "$$LoadReplay graph: {graph}"

  -- (1) Create the LAT
  dbg_trace s!"$$LR Find perform load node"
  let perform_load_node ← graph.load_global_perform_state_ctrler
    |>.throw_exception_nesting_msg s!"Error getting perform load node"
  dbg_trace s!"$$LR Find commit node"
  let commit_node ← graph.commit_state_ctrler
    |>.throw_exception_nesting_msg s!"Error getting commit node"

  -- let lat_squashing_ctrler := [
  --   perform_load_node.ctrler_name, -- Expect squash msg from Load-Replay's old vs new replay load value
  --   commit_node.ctrler_name -- Expect squash msg from Commit ctrler in case user sends squash, potentially
  --     -- either from St forwarding or perhaps a branch mispeculation.
  --     -- And Commit is the "squash ctrler" since it keeps reference of all insts, and sends squashes
  --     -- accordingly.
  -- ]
  dbg_trace s!"$$LR Create LAT"
  let (lat, lat_name, lat_seq_num_var, lat_address_var) ← CreateLoadAddressTableCtrler
    perform_load_node.ctrler_name commit_node.ctrler_name perform_load_node.ctrler_name commit_node.ctrler_name

  -- (2) Add LoadReplay to Ctrlers
  dbg_trace s!"$$LR Add LoadReplay"
  let (ctrlers_with_load_replay, commit_start_replay_state_name, original_commit_state) :=
    ← graph.AddLoadReplayToCtrlers ctrlers lat_name lat_seq_num_var lat_address_var
    |>.throw_exception_nesting_msg "Error while adding Load-Replay to Ctrlers!"

  dbg_trace s!"LoadReplay: Option MCM Ordering: ( {mcm_ordering?} )"
  dbg_trace s!"LoadReplay: Commit Start Replay State Name: ( {commit_start_replay_state_name} )"

  dbg_trace s!"$$LR Add LAT"
  let ctrlers' := ctrlers_with_load_replay ++ [lat]

  -- (3) Add insert_key(seq_num, address) into LAT
  let (load_req_address, load_req_seq_num) ← perform_load_node.load_req_address_seq_num

  -- let state_where_addr_gen'd_or_received := graph.node_where_load_addr_obtained perform_load_node
  let addr_var_name ← load_req_address.var's_identifier
  let perform_load_ctrler ← ctrlers.ctrler_from_name perform_load_node.ctrler_name
  let is_addr_a_state_var ← perform_load_ctrler.is_a_state_var_of_ctrler addr_var_name

  let (is_found_addr_var, ctrlers'')  ← LoadAddress.CDFG.Graph.update_ctrlers_at_node_where_load_addr_obtained_search -- AddInsertToLATWhenPerform
    graph
    perform_load_node
    addr_var_name is_addr_a_state_var
    ctrlers'
    []
    lat_name load_req_address load_req_seq_num

  match is_found_addr_var with
  | true =>
    -- (4) Add remove_key(seq_num) from LAT
    let commit_node := ← graph.commit_state_ctrler
      |>.throw_exception_nesting_msg s!"Error getting perform load node"

    dbg_trace s!"Adding Remove from lat when commit in Load Replay"
    let ctrlers''' ← AddRemoveFromLATWhenCommit
      ctrlers'' lat_name commit_node.ctrler_name original_commit_state List.inject_stmts_at_body [instruction, seq_num]
    dbg_trace s!"Finished adding remove from lat for load replay. Ctrlers: ({ctrlers'''})"

    -- (5) Enforce any additional orderings on the replay load
    match mcm_ordering? with
    | some mcm_ordering =>
      let ctrlers'''' := ←
        CDFG.InOrderTransform ctrlers''' mcm_ordering (some (commit_start_replay_state_name, load))
          |>.throw_exception_nesting_msg "Error while adding InOrderTfsm to Ctrlers after adding Load-Replay!"
      CDFG.InOrderTransform ctrlers'''' mcm_ordering (some (commit_start_replay_state_name, ldar))
        |>.throw_exception_nesting_msg "Error while adding InOrderTfsm to Ctrlers after adding Load-Replay!"
    | none =>
      pure ctrlers'''
  | false =>
    throw s!"Error: couldn't find the address variable in the perform load ctrler?"
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
