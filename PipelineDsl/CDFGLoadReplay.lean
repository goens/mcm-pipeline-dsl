
import PipelineDsl.AST
import PipelineDsl.CDFG

import PipelineDsl.DSLtoCDFG

import PipelineDsl.CDFGAnalysis

def CDFG.Graph.AddLoadReplayToCtrlers (graph : Graph) (ctrlers : List controller_info) : Except String (List controller_info) := do
  -- Get the relevant 4 states & ctrlers
  let commit_node ← graph.commit_state_ctrler
  let global_perform_load_node ← graph.load_global_perform_state_ctrler
  let global_complete_load_node ← graph.await_load_receive_state_ctrler
  let old_load_value_node ← graph.old_load_value_stmt_state_ctrler

  -- Item A. from comments in the top level function below
  -- For our LSQs we're modeling, the commit & load API nodes are in different ctrlers
  -- don't bother with this check...

  -- If the Load API nodes is in a Queue vs Ctrler

  -- Load API Node's Ctrler .is_predicated_on_commit_ctrler:
  -- Then we can add the Load Replay right before the state where it is predicated on the commit ctrler
  let is_load_api_ctrler_pred_on_commit : Bool := graph.is_ctrler_completion_pred_on_commit_states global_perform_load_node.ctrler_name commit_node


  default

def Ctrlers.CDFGLoadReplayTfsm (ctrlers : Ctrlers)
: Except String (List controller_info) := do
  let graph_nodes ← DSLtoCDFG ctrlers
  let graph : CDFG.Graph := {nodes := graph_nodes}

  -- The function should do as the comments below describe.
  let graph_with_load_replay ← graph.AddLoadReplayToCtrlers ctrlers
  pure graph_with_load_replay
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
  --     if old load value is in the same ctrler as the load API,
  --       add a comparison state or if expr here as well
  --     if it's not in the same ctrler,
  --       msg the old load value ctrler and add a comparison state there
  --
  --  If comparison is equal, signal to commit ctrler (if in a different ctrler than commit), if it's in the commit ctrler, just transition to next state (commit)
  --  If comparison is not equal, signal to commit ctrler to

  -- ** Squash
  -- D.
  -- Msg Commit ctrler to squash if the comparison fails