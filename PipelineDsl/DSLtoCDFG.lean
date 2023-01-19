
import PipelineDsl.AST
import PipelineDsl.AnalysisHelpers
import PipelineDsl.CDFG

/-
1. Start by taking a DSL list of ctrlers.
Start from the "inst" source and "type" (load/store)
and see what the cdfg of all ctrler's state machines is.
-/

-- Open CDFG namespace, use the "nodes"
open CDFG

structure StateTranslationInfo where
State : Pipeline.Description
Ctrler : controller_info

def StateToCDFGNode
(StateTransInfo : StateTranslationInfo)
: Except String (CDFGNode)
:= do
  let queue_type : String := get_ctrler_elem_ordering StateTransInfo.Ctrler
  -- Do a queue type check like this, assign type
  -- accordingly, i.e. if is_head is required?
  -- Hm, but this depends on previous state,
  -- so I can't assign each one directly, since it
  -- depends on if the previous state requires Head or not
  -- But this is not important right now.
  -- A simple implementation is enough for now.
  -- let queue_info : QueueInfo :=
  --   if queue_type == "FIFO" then
  --     QueueInfo.FIFOQueue 

  return default

structure CtrlerTranslationInfo where
Ctrler : controller_info
AllCtrlers : List controller_info

def test := List.foldl (λ int1 int2 => int1+int2) 0 [1,2]
#eval test

def test_list := [1,2]
-- def thing := List.foldl (λ int1 int2 => int1 + int2 + ) 0 [1,2]

structure TransitionsLists where
incomplete_transitions : Transitions
complete_transitions : Transitions

def UpdateTransitionDest (transition : Transition) (dest : String)
: Transition
:= {
  predicate := transition.predicate -- ++ [(Condition.DSLExpr condition_expr)]
  dest_state := dest
  messages := transition.messages
  effects := transition.effects
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}
def UpdateTransitionType (transition : Transition) (trans_type : TransitionType)
: Transition
:= {
  predicate := transition.predicate -- ++ [(Condition.DSLExpr condition_expr)]
  dest_state := transition.dest_state
  messages := transition.messages
  effects := transition.effects
  trans_type := trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}
def UpdateTransitionPredicate (transition : Transition) (condition : Condition)
: Transition
:= {
  predicate := transition.predicate ++ [condition]
  dest_state := transition.dest_state
  messages := transition.messages
  effects := transition.effects
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}

def StmtsToTransitions
(all_transitions : TransitionsLists)
(stmts : List Pipeline.Statement)
: /- Except String -/ (TransitionsLists)
:= --do
/-
1. Use provided stmts as stmts that still need to be "checked"
   Also take a list of stmts we've already seen on this "path"
2. Take a look at the next stmt in the list to still be checked
3. (a) If a stmt is a conditional; Add the condition for this case
   and recursively call this func on the stmts in the condition.

   Append the stmts from that to our list? (inside a transition obj?)
   (b) If a stmt is a conditional; Add the condition for this case
   and recursively call this func on the stmts in the condition.

   If checking a branch (if conditional or await when) returns a
   complete transition (with a transition), then add it as a new
   transition, while keeping this current transition we're building.
   If no else: 

   If a stmt is a message, record the it in the Messages list

   If a stmt is a transition, that's it, end here basically
4. If the last stmt is a transition, then keep the transition we built?
 -/
  let final_transitions_lists : TransitionsLists :=
    List.foldl (
      λ (transitions_lists : TransitionsLists) (stmt : Pipeline.Statement) =>
      let transitions := transitions_lists.incomplete_transitions
      -- let transition := match transitions with
      -- | [one] => one
      -- | _ => throw s!"Expecting just 1 transition? ({transitions})"

      -- if stmt is a branching path stmt, prepare that branch
      let transition_after_stmt := 
      match stmt with
      | .transition dest_identifier =>
        -- update all transitions so far with the dest ident
        let updated_transitions_dest : Transitions := transitions.map (
          λ trans => UpdateTransitionDest trans dest_identifier)
        let updated_transitions_type : Transitions := transitions.map (
          λ trans => UpdateTransitionType trans TransitionType.Transition)
        let updated_transitions_list : TransitionsLists := {
          incomplete_transitions := []
          complete_transitions :=
            transitions_lists.complete_transitions.append updated_transitions_type
        }
        updated_transitions_list
      | .conditional_stmt conditional =>
        match conditional with
        | .if_statement condition_expr cond_stmt =>
          -- add condition to a new transition?
          -- if it has a transition, add it to a list of completed transitions
          -- if it has no transition, I somehow need to continue with
          --   this new transition and original
          --   Should be doable with a map, just update both of the transitions
          let transitions_with_cond : Transitions :=
          transitions.map (
            λ transition =>
              let new_transition_with_cond := 
                UpdateTransitionPredicate transition (Condition.DSLExpr condition_expr)
              new_transition_with_cond
          );
          let stmt_list : List Pipeline.Statement :=
            match cond_stmt with
            | .block lst_stmt => lst_stmt
            | _ => [cond_stmt]
          let transitions_list_with_cond : TransitionsLists := {
            incomplete_transitions := transitions_with_cond
            complete_transitions := all_transitions.complete_transitions
          }
          let list_trans := StmtsToTransitions transitions_list_with_cond stmt_list
            -- check if any trans has it's dest_name added..
          let updated_transitions_lists : TransitionsLists := {
            -- Append, it's either new ones with if stmt code, or empty if there was a transition
            incomplete_transitions := transitions ++ list_trans.incomplete_transitions,
            -- Just update, since it's either the same, or has new ones from the if stmt
            complete_transitions := list_trans.complete_transitions
          }
          updated_transitions_lists

      transition_after_stmt
    ) (all_transitions) (stmts)
  final_transitions_lists
  -- return []

def mapStateToCDFGNode
(state : Pipeline.Description)
: Except String (List CDFGNode)
:= do
  -- Take state fill in fields, and
  -- do searches to get stmts
  -- up to transitions & messages
  let (state_name, stmts) ← get_state_name_stmts state; --: String × Pipeline.Statement

  -- To get transitions, iterate through
  -- (fold?) stmts list.
  -- Every if conditional & await is a
  -- "branch" point where we consider stmts
  -- for a specific transition.

  return []

def CtrlersToCDFG
(CtrlerTransInfo : CtrlerTranslationInfo)
: Except String (List CDFGNode)
:= do
  -- Take a ctrler, check type
  -- Gen FIFO info if needed
  -- 
  -- 
  -- Take ctrler info's description, check for a line that
  -- has queue info

-- inductive CDFGNode
-- | mk: StateName → CtrlerName → VarList → Stmts → Transitions →
-- Messages → QueueInfo → CDFGNode
  let queue_type : String := get_ctrler_elem_ordering CtrlerTransInfo.Ctrler
  let ctrler_name : String := CtrlerTransInfo.Ctrler.name
  let var_list : List Pipeline.TypedIdentifier ←
    get_ctrler_state_vars CtrlerTransInfo.Ctrler

  -- Iterate through states to assign node info
  -- This is important for both queue_info and
  -- State value updates or constraints on variables
  -- TODO: So, start writing my fold!
  -- Use First arg α as visited list
  -- Use second arg List β as nodes to visit
  -- Start from source node (init state)
  -- Populate state with state of previous state
    -- i.e. FIFO info, Var state
  -- 
  -- For a state, iterate through it's stmts,
  -- stmts up to a transition are added to a
  -- transition's effects
  -- If / Await are added as guards
  -- Can leave queue info for now
  -- 
  -- Later, I could do a second pass that attempts
  -- to resolve var state that changes based on
  -- exprs that assign it
  --
  -- Then is there anything that needs to be done sequentially?
  -- let cdfg_graph : List CDFGNode :=
  --   List.foldl 
  let ctrler_states : List Pipeline.Description ←
    get_ctrler_states CtrlerTransInfo.Ctrler
  let cdfg_graph : List CDFGNode :=
    -- TODO: We can probably do the translation per
    -- state, if we don't cumulatively check the
    -- constrained values
    -- The ROB.set_executed and set_unexecuted
    -- will need to be changed
    -- (1) because they're hacky
    -- (2) and their state/var update is not clear
    -- regardless, the message should go to another state,
    -- it it doesn't, then it's hard to track if there's been
    -- any update...
    []

  return []

def DSLtoCDFG
(ctrlers : List controller_info)
: Except String (List CDFGNode)
:= do
/-
Take the DSL ctrler info objs, convert to Graph
1. For each ctrler
2. If the ctrler we're looking at is FIFO we add in FIFO info
3. For each state (except the init state) search it's stmts for
   -> transitions (also note the guards or API conditions used)
   -> message passing (also note the guards or API conditions used)
   and create edges for these

Do I need to iterate through states?
-- I don't think so, information is readily available from
-- the DSL ctrler and states
-/
  -- let cdfg_nodes := ctrlers.map CtrlersToCDFG

  return []

-- TODO: Function to identify post-"receive" states
-- TODO: We can include a field in the CDFGNode type for
-- marking a state is a "receive state" or "send state"
