
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
deriving Inhabited

def UpdateTransitionDest (transition : Transition) (dest : String)
: Transition
:= {
  predicate := transition.predicate -- ++ [(Condition.DSLExpr condition_expr)]
  dest_state := dest
  messages := transition.messages
  effects := transition.effects
  stmts := transition.stmts
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
  stmts := transition.stmts
  trans_type := trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}
def AppendTransitionPredicate (transition : Transition) (condition : Condition)
: Transition
:= {
  predicate := transition.predicate ++ [condition]
  dest_state := transition.dest_state
  messages := transition.messages
  effects := transition.effects
  stmts := transition.stmts
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}
def AppendTransitionStmt (transition : Transition) (stmt : Pipeline.Statement)
: Transition
:= {
  predicate := transition.predicate
  dest_state := transition.dest_state
  messages := transition.messages
  effects := transition.effects
  stmts := transition.stmts.concat stmt
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}
def AppendTransitionEffect (transition : Transition) (effect : Pipeline.Statement)
: Transition
:= {
  predicate := transition.predicate
  dest_state := transition.dest_state
  messages := transition.messages
  effects := transition.effects.concat effect
  stmts := transition.stmts
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}

def UpdateCompleteTransitionsListsDestType
(transitions_lists : TransitionsLists)
(dest_identifier : String)
(trans_type : TransitionType)
: TransitionsLists
:=
  let updated_transitions_dest : Transitions := transitions_lists.incomplete_transitions.map (
    λ trans => UpdateTransitionDest trans dest_identifier)
  let updated_transitions_type : Transitions := updated_transitions_dest.map (
    λ trans => UpdateTransitionType trans trans_type)
  let updated_transitions_list : TransitionsLists := {
    incomplete_transitions := []
    complete_transitions :=
      transitions_lists.complete_transitions.append updated_transitions_type
  }
  updated_transitions_list

def UpdateIncompleteTransitionsLists
(transitions_lists : TransitionsLists)
(incomplete_transitions : Transitions)
: TransitionsLists
:= {
  incomplete_transitions := incomplete_transitions
  complete_transitions := transitions_lists.complete_transitions
}

def PrepareIfExprCondAndStmts
(incomplete_transitions : Transitions)
(condition_expr : Pipeline.Expr)
(cond_stmt : Pipeline.Statement)
: Transitions × (List Pipeline.Statement)
:=
  let transitions_with_added_cond : Transitions :=
  incomplete_transitions.map ( λ transition =>
      AppendTransitionPredicate transition (Condition.DSLExpr condition_expr) );
  let stmt_list : List Pipeline.Statement :=
    match cond_stmt with
    | .block lst_stmt => lst_stmt
    | _ => [cond_stmt]
  (transitions_with_added_cond , stmt_list)

def PrepareAwaitWhenCondAndStmts
(incomplete_transitions : Transitions)
(await : AwaitStmt)
(when's_stmt : Pipeline.Statement)
: Transitions × (List Pipeline.Statement)
:=
  let transitions_with_added_cond : Transitions :=
  incomplete_transitions.map ( λ transition =>
      AppendTransitionPredicate transition (Condition.AwaitCondition await) );
  let stmt_list : List Pipeline.Statement :=
    match when's_stmt with
    | .block lst_stmt => lst_stmt
    | _ => [when's_stmt]
  (transitions_with_added_cond , stmt_list)

def PrepareHandleCondAndStmts
(incomplete_transitions : Transitions)
(handle_blk : Pipeline.HandleBlock)
(handle_stmt : Pipeline.Statement)
: Transitions × (List Pipeline.Statement)
:=
  let transitions_with_added_cond : Transitions :=
  incomplete_transitions.map ( λ transition =>
      AppendTransitionPredicate transition (Condition.HandleCondition handle_blk) );
  let stmt_list : List Pipeline.Statement :=
    match handle_stmt with
    | .block lst_stmt => lst_stmt
    | _ => [handle_stmt]
  (transitions_with_added_cond , stmt_list)

def EmptyTransitionsLists : TransitionsLists :=
{incomplete_transitions := [], complete_transitions := []}

def AppendTransitionMessages (transition : Transition) (message : Message)
: Transition
:= {
  predicate := transition.predicate
  dest_state := transition.dest_state
  messages := transition.messages.concat message
  effects := transition.effects
  stmts := transition.stmts
  trans_type := transition.trans_type
  queue_info := transition.queue_info
  constraint_info := transition.constraint_info
}

def StmtsToTransitions
-- Add ctrlers after merge, to get ctrler state vars
-- (ctrlers : List controller_info)
(incomplete_transitions : Transitions)
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
      -- let transitions := transitions_lists.incomplete_transitions
      -- let transition := match transitions with
      -- | [one] => one
      -- | _ => throw s!"Expecting just 1 transition? ({transitions})"

      -- if stmt is a branching path stmt, prepare that branch
      let transition_after_stmt := 
        match stmt with
        | .transition dest_identifier =>
          -- update all transitions with the dest ident
          let updated_transitions_list : TransitionsLists := (UpdateCompleteTransitionsListsDestType
            transitions_lists dest_identifier TransitionType.Transition)
          updated_transitions_list
        | .reset dest_identifier =>
          let updated_transitions_list : TransitionsLists := (UpdateCompleteTransitionsListsDestType
            transitions_lists dest_identifier TransitionType.Reset)
          updated_transitions_list
        | .complete dest_identifier =>
          let updated_transitions_list : TransitionsLists := (UpdateCompleteTransitionsListsDestType
            transitions_lists dest_identifier TransitionType.Completion)
          updated_transitions_list
        | .conditional_stmt conditional =>
          match conditional with
          | .if_statement condition_expr cond_stmt =>
            -- add condition to a new transition?
            -- if it has a transition, add it to a list of completed transitions
            -- if it has no transition, I somehow need to continue with
            --   this new transition and original
            --   Should be doable with a map, just update both of the transitions
            let (transitions_list_with_added_cond, stmt_list) :=
              PrepareIfExprCondAndStmts transitions_lists.incomplete_transitions condition_expr cond_stmt
            let list_trans := StmtsToTransitions transitions_list_with_added_cond stmt_list
              -- check if any trans has it's dest_name added..
            let updated_transitions_lists : TransitionsLists := {
              -- Append any new incomplete transition paths
              incomplete_transitions := transitions_lists.incomplete_transitions ++
                list_trans.incomplete_transitions,
              -- Append new completed transitions
              complete_transitions := transitions_lists.complete_transitions ++ list_trans.complete_transitions
            }
            updated_transitions_lists
          | .if_else_statement cond_expr if_true_stmts else_stmts =>
            -- Repeat cond but here.
            let (if_true_transitions_list_with_added_cond, if_true_stmt_list) :=
              PrepareIfExprCondAndStmts transitions_lists.incomplete_transitions cond_expr if_true_stmts
            let if_true_list_trans := StmtsToTransitions if_true_transitions_list_with_added_cond if_true_stmt_list

            -- similar case, duplicated for the else case
            let negated_else_cond : Pipeline.Expr := Pipeline.Expr.some_term (
              Pipeline.Term.negation (Pipeline.Term.expr cond_expr))
            let (if_false_transitions_list_with_added_cond, if_false_stmt_list) :=
              PrepareIfExprCondAndStmts transitions_lists.incomplete_transitions negated_else_cond else_stmts
            let if_false_list_trans := StmtsToTransitions if_false_transitions_list_with_added_cond if_false_stmt_list

            let added_branches_transitions_lists : TransitionsLists := {
              -- Append, it's either new ones with if stmt code, or empty if there was a transition
              incomplete_transitions := if_true_list_trans.incomplete_transitions ++
                if_false_list_trans.incomplete_transitions,
              -- Just update, since it's either the same, or has new ones from the if stmt
              complete_transitions := transitions_lists.complete_transitions ++
                if_true_list_trans.complete_transitions ++ if_false_list_trans.complete_transitions
            }
            added_branches_transitions_lists
        | .block lst_stmt =>
          -- recurse to get incomplete stmts & complete stmts
          let transitions : TransitionsLists := StmtsToTransitions transitions_lists.incomplete_transitions lst_stmt
          transitions
        | .await none when_stmts =>
          -- this just receives a message
          -- record the when messages here as guards
          -- Map the list of when stmts to transitions
          let all_when_trans_lists : List TransitionsLists := when_stmts.map (λ when_stmt =>
            -- should be when stmt
            match when_stmt with
            | .when ctrler_msg_name args when's_stmt =>
              let (trans_with_await_when, stmt_list) :=
                PrepareAwaitWhenCondAndStmts
                transitions_lists.incomplete_transitions
                (Pipeline.Statement.await (none) [when_stmt]) when's_stmt
              let trans_lists : TransitionsLists := StmtsToTransitions trans_with_await_when stmt_list
              trans_lists
            -- | _ => throw s!"Expecting when stmt? ({when_stmt})"
            | _ =>
              dbg_trace "Expecting when stmt? Figure out how to throw in dsl to cdfg translation"
              default -- Need to figure out how to throw here
            )
          let updated_transitions_lists : TransitionsLists :=
            List.foldl (λ accum_transitions_lists when_trans_lists => 
              { incomplete_transitions := accum_transitions_lists.incomplete_transitions ++ when_trans_lists.incomplete_transitions,
                complete_transitions := accum_transitions_lists.complete_transitions ++ when_trans_lists.complete_transitions }
            ) (EmptyTransitionsLists) all_when_trans_lists
          updated_transitions_lists
        | .when _ _ _ =>
          dbg_trace "Shouldn't see a when by itself. Figure out how to throw in dsl to cdfg translation"
          default
        | .await (some api_term) when_stmts =>
          let all_when_trans_lists : List TransitionsLists := when_stmts.map (λ when_stmt =>
            -- should be when stmt
            match when_stmt with
            | .when ctrler_msg_name args when's_stmt =>
              let (trans_with_await_when, stmt_list) :=
                PrepareAwaitWhenCondAndStmts
                transitions_lists.incomplete_transitions
                (Pipeline.Statement.await (api_term) [when_stmt]) when's_stmt
              let trans_lists : TransitionsLists := StmtsToTransitions trans_with_await_when stmt_list
              trans_lists
            -- | _ => throw s!"Expecting when stmt? ({when_stmt})"
            | _ =>
              dbg_trace "Expecting when stmt? Figure out how to throw in dsl to cdfg translation"
              default -- Need to figure out how to throw here
            )
          let updated_transitions_lists : TransitionsLists :=
            List.foldl (λ accum_transitions_lists when_trans_lists => 
              { incomplete_transitions := accum_transitions_lists.incomplete_transitions ++ when_trans_lists.incomplete_transitions,
                complete_transitions := accum_transitions_lists.complete_transitions ++ when_trans_lists.complete_transitions }
            ) (EmptyTransitionsLists) all_when_trans_lists
          updated_transitions_lists
        | .stray_expr expr =>
          -- stray expr is func call or message
          -- TODO Handle accordingly!
          match expr with
          | Pipeline.Expr.some_term term =>
            match term with
            | Pipeline.Term.function_call qual_name lst_expr =>
              -- append transition message
              let updated_incomplete_transitions := transitions_lists.incomplete_transitions.map (
                λ transition =>
                  AppendTransitionMessages transition (Message.mk term)
              )
              let updated_transitions := {
                incomplete_transitions := updated_incomplete_transitions,
                complete_transitions := transitions_lists.complete_transitions
              }
              updated_transitions
            | _ =>
              dbg_trace "Expecting when stmt? Figure out how to throw in dsl to cdfg translation"
              default -- Figure out how to throw
          | _ =>
            dbg_trace "Expecting when stmt? Figure out how to throw in dsl to cdfg translation"
            default -- Figure out how to throw
        | .listen_handle stmt lst_handle =>
          let body_transition_lists : TransitionsLists := StmtsToTransitions transitions_lists.incomplete_transitions [stmt]
          -- each handle block is a separate transition
          let handle_blks_transitions : List TransitionsLists :=
            lst_handle.map (
              λ handle_blk =>
                match handle_blk with
                | .mk ctrler_msg_name args handle_stmt =>
                  let ( transitions_with_handle_predicate, lst_stmts ) : Transitions × (List Pipeline.Statement) :=
                    PrepareHandleCondAndStmts transitions_lists.incomplete_transitions handle_blk handle_stmt
                  let transitions_lists : TransitionsLists := StmtsToTransitions transitions_with_handle_predicate lst_stmts
                  transitions_lists
            )
          let updated_transitions_lists : TransitionsLists :=
            -- foldl to combine all the transitions
            List.foldl (λ (accumulated_transitions_lists) (input_transitions_lists) =>
              {
                incomplete_transitions := accumulated_transitions_lists.incomplete_transitions ++ input_transitions_lists.incomplete_transitions,
                complete_transitions := accumulated_transitions_lists.complete_transitions ++ input_transitions_lists.complete_transitions
              }
            ) (body_transition_lists) (handle_blks_transitions)
          updated_transitions_lists
        | .value_declaration _ _ =>
          let transitions_with_stmt : Transitions := transitions_lists.incomplete_transitions.map (
            λ transition =>
              AppendTransitionStmt transition stmt
          )
          let lists : TransitionsLists :=
          { incomplete_transitions := transitions_with_stmt, complete_transitions := transitions_lists.complete_transitions}
          lists
        | .variable_declaration _ =>
          let transitions_with_stmt : Transitions := transitions_lists.incomplete_transitions.map (
            λ transition =>
              AppendTransitionStmt transition stmt
          )
          let lists : TransitionsLists :=
          { incomplete_transitions := transitions_with_stmt, complete_transitions := transitions_lists.complete_transitions}
          lists
        | .variable_assignment qual_name expr =>
          let transitions_with_stmt : Transitions := transitions_lists.incomplete_transitions.map (
            λ transition =>
              AppendTransitionStmt transition stmt
          )
          let transitions_with_effect : Transitions := transitions_with_stmt.map (
            λ transition =>
              AppendTransitionEffect transition stmt
          )
          -- TODO: Try to do some basic constrait checking,
          -- if we can find variable assignments to literals
          -- like true/false that would be useful to start with.
          -- Can do a basic check on RHS => some_expr, expr, term, Const, num_lit or str_lit
          -- and just assume the type is right

          -- finish later after merge

          -- let var_ := match qual_name with
          --   | .mk list_ident => list_ident
          -- let base_var := var_[0]!
          -- -- check if base_var is in ctrler vars
          -- let if_lit : ConstraintInfo := match expr with
          --   | .some_term term =>
          --     match term with
          --     | .const const_ =>
          --       match const_ with
          --       | .str_lit str =>  -- return an "equals" constraint
          --       | .num_lit nat =>  -- return a boolean constraint if str is "true" or "false"
          ({incomplete_transitions := transitions_with_effect,
            complete_transitions := transitions_lists.complete_transitions } : TransitionsLists)
        | .labelled_statement _ stmt' =>
          StmtsToTransitions transitions_lists.incomplete_transitions [stmt']
        /-
        -- these cases are not really used. skip!
        -/
        | .stall _ =>
          transitions_lists
        | .return_stmt _ =>
          transitions_lists

      transition_after_stmt
    )
    ({incomplete_transitions := incomplete_transitions, complete_transitions := []} : TransitionsLists)
    (stmts)
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
