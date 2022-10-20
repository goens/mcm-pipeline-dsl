
import PipelineDsl.AST
import PipelineDsl.Translation

-- import PipelineDsl.AST
import PipelineDsl.AnalysisHelpers
import PipelineDsl.Transformation

open Pipeline


-- partial def true_if_stmts_have_mem_access
-- (stmt : Statement)
-- :=
--   -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
--   -- dbg_trace stmt
--   -- dbg_trace "==END GET-TRANSITIONS ==\n"
--   match stmt with
--   | Statement.transition ident => []
--   | Statement.stray_expr expr' => 
--     match expr' with
--     | Expr.some_term term' =>
--       match term' with
--       | Term.function_call qual_name lst_expr =>
--         match qual_name with
--         | QualifiedName.mk lst_idents' =>
--           if (lst_idents'.contains "memory_interface")
--             then [true]
--             else []
--       | _ => []
--     | _ => []
--   | Statement.listen_handle stmt lst =>
--     List.join
--     (
--       [true_if_stmts_have_mem_access stmt]
--       ++
--       (
--         lst.map
--         (
--           λ handl =>
--           match handl with
--           | HandleBlock.mk qname iden_list stmt1 =>
--             true_if_stmts_have_mem_access stmt1
--         )
--       )
--     )
--   | Statement.conditional_stmt cond =>
--     match cond with
--     | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map true_if_stmts_have_mem_access)
--     | Conditional.if_statement expr1 stmt1 => true_if_stmts_have_mem_access stmt1
--   | Statement.block lst_stmt => List.join (lst_stmt.map true_if_stmts_have_mem_access)
--   | Statement.await none lst_stmt1 => List.join (lst_stmt1.map true_if_stmts_have_mem_access)
--   | Statement.when qname list_idens stmt => true_if_stmts_have_mem_access stmt
--   -- | Statement.listen_handle  => 
--   | _ => []

-- def match_stmt_with_assgn_entry_type_load
-- (stmt' : Statement)
-- :=
--   match stmt' with
--   | Statement.variable_assignment qual_name expr =>
--     let the_var_name :=
--       match qual_name with
--       | QualifiedName.mk lst_idents' =>
--         match lst_idents' with
--         | [a_name] => a_name
--         | _ => default
--     if the_var_name == "entry_types"
--       then
--         -- check if expr contains "load"
--         -- Expr is either a list or just
--         -- "load"
--         match expr with
--         | Expr.some_term term =>
--           match term with
--           | Term.var term_ident =>
--             if term_ident == "load"
--               then true
--               else false
--           | _ => false
--         | Expr.list lst_expr =>
--           let load_lst :=
--           lst_expr.filter (
--             λ expr' =>
--               match expr' with
--               | Expr.some_term term' =>
--                 match term' with
--                 | Term.var term_ident' =>
--                   if term_ident' == "load"
--                     then true
--                     else false
--                 | _ => false
--               | _ => false
--           )
--           let found_load :=
--           match load_lst with
--           | [one] => true
--           | _ => false
--           found_load
--         | _ => false

--       else
--         false
--   | _ => false

-- def find_load_begin_perform_info
-- (ctrler_lst : List controller_info)
-- :=
--   let filtered_by_load_controllers
--   :=
--     ctrler_lst.filter (
--       -- [1] check by entry types?
--       -- [2] check for sending a mem req 
--       -- in a subsequent 'let statement
--       λ ctrler =>
--         match ctrler.controller_descript with
--         | Description.controller ident stmt =>
--           match stmt with
--           | Statement.block lst_stmts =>
--             -- [1] check the statements
--             -- Find the statement assigning to
--             -- entry_types
--             -- Check if it matches the String
--             -- "load"
--             let lst_stmt_with_load :=
--             lst_stmts.filter (
--               match_stmt_with_assgn_entry_type_load
--             )
--             match lst_stmt_with_load with
--             | [a_ld_stmt] => true
--             | [] => false
--             | h::t =>
--               dbg_trace "\nmultiple entry assgns?\n"
--               true
--           | _ => false
--         | _ => false
--     )
--   -- [2] Check if there's a
--   -- memory access in the
--   -- transitions of this
--   -- controller
--   let filtered_by_load_and_mem_req
--   :=
--     filtered_by_load_controllers.filter (
--       λ ctrler =>
--         -- is there a mem access?
--         let lst_trans_with_mem_access :=
--         ctrler.transition_list.filter (
--           λ trans_descript =>
--             match trans_descript with
--             | Description.state ident stmt =>
--               -- want to find a stmt that contains
--               -- a memory_interface access/request
--               let bool_lst :=
--               true_if_stmts_have_mem_access stmt

--               let found_mem_interface_access :=
--               match bool_lst with
--               | [] => false
--               | [true] => true
--               | h::t =>
--                 if bool_lst.all (λ bool' => bool' == true)
--                  then true
--                  else false

--               found_mem_interface_access
--             | _ => false
--         )

--         let found_trans_with_mem_access :=
--         match lst_trans_with_mem_access with
--         | [] => false
--         | [one_trans] => true
--         | h::t => true

--         found_trans_with_mem_access
--     )

--   -- have ctrlers where [1] and [2]
--   -- are both true!
--   -- have found controllers
--   -- that both [1] hold loads
--   -- and [2] send mem reqs!
--   filtered_by_load_and_mem_req

-- TODO:
-- This would find ctrlrs with that hold loads and send load reqs
-- Maybe just need one that finds ctrlers that send load reqs?
-- Not important right now.
-- Just write a func that takes a ctrler_info obj and
-- (1) find the transition name with the globally perform load req

def get_ctrler_state_with_mem_load_req
( ctrler : controller_info ) -- really is Description.state
: ( List String ) -- The name of the transition with the mem load req
:=
  -- search and find the transition
  let lst_trans_with_mem_access : List String := List.join (
    ctrler.transition_list.map (
      λ trans_descript =>
        match trans_descript with
        | Description.state ident stmt =>
          -- want to find a stmt that contains
          -- a memory_interface access/request
          let bool_lst :=
          true_if_stmts_have_mem_access stmt

          let found_mem_interface_access :=
          match bool_lst with
          | [] => []
          | [true] => [ident]
          | _::_ =>
            if bool_lst.all (λ bool' => bool' == true)
             then [ident]
             else []

          found_mem_interface_access
        | _ => []
    )
  )
  
  -- NOTE: lst_trans_with_mem_access has the states
  -- with mem-int load access
  -- Just need to figure out if we want to return a list
  -- of strings (all states) or just 1...
  -- TODO: do this friday morning...
  lst_trans_with_mem_access
  
def get_ctrler_state_awaiting_mem_load_resp
( ctrler : controller_info ) -- really is Description.state
: ( List String ) -- The name of the transition with the mem load req
:=
  -- search and find the transition
  let lst_trans_with_mem_access : List String := List.join (
    ctrler.transition_list.map (
      λ trans_descript =>
        match trans_descript with
        | Description.state ident stmt =>
          -- want to find a stmt that contains
          -- a memory_interface access/request
          let bool_lst :=
          true_if_stmts_awaits_ld_mem_response stmt

          let found_mem_interface_access :=
          match bool_lst with
          | [] => []
          | [true] => [ident]
          | _::_ =>
            if bool_lst.all (λ bool' => bool' == true)
             then [ident]
             else []

          found_mem_interface_access
        | _ => []
    )
  )
  
  -- NOTE: lst_trans_with_mem_access has the states
  -- with mem-int load access
  -- Just need to figure out if we want to return a list
  -- of strings (all states) or just 1...
  -- TODO: do this friday morning...
  lst_trans_with_mem_access

-- Should try to prove termination at some point...
partial def recursively_find_stmt_with_transition_to_arg
( state_stmt : String × Statement )
: List Bool
:=
  let state : String := state_stmt.1
  let stmt : Statement := state_stmt.2

  -- try to match statment
  let bool_list :=
  match stmt with
  | Statement.transition ident =>
    if ident == state then [true]
    else []
  -- TODO NOTE: Should count both "reset" & "transition"
  -- but ignore "completion...?"
  | Statement.reset _ => []
  | Statement.stall _ => []
  | Statement.return_stmt _ => []
  | Statement.block list_statment =>
    let stmts_with_state_name : List (String × Statement) :=
      list_statment.map (λ stmt => (state, stmt))
    
    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
    bool_list
  | Statement.stray_expr _ => []
  | Statement.when _ _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)
  | Statement.await (some _) list_stmt =>
    let stmts_with_state_name : List (String × Statement) :=
      list_stmt.map (λ stmt => (state, stmt))
    
    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
    bool_list
  | Statement.await none list_stmt =>
    let stmts_with_state_name : List (String × Statement) :=
      list_stmt.map (λ stmt => (state, stmt))
    
    let bool_list : List Bool :=
    List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
    bool_list
  | Statement.listen_handle stmt list_handles =>
    List.join
    (
      [recursively_find_stmt_with_transition_to_arg (state, stmt)]
      ++
      ( list_handles.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk _ _ stmt1 =>
            recursively_find_stmt_with_transition_to_arg (state, stmt1)
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement _ stmt1 stmt2 => List.join (
      [(state, stmt1),(state, stmt2)].map recursively_find_stmt_with_transition_to_arg
      )
    | Conditional.if_statement _ stmt1 => recursively_find_stmt_with_transition_to_arg (state, stmt1)
  | Statement.variable_assignment _ _ => []
  | Statement.value_declaration _ _ => []
  | Statement.variable_declaration _ => []
  | Statement.labelled_statement _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)

  bool_list

-- Then do (2)
partial def get_states_leading_to_given_state
( state_and_state_list : (String × List Description) )
-- : Except String (List String)
: List String
:=
  let state : String := state_and_state_list.1
  let state_list : List Description := state_and_state_list.2
  -- 1. check for states which transition to the given state
  -- 2. recursively check if those states have states which transition to them
  -- 3. combine these with the previously found states
  -- 4. return all found states

  -- 1.
  let states_that_transition_to_state : List Description :=
    state_list.filter ( λ state_descript =>
      -- filter for stmts which have a transition that matches this input state var
      match state_descript with
      | Description.state _ stmt =>
        -- check stmt, if there's a transition to this state
        let bool_list : List Bool := recursively_find_stmt_with_transition_to_arg (state, stmt)
        -- return bool
        match bool_list with
        | [] => false
        | [true] => true
        | _::_ => bool_list.all (λ bool' => bool' == true)
      -- | _ => throw state_descript
      | _ => false
    )
  
  let state_names : List String :=
    List.join (
    states_that_transition_to_state.map ( λ state_descript =>
      match state_descript with
      | Description.state ident _ => [(ident)]
      | _ => []
      ))
  let state_names_and_list_that_transition_to_state : List (String × List Description) :=
    state_names.map (λ state_name => (state_name, state_list))

  -- 2.
  let successor_states : List String := List.join (
    state_names_and_list_that_transition_to_state.map get_states_leading_to_given_state
  )

  -- 3. 4.
  -- Leave it up to the caller to include the provided "state"
  state_names ++ successor_states

-- then (3)
-- (a) Function to get just the states which directly transition to this state
def get_states_directly_leading_to_given_state
( state_and_state_list : (String × List Description) )
-- : Except String (List String)
: List String
:=
  let state : String := state_and_state_list.1
  let state_list : List Description := state_and_state_list.2
  -- 1. check for states which transition to the given state
  -- 2. recursively check if those states have states which transition to them
  -- 3. combine these with the previously found states
  -- 4. return all found states

  -- 1.
  let states_that_transition_to_state : List Description :=
    state_list.filter ( λ state_descript =>
      -- filter for stmts which have a transition that matches this input state var
      match state_descript with
      | Description.state _ stmt =>
        -- check stmt, if there's a transition to this state
        let bool_list : List Bool := recursively_find_stmt_with_transition_to_arg (state, stmt)
        -- return bool
        match bool_list with
        | [] => false
        | [true] => true
        | _::_ => bool_list.all (λ bool' => bool' == true)
      -- | _ => throw state_descript
      | _ => false
    )
  
  let state_names : List String :=
    List.join (
    states_that_transition_to_state.map ( λ state_descript =>
      match state_descript with
      | Description.state ident _ => [(ident)]
      | _ => []
      ))

  state_names

-- (a) a func to make the DSL or tree
def convert_state_names_to_dsl_or_tree_state_check
( state_names : List String )
: Except String Pipeline.Expr
-- : Pipeline.Expr
:= do
  match state_names with
  | [a_state_name] => -- just do this one comparison
    return Pipeline.Expr.equal ( Pipeline.Term.var "curr_state" ) ( Pipeline.Term.var a_state_name )
  | h :: t =>
    -- use recursion
    let head_equal_check : Pipeline.Expr := Pipeline.Expr.equal ( Pipeline.Term.var "curr_state" ) ( Pipeline.Term.var h )
    let expr : Pipeline.Expr ← (convert_state_names_to_dsl_or_tree_state_check t)
    return Pipeline.Expr.binor (Pipeline.Term.expr head_equal_check) (Pipeline.Term.expr expr)
  | [] => throw s!"Blank List of Strings was provided!"

partial def recursively_find_stmt_and_update_transitions
( old_name_new_name_and_stmt : String × (String × Statement) )
: Statement
:=
  let old_name : String := old_name_new_name_and_stmt.1
  let new_name : String := old_name_new_name_and_stmt.2.1
  let stmt : Statement := old_name_new_name_and_stmt.2.2

  -- try to match statment
  let bool_list :=
  match stmt with
  | Statement.transition ident =>
    if ident == old_name then Statement.transition new_name
    else stmt
  -- TODO NOTE: Should count both "reset" & "transition"
  -- but ignore "completion...?"
  | Statement.reset _ => stmt
  | Statement.stall _ => stmt
  | Statement.return_stmt _ => stmt
  | Statement.block list_statment =>
    let stmts_with_name_info : List (String × String × Statement) :=
      list_statment.map (λ stmt' => (old_name, new_name, stmt'))
    
    let blk_stmt : Statement :=
    Statement.block ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
    blk_stmt
  | Statement.stray_expr _ => stmt
  | Statement.when q_name list_ident stmt' =>
    let updated_stmt : Statement := recursively_find_stmt_and_update_transitions (old_name, new_name, stmt')
    let new_when : Statement := Statement.when q_name list_ident updated_stmt
    new_when
  | Statement.await term' list_stmt =>
    let stmts_with_name_info : List (String × String × Statement) :=
      list_stmt.map (λ stmt' => (old_name, new_name, stmt'))
    
    let await_stmt : Statement :=
    Statement.await term' ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
    await_stmt
  -- | Statement.await none list_stmt =>
  --   let stmts_with_name_info : List (String × String × Statement) :=
  --     list_stmt.map (λ stmt' => (old_name, new_name, stmt'))
    
  --   let await_stmt : Statement :=
  --   Statement.await none ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
  --   await_stmt
  | Statement.listen_handle stmt' list_handles =>
    Statement.listen_handle (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt'))
      ( list_handles.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qual_name list_ident stmt1 =>
            HandleBlock.mk qual_name list_ident ( recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) )
        )
      )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr stmt1 stmt2 =>
      Statement.conditional_stmt (
      Conditional.if_else_statement expr (
        recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1)
      ) (
        recursively_find_stmt_and_update_transitions (old_name, new_name, stmt2)
      ))
    | Conditional.if_statement expr stmt1 =>
      Statement.conditional_stmt (
      Conditional.if_statement expr (
        recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1)
      ) )
  | Statement.variable_assignment _ _ => stmt
  | Statement.value_declaration _ _ => stmt
  | Statement.variable_declaration _ => stmt
  | Statement.labelled_statement label stmt => Statement.labelled_statement label (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt))

  bool_list

-- (b) Function to re-write the states which directly transition to the state
def update_state_transitions_matching_name_to_replacement_name
( orig_name : String)
( replacement_name : String)
( list_states_names_to_update : List String)
( all_state_descriptions : List Description)
: (List Description)
:= --do

  -- take the state_descriptions, and see if it's name matches one we need to update
  -- if it matches, map all of it's statements
  --   map func should check if stmt is a transition & if it is then replace the identifier
  --   map func should recursively descend into anything which holds list(s) of statements
  --   map func should return the original otherwise
  -----------------------------------------
  -- high level pseudo code : Do the match on the state descriptions
  -- match all_state_descriptions with
  -- if state name == orig name
  -- then do recursive update rewrite
  -- else return original state description.

  let updated_states : (List Description) :=
    List.join (
    all_state_descriptions.map ( λ state_descript =>
      match state_descript with
      | Description.state this_state_name stmt =>
        if list_states_names_to_update.contains this_state_name then
          [Description.state this_state_name (recursively_find_stmt_and_update_transitions (orig_name, replacement_name, stmt))]
        else
          [state_descript]
      | _ => []
    ))
  updated_states
  -- let updated_states : Except String (List Description) :=
  --   all_state_descriptions.mapM ( λ state_descript =>
  --     match state_descript with
  --     | Description.state this_state_name stmt =>
  --       if list_states_names_to_update.contains this_state_name then
  --         Description.state this_state_name (recursively_find_stmt_and_update_transitions (orig_name, replacement_name, stmt))
  --       else
  --         state_descript
  --     | descript => throw s!"Should have gotten a state?: {descript}"
  --   )
  
  -- match updated_states with
  -- | Except.ok state_descript_list => return state_descript_list
  -- | Except.error error_msg =>
  --   throw s!"Couldn't rename all state Descriptions?
  --   error Msg: {error_msg}"

  -- []
  
  def gen_stall_dsl_state
  (state_name : String)
  (ctrler_name : String)
  (state_check_or_tree : Pipeline.Expr)
  (stall_on_inst_type : InstType)
  : Description
  :=

    let ctrler_search : QualifiedName :=
      QualifiedName.mk [ctrler_name, "search"]
    
    -- (entry.instruction.seq_num < instruction.seq_num)
    let entry_is_earlier_than_this_one : Term :=
      Pipeline.Term.expr (
      Pipeline.Expr.less_than
      (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"]))
      (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
      )
    -- (entry.instruction.op == stall_on_inst_type)
    let entry_is_of_desired_type : Term :=
      Pipeline.Term.expr (
      Pipeline.Expr.equal
      (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "op"]))
      (Pipeline.Term.const (Const.str_lit (stall_on_inst_type.toString)))
      )

    let search_condition : Expr :=
      Pipeline.Expr.binand
      entry_is_earlier_than_this_one
      entry_is_of_desired_type

    --  min(instruction.seq_num - entry.instruction.seq_num)
    let search_min : Expr :=
      Pipeline.Expr.some_term (
        Pipeline.Term.function_call 
        (QualifiedName.mk ["min"])
        [(Pipeline.Expr.sub
          (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
          (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"])))]
      )

    let ctrler_search_call : Term :=
      Pipeline.Term.function_call
      (QualifiedName.mk [ctrler_name, "search"])
      [search_condition, search_min]

    -- TODO: Fill in the when stmts, and the condition in it
    let await_stmt : Pipeline.Statement :=
      Statement.await ctrler_search_call []
    -- TODO: find the function I use to generate the if statement

    let stall_state_name := ctrler_name ++ "stall" ++ state_name
    let when_success_stmt_blk : Pipeline.Statement :=
      Pipeline.Statement.block [
        Pipeline.Statement.conditional_stmt (
        Pipeline.Conditional.if_else_statement
        state_check_or_tree
        (Pipeline.Statement.reset stall_state_name)
        (Pipeline.Statement.transition state_name)
        )
      ]
    let when_success : Pipeline.Statement :=
      Pipeline.Statement.when
      (QualifiedName.mk [ctrler_name, "search_success"])
      (["curr_state"])
      when_success_stmt_blk

    let when_fail_stmt_blk : Pipeline.Statement :=
      Pipeline.Statement.block [
        (Pipeline.Statement.transition state_name)
      ]
    let when_fail : Pipeline.Statement :=
      Pipeline.Statement.when
      (QualifiedName.mk [ctrler_name, "search_fail"])
      ([])
      when_fail_stmt_blk

    Description.state stall_state_name (
      Statement.block [when_success, when_fail])
    -- some expression condition...
    -- Check the DSL code, something like
    -- entry.instruction.seq_num < instruction.seq_num

    -- let search_func_call : Term :=
    --   Term.function_call ctrler_search search_condition

    -- TODO: Call the func to make the condition for the if.
    -- TODO: Make the if stmt
    -- TODO: Fill in both when cases.

    -- await LQ.search(
    --  (entry.instruction.seq_num < instruction.seq_num) &
    --  (entry.instruction.op == stall_on_inst_type Load, Store, Fence..),
    --  min(instruction.seq_num - entry.instruction.seq_num) ) {
    --   when search_success(curr_state) from LQ {
    --     if (
    --       (curr_state == await_creation) |
    --       (curr_state == await_scheduled) |
    --       (curr_state == await_translation) |
    --       (curr_state == await_fwd_check) |
    --       (curr_state == await_sb_fwd_check_response) |
    --       (curr_state == stall_on_next_entry_state) |
    --       (curr_state == build_packet_send_mem_request) |
    --       (curr_state == await_mem_response) |
    --       (curr_state == squashed_await_ld_mem_resp)
    --     ) {
    --       reset stall_on_next_entry_state;
    --     } else {
    --       transition build_packet_send_mem_request;
    --     }
    --   }
    --   when search_fail() from LQ {
    --     # Could also call it something like "when not speculative"
    --     transition build_packet_send_mem_request;
    --   }
    -- }



-- Use/Compose these functions:
-- (1) get_ctrler_state_with_mem_load_req: Get ctrler state name w/ mem load req
-- (2) get_states_leading_to_given_state: Get list of states for if stmt
-- (3) gen_stall_dsl_state: Generate stall state in DSL
-- (4) Update state transitions, add this new stall state, ret a new ctrler
-- TODO:
-- (5) Write a function to compose together all of these funcs
-- make this new func take a list of the states in the ctrler

-- (5) this is the func to compose these actions together
-- This will primarily be for enforcing in-order exec for
-- insts that were inserted into structures all at the same time
def naive_update_add_stall_to_global_perform
( ctrler : controller_info )
: Except String ( controller_info )
:= do
  -- (1) Get ctrler state awaiting mem resp
  let states_awaiting_mem_ld_resp : List String :=
    get_ctrler_state_awaiting_mem_load_resp ctrler

  let state_awaiting_mem_ld_resp : String ←
    match states_awaiting_mem_ld_resp with
    | [state_name] => pure state_name
    | _ => -- "empty list or multiple states that send a load req?"
      dbg_trace "empty list or multiple states that send a load req?"
      let error_msg : String :=
        "FAIL: Naive in-order load tsfm found multiple states" ++
        s!"that send load requests!\nCtrler: {ctrler.name}"
      throw error_msg

  -- (2) Get the states that transition to this state
  -- to build the OR tree of states to check...
  let states_to_stall_on : List String := (
    get_states_leading_to_given_state (state_awaiting_mem_ld_resp, ctrler.transition_list))
    ++ [state_awaiting_mem_ld_resp]

  -- Build the tree of (curr_state == <state>) |
  -- Use convert_state_names_to_dsl_or_tree_state_check
  let not_yet_gotten_mem_resp_state_check : Pipeline.Expr ←
    convert_state_names_to_dsl_or_tree_state_check states_to_stall_on

  -- (3) Gen the Stall state
  -- use the gen_stall_dsl_state function to make a new stall state
  let new_stall_state_name : String := String.join [ctrler.name, "_stall_", state_awaiting_mem_ld_resp]
  let new_stall_state : Description :=
    gen_stall_dsl_state state_awaiting_mem_ld_resp ctrler.name 
    not_yet_gotten_mem_resp_state_check load

  -- (4) Update the stall state with these Helper Funcs
  -- Update states which transitioned to the original state:
  -- a) get_states_directly_leading_to_given_state
  -- b) update_state_transitions_matching_name_to_replacement_name
  -- c) add the new state to the list of states of the ctrler,
  --    will need to return a new ctrler
  let states_that_transition_to_original_await_mem_resp_state : List String :=
    get_states_directly_leading_to_given_state (state_awaiting_mem_ld_resp, ctrler.transition_list)

  let updated_state_list_transition_to_stall_state : List Description :=
    update_state_transitions_matching_name_to_replacement_name
    state_awaiting_mem_ld_resp new_stall_state_name states_that_transition_to_original_await_mem_resp_state ctrler.transition_list

  let new_state_machine_with_stall_state : List Description :=
    updated_state_list_transition_to_stall_state ++ [new_stall_state]
  
  let new_ctrler : controller_info := {
    name := ctrler.name,
    controller_descript := ctrler.controller_descript,
    entry_descript := ctrler.entry_descript,
    init_trans := ctrler.init_trans,
    state_vars := ctrler.state_vars,
    transition_list := new_state_machine_with_stall_state
  }

  return new_ctrler