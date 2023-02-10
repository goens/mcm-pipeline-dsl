
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
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let lst_trans_with_mem_access : List String := List.join (
    transitions.map (
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
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let lst_trans_with_mem_access : List String := List.join (
    transitions.map (
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

def get_ctrler_state_globally_performing_load
( ctrler : controller_info ) -- really is Description.state
: ( List String ) -- The name of the transition with the mem load req
:=
  -- search and find the transition
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let lst_trans_with_mem_access : List String := List.join (
    transitions.map (
      λ trans_descript =>
        match trans_descript with
        | Description.state ident stmt =>
          -- want to find a stmt that contains
          -- a memory_interface access/request
          let bool_lst :=
          true_if_stmts_have_load_mem_access stmt

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
-- partial def recursively_find_stmt_with_transition_to_arg
-- ( state_stmt : String × Statement )
-- : List Bool
-- :=
--   let state : String := state_stmt.1
--   let stmt : Statement := state_stmt.2

--   -- try to match statment
--   let bool_list :=
--   match stmt with
--   | Statement.transition ident =>
--     if ident == state then [true]
--     else []
--   -- TODO NOTE: Should count both "reset" & "transition"
--   -- but ignore "completion...?"
--   | Statement.reset _ => []
--   | Statement.complete _ => []
--   | Statement.stall _ => []
--   | Statement.return_stmt _ => []
--   | Statement.block list_statment =>
--     let stmts_with_state_name : List (String × Statement) :=
--       list_statment.map (λ stmt => (state, stmt))
    
--     let bool_list : List Bool :=
--     List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
--     bool_list
--   | Statement.stray_expr _ => []
--   | Statement.when _ _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)
--   | Statement.await (some _) list_stmt =>
--     let stmts_with_state_name : List (String × Statement) :=
--       list_stmt.map (λ stmt => (state, stmt))
    
--     let bool_list : List Bool :=
--     List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
--     bool_list
--   | Statement.await none list_stmt =>
--     let stmts_with_state_name : List (String × Statement) :=
--       list_stmt.map (λ stmt => (state, stmt))
    
--     let bool_list : List Bool :=
--     List.join ( stmts_with_state_name.map recursively_find_stmt_with_transition_to_arg )
    
--     bool_list
--   | Statement.listen_handle stmt list_handles =>
--     List.join
--     (
--       [recursively_find_stmt_with_transition_to_arg (state, stmt)]
--       ++
--       ( list_handles.map
--         (
--           λ handl =>
--           match handl with
--           | HandleBlock.mk _ _ stmt1 =>
--             recursively_find_stmt_with_transition_to_arg (state, stmt1)
--         )
--       )
--     )
--   | Statement.conditional_stmt cond =>
--     match cond with
--     | Conditional.if_else_statement _ stmt1 stmt2 => List.join (
--       [(state, stmt1),(state, stmt2)].map recursively_find_stmt_with_transition_to_arg
--       )
--     | Conditional.if_statement _ stmt1 => recursively_find_stmt_with_transition_to_arg (state, stmt1)
--   | Statement.variable_assignment _ _ => []
--   | Statement.value_declaration _ _ => []
--   | Statement.variable_declaration _ => []
--   | Statement.labelled_statement _ stmt => recursively_find_stmt_with_transition_to_arg (state, stmt)

--   bool_list

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
-- def get_states_directly_leading_to_given_state
-- ( state_and_state_list : (String × List Description) )
-- -- : Except String (List String)
-- : List String
-- :=
--   let state : String := state_and_state_list.1
--   let state_list : List Description := state_and_state_list.2
--   -- 1. check for states which transition to the given state
--   -- 2. recursively check if those states have states which transition to them
--   -- 3. combine these with the previously found states
--   -- 4. return all found states

--   -- 1.
--   let states_that_transition_to_state : List Description :=
--     state_list.filter ( λ state_descript =>
--       -- filter for stmts which have a transition that matches this input state var
--       match state_descript with
--       | Description.state _ stmt =>
--         -- check stmt, if there's a transition to this state
--         let bool_list : List Bool := recursively_find_stmt_with_transition_to_arg (state, stmt)
--         -- return bool
--         match bool_list with
--         | [] => false
--         | [true] => true
--         | _::_ => bool_list.all (λ bool' => bool' == true)
--       -- | _ => throw state_descript
--       | _ => false
--     )
  
--   let state_names : List String :=
--     List.join (
--     states_that_transition_to_state.map ( λ state_descript =>
--       match state_descript with
--       | Description.state ident _ => [(ident)]
--       | _ => []
--       ))

--   state_names

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

-- partial def recursively_find_stmt_and_update_transitions
-- ( old_name_new_name_and_stmt : String × (String × Statement) )
-- : Statement
-- :=
--   let old_name : String := old_name_new_name_and_stmt.1
--   let new_name : String := old_name_new_name_and_stmt.2.1
--   let stmt : Statement := old_name_new_name_and_stmt.2.2

--   -- try to match statment
--   let bool_list :=
--   match stmt with
--   | Statement.transition ident =>
--     dbg_trace s!"--== BEGIN OLD TRANS NAME: {stmt}"
--     dbg_trace s!"--== THE OLD TRANS NAME: {old_name}"
--     dbg_trace s!"--== END REPLACEMENT TRANS NAME: {new_name}"
--     if ident == old_name then Statement.transition new_name
--     else stmt
--   -- TODO NOTE: Should count both "reset" & "transition"
--   -- but ignore "completion...?"
--   | Statement.reset _ => stmt
--   | Statement.complete _ => stmt
--   | Statement.stall _ => stmt
--   | Statement.return_stmt _ => stmt
--   | Statement.block list_statment =>
--     let stmts_with_name_info : List (String × String × Statement) :=
--       list_statment.map (λ stmt' => (old_name, new_name, stmt'))
    
--     let blk_stmt : Statement :=
--     Statement.block ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
--     blk_stmt
--   | Statement.stray_expr _ => stmt
--   | Statement.when q_name list_ident stmt' =>
--     let updated_stmt : Statement := recursively_find_stmt_and_update_transitions (old_name, new_name, stmt')
--     let new_when : Statement := Statement.when q_name list_ident updated_stmt
--     new_when
--   | Statement.await term' list_stmt =>
--     let stmts_with_name_info : List (String × String × Statement) :=
--       list_stmt.map (λ stmt' => (old_name, new_name, stmt'))
    
--     let await_stmt : Statement :=
--     Statement.await term' ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
--     await_stmt
--   -- | Statement.await none list_stmt =>
--   --   let stmts_with_name_info : List (String × String × Statement) :=
--   --     list_stmt.map (λ stmt' => (old_name, new_name, stmt'))
    
--   --   let await_stmt : Statement :=
--   --   Statement.await none ( stmts_with_name_info.map recursively_find_stmt_and_update_transitions )
    
--   --   await_stmt
--   | Statement.listen_handle stmt' list_handles =>
--     Statement.listen_handle (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt'))
--       ( list_handles.map
--         (
--           λ handl =>
--           match handl with
--           | HandleBlock.mk qual_name list_ident stmt1 =>
--             HandleBlock.mk qual_name list_ident ( recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1) )
--         )
--       )
--   | Statement.conditional_stmt cond =>
--     match cond with
--     | Conditional.if_else_statement expr stmt1 stmt2 =>
--       Statement.conditional_stmt (
--       Conditional.if_else_statement expr (
--         recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1)
--       ) (
--         recursively_find_stmt_and_update_transitions (old_name, new_name, stmt2)
--       ))
--     | Conditional.if_statement expr stmt1 =>
--       Statement.conditional_stmt (
--       Conditional.if_statement expr (
--         recursively_find_stmt_and_update_transitions (old_name, new_name, stmt1)
--       ) )
--   | Statement.variable_assignment _ _ => stmt
--   | Statement.value_declaration _ _ => stmt
--   | Statement.variable_declaration _ => stmt
--   | Statement.labelled_statement label stmt => Statement.labelled_statement label (recursively_find_stmt_and_update_transitions (old_name, new_name, stmt))

--   bool_list

-- (b) Function to re-write the states which directly transition to the state
-- def update_state_transitions_matching_name_to_replacement_name
-- ( orig_name : String)
-- ( replacement_name : String)
-- ( list_states_names_to_update : List String)
-- ( all_state_descriptions : List Description)
-- : (List Description)
-- := --do

--   -- take the state_descriptions, and see if it's name matches one we need to update
--   -- if it matches, map all of it's statements
--   --   map func should check if stmt is a transition & if it is then replace the identifier
--   --   map func should recursively descend into anything which holds list(s) of statements
--   --   map func should return the original otherwise
--   -----------------------------------------
--   -- high level pseudo code : Do the match on the state descriptions
--   -- match all_state_descriptions with
--   -- if state name == orig name
--   -- then do recursive update rewrite
--   -- else return original state description.

--   let updated_states : (List Description) :=
--     List.join (
--     all_state_descriptions.map ( λ state_descript =>
--       match state_descript with
--       | Description.state this_state_name stmt =>
--         if list_states_names_to_update.contains this_state_name then
--           [Description.state this_state_name (recursively_find_stmt_and_update_transitions (orig_name, replacement_name, stmt))]
--         else
--           [state_descript]
--       | _ => []
--     ))
--   updated_states
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
  
  -- def gen_stall_dsl_state
  -- (new_stall_state_name : String)
  -- (original_state_name : String)
  -- (ctrler_name : String)
  -- (state_check_or_tree : Pipeline.Expr)
  -- (stall_on_inst_type : InstType)
  -- (original_state's_handleblks : Option ( List HandleBlock ))
  -- : Description
  -- :=

  --   -- (entry.instruction.seq_num < instruction.seq_num)
  --   let entry_is_earlier_than_this_one : Term :=
  --     Pipeline.Term.expr (
  --     Pipeline.Expr.less_than
  --     (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"]))
  --     (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
  --     )
  --   -- (entry.instruction.op == stall_on_inst_type)
  --   let entry_is_of_desired_type : Term :=
  --     Pipeline.Term.expr (
  --     Pipeline.Expr.equal
  --     (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "op"]))
  --     (Pipeline.Term.const (Const.str_lit (stall_on_inst_type.toMurphiString)))
  --     )
  --   -- let entry_is_valid : Term :=
  --   --   Pipeline.Term.expr (
  --   --   Pipeline.Expr.not_equal
  --   --   (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"]))
  --   --   (Pipeline.Term.const (Const.num_lit (0))) -- using 0 as an "invalid inst"
  --   --   )

  --   let search_condition : Expr := -- ERROR! TODO: Adjust to ignore invalid entries!
  --     -- (Pipeline.Expr.binand
  --       -- (Pipeline.Term.expr
  --       (Pipeline.Expr.binand
  --       entry_is_earlier_than_this_one
  --       entry_is_of_desired_type)
  --       -- )
  --       -- entry_is_valid
  --     -- )

  --   --  min(instruction.seq_num - entry.instruction.seq_num)
  --   let search_min : Expr :=
  --     Pipeline.Expr.some_term (
  --       Pipeline.Term.function_call 
  --       (QualifiedName.mk ["min"])
  --       [(Pipeline.Expr.sub
  --         (Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"]))
  --         (Pipeline.Term.qualified_var (QualifiedName.mk ["entry", "instruction", "seq_num"])))]
  --     )

  --   let ctrler_search_call : Term :=
  --     Pipeline.Term.function_call
  --     (QualifiedName.mk [ctrler_name, "search"])
  --     [search_condition, search_min]

  --   -- let stall_state_name := ctrler_name ++ "stall" ++ original_state_name
  --   let when_success_stmt_blk : Pipeline.Statement :=
  --     Pipeline.Statement.block [
  --       Pipeline.Statement.conditional_stmt (
  --       Pipeline.Conditional.if_else_statement
  --       state_check_or_tree
  --       (Pipeline.Statement.reset new_stall_state_name)
  --       (Pipeline.Statement.transition original_state_name)
  --       )
  --     ]
  --   let when_success : Pipeline.Statement :=
  --     Pipeline.Statement.when
  --     (QualifiedName.mk [ctrler_name, "search_success"])
  --     (["curr_state"])
  --     when_success_stmt_blk

  --   let when_fail_stmt_blk : Pipeline.Statement :=
  --     Pipeline.Statement.block [
  --       (Pipeline.Statement.transition original_state_name)
  --     ]
  --   let when_fail : Pipeline.Statement :=
  --     Pipeline.Statement.when
  --     (QualifiedName.mk [ctrler_name, "search_fail"])
  --     ([])
  --     when_fail_stmt_blk


  --   -- TODO: Fill in the when stmts, and the condition in it
  --   let await_stmt : Pipeline.Statement :=
  --     Statement.await ctrler_search_call [when_success, when_fail]
    
  --   let stall_state_stmt : Pipeline.Statement :=
  --     if original_state's_handleblks.isSome then
  --       Statement.listen_handle (Statement.block [await_stmt]) original_state's_handleblks.get!
  --     else
  --       await_stmt

  --   Description.state new_stall_state_name (
  --     Statement.block [stall_state_stmt])
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

-- Algo to find states that don't lead to the complete state
-- NOTE: probably easier if we use "complete" to mark the
-- point where the state machine completes
-- Draw each unique path
-- 1. This will be some kind of recurisve map
-- - Keep list of traversed states as an arg

-- 2. If we reach "complete" state we return []
-- 3. If we reach a dead end, then we just return the list

-- 4. just combine unique elems
-- - we need a function to get all of the transition
-- statements, and complete statements

structure state_graph_traversal_info where
visited_states : List String
visiting : String
completion_states : List String
all_states : List Description

-- abbrev M := StateT decl_and_init_list Id
-- input: state to start from...
partial def find_states_that_don't_lead_to_completion
(traversal_info : state_graph_traversal_info)
: Except String (List (List String))
:= do
  let visited_states : List String := traversal_info.visited_states
  let visiting : String := traversal_info.visiting
  let completion : List String := traversal_info.completion_states

  -- Check if visiting state is a complete state...
  -- if it is, return empty list
  -- if not, Check if there are any reachable states that we haven't visited yet
  -- if there are none, then we return the list of states
  -- if there are some, then we add this visiting state to the list of visited states,
    -- and take the list of possible reachable states by /transition/ and recursively call
  
  let all_states : List Description := traversal_info.all_states
  let list_visiting_state's_code : List Pipeline.Statement := List.join (
    all_states.map
    λ state_descript =>
      match state_descript with
      | Pipeline.Description.state ident stmt => 
        if ident == visiting then
          [stmt]
        else
          []
      | _ => []
  )
  dbg_trace s!"VISITING STATE: {visiting}"
  let visiting_state's_code : Pipeline.Statement ← match list_visiting_state's_code with
  | [] =>
    let msg : String := s!"ERROR: while traversing states, found " ++
      "no states matching a traversed state name!\n" ++
      s!"State to Find: {visiting}\n" ++
      s!"All States: {all_states}"
    throw msg
  | [one_stmt] => pure one_stmt
  | _::_ =>
    let msg : String := s!"ERROR: while traversing states, found multiple states with " ++
      "the same name matching a traversed state name!\n" ++
      "no states matching a traversed state name!\n" ++
      s!"State to Find: {visiting}\n" ++
      s!"All States: {all_states}"
    throw msg
  dbg_trace s!"VISITING STATE CODE: {visiting_state's_code}"
  let states_transitioned_to : List String := get_only_transitions_recursively visiting_state's_code
  dbg_trace s!"states_transitioned_to: {states_transitioned_to}"

  dbg_trace s!"Completion List: {completion}"
  dbg_trace s!"Visited States: {visited_states}"
  if completion.contains visiting then
    return []
    -- NOTE: The case below shouldn't happen if "reset" is used properly...
  else if visited_states.contains visiting then
    let msg : String := "Detected cycle with transition keyword during transition analysis!\n" ++
    s!"Revisited State: {visiting}, Visited State List: {visited_states}\n" ++
    s!"Please use reset keyword instead of transition when transitioning back!"
    throw msg
    -- return [visited_states]
  else if states_transitioned_to.isEmpty then
    return [visited_states.concat visiting]
  else
    -- traverse further for each transition
    let next_transitions : List state_graph_traversal_info :=
      states_transitioned_to.map λ state : String =>
      let next_traversal_info : state_graph_traversal_info := {
        visited_states := visited_states.concat visiting,
        visiting := state,
        completion_states := traversal_info.completion_states,
        all_states := traversal_info.all_states
      }
      next_traversal_info

    -- NOTE: Couldn't use List.join all on the same line...
    let list_list_of_traversals : List ( List ( List String )) ← (next_transitions.mapM find_states_that_don't_lead_to_completion)
    let list_of_traversals : ( List ( List String )) := List.join list_list_of_traversals
    dbg_trace s!"list_of_traversals: {list_of_traversals}"

    return list_of_traversals


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
def naive_update_add_stall_to_global_perform_ld
( ctrler_and_all_state_descriptions : controller_info × (List Description) )
: Except String ( controller_info )
:= do
  let ctrler : controller_info := ctrler_and_all_state_descriptions.1
  -- (1) Get ctrler state awaiting mem resp
  let states_awaiting_mem_ld_resp : List String :=
    get_ctrler_state_awaiting_mem_load_resp ctrler

  let state_awaiting_mem_ld_resp : String ←
    match states_awaiting_mem_ld_resp with
    | [state_name] => pure state_name
    | [] => return ctrler
    | _ => -- "empty list or multiple states that send a load req?"
      dbg_trace "multiple states that receive a load resp?"
      let error_msg : String :=
        "FAIL: Naive in-order load tsfm found multiple states" ++
        s!" that receive ld responses!\nCtrler: {ctrler.name}\nStates: {states_awaiting_mem_ld_resp}"
      throw error_msg

  -- (3) Gen the Stall state
  -- use the gen_stall_dsl_state function to make a new stall state

  -- Get ctrler state sending the mem req
  let states_globally_performing_mem_ld : List String :=
    get_ctrler_state_globally_performing_load ctrler

  let state_globally_performing_mem_ld : String ←
    match states_globally_performing_mem_ld with
    | [state_name] => pure state_name
    | [] => return ctrler
    | _ => -- "empty list or multiple states that send a load req?"
      dbg_trace "multiple states that send a load req?"
      let error_msg : String :=
        "FAIL: Naive in-order load tsfm found multiple states" ++
        s!" that send load requests!\nCtrler: {ctrler.name}\nStates: {states_awaiting_mem_ld_resp}"
      throw error_msg

  let new_stall_state_name : String := String.join [ctrler.name, "_stall_", state_globally_performing_mem_ld]

  -- (2) Get the states that transition to this state
  -- to build the OR tree of states to check...
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let states_predecessor_to_await_mem_resp_state : List String := (
    get_states_leading_to_given_state (state_awaiting_mem_ld_resp, transitions))
    ++ [state_awaiting_mem_ld_resp] --, new_stall_state_name]

  let all_state_descriptions : List Description := ctrler_and_all_state_descriptions.2
  let list_completion_states : List String :=
  List.join (
    all_state_descriptions.map λ state : Description =>
    match state with
    | Description.state ident stmt =>
      -- CHECKPOINT TODO: if ident has complete, then return ident
      let list_complete_transitions : List String := get_complete_transition stmt
      dbg_trace s!"Transitions that use 'complete' keyword: {list_complete_transitions}"
      -- if there were entries, then get the 'complete' transition's ident
      match list_complete_transitions with
      | [] => []
      | _ => [ident]
    | _ => []
    )

  dbg_trace s!"LIST OF COMPLETION STATES: {list_completion_states}"

  let states_reachable_from_predecessors_not_to_completion_state : List (List ( List String )) ←
    states_predecessor_to_await_mem_resp_state.mapM (
      λ state : String => 
      let prep_traversal_info : state_graph_traversal_info := {
        visited_states := [],
        visiting := state,
        completion_states := list_completion_states,
        all_states := all_state_descriptions
      }
      -- find_states_that_don't_lead_to_completion prep_traversal_info
      match find_states_that_don't_lead_to_completion prep_traversal_info with
      | .error msg => throw s!"ERROR: Searching for reachable states from await_ld_mem_resp: {msg}"
      | .ok list_of_traversal_paths => pure list_of_traversal_paths
      )

  dbg_trace s!"REACHABLE PATHS: {states_reachable_from_predecessors_not_to_completion_state}"

  let non_completion_path_states := List.join (List.join states_reachable_from_predecessors_not_to_completion_state)
  let unique_non_completion_path_states := List.foldl (
    λ (unique_states : List String) ( state : String ) =>
      if unique_states.contains state then
        unique_states
      else
        unique_states.concat state
    ) [] non_completion_path_states
  
  dbg_trace s!"UNIQUE STATES: {unique_non_completion_path_states}"

  let states_to_stall_on : List String := unique_non_completion_path_states.concat new_stall_state_name

  -- Build the tree of (curr_state == <state>) |
  -- Use convert_state_names_to_dsl_or_tree_state_check
  let not_yet_gotten_mem_resp_state_check : Pipeline.Expr ←
    convert_state_names_to_dsl_or_tree_state_check states_to_stall_on

  let new_stall_state : Description :=
    gen_stall_dsl_state new_stall_state_name state_globally_performing_mem_ld
    ctrler.name not_yet_gotten_mem_resp_state_check load Option.none

  -- (4) Update the stall state with these Helper Funcs
  -- Update states which transitioned to the original state:
  -- a) get_states_directly_leading_to_given_state
  -- b) update_state_transitions_matching_name_to_replacement_name
  -- c) add the new state to the list of states of the ctrler,
  --    will need to return a new ctrler
  let states_that_transition_to_original_global_perform_ld : List String :=
    get_states_directly_leading_to_given_state (state_globally_performing_mem_ld, transitions)
  dbg_trace s!"======= STATES directly leading to state {state_globally_performing_mem_ld}"
  dbg_trace states_that_transition_to_original_global_perform_ld
  dbg_trace s!"======= END States that directly lead to state {state_globally_performing_mem_ld}"

  let updated_state_list_transition_to_stall_state : List Description :=
    update_state_transitions_matching_name_to_replacement_name
    state_globally_performing_mem_ld new_stall_state_name states_that_transition_to_original_global_perform_ld transitions

  let new_state_machine_with_stall_state : List Description :=
    updated_state_list_transition_to_stall_state ++ [new_stall_state]
  
  let new_ctrler : controller_info := {
    name := ctrler.name,
    controller_descript := ctrler.controller_descript,
    entry_descript := ctrler.entry_descript,
    init_trans := ctrler.init_trans,
    state_vars := ctrler.state_vars,
    transition_list := new_state_machine_with_stall_state
    ctrler_init_trans := ctrler.ctrler_init_trans
    ctrler_state_vars := ctrler.ctrler_state_vars
    ctrler_trans_list := ctrler.ctrler_trans_list
  }

  return new_ctrler

def get_ctrler_state_awaiting_mem_store_resp
( ctrler : controller_info ) -- really is Description.state
: ( List String ) -- The name of the transition with the mem load req
:=
  -- search and find the transition
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let lst_trans_with_mem_access : List String := List.join (
    transitions.map (
      λ trans_descript =>
        match trans_descript with
        | Description.state ident stmt =>
          -- want to find a stmt that contains
          -- a memory_interface access/request
          let bool_lst :=
          true_if_stmts_awaits_st_mem_response stmt

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

  lst_trans_with_mem_access

-- TODO: change to find globally perform store..
def get_ctrler_state_globally_performing_store
( ctrler : controller_info ) -- really is Description.state
: ( List String ) -- The name of the transition with the mem load req
:=
  -- search and find the transition
  let transitions : List Description :=
    if ctrler.init_trans.isSome then
      ctrler.transition_list.get!
    else if ctrler.ctrler_init_trans.isSome then
      ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
        default
  let lst_trans_with_mem_access : List String := List.join (
    transitions.map (
      λ trans_descript =>
        match trans_descript with
        | Description.state ident stmt =>
          -- want to find a stmt that contains
          -- a memory_interface access/request
          let bool_lst :=
          true_if_stmts_have_store_mem_access stmt

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

-- def get_ctrler_state_handle_blocks
-- -- (ctrlers : List controller_info)
-- (ctrler : controller_info)
-- (state_name : String)
-- : Except String (Option (List HandleBlock))
-- := do
--   let transitions : List Description :=
--     if ctrler.init_trans.isSome then
--       ctrler.transition_list.get!
--     else if ctrler.ctrler_init_trans.isSome then
--       ctrler.ctrler_trans_list.get!
--     else
--       dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler})"
--         default
--   let list_state_matching_name : List Description :=
--   transitions.filter (λ state : Description =>
--     match state with
--     | .state name _ =>
--       if name == state_name then true
--       else false
--     | _ => false
--     )
--   let state_matching_name : Description ←
--     match list_state_matching_name with
--     | [matching_state] => pure matching_state
--     | _ =>
--       let msg : String :=
--         "While trying to match a state with a name:" ++
--         "Found either multiple or no states\n" ++
--         s!"List contents: ({list_state_matching_name})"
--       throw msg

--   -- TODO: Get the stmt block, get the stmts from the block
--   -- Call the get_listen_handle_blks_from_stmts function
--   -- Check if there was anything, if there is use Option.some...
--   let state_stmt_blk : Pipeline.Statement ←
--   match state_matching_name with
--   | .state _ stmt => pure stmt
--   | _ =>
--     let msg : String := "Shouldn't have something besides .state type ..."
--     throw msg
  
--   let state_stmts : List Pipeline.Statement ←
--   match state_stmt_blk with
--   | .block stmts => pure stmts
--   | _ =>
--     let msg : String :=
--       "Error opening stmt from state: state should have had a block statement?\n" ++
--       s!"State stmt: ({state_stmt_blk})"
--     throw msg
  
--   let handle_blks : List HandleBlock ←
--     get_listen_handle_blks_from_stmts state_stmts

--   if handle_blks.isEmpty then
--     return Option.none
--   else
--     return Option.some handle_blks

-- Try to generalize to structures in a core
-- Stall by adding a search-and-await
-- Next step: For memory messages, probably need to await-response
-- work with ctrler-entry type designs
def more_generic_core_in_order_stall_transform
-- ( ctrler : controller_info )
( all_ctrlers : List controller_info )
-- first_inst -> second_inst, st -> ld, ld -> ld
( first_inst : InstType)
( second_inst : InstType)
: Except String ( List controller_info )
:= do
  dbg_trace "##### Reach here?"
  -- What are the steps?
  /-
  1. Generically find the structure that awaits the response from mem.
  Filter for a ctrler from the list, check
  -/
  -- Also, 
  let get_expected_inst_await_completion_msg_ctrler
    : controller_info → List String :=
    match second_inst with
    | .load => get_ctrler_state_awaiting_mem_load_resp
    | .store => get_ctrler_state_awaiting_mem_store_resp
  let ctrlers_that_await_mem_completion
    : List (controller_info × List String) :=
    -- use the function on list of ctrlers
    List.join (
    all_ctrlers.map ( λ ctrler : controller_info =>
      let list_states_with_mem_access : List String :=
        get_expected_inst_await_completion_msg_ctrler ctrler

      -- let ctrler_and_states_that_await_mem_completion
      if list_states_with_mem_access.length > 0 then
        [(ctrler, list_states_with_mem_access)]
      else
        []
    ))
  let (ctrler_that_await_mem_completion, states_that_await_mem_completion)
    -- Couldn't get it to work with declaring it's type.. keeps expecting ':='
    -- : (controller_info × (List String))
    ←
    match ctrlers_that_await_mem_completion with
    | [] =>
      let msg : String :=
        "ERROR: in 'more_generic_core_in_order_stall_transform\n" ++
        s!"Found no ctrlers that await InstType:({second_inst.toString}) response"++
        s!"\nCtrlrs_that_await_mem_completion: ({ctrlers_that_await_mem_completion})"++
        s!"\nall_ctrlers: ({all_ctrlers})"
      throw msg
    | [one] => pure one
    | _ :: _ =>
      let msg : String :=
        "ERROR: in 'more_generic_core_in_order_stall_transform\n" ++
        s!"Found multiple ctrlers that await InstType:({second_inst.toString}) response\n" ++
        s!"Ctrlers & Found States: ({ctrlers_that_await_mem_completion})"
      throw msg
  dbg_trace "***** await mem completion controller"
  dbg_trace s!"({ctrler_that_await_mem_completion})"
    
  /-
  2. After finding the ctrler & state(s) that await the mem response,
  find the ctrler & state(s) that send a mem response
  -/
  let get_expected_inst_send_mem_req_ctrler
    : controller_info → List String :=
    match first_inst with
    | .load => get_ctrler_state_globally_performing_load
    | .store => get_ctrler_state_globally_performing_store

  let ctrlers_that_send_mem_req
    : List (controller_info × List String) :=
    -- use the function on list of ctrlers
    List.join (
    all_ctrlers.map ( λ ctrler : controller_info =>
      let list_states_with_send_mem_req : List String :=
        get_expected_inst_send_mem_req_ctrler ctrler

      if list_states_with_send_mem_req.length > 0 then
        [ (ctrler, list_states_with_send_mem_req) ]
      else
        []
    ))
  let (ctrler_that_send_mem_req, states_that_send_mem_req)
    -- Couldn't get it to work with declaring it's type.. keeps expecting ':='
    -- : (controller_info × (List String))
    ←
    match ctrlers_that_send_mem_req with
    | [] =>
      let msg : String :=
        "ERROR: Unexpected in 'more_generic_core_in_order_stall_transform\n" ++
        s!"Found no ctrlers that send InstType:({first_inst.toString}) request"
      throw msg
    | [one] => pure one
    | _ :: _ =>
      let msg : String :=
        "ERROR: Unexpected in 'more_generic_core_in_order_stall_transform\n" ++
        s!"Found multiple ctrlers that send InstType:({first_inst.toString}) request\n" ++
        s!"Ctrlers & Found States: ({ctrlers_that_send_mem_req})"
      throw msg
  dbg_trace "***** send_mem_req controller"
  dbg_trace s!"({ctrler_that_send_mem_req})"

  /-
  3. Gen the new stall state's name
  -/
  -- Get ctrler state sending the mem req
  let global_perform_state_that_send_mem_req : String ←
    match states_that_send_mem_req with
    | [state_name] => pure state_name
    | [] => 
      let error_msg : String :=
        "FAIL: more_generic_core_in_order_stall_transform found NO states\n" ++
        s!" that send ({first_inst.toString}) requests!" ++
        s!"\nCtrler: {ctrler_that_send_mem_req.name}"
      throw error_msg
    | _ => -- "empty list or multiple states that send a load req?"
      dbg_trace "multiple states that send a load req?"
      let error_msg : String :=
        "FAIL: more_generic_core_in_order_stall_transform found MULTIPLE states\n" ++
        s!" that send load requests!" ++
        s!"\nCtrler: {ctrler_that_send_mem_req.name}" ++
        s!"\nStates: {states_that_send_mem_req}"
      throw error_msg

  let new_stall_global_perform_state_name : String :=
    String.join [ctrler_that_send_mem_req.name, "_stall_",
      global_perform_state_that_send_mem_req]

  /-
  4. Find all previous states to the await mem response state
  that are not yet complete
  -/
  let state_that_await_mem_completion : String ←
    match states_that_await_mem_completion with
    | [] =>
      let error_msg : String :=
        "FAIL: more_generic_core_in_order_stall_transform found NO states that await mem completion\n" ++
        s!" that await ({second_inst.toString}) requests!" ++
        s!"\nCtrler: {ctrler_that_await_mem_completion.name}"
      throw error_msg 
    | [one] => pure one
    | _ :: _ =>
      let error_msg : String :=
        "FAIL: more_generic_core_in_order_stall_transform found MULTIPLE states that await mem completion\n" ++
        s!" that await ({second_inst.toString}) requests!" ++
        s!"\nCtrler: {ctrler_that_await_mem_completion.name}" ++
        s!"\nStates: {states_that_await_mem_completion}"
      throw error_msg

  let transitions : List Description :=
    if ctrler_that_await_mem_completion.init_trans.isSome then
      ctrler_that_await_mem_completion.transition_list.get!
    else if ctrler_that_await_mem_completion.ctrler_init_trans.isSome then
      ctrler_that_await_mem_completion.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler_that_await_mem_completion})"
        default
  let states_predecessor_to_await_mem_resp_state : List String := (
    get_states_leading_to_given_state (state_that_await_mem_completion, transitions))
    ++ [state_that_await_mem_completion] --, new_stall_state_name]

  /-
  Find the "complete" transition/state (where this State Machine finishes...)
  Perhaps one can think of this as a sub-transaction in a larger transaction...
  -/
  let all_states_in_completion_ctrler : List Description := transitions
  let list_list_completion_states : List (List String) ← (
    all_states_in_completion_ctrler.mapM λ state : Description =>
    match state with
    | Description.state ident stmt =>
      let list_complete_transitions : List String := get_complete_transition stmt
      dbg_trace s!"Transitions that use 'complete' keyword: {list_complete_transitions}"
      -- if there were entries, then get the 'complete' transition's ident
      match list_complete_transitions with
      | [] => pure []
        -- should throw here? Yes, any state machine that "completes" should use the keyword
        -- Nevermind, don't throw here. throw elsewhere.
        -- let msg : String :=
        -- "FAIL: in func more_generic_core_in_order_stall_transform\n" ++
        -- s!"State Machine ({ctrler_that_await_mem_completion.name}) has no 'complete' transition!\n"++
        -- s!"Controller: ({ctrler_that_await_mem_completion})"
        -- throw msg
      | _ => pure [ident]
    | _ => pure []
    )
  let list_completion_states : (List String) := List.join list_list_completion_states
  let list_completion_states : (List String) ← match list_completion_states with
  | [] =>
    -- should throw here? Yes, any state machine that "completes" should use the keyword
    -- Nevermind, don't throw here. throw elsewhere.
    let msg : String :=
    "FAIL: in func more_generic_core_in_order_stall_transform\n" ++
    s!"State Machine ({ctrler_that_await_mem_completion.name}) has no 'complete' transition!\n"++
    s!"Controller: ({ctrler_that_await_mem_completion})"
    throw msg
  | _ => pure list_completion_states

  dbg_trace s!"LIST OF COMPLETION STATES: {list_completion_states}"

  /-
  Get the rest of the states that are "before" this state machine's 'complete' state
  -/
  let states_reachable_from_predecessors_not_to_completion_state : List (List ( List String )) ←
    states_predecessor_to_await_mem_resp_state.mapM (
      λ state : String => 
      let prep_traversal_info : state_graph_traversal_info := {
        visited_states := [],
        visiting := state,
        completion_states := list_completion_states,
        all_states := all_states_in_completion_ctrler
      }
      -- find_states_that_don't_lead_to_completion prep_traversal_info
      match find_states_that_don't_lead_to_completion prep_traversal_info with
      | .error msg => 
        let msg' : String :=
        s!"FAIL: in func more_generic_core_in_order_stall_transform\n" ++
        s!"ERROR: Searching for reachable states from await_mem_response:\n({msg})"
        throw msg'
      | .ok list_of_traversal_paths => pure list_of_traversal_paths
      )

  dbg_trace s!"REACHABLE PATHS: {states_reachable_from_predecessors_not_to_completion_state}"

  /-
  i.e. states that are before the await_mem_response state
  -/
  let non_completion_path_states :=
   (List.join (List.join states_reachable_from_predecessors_not_to_completion_state)) ++
   states_predecessor_to_await_mem_resp_state
  let unique_non_completion_path_states := List.foldl (
    λ (unique_states : List String) ( state : String ) =>
      if unique_states.contains state then
        unique_states
      else
        unique_states.concat state
    ) [] non_completion_path_states
  
  dbg_trace s!"UNIQUE STATES: {unique_non_completion_path_states}"

  let states_to_stall_on : List String := unique_non_completion_path_states.concat new_stall_global_perform_state_name

  -- Build the tree of (curr_state == <state>) |
  -- Use convert_state_names_to_dsl_or_tree_state_check
  let not_yet_gotten_mem_resp_state_check : Pipeline.Expr ←
    convert_state_names_to_dsl_or_tree_state_check states_to_stall_on

  /-
  5. Generate the new stall state
  -/

  /- Consider if original state has a listen-handle -/
  -- Get the stmts
  let handle_blks : Option (List HandleBlock) ←
  get_ctrler_state_handle_blocks ctrler_that_send_mem_req global_perform_state_that_send_mem_req

  let new_stall_state : Description :=
    gen_stall_dsl_state new_stall_global_perform_state_name global_perform_state_that_send_mem_req
    ctrler_that_await_mem_completion.name not_yet_gotten_mem_resp_state_check first_inst
    handle_blks
  dbg_trace s!"New stall state: \n{new_stall_state}"

  /-
  6. Update states that point to the previous perform
  -/

  let send_mem_req_transitions : List Description :=
    if ctrler_that_send_mem_req.init_trans.isSome then
      ctrler_that_send_mem_req.transition_list.get!
    else if ctrler_that_send_mem_req.ctrler_init_trans.isSome then
      ctrler_that_send_mem_req.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler_that_send_mem_req})"
        default

  let states_that_transition_to_original_global_perform : List String :=
    get_states_directly_leading_to_given_state (global_perform_state_that_send_mem_req, send_mem_req_transitions)
  dbg_trace s!"======= STATES directly leading to state {global_perform_state_that_send_mem_req}"
  dbg_trace states_that_transition_to_original_global_perform
  dbg_trace s!"======= END States that directly lead to state {global_perform_state_that_send_mem_req}"

  let updated_state_list_transition_to_stall_state : List Description :=
    update_state_transitions_matching_name_to_replacement_name
    global_perform_state_that_send_mem_req new_stall_global_perform_state_name
    states_that_transition_to_original_global_perform send_mem_req_transitions

  let new_state_machine_with_stall_state : List Description :=
    updated_state_list_transition_to_stall_state ++ [new_stall_state]
  
  let new_ctrler : controller_info := {
    name := ctrler_that_send_mem_req.name,
    controller_descript := ctrler_that_send_mem_req.controller_descript,
    entry_descript := ctrler_that_send_mem_req.entry_descript,
    init_trans := ctrler_that_send_mem_req.init_trans,
    state_vars := ctrler_that_send_mem_req.state_vars,
    transition_list := -- new_state_machine_with_stall_state
      if ctrler_that_send_mem_req.init_trans.isSome then
        Option.some new_state_machine_with_stall_state
      else
        Option.none
    ctrler_init_trans := ctrler_that_send_mem_req.init_trans,
    ctrler_state_vars := ctrler_that_send_mem_req.state_vars,
    ctrler_trans_list := -- new_state_machine_with_stall_state
      if ctrler_that_send_mem_req.ctrler_init_trans.isSome then
        Option.some new_state_machine_with_stall_state
      else
        Option.none
  }

  let updated_ctrler_list : List controller_info :=
    all_ctrlers.map (
      λ ctrler =>
        if ctrler.name == ctrler_that_send_mem_req.name then
          new_ctrler
        else
          ctrler
    )
  
  -- dbg_trace s!"The updated ctrler list with the ctrler + stall state: ({updated_ctrler_list})"

  return updated_ctrler_list

-- def more_generic_core_in_order_stall_transform_with_back_trace
-- -- ( ctrler : controller_info )
-- ( all_ctrlers : List controller_info )
-- -- first_inst -> second_inst, st -> ld, ld -> ld
-- ( first_inst : InstType)
-- ( second_inst : InstType)
-- : Except String ( List controller_info )
-- := do
--   dbg_trace "##### Reach here?"
--   -- What are the steps?
--   /-
--   1. Generically find the structure that awaits the response from mem.
--   Filter for a ctrler from the list, check
--   -/
--   -- Also, 
--   let get_expected_inst_await_completion_msg_ctrler
--     : controller_info → List String :=
--     match second_inst with
--     | .load => get_ctrler_state_awaiting_mem_load_resp
--     | .store => get_ctrler_state_awaiting_mem_store_resp
--   let ctrlers_that_await_mem_completion
--     : List (controller_info × List String) :=
--     -- use the function on list of ctrlers
--     List.join (
--     all_ctrlers.map ( λ ctrler : controller_info =>
--       let list_states_with_mem_access : List String :=
--         get_expected_inst_await_completion_msg_ctrler ctrler

--       -- let ctrler_and_states_that_await_mem_completion
--       if list_states_with_mem_access.length > 0 then
--         [(ctrler, list_states_with_mem_access)]
--       else
--         []
--     ))
--   let (ctrler_that_await_mem_completion, states_that_await_mem_completion)
--     -- Couldn't get it to work with declaring it's type.. keeps expecting ':='
--     -- : (controller_info × (List String))
--     ←
--     match ctrlers_that_await_mem_completion with
--     | [] =>
--       let msg : String :=
--         "ERROR: in 'more_generic_core_in_order_stall_transform\n" ++
--         s!"Found no ctrlers that await InstType:({second_inst.toString}) response"++
--         s!"\nCtrlrs_that_await_mem_completion: ({ctrlers_that_await_mem_completion})"++
--         s!"\nall_ctrlers: ({all_ctrlers})"
--       throw msg
--     | [one] => pure one
--     | _ :: _ =>
--       let msg : String :=
--         "ERROR: in 'more_generic_core_in_order_stall_transform\n" ++
--         s!"Found multiple ctrlers that await InstType:({second_inst.toString}) response\n" ++
--         s!"Ctrlers & Found States: ({ctrlers_that_await_mem_completion})"
--       throw msg
--   dbg_trace "***** await mem completion controller"
--   dbg_trace s!"({ctrler_that_await_mem_completion})"
    
--   /-
--   2. After finding the ctrler & state(s) that await the mem response,
--   find the ctrler & state(s) that send a mem response
--   -/
--   let get_expected_inst_send_mem_req_ctrler
--     : controller_info → List String :=
--     match first_inst with
--     | .load => get_ctrler_state_globally_performing_load
--     | .store => get_ctrler_state_globally_performing_store

--   let ctrlers_that_send_mem_req
--     : List (controller_info × List String) :=
--     -- use the function on list of ctrlers
--     List.join (
--     all_ctrlers.map ( λ ctrler : controller_info =>
--       let list_states_with_send_mem_req : List String :=
--         get_expected_inst_send_mem_req_ctrler ctrler

--       if list_states_with_send_mem_req.length > 0 then
--         [ (ctrler, list_states_with_send_mem_req) ]
--       else
--         []
--     ))
--   let (ctrler_that_send_mem_req, states_that_send_mem_req)
--     -- Couldn't get it to work with declaring it's type.. keeps expecting ':='
--     -- : (controller_info × (List String))
--     ←
--     match ctrlers_that_send_mem_req with
--     | [] =>
--       let msg : String :=
--         "ERROR: Unexpected in 'more_generic_core_in_order_stall_transform\n" ++
--         s!"Found no ctrlers that send InstType:({first_inst.toString}) request"
--       throw msg
--     | [one] => pure one
--     | _ :: _ =>
--       let msg : String :=
--         "ERROR: Unexpected in 'more_generic_core_in_order_stall_transform\n" ++
--         s!"Found multiple ctrlers that send InstType:({first_inst.toString}) request\n" ++
--         s!"Ctrlers & Found States: ({ctrlers_that_send_mem_req})"
--       throw msg
--   dbg_trace "***** send_mem_req controller"
--   dbg_trace s!"({ctrler_that_send_mem_req})"

--   /-
--   3. Gen the new stall state's name
--   -/
--   -- Get ctrler state sending the mem req
--   let global_perform_state_that_send_mem_req : String ←
--     match states_that_send_mem_req with
--     | [state_name] => pure state_name
--     | [] => 
--       let error_msg : String :=
--         "FAIL: more_generic_core_in_order_stall_transform found NO states\n" ++
--         s!" that send ({first_inst.toString}) requests!" ++
--         s!"\nCtrler: {ctrler_that_send_mem_req.name}"
--       throw error_msg
--     | _ => -- "empty list or multiple states that send a load req?"
--       dbg_trace "multiple states that send a load req?"
--       let error_msg : String :=
--         "FAIL: more_generic_core_in_order_stall_transform found MULTIPLE states\n" ++
--         s!" that send load requests!" ++
--         s!"\nCtrler: {ctrler_that_send_mem_req.name}" ++
--         s!"\nStates: {states_that_send_mem_req}"
--       throw error_msg

--   let new_stall_global_perform_state_name : String :=
--     String.join [ctrler_that_send_mem_req.name, "_stall_",
--       global_perform_state_that_send_mem_req]

--   /-
--   4. Find all previous states to the await mem response state
--   that are not yet complete
--   -/
--   let state_that_await_mem_completion : String ←
--     match states_that_await_mem_completion with
--     | [] =>
--       let error_msg : String :=
--         "FAIL: more_generic_core_in_order_stall_transform found NO states that await mem completion\n" ++
--         s!" that await ({second_inst.toString}) requests!" ++
--         s!"\nCtrler: {ctrler_that_await_mem_completion.name}"
--       throw error_msg 
--     | [one] => pure one
--     | _ :: _ =>
--       let error_msg : String :=
--         "FAIL: more_generic_core_in_order_stall_transform found MULTIPLE states that await mem completion\n" ++
--         s!" that await ({second_inst.toString}) requests!" ++
--         s!"\nCtrler: {ctrler_that_await_mem_completion.name}" ++
--         s!"\nStates: {states_that_await_mem_completion}"
--       throw error_msg

--   let transitions : List Description :=
--     if ctrler_that_await_mem_completion.init_trans.isSome then
--       ctrler_that_await_mem_completion.transition_list.get!
--     else if ctrler_that_await_mem_completion.ctrler_init_trans.isSome then
--       ctrler_that_await_mem_completion.ctrler_trans_list.get!
--     else
--       dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({ctrler_that_await_mem_completion})"
--         default
--   let states_predecessor_to_await_mem_resp_state : List String := (
--     get_states_leading_to_given_state (state_that_await_mem_completion, transitions))
--     ++ [state_that_await_mem_completion] --, new_stall_state_name]

--   /-
--   Find the "complete" transition/state (where this State Machine finishes...)
--   Perhaps one can think of this as a sub-transaction in a larger transaction...
--   -/
--   let all_states_in_completion_ctrler : List Description := transitions
--   let list_list_completion_states : List (List String) ← (
--     all_states_in_completion_ctrler.mapM λ state : Description =>
--     match state with
--     | Description.state ident stmt =>
--       let list_complete_transitions : List String := get_complete_transition stmt
--       dbg_trace s!"Transitions that use 'complete' keyword: {list_complete_transitions}"
--       -- if there were entries, then get the 'complete' transition's ident
--       match list_complete_transitions with
--       | [] => pure []
--         -- should throw here? Yes, any state machine that "completes" should use the keyword
--         -- Nevermind, don't throw here. throw elsewhere.
--         -- let msg : String :=
--         -- "FAIL: in func more_generic_core_in_order_stall_transform\n" ++
--         -- s!"State Machine ({ctrler_that_await_mem_completion.name}) has no 'complete' transition!\n"++
--         -- s!"Controller: ({ctrler_that_await_mem_completion})"
--         -- throw msg
--       | _ => pure [ident]
--     | _ => pure []
--     )
--   let list_completion_states : (List String) := List.join list_list_completion_states
--   let list_completion_states : (List String) ← match list_completion_states with
--   | [] =>
--     -- should throw here? Yes, any state machine that "completes" should use the keyword
--     -- Nevermind, don't throw here. throw elsewhere.
--     let msg : String :=
--     "FAIL: in func more_generic_core_in_order_stall_transform\n" ++
--     s!"State Machine ({ctrler_that_await_mem_completion.name}) has no 'complete' transition!\n"++
--     s!"Controller: ({ctrler_that_await_mem_completion})"
--     throw msg
--   | _ => pure list_completion_states

--   dbg_trace s!"LIST OF COMPLETION STATES: {list_completion_states}"

--   /-
--   Get the rest of the states that are "before" this state machine's 'complete' state
--   -/
--   let states_reachable_from_predecessors_not_to_completion_state : List (List ( List String )) ←
--     states_predecessor_to_await_mem_resp_state.mapM (
--       λ state : String => 
--       let prep_traversal_info : state_graph_traversal_info := {
--         visited_states := [],
--         visiting := state,
--         completion_states := list_completion_states,
--         all_states := all_states_in_completion_ctrler
--       }
--       -- find_states_that_don't_lead_to_completion prep_traversal_info
--       match find_states_that_don't_lead_to_completion prep_traversal_info with
--       | .error msg => 
--         let msg' : String :=
--         s!"FAIL: in func more_generic_core_in_order_stall_transform\n" ++
--         s!"ERROR: Searching for reachable states from await_mem_response:\n({msg})"
--         throw msg'
--       | .ok list_of_traversal_paths => pure list_of_traversal_paths
--       )

--   dbg_trace s!"REACHABLE PATHS: {states_reachable_from_predecessors_not_to_completion_state}"

--   /-
--   i.e. states that are before the await_mem_response state
--   -/
--   let non_completion_path_states :=
--    (List.join (List.join states_reachable_from_predecessors_not_to_completion_state)) ++
--    states_predecessor_to_await_mem_resp_state
--   let unique_non_completion_path_states := List.foldl (
--     λ (unique_states : List String) ( state : String ) =>
--       if unique_states.contains state then
--         unique_states
--       else
--         unique_states.concat state
--     ) [] non_completion_path_states
  
--   dbg_trace s!"UNIQUE STATES: {unique_non_completion_path_states}"

--   let states_to_stall_on : List String := unique_non_completion_path_states.concat new_stall_global_perform_state_name

--   -- Build the tree of (curr_state == <state>) |
--   -- Use convert_state_names_to_dsl_or_tree_state_check
--   let not_yet_gotten_mem_resp_state_check : Pipeline.Expr ←
--     convert_state_names_to_dsl_or_tree_state_check states_to_stall_on

--   /-
--   5. Generate the new stall state
--   -/

--   /- Consider if original state has a listen-handle -/
--   -- Get the stmts
--   let handle_blks : Option (List HandleBlock) ←
--   get_ctrler_state_handle_blocks ctrler_that_send_mem_req global_perform_state_that_send_mem_req

--   let new_stall_state : Description :=
--     gen_stall_dsl_state new_stall_global_perform_state_name global_perform_state_that_send_mem_req
--     ctrler_that_await_mem_completion.name not_yet_gotten_mem_resp_state_check first_inst
--     handle_blks
--   dbg_trace s!"New stall state: \n{new_stall_state}"

--   /-
--   6. Update states that point to the previous perform
--   -/
--   let states_that_transition_to_original_global_perform : List String :=
--     get_states_directly_leading_to_given_state (global_perform_state_that_send_mem_req, ctrler_that_send_mem_req.transition_list)
--   dbg_trace s!"======= STATES directly leading to state {global_perform_state_that_send_mem_req}"
--   dbg_trace states_that_transition_to_original_global_perform
--   dbg_trace s!"======= END States that directly lead to state {global_perform_state_that_send_mem_req}"

--   let updated_state_list_transition_to_stall_state : List Description :=
--     update_state_transitions_matching_name_to_replacement_name
--     global_perform_state_that_send_mem_req new_stall_global_perform_state_name
--     states_that_transition_to_original_global_perform ctrler_that_send_mem_req.transition_list

--   let new_state_machine_with_stall_state : List Description :=
--     updated_state_list_transition_to_stall_state ++ [new_stall_state]
  
--   let new_ctrler : controller_info := {
--     name := ctrler_that_send_mem_req.name,
--     controller_descript := ctrler_that_send_mem_req.controller_descript,
--     entry_descript := ctrler_that_send_mem_req.entry_descript,
--     init_trans := ctrler_that_send_mem_req.init_trans,
--     state_vars := ctrler_that_send_mem_req.state_vars,
--     transition_list := new_state_machine_with_stall_state
--   }

--   let updated_ctrler_list : List controller_info :=
--     all_ctrlers.map (
--       λ ctrler =>
--         if ctrler.name == ctrler_that_send_mem_req.name then
--           new_ctrler
--         else
--           ctrler
--     )
  
--   -- dbg_trace s!"The updated ctrler list with the ctrler + stall state: ({updated_ctrler_list})"

--   return updated_ctrler_list