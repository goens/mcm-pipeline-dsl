
import PipelineDsl.AST
import PipelineDsl.Translation

-- import PipelineDsl.AST
import PipelineDsl.AnalysisHelpers

open Pipeline


partial def true_if_stmts_have_mem_access
(stmt : Statement)
:=
  -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
  -- dbg_trace stmt
  -- dbg_trace "==END GET-TRANSITIONS ==\n"
  match stmt with
  | Statement.transition ident => []
  | Statement.stray_expr expr' => 
    match expr' with
    | Expr.some_term term' =>
      match term' with
      | Term.function_call qual_name lst_expr =>
        match qual_name with
        | QualifiedName.mk lst_idents' =>
          if (lst_idents'.contains "memory_interface")
            then [true]
            else []
      | _ => []
    | _ => []
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [true_if_stmts_have_mem_access stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            true_if_stmts_have_mem_access stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map true_if_stmts_have_mem_access)
    | Conditional.if_statement expr1 stmt1 => true_if_stmts_have_mem_access stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map true_if_stmts_have_mem_access)
  | Statement.await none lst_stmt1 => List.join (lst_stmt1.map true_if_stmts_have_mem_access)
  | Statement.when qname list_idens stmt => true_if_stmts_have_mem_access stmt
  -- | Statement.listen_handle  => 
  | _ => []

def match_stmt_with_assgn_entry_type_load
(stmt' : Statement)
:=
  match stmt' with
  | Statement.variable_assignment qual_name expr =>
    let the_var_name :=
      match qual_name with
      | QualifiedName.mk lst_idents' =>
        match lst_idents' with
        | [a_name] => a_name
        | _ => default
    if the_var_name == "entry_types"
      then
        -- check if expr contains "load"
        -- Expr is either a list or just
        -- "load"
        match expr with
        | Expr.some_term term =>
          match term with
          | Term.var term_ident =>
            if term_ident == "load"
              then true
              else false
          | _ => false
        | Expr.list lst_expr =>
          let load_lst :=
          lst_expr.filter (
            λ expr' =>
              match expr' with
              | Expr.some_term term' =>
                match term' with
                | Term.var term_ident' =>
                  if term_ident' == "load"
                    then true
                    else false
                | _ => false
              | _ => false
          )
          let found_load :=
          match load_lst with
          | [one] => true
          | _ => false
          found_load
        | _ => false

      else
        false
  | _ => false

def find_load_begin_perform_info
(ctrler_lst : List controller_info)
:=
  let filtered_by_load_controllers
  :=
    ctrler_lst.filter (
      -- [1] check by entry types?
      -- [2] check for sending a mem req 
      -- in a subsequent 'let statement
      λ ctrler =>
        match ctrler.controller_descript with
        | Description.controller ident stmt =>
          match stmt with
          | Statement.block lst_stmts =>
            -- [1] check the statements
            -- Find the statement assigning to
            -- entry_types
            -- Check if it matches the String
            -- "load"
            let lst_stmt_with_load :=
            lst_stmts.filter (
              match_stmt_with_assgn_entry_type_load
            )
            match lst_stmt_with_load with
            | [a_ld_stmt] => true
            | [] => false
            | h::t =>
              dbg_trace "\nmultiple entry assgns?\n"
              true
          | _ => false
        | _ => false
    )
  -- [2] Check if there's a
  -- memory access in the
  -- transitions of this
  -- controller
  let filtered_by_load_and_mem_req
  :=
    filtered_by_load_controllers.filter (
      λ ctrler =>
        -- is there a mem access?
        let lst_trans_with_mem_access :=
        ctrler.transition_list.filter (
          λ trans_descript =>
            match trans_descript with
            | Description.state ident stmt =>
              -- want to find a stmt that contains
              -- a memory_interface access/request
              let bool_lst :=
              true_if_stmts_have_mem_access stmt

              let found_mem_interface_access :=
              match bool_lst with
              | [] => false
              | [true] => true
              | h::t =>
                if bool_lst.all (λ bool' => bool' == true)
                 then true
                 else false

              found_mem_interface_access
            | _ => false
        )

        let found_trans_with_mem_access :=
        match lst_trans_with_mem_access with
        | [] => false
        | [one_trans] => true
        | h::t => true

        found_trans_with_mem_access
    )

  -- have ctrlers where [1] and [2]
  -- are both true!
  -- have found controllers
  -- that both [1] hold loads
  -- and [2] send mem reqs!
  filtered_by_load_and_mem_req

-- TODO:
-- This would find ctrlrs with that hold loads and send load reqs
-- Maybe just need one that finds ctrlers that send load reqs?
-- Not important right now.
-- Just write a func that takes a ctrler_info obj and
-- (1) find the transition name with the globally perform load req

def get_ctrler_state_with_mem_load_req
( ctrler : controller_info ) -- really is Description.state
: ( String ) -- The name of the transition with the mem load req
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
          | h::t =>
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

  let found_trans_with_mem_access :=
  match lst_trans_with_mem_access with
  | [] => false
  | [one_trans] => true
  | h::t => true

  --found_trans_with_mem_access
  ""

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
: List Description
:=
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
  []