
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

-- Then do (2)

-- then (3)
