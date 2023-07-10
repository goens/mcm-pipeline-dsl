import PipelineDsl.AnalysisHelpers

import Murphi

import PipelineDsl.AST

open Pipeline AST

structure lst_stmts_decls where
stmts : List Murϕ.Statement
decls : List Murϕ.Decl
deriving Inhabited

def empty_stmt_decl_lsts : lst_stmts_decls := {
  stmts := [],
  decls := []
}

inductive how_many_found
| nothing
| one
| two_or_more


structure dsl_trans_info where
ctrler_name: Identifier
ctrler_lst : List controller_info
trans : Description -- Description.state

inductive tail_or_entry
| tail : tail_or_entry
| entry : tail_or_entry
| custom_entry : tail_or_entry
def tail_or_entry.toString : tail_or_entry → String
| .tail => "tail_or_entry.tail"
| .entry => "tail_or_entry.entry"  
| .custom_entry => "tail_or_entry.custom_entry"  

  
inductive await_or_not_state
| await : await_or_not_state
| not_await : await_or_not_state
def await_or_not_state.toString : await_or_not_state → String
| .await => "This state has a top level 'await' statement"
| .not_await => "This state has no top level 'await' statements"  

  
structure term_translation_info where
term : Pipeline.Term
lst_ctrlers : List controller_info
ctrler_name : Identifier
-- when statement stuff
src_ctrler : Option Identifier
lst_src_args : Option (List Identifier)
func : Option Identifier
is_await : await_or_not_state
entry_keyword_dest : Option Identifier
trans_obj : Description
specific_murphi_dest_expr : Option Murϕ.Expr
lst_decls : List Murϕ.Decl
is_rhs : Bool
-- Do we use specific_murphi_dest_expr in sth
-- like LQ.entries[ <specific_murphi_dest_expr> ].state := state_
-- or just LQ.entries[ i ].state := state_
use_specific_dest_in_transition : Bool
curr_ctrler_designator_idx : Option Murϕ.Expr
lhs_var_is_just_default : Bool
translate_entry_or_ctrler : entry_or_ctrler

structure expr_translation_info where
expr : Pipeline.Expr
lst_ctrlers : List controller_info
ctrler_name : Identifier
-- when statement stuff
src_ctrler : Option Identifier
lst_src_args : Option (List Identifier)
func : Option Identifier
is_await : await_or_not_state
entry_keyword_dest : Option Identifier
trans_obj : Description
specific_murphi_dest_expr : Option Murϕ.Expr
lst_decls : List Murϕ.Decl
is_rhs : Bool
use_specific_dest_in_transition : Bool
curr_ctrler_designator_idx : Option Murϕ.Expr
lhs_var_is_just_default : Bool
translate_entry_or_ctrler : entry_or_ctrler

structure stmt_translation_info where
stmt : Pipeline.Statement
lst_ctrlers : List controller_info
ctrler_name : Identifier
-- when statement stuff
src_ctrler : Option Identifier
lst_src_args : Option (List Identifier)
func : Option Identifier
is_await : await_or_not_state
entry_keyword_dest : Option Identifier
trans_obj : Description
specific_murphi_dest_expr : Option Murϕ.Expr
lst_decls : List Murϕ.Decl
is_rhs : Bool
use_specific_dest_in_transition : Bool
curr_ctrler_designator_idx : Option Murϕ.Expr
lhs_var_is_just_default : Bool
translate_entry_or_ctrler : entry_or_ctrler


structure trans_and_expected_func where
expected_func : Identifier
expected_struct : Identifier
trans : Pipeline.Description
parent_trans_info : stmt_translation_info
stmt_trans_info : stmt_translation_info
dest_ctrler_name : Identifier
-- dest_ctrler_entry : Murϕ.Expr
-- NOTE: This is for translating and 
-- using a specific entry name with a
-- ctrler which has entries
specific_murphi_dest_expr : Murϕ.Expr
-- lst_decls : List Murϕ.Decl
curr_ctrler_designator_idx : Option Murϕ.Expr


partial def assn_stmt_to_stmt_translation_info
(translation_info : stmt_translation_info)
(stmt : Pipeline.Statement)
: stmt_translation_info
:= (
  stmt_translation_info.mk
  stmt
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)


partial def assn_stmt_to_term_translation_info
(translation_info : stmt_translation_info)
(term : Pipeline.Term)
: term_translation_info
:= (
  term_translation_info.mk
  term
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)

partial def assn_stmt_to_expr_translation_info
(translation_info : stmt_translation_info)
(expr : Pipeline.Expr)
: expr_translation_info
:= (
  expr_translation_info.mk
  expr
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)


partial def assn_expr_to_term_translation_info
(translation_info : expr_translation_info)
(term : Pipeline.Term)
:
term_translation_info
:= (
  term_translation_info.mk
  term
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)

partial def assn_term_to_term_translation_info
(translation_info : term_translation_info)
(term : Pipeline.Term)
:
term_translation_info
:= (
  term_translation_info.mk
  term
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)

partial def assn_term_to_expr_translation_info
(translation_info : term_translation_info)
(expr : Pipeline.Expr)
:
expr_translation_info
:= (
  expr_translation_info.mk
  expr 
  translation_info.lst_ctrlers
  translation_info.ctrler_name
  translation_info.src_ctrler
  translation_info.lst_src_args
  translation_info.func
  translation_info.is_await
  translation_info.entry_keyword_dest
  translation_info.trans_obj
  translation_info.specific_murphi_dest_expr
  translation_info.lst_decls
  translation_info.is_rhs
  translation_info.use_specific_dest_in_transition
  translation_info.curr_ctrler_designator_idx
  translation_info.lhs_var_is_just_default
  translation_info.translate_entry_or_ctrler
)

-- NOTE: Also get the resets!
-- But may want to write a different function for this...
partial def get_stmts_with_transitions
(stmt : Statement)
:=
          -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
          -- dbg_trace stmt
          -- dbg_trace "==END GET-TRANSITIONS ==\n"

  match stmt with
  | Statement.transition ident => [ident]
  | Statement.reset ident => [ident]
  | Statement.complete ident => [ident]
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [get_stmts_with_transitions stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            get_stmts_with_transitions stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_stmts_with_transitions)
    | Conditional.if_statement expr1 stmt1 => get_stmts_with_transitions stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_stmts_with_transitions)
  | Statement.await _ lst_stmt1 => List.join (lst_stmt1.map get_stmts_with_transitions)
  | Statement.when qname list_idens stmt => get_stmts_with_transitions stmt
  | Statement.labelled_statement /-label-/ _ stmt => get_stmts_with_transitions stmt
  -- | Statement.listen_handle  => 
  | _ => default



partial def ast0038_trans_ident_to_trans_list
(trans_name : Identifier)
-- basically, list of all transitions, the "graph". this should always be
-- the same.
(list : List Description)
(visited : List Identifier)
:=
  List.foldl
  (
    λ visited next =>
    if (visited.contains next)
      then visited
      else -- find the child nodes
        -- dbg_trace "==&&&&&&&&==\n"
        -- dbg_trace next
        -- dbg_trace "==&&&&&&&&==\n"
        ast0038_trans_ident_to_trans_list next list visited
  )
  -- append the current node (trans_name)
  (visited.cons trans_name)
  (
  List.join
  (
  -- Attempt to get child nodes from this node
  -- If element's identifier matches trans_name
  -- NOTE:
  -- Must also do a kind of "deeper"
  -- search if Description can contain
  -- more statements, like await, or when
  -- NOTE:
  -- Also Conditionals (if statements) as well!
  (
  List.join (
  -- This list is actually just the current node...
  (
  list.filter (
    λ descript => match descript with
    | Description.state iden stmt =>
      iden == trans_name
    | _ => false
  )
  ).map
  -- current node (in a list) now we find the transition stmts inside the
  -- matching transitions, these transition identifiers are
  -- the "child nodes"
  (
    λ transit => match transit with
    | Description.state iden stmt =>
          
        -- dbg_trace "==BEGIN &&&&&&&&==\n"
        -- dbg_trace trans_name
        -- dbg_trace stmt
        -- dbg_trace "==END &&&&&&&&==\n"
      match stmt with
      | Statement.block lst =>
        lst.filter
        (
          λ stmt1 => match stmt1 with
          | Statement.conditional_stmt cond => true
          | Statement.transition iden1 => true
          | Statement.reset iden1 => true
          | Statement.complete iden1 => true
          | Statement.block lst_stmts1 => true
          | Statement.await _ await_lst =>
          -- dbg_trace "==BEGIN await ==\n"
          -- dbg_trace trans_name
          -- dbg_trace await_lst
          -- dbg_trace "==END await ==\n"
          true
          | Statement.when qname ident_list stmt =>
          -- dbg_trace "==BEGIN when ==\n"
          -- dbg_trace trans_name
          -- dbg_trace stmt
          -- dbg_trace "==END when ==\n"
          true
          | Statement.listen_handle stmt1 lst => true
          | Statement.labelled_statement label stmt' => true
          | _ => false
        )
      | Statement.await _ await_lst => await_lst
      | Statement.when qname ident_list stmt => [stmt]
      | Statement.transition iden2 => [stmt]
      | Statement.reset iden2 => [stmt]
      | Statement.complete iden2 => [stmt]
      | Statement.conditional_stmt cond => [stmt]
      | Statement.listen_handle stmt1 lst => [stmt]
      | Statement.labelled_statement label stmt' => [stmt]
      | _ => []
    | _ => []
  )
  )
  ).map
  get_stmts_with_transitions
  )
  )

  
def ast0039_trans_ident_to_list
(trans_names : List Identifier)
(list : List Description)
:=
  -- for each transition identifier,
  -- find it's corresponding transition object!
  List.join
  (
  trans_names.map
  (
    λ iden =>
      list.filter
      (
        λ descript =>
          match descript with
          | Description.state iden1 stmt =>
            if (iden1 == iden)
              then true
              else false
          | _ => false
      )
  )
  )

def ast0040_get_trans
(ast : AST)
:=
  match ast with
  | structure_descriptions lst =>
    lst.filter
    (
      λ descript => match descript with
        | Description.state iden stmt => true
        | _ => false
    )


