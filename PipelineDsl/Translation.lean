-- import PipelineDsl
-- import Mathlib.Tactic.Linarith
import PipelineDsl.AnalysisHelpers

import Murphi

import PipelineDsl.AST
-- import PipelineDsl.Transformation

-- start at the top of the AST
-- (1) Collect the state_queues or "controllers"
-- (2) Match a state_queue/controller to an entry description
--     Tells us what variables the entry has
-- (3) Match state_queue/controller to transitions
-- (4) break up transitions at await

-- (5) Convert entries into records
-- (6) Convert controllers into Murphi records
--     consisting of an array of entries
-- (7) Convert transitions into Rules

-- The error message you were getting here is not so helpful.
-- It seems that for some reason it was getting confused with the
-- (embedded) syntax declaration when opening the whole namespace, i.e.
-- it was interpreting `transition` in the DSL syntax, instead of the
-- constructor for Description. This seems to have fixed that.
open Pipeline in
def ast0001_descript (descript : Pipeline.Description) : Identifier :=
  match descript with
  | Description.control_flow a _ => a
  | Description.entry a _ => a
  | Description.state a _ => a
  | Description.controller a _ => a
  | Description.function_definition (TypedIdentifier.mk _ a) _ _ => a


  -- | controller a b => a
  --| entry a b => a
  --| transition a b => a b
  -- | function_definition a b c => a

-- BTW: you can open something for the entire file
-- if you want it in scope there, e.g.
--open Pipeline AST
-- def ast0000 ( input : AST) :=
-- ...
open Pipeline.AST in
def ast0000 ( input : Pipeline.AST) : Identifier :=
  match input with
  | structure_descriptions lst => String.join (lst.map ast0001_descript)
  -- note that this will just glue together all strings with nothing in between,
  -- if you want to have something between them, you can try List.intercalate

-- def ex0 : Pipeline.AST := Pipeline.AST (Pipeline.AST.structure_descriptions)

-- Idea: Build up Murphi "ast"
-- Murphi "ast" would have:
-- (1) list of: types and records
-- (2) list of: functions
-- (3) list of: transitions
-- Also add a field in the murphi objects for
-- structure/controller name, so we can
-- generate comments in murphi to just state which
-- structure is the generated record/type/function/transition
-- for

-- First do some preprocessing.
-- transitions would be .split() at
-- await
-- actually they don't need to be
-- can just translate it on the fly,
-- so long as we know to make another Murphi transition
-- rule
-- Just need to make sure the lean func returns a list
-- of murphi transitions?

-- So to this effect, we need to
-- start from the controller and entry
-- This gives us the structure name and entry values
-- But do we need these? Just the controller defn
-- Since that gives us the chain of transitions that are
-- a part of the structure
-- But the entry values can be used to check if var (ident 's)
-- are a part of a controller entry or not

-- So yes, gather controller and entry info
-- Then use the info to try to traverse the list of transitions
-- to reach all affected transitions
-- (list of transitions & bool of if traversed or not)
-- Then write a set of functions to extract a controller's transitions
-- Then translate the set of {controller, entry, transitions}

-- So (1), let's try to find the controller entries

---------- Attempt to Extract controller info ----------

open Pipeline AST
-- How to write this more idiomatically?

structure lst_stmts_decls where
stmts : List Murϕ.Statement
decls : List Murϕ.Decl
deriving Inhabited

instance : ToString lst_stmts_decls where
  toString (stmts_decls : lst_stmts_decls) :=
  let stmts_str : String := toString stmts_decls.stmts
  let decls_str : String := toString stmts_decls.decls
  let str : String := s!"lst_stmts_decls;"++
  s!"\nstmts: ({stmts_str})"++
  s!"\ndecls: ({decls_str})";
  str

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
instance : ToString tail_or_entry where toString := tail_or_entry.toString

inductive await_or_not_state
| await : await_or_not_state
| not_await : await_or_not_state
def await_or_not_state.toString : await_or_not_state → String
| .await => "This state has a top level 'await' statement"
| .not_await => "This state has no top level 'await' statements"  
instance : ToString await_or_not_state where toString := await_or_not_state.toString


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

instance : ToString stmt_translation_info where
  toString (info : stmt_translation_info) :=
  let stmt_str : String := toString info.stmt
  let ctrler_name_str : String := toString info.ctrler_name
  let src_ctrler_str : String := toString info.src_ctrler
  let lst_src_args_str : String := toString info.lst_src_args
  let func_str : String := toString info.func
  let is_await_str : String := toString info.is_await
  let entry_keyword_str : String := toString info.entry_keyword_dest
  let trans_obj_str : String := toString info.trans_obj
  let specific_murphi_dest_str : String := toString info.specific_murphi_dest_expr
  let lst_decls_str : String := toString info.lst_decls
  let is_rhs_str : String := toString info.is_rhs
  let use_specific_dest_expr_in_transition_str : String := toString info.use_specific_dest_in_transition
  let curr_ctrler_designator_str : String := toString info.curr_ctrler_designator_idx
  let lhs_var_is_just_default : String := toString info.lhs_var_is_just_default
  let entry_or_ctrler : String := toString info.translate_entry_or_ctrler
  let str : String := s!"Stmt_translation_info:"++
  s!"\nstmt_str: ({stmt_str})"++
  s!"\nctrler_name_str: ({ctrler_name_str})"++
  s!"\nsrc_ctrler_str: ({src_ctrler_str})"++
  s!"\nlst_src_args_str: ({lst_src_args_str})"++
  s!"\nfunc_str: ({func_str})"++
  s!"\nis_await_str_str: ({is_await_str})"++
  s!"\nentry_keyword_str: ({entry_keyword_str})"++
  s!"\ntrans_obj_str: ({trans_obj_str})"++
  s!"\nspecific_murphi_dest_str: ({specific_murphi_dest_str})"++
  s!"\nlst_decls_str: ({lst_decls_str})"++
  s!"\nis_rhs_str: ({is_rhs_str})"++
  s!"\nuse_specific_dest_expr_in_transition_str: ({use_specific_dest_expr_in_transition_str})"++
  s!"\ncurr_ctrler_designator_str: ({curr_ctrler_designator_str})"++
  s!"\nlhs_var_is_just_default: ({lhs_var_is_just_default})"++
  s!"\nentry_or_ctrler: ({entry_or_ctrler})"
  ;
  str

instance : ToString expr_translation_info where
  toString (info : expr_translation_info) :=
  let expr_str : String := toString info.expr
  let ctrler_name_str : String := toString info.ctrler_name
  let src_ctrler_str : String := toString info.src_ctrler
  let lst_src_args_str : String := toString info.lst_src_args
  let func_str : String := toString info.func
  let is_await_str : String := toString info.is_await
  let entry_keyword_str : String := toString info.entry_keyword_dest
  let trans_obj_str : String := toString info.trans_obj
  let specific_murphi_dest_str : String := toString info.specific_murphi_dest_expr
  let lst_decls_str : String := toString info.lst_decls
  let is_rhs_str : String := toString info.is_rhs
  let use_specific_dest_expr_in_transition_str : String := toString info.use_specific_dest_in_transition
  let curr_ctrler_designator_str : String := toString info.curr_ctrler_designator_idx
  let lhs_var_is_just_default : String := toString info.lhs_var_is_just_default
  let entry_or_ctrler : String := toString info.translate_entry_or_ctrler
  let str : String := s!"Stmt_translation_info:"++
  s!"\nexpr_str: ({expr_str})"++
  s!"\nctrler_name_str: ({ctrler_name_str})"++
  s!"\nsrc_ctrler_str: ({src_ctrler_str})"++
  s!"\nlst_src_args_str: ({lst_src_args_str})"++
  s!"\nfunc_str: ({func_str})"++
  s!"\nis_await_str_str: ({is_await_str})"++
  s!"\nentry_keyword_str: ({entry_keyword_str})"++
  s!"\ntrans_obj_str: ({trans_obj_str})"++
  s!"\nspecific_murphi_dest_str: ({specific_murphi_dest_str})"++
  s!"\nlst_decls_str: ({lst_decls_str})"++
  s!"\nis_rhs_str: ({is_rhs_str})"++
  s!"\nuse_specific_dest_expr_in_transition_str: ({use_specific_dest_expr_in_transition_str})"++
  s!"\ncurr_ctrler_designator_str: ({curr_ctrler_designator_str})"++
  s!"\nlhs_var_is_just_default: ({lhs_var_is_just_default})"++
  s!"\nentry_or_ctrler: ({entry_or_ctrler})";
  str

instance : ToString term_translation_info where
  toString (info : term_translation_info) :=
  let term_str : String := toString info.term
  let ctrler_name_str : String := toString info.ctrler_name
  let src_ctrler_str : String := toString info.src_ctrler
  let lst_src_args_str : String := toString info.lst_src_args
  let func_str : String := toString info.func
  let is_await_str : String := toString info.is_await
  let entry_keyword_str : String := toString info.entry_keyword_dest
  let trans_obj_str : String := toString info.trans_obj
  let specific_murphi_dest_str : String := toString info.specific_murphi_dest_expr
  let lst_decls_str : String := toString info.lst_decls
  let is_rhs_str : String := toString info.is_rhs
  let use_specific_dest_expr_in_transition_str : String := toString info.use_specific_dest_in_transition
  let curr_ctrler_designator_str : String := toString info.curr_ctrler_designator_idx
  let lhs_var_is_just_default : String := toString info.lhs_var_is_just_default
  let entry_or_ctrler : String := toString info.translate_entry_or_ctrler
  let str : String := s!"Stmt_translation_info:"++
  s!"\nterm_str: ({term_str})"++
  s!"\nctrler_name_str: ({ctrler_name_str})"++
  s!"\nsrc_ctrler_str: ({src_ctrler_str})"++
  s!"\nlst_src_args_str: ({lst_src_args_str})"++
  s!"\nfunc_str: ({func_str})"++
  s!"\nis_await_str_str: ({is_await_str})"++
  s!"\nentry_keyword_str: ({entry_keyword_str})"++
  s!"\ntrans_obj_str: ({trans_obj_str})"++
  s!"\nspecific_murphi_dest_str: ({specific_murphi_dest_str})"++
  s!"\nlst_decls_str: ({lst_decls_str})"++
  s!"\nis_rhs_str: ({is_rhs_str})"++
  s!"\nuse_specific_dest_expr_in_transition_str: ({use_specific_dest_expr_in_transition_str})"++
  s!"\ncurr_ctrler_designator_str: ({curr_ctrler_designator_str})"++
  s!"\nlhs_var_is_just_default: ({lhs_var_is_just_default})"++
  s!"\nentry_or_ctrler: ({entry_or_ctrler})";
  str

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
--- =========== CUT FROM TRANSFORMATION ================

--- =========== CUT FROM TRANSFORMATION ================


-- === Funcs to get last assn stmt identifier from controllers list ===

-- NOTE: Shouldn't get an empty list?
-- But going to write this for now...
def ast0009_last_identifier (idens : List Identifier) [Inhabited Identifier] :=
  match idens with 
  | [one] => --dbg_trace idens
  one
  | h::t => ast0009_last_identifier t
  | [] => default

def ast0008_qname (qname : QualifiedName) :=
  match qname with
  | QualifiedName.mk lst => -- dbg_trace "TEST5"
  ast0009_last_identifier lst

def ast0018_term_var (term : Term) :=
  match term with
  | Term.var iden => iden 
  | _ => default

def ast0017_expr_to_ident (expr : Expr) :=
  match expr with
  | Expr.some_term term => ast0018_term_var term
  | _ => default
  

-- Now this returns a "list" of identifiers
-- for a given statement
def ast0007_find_init (stmt : Statement) :=
  match stmt with
  | Statement.variable_assignment qname expr => -- dbg_trace "TEST4"
  if (ast0008_qname qname) == "init_entry"
    then [ast0017_expr_to_ident expr]
    else []
  | _ => default


def ast0006_match_block (stmts : Statement) :=
  match stmts with
  | Statement.block blk => -- dbg_trace "TEST3"
    List.join (blk.map ast0007_find_init)
  | _ => []


def ast0005 (ctrl : Description) :=
  match ctrl with
  | Description.controller identifier stmt => -- dbg_trace "TEST2"
    ast0006_match_block stmt
  | _ => []
  -- find stmt with init transition
  -- ctrl.statement

-- from list of controllers, process each one
def ast0004 (controller_list : List Description) :=
  -- match controller_list with
  -- -- with the controller entries, get the init transition
  -- -- then all other transitions
  -- | lst => lst
  -- | [] => []
 --dbg_trace "TEST1"
  List.join (controller_list.map ast0005)

-- ======= Funcs used to get controller descripts =========

def ast0003_get_controllers (descript : Description) : List Description :=
  match descript with
  | Description.controller identifier stmts => [descript]
  | _ => []

def ast0002_get_controllers (ast : AST) : List Description :=
  match ast with
  | structure_descriptions lst => --dbg_trace "TEST"
  List.join (lst.map ast0003_get_controllers)

-- ======= Funcs used to get controller entries descripts =========

def ast0011_get_entries (descript : Description) : List Description :=
  match descript with
  | Description.entry iden stmt => [descript]
  | _ => []

structure murphi_consts where
  num_elems : Nat
structure murphi_records where
  state_vars : List TypedIdentifier
-- Transitions... is there a Murphi format
-- that's convenient to use?
-- AZ CHECKPOINT:
-- First thing to do is to do a "basic block"
  -- or "basic transition break down"
  -- This is since the await + when combinations
  -- are allowed to be interleaved at the moment

def ast0021_empty_controller : controller_info
:= {
  name := default,
  controller_descript := default,
  entry_descript := default,
  init_trans := default,
  state_vars := default,
  transition_list := default,
  ctrler_init_trans := default,
  ctrler_state_vars := default
  ctrler_trans_list := default
  }

def ast0022_set_controller_name ( name : Identifier ) (ctrl : controller_info) : controller_info
:= {
  name := name,
  controller_descript := ctrl.controller_descript,
  entry_descript := ctrl.entry_descript,
  init_trans := ctrl.init_trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

def ast0024_set_entry_descript (ctrl : controller_info) ( descript : Description ) : controller_info
:= {
  name := ctrl.name,
  controller_descript := ctrl.controller_descript,
  entry_descript := descript,
  init_trans := ctrl.init_trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

def ast0025_set_entry_descript ( ctrl_and_entry : controller_info × Description ) :=
  ast0024_set_entry_descript ctrl_and_entry.1 ctrl_and_entry.2 

def ast0026_set_controller_init (ctrl : controller_info) ( trans : Identifier ) : controller_info := {
  name := ctrl.name,
  controller_descript := ctrl.controller_descript,
  entry_descript := ctrl.entry_descript,
  init_trans := trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

def ast0027_set_controller_init ( ctrl_and_entry : controller_info × Identifier ) :=
  ast0026_set_controller_init ctrl_and_entry.1 ctrl_and_entry.2 

def ast0023_entry_to_name (entry : Description) :=
  match entry with
  | Description.entry iden stmt => [iden]
  | _ => []

-- Description is really entries (return type)
def ast0010_get_entries (ast : AST) : List Description :=
  match ast with
  | structure_descriptions lst =>
  --dbg_trace "gettin' entries y'all!"
  List.join (lst.map ast0011_get_entries)

-- Get Description of Controller

def ast0028_get_controllers (descript : Description) : List Description :=
  match descript with
  | Description.controller iden stmt => [descript]
  | _ => []

def ast0029_get_controllers (ast : AST) : List Description :=
  match ast with
  | structure_descriptions lst =>
  --dbg_trace "gettin' entries y'all!"
  List.join (lst.map ast0028_get_controllers)

def set_ctrler_name
(ctrl : controller_info)
(name : String)
: controller_info := {
  name := name
  controller_descript := ctrl.controller_descript,
  entry_descript := ctrl.entry_descript,
  init_trans := ctrl.init_trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }
  

def ast0030_set_controller_descript (ctrl : controller_info) ( descript : Description ) : controller_info
:= {
  name := ctrl.name,
  controller_descript := descript,
  entry_descript := ctrl.entry_descript,
  init_trans := ctrl.init_trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

def ast0031_set_controller_descript ( ctrl_and_entry : controller_info × Description ) :=
  ast0030_set_controller_descript ctrl_and_entry.1 ctrl_and_entry.2 

---------- Extract entry info ----------
-- find stmt with init transition
def ast0014_map_statements (ctrl : Description) :=
  match ctrl with
  | Description.entry identifier stmt =>
    --dbg_trace "searching thru stmts"
    ast0006_match_block stmt
  | _ => []

def ast0016_get_ident ( input: Description ) :=
  -- Get the get ident
  match input with
  | Description.entry iden stmt => [iden]
  | _ => []

--def ast0015_pair_together (entry_list)
def ast0015_entry_name (entry_list : List Description) :=
  --dbg_trace "get entry structure names"
    List.join (entry_list.map ast0016_get_ident)

-- from list of entries, process each one
def ast0013_map_entries (entry_list : List Description) :=
  --dbg_trace "Workin' through dem' entries"
  List.join (entry_list.map ast0014_map_statements)
  -- don't join list, for each one we try to find transitions
  -- connected to each list
  -- (entry_list.map ast0014_map_statements)

-- ===== Get info about 

-- def ast0020_combine_controller_lists ( entries : list description) (init_t : identifier) :=
--   -- join the two lists elem by elem
--   ...
--   -- then run a function on them to join each tuple into the struct
def ast0020_controllers_from_ident_list (iden : Identifier) :=
  ast0022_set_controller_name iden ast0021_empty_controller

def ast0034_get_var_decls ( decl : Statement ) :=
  match decl with
  | Statement.variable_declaration type_iden => [type_iden]
  | _ => []

def ast0033_get_block ( blk : Statement ) :=
  match blk with
  | Statement.block lst => List.join (lst.map ast0034_get_var_decls)
  | _ => []

def ast0032_get_entry_vars ( entry : Description ) :=
  match entry with
  | Description.entry iden stmt => ast0033_get_block stmt
  | _ => default

def get_ctrler_descript_vars ( entry : Description ) :=
  dbg_trace s!"The controller to get ctrler state vars for: ({entry})"
  match entry with
  | Description.controller iden stmt => ast0033_get_block stmt
  | _ => default

def ast0035_ctrl_obj_set_vars (ctrl : controller_info) : controller_info := {
  name := ctrl.name,
  controller_descript := ctrl.controller_descript,
  entry_descript := ctrl.entry_descript,
  init_trans := ctrl.init_trans,
  state_vars := ast0032_get_entry_vars ctrl.entry_descript.get!,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := ctrl.ctrler_state_vars
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

def ctrl_set_ctrl_state_vars (ctrl : controller_info) : controller_info := {
  name := ctrl.name,
  controller_descript := ctrl.controller_descript,
  entry_descript := ctrl.entry_descript,
  init_trans := ctrl.init_trans,
  state_vars := ctrl.state_vars,
  transition_list := ctrl.transition_list
  ctrler_init_trans := ctrl.ctrler_init_trans,
  ctrler_state_vars := get_ctrler_descript_vars ctrl.controller_descript,
  ctrler_trans_list := ctrl.ctrler_trans_list
  }

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

def ast0036_ctrl_obj_find_trans
-- (ctrl : controller_info)
-- (all_transitions : List Description)
(ctrl_and_all_trans : controller_info × List Description)
: controller_info :=
  {
    name := ctrl_and_all_trans.1.name,
    controller_descript := ctrl_and_all_trans.1.controller_descript,
    entry_descript := ctrl_and_all_trans.1.entry_descript,
    init_trans := ctrl_and_all_trans.1.init_trans,
    state_vars := ctrl_and_all_trans.1.state_vars,
    transition_list := ast0039_trans_ident_to_list (ast0038_trans_ident_to_trans_list ctrl_and_all_trans.1.init_trans.get! ctrl_and_all_trans.2 []) ctrl_and_all_trans.2
    ctrler_init_trans := ctrl_and_all_trans.1.ctrler_init_trans,
    ctrler_state_vars := ctrl_and_all_trans.1.ctrler_state_vars
    ctrler_trans_list := ctrl_and_all_trans.1.ctrler_trans_list
  }

def ctrl_find_entry_states
(ctrl : controller_info)
(all_states : List Description)
: controller_info :=
  {
    name := ctrl.name,
    controller_descript := ctrl.controller_descript,
    entry_descript := ctrl.entry_descript,
    init_trans := ctrl.init_trans,
    state_vars := ctrl.state_vars,
    transition_list := ast0039_trans_ident_to_list (ast0038_trans_ident_to_trans_list ctrl.init_trans.get! all_states []) all_states
      -- let transitioned_to_state_names := (ast0038_trans_ident_to_trans_list ctrl.init_trans.get! all_states [])
      -- dbg_trace s!"$$transitioned_to_state_names: {transitioned_to_state_names}"
      -- ast0039_trans_ident_to_list transitioned_to_state_names all_states,
    ctrler_init_trans := ctrl.ctrler_init_trans,
    ctrler_state_vars := ctrl.ctrler_state_vars
    ctrler_trans_list := ctrl.ctrler_trans_list
  }
def ctrl_find_ctrl_states
(ctrl : controller_info)
(all_states : List Description)
: controller_info :=
  {
    name := ctrl.name,
    controller_descript := ctrl.controller_descript,
    entry_descript := ctrl.entry_descript,
    init_trans := ctrl.init_trans,
    state_vars := ctrl.state_vars,
    transition_list := ctrl.transition_list
    ctrler_init_trans := ctrl.ctrler_init_trans,
    ctrler_state_vars := ctrl.ctrler_state_vars
    ctrler_trans_list := ast0039_trans_ident_to_list (ast0038_trans_ident_to_trans_list ctrl.ctrler_init_trans.get! all_states []) all_states
  }

def ast0041_list_ctrl_find_trans
(ctrls : List controller_info)
(all_transitions : List Description)
:=
  (
  ctrls.zip
  (List.replicate ctrls.length all_transitions)
  ).map
  ast0036_ctrl_obj_find_trans

-- -- AZ TODO: Generate any constants for Murphi
-- def ast0042_gen_constants
-- (ctrl : controller_info)
-- :=
--   none


-- def ast0045_recursive_check_await
-- (stmt : Statement)
-- :=
--   match stmt with
--   | Statement.when qname lst_iden stmt' => ast0045_recursive_check_await stmt'
--   | Statement.listen_handle stmt' lst => ast0045_recursive_check_await stmt'
--   | Statement.conditional_stmt cond => true
--   | Statement.block lst_stmt =>
--     -- either 1. this is simple, there's an await in a block and we split there
--     -- 2. There's an await nested in sth, like a conditional
--     -- meaning the block must be split at this conditional
--     -- but this must be split until a common point in the CFG
--     -- so original might be: trans{code -> if with some branches -> await -> code}
--     -- split: trans{code -> if with some branches} trans{await} trans{code}
--     -- I believe i have this case somewhere, so i should fill in this case
--     -- at some point...
--     -- (*) and this is a separate case from the simple await case below...
--     -- Or maybe there is a nice way to handle this recursively???
--     -- if we could go up and down the tree
--   | Statement.await lst_stmt =>
--     -- split this block....
--     -- i.e. take the contents of this await block, and
--     -- move into a new transition, gen a name
--     -- return the original and the new one

-- def ast0044_check_await_and_split_transition_bb
-- (stmt : Statement)
-- :=
--   if 
-- -- Map Description to list of description
-- -- This will be likely in a list of list of descript
-- remember to join / flatten

-- def ast0043_transition_to_bb
-- (descript : Description)
-- :=
--   -- try to split transitions await
--   -- Cases of await stmt to handle:
--   -- (1) There is code then an await (i.e. await isn't first thing):
--   -- (a) split transition at this pt, results in 2 transitions.
--   -- The first transition can have the old name
--   -- The second one can have some string appended to it's name
--   -- (b) add second transition to list of transitions to check
--   -- (2) There's an await+when inside an await+when
--   -- This can be seen as a chain of await transitions
--   -- (3) There's an await with multiple whens
--   -- 
--   -- General strategy
--   -- Check if transition has stmts then await block(s), split block
--   -- return the first half and the second half with the await
--   -- else if await block is the first stmt
--   -- then we "process" the await block
--   -- which is:
--   -- (a) breaking down nested await into an await+when chain
--   -- (b) if there are multiple whens, this doesn't need to be handled
--   -- here (no splitting or whatever, it's already a fork in the BB)
--   -- else there are no awaits, just return the transition
--   match descript with
--   | Description.state iden stmt =>
--     match stmt with
--     | Statement.block lst_stmts =>
--       (lst_stmts.take 1).map
--       (
--         λ lst_stmts' =>
--           if
--             (
--               -- wait this doesn't check for the first one!
--               match lst_stmts' with
--               | Statement.await await_lst_stmt => true
--               | _ => false
--             )
--             then
--               -- handle the await blk (check for nested await+when)
--               -- Things to do: 
--             else
--               -- no first await, means we check if there're any awaits
--               -- if there are, we split at that await in the stmt list
--               -- (*) This means we either foldl or apply this iteratively to
--               -- a fixed point

--               -- when splitting:
--               -- (1) create the first half transition
--               -- (2) create the 2nd half transition, and recursively
--               -- call this function ast0043 on it
--       )

-- do a BB traversal
-- def ast0047_traverse_stmt_ast
-- ()
-- ()

-- -- Info/State for the ast0046_transition_to_bb func
-- structure splitting_info where
--   -- (Actually should be Description.state)
--   lst_transitions : List Description
--   -- lst_stmts : List Statement
--   nested_stmts : List Statement
--   top_transition_ident : Identifier

-- instance : ToString splitting_info := ⟨
--   λ i =>
--     "\n=== Splitting Info ===\n" ++
--     "TRANSITIONS CREATED: " ++ toString i.lst_transitions ++ "\n" ++
--     -- "STATEMENTS CHECKED: " ++ toString i.lst_stmts ++ "\n" ++
--     "\n=== End Splitting Info ===\n"
--   ⟩

-- def create_splitting_info_with_lst_stmts
-- (lst_stmts : List Statement)
-- (split_info : splitting_info)
-- : (splitting_info)
-- :=
--   {lst_stmts := lst_stmts, lst_transitions := split_info.lst_transitions, nested_stmts := split_info.nested_stmts, top_transition_ident := split_info.top_transition_ident}

-- def add_stmt_to_checked_list
-- ( stmt : Statement )
-- ( splitting_inf : splitting_info )
-- : (splitting_info)
-- :=
--   {lst_stmts := splitting_inf.lst_stmts.cons stmt, lst_transitions := splitting_inf.lst_transitions, nested_stmts := splitting_inf.nested_stmts, top_transition_ident := splitting_inf.top_transition_ident}

def create_transition_from_lst_stmts
-- stmts in the transition...
( lst_stmts : List Statement )
-- the name of the transition...
( identifier : Identifier )
-- return the Description.state
: (Description)
:=
  -- Create a Description.state
  Description.state
  identifier
  (Statement.block lst_stmts)

def ast0019_controller_info (ast : AST)
: Except String (List controller_info)
:= do
  dbg_trace "Start ctrler info extraction from parsed AST"
  -- Get all AST descriptions
  let ast_descriptions : List Description := match ast with | structure_descriptions lst => lst;
  -- -- Get description Identifiers / Names of controllers
  -- let description_idents : List Identifier := List.join (ast_descriptions.map ast0023_entry_to_name)
  -- -- make ctler_info objs with the name of controllers/structures
  -- let ctrlers_with_name : List controller_info := description_idents.map ast0020_controllers_from_ident_list
  -- -- Then add the entry AST objs to the controller
  -- -- First zip the list of controllers & entry AST objs
  -- let ctrlers_and_entries : List (controller_info × Description) := ctrlers_with_name.zip (ast0010_get_entries ast)
  -- -- ctrler_info with entry Descript added
  -- let ctrlers_with_entries : List controller_info := ctrlers_and_entries.map ast0025_set_entry_descript
  -- let zip_ctrler_and_init_state : List (controller_info × Identifier) := ctrlers_with_entries.zip (ast0013_map_entries (ast0010_get_entries ast))
  
  -- -- add the init_transition name to the controller
  -- let ctrler_with_init_state : List controller_info := zip_ctrler_and_init_state.map ast0027_set_controller_init
  -- -- Zip with the controller info
  -- let zip_ctrler_with_ctrler_descript : List (controller_info × Description) := ctrler_with_init_state.zip (ast0029_get_controllers ast)
  -- -- add it to the controller description
  -- let ctrler_with_ctrler_descript : List controller_info := zip_ctrler_with_ctrler_descript.map ast0031_set_controller_descript
  -- let ctrler_with_state_vars : List controller_info := ctrler_with_ctrler_descript.map ast0035_ctrl_obj_set_vars
  -- let all_states : List Description := (ast0040_get_trans ast)
  -- let ctrler_with_states : List controller_info := ast0041_list_ctrl_find_trans ctrler_with_state_vars all_states
  -- ======== Old code

  -- ======== New code
  -- let ctrlers_with_name_and_descript : List controller_info ← 
  dbg_trace "Get ctrler info"
  let ctrlers : List (List (controller_info)) ← 
    (
      dbg_trace "got each description, match to ctrler!"
    ast_descriptions.mapM (λ descript : Description =>
      match descript with
      | .controller ident stmt =>
        let ctrler : controller_info := ast0021_empty_controller -- default
        let ctrler_with_name : controller_info := set_ctrler_name ctrler ident
        let ctrler_with_descript : controller_info := ast0030_set_controller_descript ctrler_with_name descript

        dbg_trace s!"ctrler_with_descript: ({ctrler_with_descript})"
        -- let ctrler_with_ctrler_state_machine : controller_info := 
        let option_init_state : Except String (List (Option Identifier)) := 
          match stmt with
          | .block lst_stmts => 
            let init_state_name_list : Except String (List (List String)) := (
            lst_stmts.mapM (λ stmt : Statement =>
              match stmt with 
              | .variable_assignment qual_ident expr =>
                let if_assign_init_entry : Bool :=
                  match qual_ident with
                  | .mk lst_ident => lst_ident == ["init_entry"]
                if if_assign_init_entry then
                  let init_entry_name : Except String String :=
                    match expr with
                    | .some_term term =>
                      match term with
                      | .var ident => pure ident
                      | _ => throw s!"Error, init_entry found isn't a simple name string"
                    | _ => throw s!"Error, init_entry found isn't a simple name string expr"
                  match init_entry_name with
                  | .ok init_entry => pure [init_entry]
                  | .error msg => throw s!"Error: {msg}"
                else
                  pure []
              | _ => pure []
              )
            )
            let init_state_list : (List String) :=
              match init_state_name_list with
              | .ok state_name_list =>
                let list_init_state_names : List String := List.join state_name_list;
                list_init_state_names
                -- pure []
              | .error msg =>
                -- throw s!"Error: {msg}"
                dbg_trace s!"Error: {msg}"
                default
            
            match init_state_list with
            | [] => pure []
            | [one_name] => pure [Option.some one_name]
            | _ :: _ => throw s!"Error, multiple init entry names found"
          | _ => pure []
        let list_option_init : List ( Option Identifier ) :=
          match option_init_state with
          | .ok lst_opt_ident => lst_opt_ident
          | .error msg => 
            dbg_trace s!"{msg}"
            default
        let option_init : Option Identifier :=
          match list_option_init with
          | [] => Option.none
          | [one] => one 
          | _ => Option.none
        dbg_trace s!"option ctrler init: ({option_init})"
        let ctrler_with_ctrler_option_init : controller_info := {
          name := ctrler_with_descript.name
          controller_descript := ctrler_with_descript.controller_descript
          entry_descript := ctrler_with_descript.entry_descript
          init_trans := ctrler_with_descript.init_trans
          state_vars := ctrler_with_descript.state_vars
          transition_list := ctrler_with_descript.transition_list
          ctrler_init_trans := option_init
          ctrler_trans_list := ctrler_with_descript.ctrler_trans_list
          ctrler_state_vars := ctrler_with_descript.ctrler_state_vars
        }
        let parsed_states : List Description := ast0040_get_trans ast
        let ctrler_with_option_ctrler_trans_list : controller_info :=
          if option_init.isSome then
            -- is some, so find states & state vars.
            let ctrler_with_ctrler_state_vars : controller_info :=
              ctrl_set_ctrl_state_vars ctrler_with_ctrler_option_init
            let ctrler_with_ctrler_states     : controller_info :=
              ctrl_find_ctrl_states ctrler_with_ctrler_state_vars parsed_states
            ctrler_with_ctrler_states
          else
            ctrler_with_ctrler_option_init
        
        -- Check if there's an "Entry" Descript
        let descripts : List Description :=
        List.join (
        ast_descriptions.map
        λ descript : Description =>
        match descript with
        | .entry ident _ =>
          if ident == ctrler_with_name.name then
            [descript]
          else
            []
        | _ => []
        )

        let ctrler_with_option_entry : controller_info := 
          match descripts with
          | [] => ctrler_with_option_ctrler_trans_list
          | [one] => ({
            name := ctrler_with_option_ctrler_trans_list.name
            controller_descript := ctrler_with_option_ctrler_trans_list.controller_descript
            entry_descript := Option.some one
            init_trans := ctrler_with_option_ctrler_trans_list.init_trans
            state_vars := ctrler_with_option_ctrler_trans_list.state_vars
            transition_list := ctrler_with_option_ctrler_trans_list.transition_list
            ctrler_init_trans := ctrler_with_option_ctrler_trans_list.ctrler_init_trans
            ctrler_trans_list := ctrler_with_option_ctrler_trans_list.ctrler_trans_list
            ctrler_state_vars := ctrler_with_option_ctrler_trans_list.ctrler_state_vars
          } : controller_info)
          | _ => dbg_trace s!"Error in extracting parsed ctrlers, found multiple ctrler entry descriptions: {descripts}"
            default
        -- === Set the init state if there is one
        let option_entry_init_state : Except String (List (Option Identifier)) := 
          if ctrler_with_option_entry.entry_descript.isSome then
            match ctrler_with_option_entry.entry_descript.get! with
            | .entry ident stmt =>
              match stmt with
              | .block lst_stmts => 
                let init_state_name_list : Except String (List (List String)) := (
                lst_stmts.mapM (λ stmt : Statement =>
                  match stmt with 
                  | .variable_assignment qual_ident expr =>
                    let if_assign_init_entry : Bool :=
                      match qual_ident with
                      | .mk lst_ident => lst_ident == ["init_entry"]
                    if if_assign_init_entry then
                      let init_entry_name : Except String String :=
                        match expr with
                        | .some_term term =>
                          match term with
                          | .var ident => pure ident
                          | _ => throw s!"Error, init_entry found isn't a simple name string"
                        | _ => throw s!"Error, init_entry found isn't a simple name string expr"
                      match init_entry_name with
                      | .ok init_entry => pure [init_entry]
                      | .error msg => throw s!"Error: {msg}"
                    else
                      pure []
                  | _ => pure []
                  )
                )
                let init_state_list : (List String) :=
                  match init_state_name_list with
                  | .ok state_name_list =>
                    let list_init_state_names : List String := List.join state_name_list;
                    list_init_state_names
                    -- pure []
                  | .error msg =>
                    -- throw s!"Error: {msg}"
                    dbg_trace s!"Error: {msg}"
                    default
                
                match init_state_list with
                | [] => pure []
                | [one_name] => pure [Option.some one_name]
                | _ :: _ => throw s!"Error, multiple init entry names found"
              | _ => pure []
            | _ => throw s!"Error, saved a Descripiton that isn't an entry? : ({ctrler_with_option_entry.entry_descript})"
          else
            pure []
        let list_option_entry_init : List ( Option Identifier ) :=
          match option_entry_init_state with
          | .ok lst_opt_ident => lst_opt_ident
          | .error msg => 
            dbg_trace s!"{msg}"
            default
        let option_entry_init : Option Identifier :=
          match list_option_entry_init with
          | [] => Option.none
          | [one] => one 
          | _ => Option.none
        dbg_trace s!"option entry init: ({option_entry_init})"
        let ctrler_with_option_entry_init : controller_info := {
          name := ctrler_with_option_entry.name
          controller_descript := ctrler_with_option_entry.controller_descript
          entry_descript := ctrler_with_option_entry.entry_descript
          init_trans := option_entry_init
          state_vars := ctrler_with_option_entry.state_vars
          transition_list := ctrler_with_option_entry.transition_list
          ctrler_init_trans := ctrler_with_option_entry.ctrler_init_trans
          ctrler_trans_list := ctrler_with_option_entry.ctrler_trans_list
          ctrler_state_vars := ctrler_with_option_entry.ctrler_state_vars
        }
        dbg_trace s!"ctrler_with_option_entry_init: ({ctrler_with_option_entry_init})"
        
        -- ==== Get transitions, states ,etc...
        let ctrler_with_option_entry_states : controller_info :=
          if ctrler_with_option_entry_init.init_trans.isSome then
            -- use old Functions to get etnry state vars and states
            let ctrler_with_entry_states : controller_info :=
              ctrl_find_entry_states ctrler_with_option_entry_init parsed_states
            dbg_trace s!"ctrler_with_entry_states: ({ctrler_with_entry_states})"
            let ctrler_with_entry_state_vars : controller_info :=
              ast0035_ctrl_obj_set_vars ctrler_with_entry_states
            ctrler_with_entry_state_vars
          else
            ctrler_with_option_entry_init
        dbg_trace s!"ctrler_with_option_entry_states: ({ctrler_with_option_entry_states})"
        pure [ctrler_with_option_entry_states]
      | _ => pure []
    ))
  
  -- try to see if the controller has a state machine / ctrler init trans
  -- try to see if the controller has entries / entry init trans

  return List.join ctrlers
  
  -- Start the constants
  -- Then Records
  -- > also generate the variables to do the searchs in the controllers
  -- > like search younger than, etc.
  -- > Should be something that's expected?

--- ====== DSL AST objects to Murphi AST objs ======
-- Now that the fns for getting AST controller objs have
-- been found, we just need to convert our DSL
-- AST objs to Murphi AST objs

/-
General idea is:
This is a recursive process
We will likely call a translate function on
some "top level" AST objs (like Description)

In doing so, this function recurisvely calls
other functions in the AST
-/
open Murϕ


--======== TODO: Create State Enums for each structure ===========

--=========== Helper Funcs for DSL to Murphi Translation =============
def get_transition_name
(trans : Description) -- Description.state
:=
  match trans with
  | Description.state ident stmt =>
    ident
  | _ => dbg_trace "Didn't pass in a transition?"
    default

def get_transition_stmt
(trans : Description) -- Description.state
:=
  match trans with
  | Description.state ident stmt =>
    stmt
  | _ => dbg_trace "Didn't pass in a transition?"
    default

-- Go copy in relevant code from the DFS
def get_dest_transition_names
(trans : Description) -- Description.state
:=
  let trans_stmt := get_transition_stmt trans
  let lst_of_trans_dest_idents :=
    get_stmts_with_transitions trans_stmt
  let joined_names :=
    String.intercalate " || " lst_of_trans_dest_idents
  joined_names

partial def check_if_transition_stmt_blk_has_an_await
(stmt : Pipeline.Statement)
: List Bool
:=
  match stmt with
  -- | Statement.transition ident => [ident]
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [check_if_transition_stmt_blk_has_an_await stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            check_if_transition_stmt_blk_has_an_await stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map check_if_transition_stmt_blk_has_an_await)
    | Conditional.if_statement expr1 stmt1 => check_if_transition_stmt_blk_has_an_await stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map check_if_transition_stmt_blk_has_an_await)
  | Statement.await none lst_stmt1 =>
  -- List.join (lst_stmt1.map get_stmts_with_transitions)
  -- Treat the none case as an "await state" for now
  -- since await with term executes the code atomically right now..
    [true]
  | Statement.await term lst_stmt1 =>
  -- List.join (lst_stmt1.map get_stmts_with_transitions)
    []
  | Statement.when qname list_idens stmt => check_if_transition_stmt_blk_has_an_await stmt
  -- | Statement.listen_handle  => 
  | _ => []

-- probably can reuse some code to do this check
def does_transition_have_await
(trans : Description) -- Description.state
:=
  let trans_stmt_blk := get_transition_stmt trans
  let list_bool :=
  check_if_transition_stmt_blk_has_an_await trans_stmt_blk
  let found_await := list_bool.contains true

  found_await

--======== For checking if a transition inserts =======
-- if it does should synth a guard on dest buffer too!

partial def get_api_with_guard_function_calls
(stmt : Pipeline.Statement)
:=
          -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
          -- dbg_trace stmt
          -- dbg_trace "==END GET-TRANSITIONS ==\n"

  match stmt with
  -- | Statement.transition ident => [ident]
  | Statement.stray_expr expr =>
    match expr with
    | Expr.some_term term =>
      match term with
      | Term.function_call qual_name lst_expr =>
        match qual_name with
        | QualifiedName.mk lst_ident =>
          -- check if this is an insert func
          -- dbg_trace "===== BEGIN List of Func Identifiers ====\n"
          -- dbg_trace lst_ident
          -- dbg_trace "===== END List of Func Identifiers ====\n"

          if
          (or
          (or
          (lst_ident.contains "insert")
          (lst_ident.contains "send_load_request"))
          -- (lst_ident.contains "set_executed")
          (lst_ident.contains "send_store_request")
          )
          then
            [lst_ident]
          else
            []
      | _ => -- ignore, term isn't a func call
        []
    | _ => -- isn't going to be a func call
      []
  | Statement.labelled_statement label stmt =>
    get_api_with_guard_function_calls stmt
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [get_api_with_guard_function_calls stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            get_api_with_guard_function_calls stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_api_with_guard_function_calls)
    | Conditional.if_statement expr1 stmt1 => get_api_with_guard_function_calls stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_api_with_guard_function_calls)
  | Statement.await term lst_stmt1 => List.join (lst_stmt1.map get_api_with_guard_function_calls)
  | Statement.when qname list_idens stmt => get_api_with_guard_function_calls stmt
  -- | Statement.listen_handle  => 
  | _ => []

--========= Convert AST Stmts to Murphi Stmts =========

mutual -- BEGIN mutually recursive func region --

partial def list_ident_to_murphi_ID
(lst_ident : List Identifier)
: (List (ID ⊕ Murϕ.Expr) )
:= 
  match lst_ident with
  | [one_ident] => [Sum.inl one_ident]
  | h::t =>
    List.cons (Sum.inl h) (list_ident_to_murphi_ID t)
  | [] => []

partial def list_ident_to_murphi_designator
( lst_ident : List Identifier )
: Designator
:=
  match lst_ident with
  | [one_ident] => Murϕ.Designator.mk one_ident []
  | h::t => Murϕ.Designator.mk h (list_ident_to_murphi_ID t)
  | [] => dbg_trace "ERROR: Empty identifier list???"
    Murϕ.Designator.mk "" []

partial def ident_matches_state_var_list
(state_vars : List TypedIdentifier)
(ident : Identifier)
: Bool
:=
  -- does ident belong in state_vars list?
  let state_var_names :=
    state_vars.map (
      λ state_var =>
        match state_var with
        | TypedIdentifier.mk t_iden iden =>
          -- just return the identifier name
          -- then we can do list.contains
          iden
    )

  let ident_in_list :=
  state_var_names.contains ident

  ident_in_list

partial def ident_matches_ident_list
(lst_idents : List Identifier)
(ident : Identifier)
: Bool
:=
  -- does ident belong in state_vars list?
  let ident_in_list :=
  lst_idents.contains ident

  ident_in_list

partial def get_ctrler_matching_name
(ctrler_name : Identifier)
(lst_ctrlers : List controller_info)
-- : Except String controller_info
: controller_info
:= 
  let ctrler_lst_with_name :=
  lst_ctrlers.filter (
    λ ctrler =>
      -- match if ctrler name
      -- is the struct name
      ctrler.name == ctrler_name
  )
  
  let dest_ctrler :=
  match ctrler_lst_with_name with
  | [one_ctrler] => one_ctrler
  | h::t =>
    dbg_trace "Multiple ctrlers w/ the same name!?"
    default
  | [] => dbg_trace "dest ctrler not in ctrler list?"
    dbg_trace "is it a default ctrler?"
    dbg_trace "or a undefined ctrler?"
    -- I should do a name check, like for:
    -- memory_interface, or 
    dbg_trace "===== The ctrler name?? ====="
    dbg_trace ctrler_name
    -- dbg_trace "===== List of Controllers ====="
    -- dbg_trace lst_ctrlers
    default

  dest_ctrler

partial def list_ident_to_murphi_designator_ctrler_var_check
( qual_name_idents : List Identifier )
( lst_ctrlers : List controller_info )
( ctrler_name : Identifier )
-- (stmt_trans_info : stmt_translation_info)
( tail_entry : tail_or_entry)
( specific_murphi_dest_expr : Option Murϕ.Expr )
( entry_or_ctrlr_desig_prefix : entry_or_ctrler )
-- AZ TODO: handle these, so we can translate
-- exprs that use Entry?
-- or terms in general that match a ctrler's
-- state var(s)!
-- No wait, the thing that calls this, the
-- term translation func makes this check if the
-- args are a part of the args list?
-- ( dest_ctrler : Option Identifier)
-- ( args_list: Option (List Identifier))
: Designator
:=
    -- get this controller from the
    -- controller name
  let this_ctrler : Ctrler :=
    -- dbg_trace "===== list_ident_to_murphi_designator_ctrler_var_check ====="
    get_ctrler_matching_name ctrler_name lst_ctrlers
  dbg_trace s!"Translate Ident Var. Ctrler: ({ctrler_name})"
  dbg_trace s!"Translate Ident Var. tail_entry: ({tail_entry})"
  dbg_trace s!"Translate Ident Var. specific_murphi_dest_expr: ({specific_murphi_dest_expr})"
  dbg_trace s!"Translate Ident Var. qual_name_idents: ({qual_name_idents})"
  -- let this_ctrler_state_vars := this_ctrler.state_vars

  -- let entry_or_ctrler_translation : entry_or_ctrler :=
  --   if this_ctrler.init_trans.isSome then
  --     entry_or_ctrler.entry
  --   else if this_ctrler.ctrler_init_trans.isSome then
  --     entry_or_ctrler.ctrler
  --   else
  --     dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({this_ctrler})"
  --       default

  -- let state_vars_to_use : List TypedIdentifier :=
  --   this_ctrler.get_state_vars

  let c_type := match this_ctrler.type with
    | .ok ctype => ctype
    | .error msg =>
      dbg_trace s!"Error, Couldn't get ctrler type in list_ident_to_murphi_designator_ctrler_var_check. Msg: ({msg})"
      default

  let state_var_idents : List Identifier :=
    match this_ctrler.state_var_names with
    | .ok state_var_names => state_var_names
    | .error msg =>
      dbg_trace s!"ERROR, ctrler doesn't have state var names? ({this_ctrler}), Msg: ({msg})"
      default

  let designator : Murϕ.Designator :=
  match qual_name_idents with
  | [one_ident] =>
    let ident_matches_state_var :=
    ident_matches_ident_list state_var_idents one_ident 

    let ident_matches_state_var := ident_matches_state_var || (one_ident == "curr_state") || (one_ident == "state") || (one_ident == "valid")
    let one_ident := if (one_ident == "curr_state") then "state" else one_ident

    -- dbg_trace "!!! BEGIN IDENT !!!"
    -- dbg_trace one_ident
    -- dbg_trace state_var_idents
    -- dbg_trace "!!! END IDENT !!!"
    -- if ident_matches_state_var
    -- then we should check the designator
    -- and generate the name using the
    -- sth.name.whatever.longer.name

    dbg_trace s!"Translate Ident Var. one_ident: ({one_ident})"
    dbg_trace s!"Translate Ident Var. state_var_idents: ({state_var_idents})"
    dbg_trace s!"Translate Ident Var. Ident is a State_Var: ({ident_matches_state_var})"
    if ident_matches_state_var
    then
      -- If this matches then i should
      -- check if this var comes from
      -- a fifo structure to index into
      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_indexable : Bool :=
        IndexableCtrlerTypesStrings.contains ctrler_ordering
      -- AZ CHECKPOINT TODO:
      -- finish this

      dbg_trace s!"Translate Ident Var. is_indexable: ({is_indexable})"
      let ctrler_name_ : String := ctrler_name.append "_"
      let ctrler_not_entry_bool : Bool :=
        match c_type with
        | .FIFO | .Unordered => false
        | .BasicCtrler => true
      if is_indexable
      then
        -- if fifo, then make it with 
        -- the <struct_name>.<entries>[<struct>.tail]

        let idx : Identifier :=
        match tail_entry with
        | tail_or_entry.tail => "tail"
        | tail_or_entry.entry => "i"
        | tail_or_entry.custom_entry => "" -- directly use the provided expr..

        let specific_murphi_dest_extracted : Murϕ.Expr :=
        if specific_murphi_dest_expr.isSome then
          specific_murphi_dest_expr.get!
        else
          panic! "TODO: throw! should have passed this as a non-none arg"

        let fifo_idx_expr : Murϕ.Expr :=
        match tail_entry with
        | tail_or_entry.tail =>
          Murϕ.Expr.designator (
          Murϕ.Designator.mk "next_state" [
            -- entries
            -- Assume the buffer entries are
            -- referred to as 'i'
            Sum.inl "core_",
            Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
            Sum.inl (ctrler_name.append "_"),
            Sum.inl idx
          ])
        | tail_or_entry.entry =>
          Murϕ.Expr.designator (
          Murϕ.Designator.mk idx [])
        | tail_or_entry.custom_entry =>
          dbg_trace "PRINT THE SPECIFIC MURPHI EXTRACTED EXPR!!"
          dbg_trace specific_murphi_dest_extracted
          dbg_trace "PRINT THE SPECIFIC MURPHI EXPR!!"
          dbg_trace specific_murphi_dest_expr

          specific_murphi_dest_extracted

        let murphi_designator :=
        Murϕ.Designator.mk "next_state" [
          -- entries
          Sum.inl "core_",
          Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
          Sum.inl (ctrler_name.append "_"),
          Sum.inl "entries",
          Sum.inr fifo_idx_expr,
          Sum.inl one_ident
        ]
        murphi_designator
      else if ctrler_not_entry_bool then
        -- Generate the Murphi desig; without the entries[ designator ]
        let mur_desig : Murϕ.Designator := [murϕ_designator| next_state .core_[j] .£ctrler_name_ .£one_ident]
        mur_desig
      else 
        dbg_trace "WHAT CTRLER STRUCTURE IS NOT FIFO"
        Murϕ.Designator.mk one_ident []
    else
      dbg_trace s!"Translate Ident Var. ident doesn't match state var: ({one_ident})"
      Murϕ.Designator.mk one_ident []

  | h::t =>
    let ident_matches_ident_list :=
    ident_matches_ident_list state_var_idents h

    let ident_matches_ident_list := or ident_matches_ident_list (t.contains "curr_state") || (t.contains "state")
    let t := t.map ( fun ident_str => if (ident_str == "curr_state") then "state" else ident_str)

    dbg_trace s!"State Var Identifier List: ({state_var_idents})"
    dbg_trace s!"Head Identifier: ({h})"

    let ctrlers_with_h_as_name : List controller_info :=
      lst_ctrlers.filter (λ ctrl : controller_info => ctrl.name == h)
    -- have H_is_ctrler := 0 < ctrlers_with_h_as_name.length;
    let h_is_basic_ctrler : Bool :=
      match H : ctrlers_with_h_as_name with
      | [one] =>
        have one_or_more : 0 < ctrlers_with_h_as_name.length := by simp[H];
        let h_ctrler := ctrlers_with_h_as_name[0]'one_or_more;
        let h_ctrler_type! := h_ctrler.type;
        match h_ctrler_type! with
        | .ok c_type =>
          match c_type with
          | .BasicCtrler => true
          | .FIFO | .Unordered => false
        | .error msg =>
          dbg_trace s!"Error while translating Qualified Identifier ({qual_name_idents}): ({msg})"
          false
      | [] =>
        dbg_trace s!"while translating Qualified Identifier ({qual_name_idents}): No ctrlers matching name: ({h})"
        false
      | _ :: _ =>
        dbg_trace s!"while translating Qualified Identifier ({qual_name_idents}): Multiple ctrlers matching name: ({ctrlers_with_h_as_name.map (·.name)})"
        false

    -- This is for the second check,
    -- if "entry" is a term identifier,
    -- translate this as <ctrler>.entries[curr_idx]
    -- AZ NOTE: it's curr_idx simply because that's
    -- what the search API (for LQ -> SQ or SQ -> LQ searches)
    -- uses for indexing to the element it's searching for!
    let ident_is_entry : Bool :=
      h == "entry"

    if h_is_basic_ctrler then
      dbg_trace s!"Translate for Basic Ctrler as H: ({h})"
      let sum_list : List (String ⊕ Murϕ.Expr)
      := List.append [
        -- entries
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (h.append "_")
      ] (list_ident_to_murphi_ID t)

      -- AZ TODO: Use dest ctrler!
      -- TODO Tuesday, Aug 16, 2022
      let murphi_designator :=
        -- Murϕ.Designator.mk dest_ctrler sum_list
        Murϕ.Designator.mk "next_state" sum_list

      dbg_trace s!"Translated BasicCtrler member state var/state access: ({qual_name_idents}) to: ({murphi_designator})"
      murphi_designator
    else if ident_is_entry && ident_matches_ident_list then
      dbg_trace s!"Translate for H as Entry: ({h})"
      -- special case: we use the entry designator base qualifier, then the ident
      let entries := "entries"
      -- let idx : Murϕ.Designator := match specific_murphi_dest_expr with
      -- | Murϕ.Expr.designator desig =>
      --   dbg_trace "===== BEGIN The Designator!!! "
      --   dbg_trace specific_murphi_dest_expr
      --   -- dbg_trace desig
      --   dbg_trace "===== END The Designator!!! "
      --   desig
      -- | _ => dbg_trace "Throw!!! Didn't pass an expr with a designator!!!"
      --   default

      let curr_idx_designator_expr :=
      Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])
      -- Murϕ.Expr.designator idx

      let sum_list : List (String ⊕ Murϕ.Expr)
      := List.append [
        -- entries
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (ctrler_name.append "_"),
        Sum.inl entries,
        Sum.inr (if specific_murphi_dest_expr.isNone then curr_idx_designator_expr else specific_murphi_dest_expr.get!)
      ] (list_ident_to_murphi_ID t)

      -- AZ TODO: Use dest ctrler!
      -- TODO Tuesday, Aug 16, 2022
      let murphi_designator :=
      -- Murϕ.Designator.mk dest_ctrler sum_list
      Murϕ.Designator.mk "next_state" sum_list
      -- Murϕ.Designator.

        dbg_trace "===== BEGIN OVERALL Designator!!! "
        dbg_trace specific_murphi_dest_expr
        dbg_trace "-----------"
        dbg_trace murphi_designator
        -- dbg_trace desig
        dbg_trace "===== END OVERALL Designator!!! "
      murphi_designator
    else
    if ident_matches_ident_list
    then
      dbg_trace s!"Translate ident in state_vars: Qual Var: ({h::t}), State Vars: ({state_var_idents})"
      -- If this matches then i should
      -- check if this var comes from
      -- a fifo structure to index into
      let h_is_ctrler_type : Bool :=
        (lst_ctrlers.filter (λ ctrl : controller_info => ctrl.name == h)).length > 0

      let ctrler_ordering :=
        if !h_is_ctrler_type then
          get_ctrler_elem_ordering this_ctrler
        else
          let h_ctrler : controller_info :=
            get_ctrler_matching_name h lst_ctrlers
          get_ctrler_elem_ordering h_ctrler

      let is_indexable : Bool :=
        IndexableCtrlerTypesStrings.contains ctrler_ordering
      -- AZ CHECKPOINT TODO:
      -- is there a case where I want to
      -- replace "i" with "<structure>.tail?"
      dbg_trace s!"Is this structure indexable: ({ctrler_ordering}, ({IndexableCtrlerTypesStrings}), {is_indexable})"

      let ctrler_name_ : String := ctrler_name.append "_"

      -- let h_is_ctrler_type : Bool :=
      --   (lst_ctrlers.filter (λ ctrl : controller_info => ctrl.name == h)).length > 0

      let ctrler_not_entry_bool : Bool :=
        h_is_ctrler_type ||
        match c_type with
        | .FIFO | .Unordered => false
        | .BasicCtrler => true
        -- match entry_or_ctrlr_desig_prefix with
        -- | .entry => false
        -- | .ctrler => true
      dbg_trace s!"Translate term for ctrler-type? : ({ctrler_not_entry_bool})";
      dbg_trace s!"h_is_ctrler_type: ({h_is_ctrler_type})";
      dbg_trace s!"entry_or_ctrlr_desig_prefix: ({entry_or_ctrlr_desig_prefix})";

      -- let list_sum : List (ID ⊕ Murϕ.Expr) := list_ident_to_murphi_ID t
      if is_indexable
      then
        -- if ctrler is indexable, then gen the name with
        -- the <struct_name>.<entries>[<struct>.tail]

        let idx : Identifier :=
        match tail_entry with
        | tail_or_entry.tail => "tail"
        | tail_or_entry.entry => "i"
        | tail_or_entry.custom_entry => "" -- directly use the provided expr..

        let specific_murphi_dest_extracted : Murϕ.Expr :=
        if specific_murphi_dest_expr.isSome then
          specific_murphi_dest_expr.get!
        else
          -- panic! "TODO: throw! should have passed this as a non-none arg"
          dbg_trace "Translate a term: Translating with 'none', so using default idx 'i'"
          dbg_trace s!"The term to translate ({qual_name_idents})"
          ([murϕ| i ] : Murϕ.Expr) -- the default

        let fifo_tail_expr : Murϕ.Expr :=
        match tail_entry with
        | tail_or_entry.tail =>
          Murϕ.Expr.designator (
          Murϕ.Designator.mk "next_state" [
            -- entries
            -- Assume the buffer entries are
            -- referred to as 'i'
            Sum.inl "core_",
            Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
            Sum.inl (ctrler_name.append "_"),
            Sum.inl idx
          ])
        | tail_or_entry.entry =>
          Murϕ.Expr.designator (
          Murϕ.Designator.mk idx [])
        | tail_or_entry.custom_entry =>
          specific_murphi_dest_extracted

        let sum_list : List (String ⊕ Murϕ.Expr)
        := List.append [
          -- entries
          Sum.inl "core_",
          Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
          Sum.inl (ctrler_name.append "_"),
          Sum.inl "entries",
          Sum.inr fifo_tail_expr,
          Sum.inl h
        ] (list_ident_to_murphi_ID t)

        let murphi_designator :=
        Murϕ.Designator.mk "next_state" sum_list
        -- Murϕ.Designator.

        dbg_trace s!"Generated Term Designator: ({murphi_designator})"
        murphi_designator
      else if ctrler_not_entry_bool then
        let mur_desig : Murϕ.Designator := --[murϕ_designator| next_state .core_[j] .£ctrler_name_ .£h .£list_sum]
          if h_is_ctrler_type then
            Murϕ.Designator.mk "next_state" ([
            Sum.inl "core_",
            Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
            Sum.inl ( h.append "_" )
            ] ++ (list_ident_to_murphi_ID t))
          else
            Murϕ.Designator.mk "next_state" ([
            Sum.inl "core_",
            Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
            Sum.inl ctrler_name_,
            Sum.inl h
            ] ++ (list_ident_to_murphi_ID t))
        mur_desig
      else 
        dbg_trace "WHAT CTRLER STRUCTURE ISN'T FIFO?"
        Murϕ.Designator.mk h (list_ident_to_murphi_ID t)
    else
    if ident_is_entry
    then
      dbg_trace s!"Translate as entry: ({h})"
      -- head entry h, is entry, translate to
      -- <ctrler>.entries[curr_idx]

      -- already have ctrler_name
      let entries := "entries"
      -- let idx : Murϕ.Designator := match specific_murphi_dest_expr with
      -- | Murϕ.Expr.designator desig =>
      --   dbg_trace "===== BEGIN The Designator!!! "
      --   dbg_trace specific_murphi_dest_expr
      --   -- dbg_trace desig
      --   dbg_trace "===== END The Designator!!! "
      --   desig
      -- | _ => dbg_trace "Throw!!! Didn't pass an expr with a designator!!!"
      --   default

      let curr_idx_designator_expr :=
      Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])
      -- Murϕ.Expr.designator idx

      let sum_list : List (String ⊕ Murϕ.Expr)
      := List.append [
        -- entries
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (ctrler_name.append "_"),
        Sum.inl entries,
        Sum.inr (if specific_murphi_dest_expr.isNone then curr_idx_designator_expr else specific_murphi_dest_expr.get!)
      ] (list_ident_to_murphi_ID t)

      -- AZ TODO: Use dest ctrler!
      -- TODO Tuesday, Aug 16, 2022
      let murphi_designator :=
      -- Murϕ.Designator.mk dest_ctrler sum_list
      Murϕ.Designator.mk "next_state" sum_list
      -- Murϕ.Designator.

        dbg_trace "===== BEGIN OVERALL Designator!!! "
        dbg_trace specific_murphi_dest_expr
        dbg_trace "-----------"
        dbg_trace murphi_designator
        -- dbg_trace desig
        dbg_trace "===== END OVERALL Designator!!! "
      murphi_designator
    else
      dbg_trace s!"Translate as default case: ({h})"
      Murϕ.Designator.mk h (list_ident_to_murphi_ID t)
  | [] => dbg_trace "ERROR: Empty identifier list???"
    Murϕ.Designator.mk "" []

  designator

--===== Helper func, DSL Term to Murphi Term. =====

partial def ast_term_to_murphi_expr
-- ( term : Pipeline.Term )
-- (lst_ctrlers : List controller_info)
-- (curr_ctrler_name : Identifier) -- "string"
(term_trans_info : term_translation_info)
 : Murϕ.Expr
:=
  let term := term_trans_info.term
  let lst_ctrlers := term_trans_info.lst_ctrlers
  let curr_ctrler_name := term_trans_info.ctrler_name
  -- for when statements
  let src_ctrler := term_trans_info.src_ctrler
  let lst_src_args := term_trans_info.lst_src_args

  dbg_trace s!"Translate DSL Term: ({term})"

  match term with
  | Term.negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let translation :=
      Murϕ.Expr.negation (ast_term_to_murphi_expr term'_trans_info)

    dbg_trace s!"DSL Negation ({term}) translated: ({translation})"

    translation
  | Term.logical_negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let translation :=
      Murϕ.Expr.negation (ast_term_to_murphi_expr term'_trans_info)
      -- ast_term_to_murphi_expr term'
    dbg_trace s!"DSL logical Negation ({term}) translated: ({translation})"

    translation
  | Term.binary_negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let ret_val :=
    ast_term_to_murphi_expr term'_trans_info

    dbg_trace s!"DSL binary Negation ({term}) translated: ({ret_val})"
    -- panic! ("Can't translate Binary Negation to Murphi, Murphi doesn't have binary negation" ++ "Instead we would need to do some more work" ++ s!"Binary Negation Term: ({term})")

    ret_val
  -- AZ NOTE:
  -- It seems the ident case of Var
  -- and qualifed_var are the ones we want...
  -- to add the designator to
  | Term.var ident =>
    -- See below NOTE!
    -- designator.
    -- Murϕ.Expr.designator (
    --   Murϕ.Designator.mk ident []
    -- )
    let is_src_ctrler_none : Bool :=
      src_ctrler == none

    dbg_trace "Translate Term.var!"
    if !is_src_ctrler_none
    then
      dbg_trace "Translate term.var, src_ctrler is not none"
      -- if not then we need to consider
      -- the src controller

      -- first, try to match the term to
      -- the var list, if it contains it,
      -- then we need to check if it's from
      -- the src ctrler's state vars

      let lst_src_args_extracted :=
        if lst_src_args.isSome
        then
          lst_src_args.get!
        else
          panic! "calling func didn't provide the list of src ctrler args!"

      let ident_in_args : Bool :=
        lst_src_args_extracted.contains ident
      
      let specific_murphi_dest_expr_is_some : Bool :=
      term_trans_info.specific_murphi_dest_expr.isSome
      
      dbg_trace s!"term.var translation: Ctrler: ({curr_ctrler_name})"
      dbg_trace s!"term.var translation Src_Ctrler: ({term_trans_info.src_ctrler})"
      dbg_trace s!"term.var translation is_rhs?: ({term_trans_info.is_rhs})"

      let curr_ctrler : Ctrler := get_ctrler_matching_name curr_ctrler_name lst_ctrlers
      let ctrler_vars := match curr_ctrler.state_var_names with
        | .ok lst_state_var_names => lst_state_var_names
        | .error msg =>
          dbg_trace s!"Error getting ctrler vars: ({msg})"
          []

      let src_ctrler_vars : List String :=
        match src_ctrler with
        | some src_ctrler_ident =>
          let src_ctrler_obj : Ctrler :=
            get_ctrler_matching_name src_ctrler_ident lst_ctrlers
          dbg_trace s!"Term.qualified_var, call didn't pass lst_src_args, using ctrler state vars: ({src_ctrler_obj.name})"
          match src_ctrler_obj.state_var_names with
          | .ok lst_state_var_names => lst_state_var_names
          | .error msg =>
            dbg_trace s!"Error getting src ctrler vars in Qual Var -> Murphi Translation: ({msg})"
            []
        | none => []

      let ident_in_src_ctrler_vars : Bool :=
        src_ctrler_vars.contains ident
      let ident_in_ctrler_vars : Bool :=
        ctrler_vars.contains ident

      if ident_in_args
      then
        dbg_trace " IDENT_IN_ARGS TRANSLATION CASE"
        -- Then we check if it's one of the
        -- src ctrler's state vars.

        let src_ctrler_extracted :=
        if src_ctrler.isSome
        then
          src_ctrler.get!
        else
          panic! "calling func didn't provide the list of src ctrler args!"
        -- if yes, we gen with the src
        -- ctrler's args
        let bool_thing : Bool :=
        if src_ctrler_extracted == "" then
          dbg_trace "===== BLANK STRING CTRLER NAME ====="
          false
        else
          true

        -- TODO NOTE: Thu Oct 6 2022
        -- This "and" condition also checks if is_rhs set
        -- but an if-cond isn't rhs....
        dbg_trace "specific dest expr"
        dbg_trace term_trans_info.specific_murphi_dest_expr
        let tail_entry :=
        if (
        -- ( specific_murphi_dest_expr_is_some ) ||
        (term_trans_info.is_rhs && specific_murphi_dest_expr_is_some) ||
        ( ident == "curr_state" )
        ) then
          dbg_trace "CUSTOM ENTRY"
          tail_or_entry.custom_entry
        else
          dbg_trace "BASIC ENTRY"
          tail_or_entry.entry

        

        let murphi_designator : Designator :=
          list_ident_to_murphi_designator_ctrler_var_check 
          ([ident]) (lst_ctrlers) src_ctrler_extracted (tail_entry)
          term_trans_info.specific_murphi_dest_expr
          term_trans_info.translate_entry_or_ctrler

        let murphi_expr_designator : Murϕ.Expr := 
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
      else
        dbg_trace " DEFAULT IDENT TRANSLATION CASE"
        -- default case, can just copy here..

        -- if it is from the state vars, then
        -- we use that ctrler gen designator
        -- function..
        let bool_thing : Bool :=
        if curr_ctrler_name == "" then
        dbg_trace "===== BLANK STRING CTRLER NAME ====="
        false
        else
        true
        -- specify tail or entry here

        dbg_trace s!"Specific Dest: ({term_trans_info.specific_murphi_dest_expr})"
        let tail_or_entry_or_custom :=
        if specific_murphi_dest_expr_is_some then
          dbg_trace " CUSTOM ENTRY "
          tail_or_entry.custom_entry
        else
          dbg_trace " BASIC ENTRY "
          tail_or_entry.entry

        let (ctrler_to_use, specific_or_curr_designator_idx) : String × (Option Murϕ.Expr) :=
          if ident_in_src_ctrler_vars /-ident_in_ctrler_vars-/ /- term_trans_info.src_ctrler.isSome -/ then
            (term_trans_info.src_ctrler.get!, term_trans_info.specific_murphi_dest_expr)
          else -- if ident_in_src_ctrler_vars /- && !ident_in_ctrler_vars -/ then
            ( curr_ctrler_name, term_trans_info.curr_ctrler_designator_idx)

        let murphi_designator : Murϕ.Designator := (
          list_ident_to_murphi_designator_ctrler_var_check
          [ident]
          lst_ctrlers
          -- Note that curr_ctrler is likely
          -- the dest ctrler
          ctrler_to_use -- curr_ctrler_name
          tail_or_entry_or_custom
          
          -- will if none naturally if it is none
          specific_or_curr_designator_idx
          term_trans_info.translate_entry_or_ctrler
        )
        let murphi_expr_designator :=
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
    else
      dbg_trace "Translate term.var, src_ctrler"
      -- let specific_murphi_dest_expr_is_some : Bool :=
      -- term_trans_info.specific_murphi_dest_expr.isSome

      -- let tail_or_entry_or_custom :=
      -- if specific_murphi_dest_expr_is_some then
      --   -- dbg_trace "THIS IS UNIMPLEMENTED?"
      --   dbg_trace " CUSTOM ENTRY "
      --   tail_or_entry.custom_entry
      -- else
      --   dbg_trace " BASIC ENTRY "
      --   tail_or_entry.entry
      let tail_or_entry_or_custom := tail_or_entry.entry

      let murphi_designator : Murϕ.Designator := (
        list_ident_to_murphi_designator_ctrler_var_check
        [ident]
        lst_ctrlers
        curr_ctrler_name
        tail_or_entry_or_custom
        -- will if none naturally if it is none
        -- If we use the current_ctrler, use the curr_ctrler_designator_idx
        term_trans_info.curr_ctrler_designator_idx -- term_trans_info.specific_murphi_dest_expr
        term_trans_info.translate_entry_or_ctrler
      )
      let murphi_expr_designator :=
      Murϕ.Expr.designator murphi_designator

      murphi_expr_designator
  | Term.qualified_var qualified_name =>
    match qualified_name with
    | QualifiedName.mk lst_ident =>
    -- AZ NOTE: This will be work when
    -- doing the Decl generation.
    -- Would need to check for the base
    -- ID to give it a Decl
    -- designator.

    let is_src_ctrler_none : Bool :=
      src_ctrler.isNone
    dbg_trace s!"Translate Term.qualified_var, src_ctrler: ({src_ctrler})";

    let root_ident_option := lst_ident[0]?
    let root_ident := if root_ident_option.isSome then
    root_ident_option.get!
    else
    panic! "there isn't even 1 ident in the lst of identifiers?"

    let is_entry_keyword : Bool :=
      root_ident == "entry"

      
    let specific_murphi_dest_expr_is_some : Bool :=
      term_trans_info.specific_murphi_dest_expr.isSome

    if is_entry_keyword
    then
      dbg_trace "Term Qualified Ident, is_entry_keyword"

      let entry_keyword_dest : Identifier := 
      if term_trans_info.entry_keyword_dest.isSome
      then
        term_trans_info.entry_keyword_dest.get!
      else
      dbg_trace "=== this is the lst of idents ==="
      dbg_trace lst_ident
      dbg_trace curr_ctrler_name
      dbg_trace term_trans_info.entry_keyword_dest
      dbg_trace term_trans_info.trans_obj
      panic! "Should have a dest if using entry keyword?"

      let bool_thing : Bool :=
      if entry_keyword_dest == "" then
      dbg_trace "===== BLANK STRING CTRLER NAME ====="
      false
      else
      true

      -- then translate using the entry_keyword_dest
      let murphi_designator : Designator :=
        list_ident_to_murphi_designator_ctrler_var_check 
        (lst_ident) (lst_ctrlers) (entry_keyword_dest) (tail_or_entry.entry)
        term_trans_info.specific_murphi_dest_expr
        term_trans_info.translate_entry_or_ctrler

      let murphi_expr_designator : Murϕ.Expr := 
      Murϕ.Expr.designator murphi_designator

      murphi_expr_designator
    else
    if !is_src_ctrler_none
    then
      dbg_trace "Term Qualified Ident, !is_src_ctrler_none case"
      let root_ident_option := lst_ident[0]?
      let root_ident := if root_ident_option.isSome then
      root_ident_option.get!
      else
      panic! "there isn't even 1 ident in the lst of identifiers?"
      let ident := root_ident
      -- if not then we need to consider
      -- the src controller

      -- first, try to match the term to
      -- the var list, if it contains it,
      -- then we need to check if it's from
      -- the src ctrler's state vars

    -- check if the first item in the list
    -- belongs to the args list

      let lst_src_args_extracted : List String :=
        if lst_src_args.isSome
        then
          lst_src_args.get!
        else
          []

      let curr_ctrler : Ctrler := get_ctrler_matching_name curr_ctrler_name lst_ctrlers
      let ctrler_vars := match curr_ctrler.state_var_names with
        | .ok lst_state_var_names => lst_state_var_names
        | .error msg =>
          dbg_trace s!"Error getting ctrler vars: ({msg})"
          []

      let src_ctrler_vars : List String :=
        match src_ctrler with
        | some src_ctrler_ident =>
          let src_ctrler_obj : Ctrler :=
            get_ctrler_matching_name src_ctrler_ident lst_ctrlers
          dbg_trace s!"Term.qualified_var, call didn't pass lst_src_args, using ctrler state vars: ({src_ctrler_obj.name})"
          match src_ctrler_obj.state_var_names with
          | .ok lst_state_var_names => lst_state_var_names
          | .error msg =>
            dbg_trace s!"Error getting src ctrler vars in Qual Var -> Murphi Translation: ({msg})"
            []
          -- src_ctrler_obj.get_state_vars.map (
          --   λ state_var : TypedIdentifier =>
          --     match state_var with
          --     | .mk _ ident => ident
          -- )
        | none => []
      let ident_in_args : Bool :=
        lst_src_args_extracted.contains ident

      let ident_in_src_ctrler_vars : Bool :=
        src_ctrler_vars.contains ident
      let ident_in_ctrler_vars : Bool :=
        ctrler_vars.contains ident

      let tail_or_entry_or_custom := -- tail_or_entry.entry
        if (
        ( specific_murphi_dest_expr_is_some ) ||
        (term_trans_info.is_rhs && specific_murphi_dest_expr_is_some) ||
        ( lst_ident[0]! == "curr_state" )
        ) then
          dbg_trace "CUSTOM ENTRY"
          tail_or_entry.custom_entry
        else
          dbg_trace "BASIC ENTRY"
          tail_or_entry.entry
      -- TODO: Also check for matching state var???? I don't know...
      dbg_trace s!"Translate Qual.var root_ident: ({root_ident})"
      dbg_trace s!"Translate Qual.var lst_src_args: ({lst_src_args})"
      dbg_trace s!"Translate Qual.var ident_in_args: ({ident_in_args})"
      if ident_in_args
      then
        dbg_trace s!"Translate Q.var ident_in_args!"
        -- Then we check if it's one of the
        -- src ctrler's state vars.

        let src_ctrler_extracted :=
          if src_ctrler.isSome
          then
            src_ctrler.get!
          else
            panic! "calling func didn't provide the list of src ctrler args!"
        -- if yes, we gen with the src
        -- ctrler's args
        -- the curr ctrler is for the src ctrler
        dbg_trace s!"Translate Term.qualified_var,specific desig: ({term_trans_info.specific_murphi_dest_expr})"
        dbg_trace s!"Translate Term.qualified_var,curr_ctrler desig: ({term_trans_info.curr_ctrler_designator_idx})"
        let entry_designator : Option Murϕ.Expr :=
          if term_trans_info.specific_murphi_dest_expr.isSome then
            term_trans_info.specific_murphi_dest_expr
          else if term_trans_info.curr_ctrler_designator_idx.isSome then
            term_trans_info.curr_ctrler_designator_idx
          else
            Option.none

        let murphi_designator : Designator :=
          list_ident_to_murphi_designator_ctrler_var_check
          (lst_ident) (lst_ctrlers) src_ctrler_extracted tail_or_entry_or_custom/-(tail_or_entry.entry)-/
          entry_designator -- term_trans_info.specific_murphi_dest_expr
          term_trans_info.translate_entry_or_ctrler

        let murphi_expr_designator : Murϕ.Expr := 
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
      else if ident_in_src_ctrler_vars && !ident_in_ctrler_vars then
        dbg_trace s!"Translate Q.var ident_in_src_ctrler_vars!"

        let tail_or_entry_or_custom :=
        if specific_murphi_dest_expr_is_some then
          tail_or_entry.custom_entry
        else
          tail_or_entry.entry

        let desig_idx : Option Murϕ.Expr :=
          if term_trans_info.src_ctrler.isSome then
            if term_trans_info.src_ctrler.get! == term_trans_info.ctrler_name then
              if term_trans_info.curr_ctrler_designator_idx.isSome then
                term_trans_info.curr_ctrler_designator_idx
              else if term_trans_info.specific_murphi_dest_expr.isSome then
                term_trans_info.specific_murphi_dest_expr
              else
                Option.none
            else
              term_trans_info.curr_ctrler_designator_idx
          else
            term_trans_info.specific_murphi_dest_expr

        let murphi_designator : Murϕ.Designator := (
          list_ident_to_murphi_designator_ctrler_var_check
          lst_ident lst_ctrlers
          src_ctrler.get!
          tail_or_entry_or_custom
          desig_idx -- term_trans_info.curr_ctrler_designator_idx -- term_trans_info.specific_murphi_dest_expr
          -- Note that curr_ctrler is likely
          -- the dest ctrler
          term_trans_info.translate_entry_or_ctrler
        )
        let murphi_expr_designator :=
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
      else
        dbg_trace s!"Translate Q.var NOT ident_in_args!"
        -- default case, can just copy here..

        -- if it is from the state vars, then
        -- we use that ctrler gen designator
        -- function..
        let bool_thing : Bool :=
        if curr_ctrler_name == "" then
        dbg_trace "===== BLANK STRING CTRLER NAME ====="
        false
        else
        true

        let tail_or_entry_or_custom :=
        if specific_murphi_dest_expr_is_some then
          tail_or_entry.custom_entry
        else
          tail_or_entry.entry

        let desig_idx : Option Murϕ.Expr :=
          if term_trans_info.src_ctrler.isSome then
            if term_trans_info.src_ctrler.get! == term_trans_info.ctrler_name then
              if term_trans_info.curr_ctrler_designator_idx.isSome then
                term_trans_info.curr_ctrler_designator_idx
              else if term_trans_info.specific_murphi_dest_expr.isSome then
                term_trans_info.specific_murphi_dest_expr
              else
                Option.none
            else
              term_trans_info.curr_ctrler_designator_idx
          else
            term_trans_info.specific_murphi_dest_expr

        let murphi_designator : Murϕ.Designator := (
          list_ident_to_murphi_designator_ctrler_var_check
          lst_ident lst_ctrlers
          curr_ctrler_name
          tail_or_entry_or_custom
          desig_idx -- term_trans_info.curr_ctrler_designator_idx -- term_trans_info.specific_murphi_dest_expr
          -- Note that curr_ctrler is likely
          -- the dest ctrler
          term_trans_info.translate_entry_or_ctrler
        )
        let murphi_expr_designator :=
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
    else
      dbg_trace "NOT Keyword entry, no src_ctrler -- Else case"


        let bool_thing : Bool :=
        if curr_ctrler_name == "" then
        dbg_trace "===== BLANK STRING CTRLER NAME ====="
        false
        else
        true

      -- let tail_or_entry_or_custom :=
      -- if specific_murphi_dest_expr_is_some then
      --   tail_or_entry.custom_entry
      -- else
      --   tail_or_entry.entry
      let specific_murphi_dest_expr_is_some : Bool :=
      term_trans_info.specific_murphi_dest_expr.isSome

      let tail_or_entry_or_custom := tail_or_entry.entry
        -- if (
        -- ( specific_murphi_dest_expr_is_some ) ||
        -- (term_trans_info.is_rhs && specific_murphi_dest_expr_is_some) ||
        -- ( lst_ident[0]! == "curr_state" )
        -- ) then
        --   dbg_trace "CUSTOM ENTRY"
        --   tail_or_entry.custom_entry
        -- else
        --   dbg_trace "BASIC ENTRY"
        --   tail_or_entry.entry

      -- NOTE: Something to consider, but i don't think this is a good idea.
      -- let desig_expr : Option Murϕ.Expr :=
      --   if term_trans_info.specific_murphi_dest_expr then

      let murphi_designator : Murϕ.Designator := (
        list_ident_to_murphi_designator_ctrler_var_check
        lst_ident
        lst_ctrlers
        curr_ctrler_name
        tail_or_entry_or_custom
        term_trans_info.specific_murphi_dest_expr
        term_trans_info.translate_entry_or_ctrler
      )
      let murphi_expr_designator :=
      Murϕ.Expr.designator murphi_designator

      murphi_expr_designator

      -- let murphi_designator :=
      --   list_ident_to_murphi_designator lst_ident
      -- let murphi_expr :=
      --   Murϕ.Expr.designator (
      --     murphi_designator
      --   )
      -- murphi_expr

      -- NOTE: Not sure if the rest will
      -- also return Designators as well?
  | Term.function_call qualified_name lst_expr =>
    dbg_trace s!"Translating Term func call: ({term})"
    -- dbg_trace "WARNING: we didn't really use DSL Funcs,
    -- and I'm not bothering with a good translation"
    
    -- AZ NOTE: a function call not from a stray expr
    -- means that this is not a structure calling
    -- That means this can assume it won't be a 
        -- insert func
    -- This also means that I need to implement
    -- another version of this function
    -- (DSL term to Murphi expr),
    -- for translating starting from statements
    -- rather than an expr

    -- I suppose the main issue here, is how to
    -- determine that this function call is from a
    -- stmt, and not some nested expr
    -- And the answer is, the top level function that
    -- processes stmts needs to do the
    -- matching to check for this case

    -- So for now it's ok to continue, and assume
    -- this is not a direct msg passing call..

    -- TODO: Implement specific API functions, like is_head()
    let qual_name_list : List String := match qualified_name with
    | QualifiedName.mk lst_names => lst_names
    let is_len_one_name : Bool := qual_name_list.length == 1
    let has_is_head_name : Bool := qual_name_list.contains "is_head"
    let has_empty_name : Bool := qual_name_list.contains "empty"
    let no_input_args : Bool := lst_expr.length == 0

  -- Check the qual name list, if it contains "full"
    let length_2_qual_name : Bool := qual_name_list.length == 2

    if length_2_qual_name then
      if qual_name_list[1]! == "full" then
        -- Then do sth like 
        -- Sta.core[j].<ctrler>_.num_entries == <ctrler>_NUM_ENTRIES_CONST
        let dest_ctrler_name : String := qual_name_list[0]!
        let dest_ctrler_name_ : String := dest_ctrler_name ++ "_"
        let dest_ctrler_max_entries : String := dest_ctrler_name ++ "_NUM_ENTRIES_CONST"
        [murϕ| next_state .core_[j] .£dest_ctrler_name_ .num_entries = £dest_ctrler_max_entries]
      else if qual_name_list[1]! == "empty" then
        -- Then do sth like 
        -- Sta.core[j].<ctrler>_.num_entries == <ctrler>_NUM_ENTRIES_CONST
        let dest_ctrler_name : String := qual_name_list[0]!
        let dest_ctrler_name_ : String := dest_ctrler_name ++ "_"
        -- let dest_ctrler_max_entries : String := dest_ctrler_name ++ "_NUM_ENTRIES_CONST"
        [murϕ| next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0]
      else if qual_name_list[1]! == "out_busy" && qual_name_list[0]! == "memory_interface"  then
        [murϕ| next_state .core_[j] .mem_interface_ .out_busy = true]
      else if qual_name_list[1]! == "read" && qual_name_list[0]! == "reg_file" then
        dbg_trace "== Translating reg_file read API =="
        let reg_idx_expr := lst_expr[0]!
        let reg_idx_trans_expr : expr_translation_info :=
          assn_term_to_expr_translation_info term_trans_info reg_idx_expr
        let reg_idx_murphi_expr : Murϕ.Expr := ast_expr_to_murphi_expr reg_idx_trans_expr
        dbg_trace s!"Reg_file idx: ({reg_idx_expr})"

        let reg_write_stmt : Murϕ.Expr :=
        [murϕ| next_state .core_[j] .rf_ .rf[ £reg_idx_murphi_expr ]]
        reg_write_stmt
      else
        let msg : String :=
          "Not prepared to handle other len 2 name functions..."++
          s!"Function 'name': ({qualified_name}), Args: ({lst_expr})"
        dbg_trace msg
        -- throw msg
        let murphi_func_id :=
          match qualified_name with
          | QualifiedName.mk lst_idents =>
            String.join lst_idents

        let lst_expr_trans_info :=
          lst_expr.map (
            λ expr =>
              assn_term_to_expr_translation_info term_trans_info expr
          )

        let murphi_expr := Murϕ.Expr.call
          murphi_func_id (lst_expr_trans_info.map ast_expr_to_murphi_expr)
          -- murphi_func_id (lst_expr.map ast_expr_to_murphi_expr)
        murphi_expr
    else if is_len_one_name && has_is_head_name && no_input_args then
      let curr_ctrler_name_ : String := curr_ctrler_name.append "_"

      -- name of func call is just "is_head"
      -- then translate term as curr_ctrler.head == i
      let murphi_expr : Murϕ.Expr := [murϕ|
        next_state .core_[j] .£curr_ctrler_name_ .head = i
      ]
      murphi_expr
    else if is_len_one_name && has_empty_name && no_input_args then
      let curr_ctrler_name_ : String := curr_ctrler_name.append "_"
      [murϕ| next_state .core_[j] .£curr_ctrler_name_ .num_entries = 0 ]
    else 
      let msg : String :=
        "Not prepared to handle other functions, or user defined funcs!¬"++
        "Just going to directly translate it as a Murphi function call"++
        s!"Function 'name': ({qualified_name}), Args: ({lst_expr})"
      dbg_trace msg
      -- throw msg
      -- Default case, just translate directly to Murphi
      -- ex. is_head() in DSL is is_head() in Murphi
      let murphi_func_id :=
        match qualified_name with
        | QualifiedName.mk lst_idents =>
          String.join lst_idents

      let lst_expr_trans_info :=
        lst_expr.map (
          λ expr =>
            assn_term_to_expr_translation_info term_trans_info expr
        )

      let murphi_expr := Murϕ.Expr.call
        murphi_func_id (lst_expr_trans_info.map ast_expr_to_murphi_expr)
        -- murphi_func_id (lst_expr.map ast_expr_to_murphi_expr)
      murphi_expr
    
  | Term.const const' => -- const is a keyword..
    match const' with
    | Const.num_lit num =>
      -- build an int lit in murphi
      Murϕ.Expr.integerConst num
    | Const.str_lit str =>
      dbg_trace "String literal in DSL found!"
      dbg_trace "Strings don't do anything in Murphi?"
      dbg_trace "WARNING: somehow encountered a string"
      dbg_trace "I'm just going to translate it into int 0"
      -- TODO For Later:
      -- Detect this case somehow ahead of time and ignore
      -- this....
      Murϕ.Expr.designator (Murϕ.Designator.mk str [])
      -- TODO: handle this case. Should be more or less (if it was done with a monad :D)
   --| Term.expr exp => -- ast_expr_to_murphi_expr exp
  | Term.expr expr =>
    let expr_trans_info := assn_term_to_expr_translation_info term_trans_info expr
    let pipeline_expr := ast_expr_to_murphi_expr expr_trans_info
    pipeline_expr
  -- panic! "unimplemented case (nested terms/expressions)"
  | Term.relative_entry Direction.Next _ =>
    dbg_trace "Future Stuff (next<>). Should throw an error here as wel...."
    default
  | Term.relative_entry Direction.Previous _ =>
    dbg_trace "Future Stuff (prev<>). Should throw an error here as wel...."
    default


--===== Helper func, DSL Expr to Murphi Expr. =====
partial def ast_binop_to_murphi_binop_expr
(term1 : Pipeline.Term)
(term2 : Pipeline.Term)
(op : Identifier)
(expr_trans_info : expr_translation_info)
: Murϕ.Expr
:=
  let term1_trans_info :=
    assn_expr_to_term_translation_info expr_trans_info term1
  let murphi_term1 := 
    ast_term_to_murphi_expr term1_trans_info
  let term2_trans_info :=
    assn_expr_to_term_translation_info expr_trans_info term2
  let murphi_term2 := 
    ast_term_to_murphi_expr term2_trans_info
  let murphi_expr :=
    Murϕ.Expr.binop op murphi_term1 murphi_term2
  murphi_expr
  
partial def ast_expr_to_murphi_expr
-- ( expr : Pipeline.Expr )
(expr_trans_info : expr_translation_info)
:=
  let expr := expr_trans_info.expr
  let lst_ctrlers := expr_trans_info.lst_ctrlers
  let ctrler_name := expr_trans_info.ctrler_name

  let src_ctrler := expr_trans_info.src_ctrler
  let lst_src_args := expr_trans_info.lst_src_args

  dbg_trace s!"Translating DSL Expr: ({expr})"

  -- match expr to some DSL expr
  match expr with
  | Pipeline.Expr.add term1 term2 =>
    let murphi_add_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "+" expr_trans_info
    murphi_add_expr
  -- Put this in the catch-all _ case,
  -- since this is really more of a description
  -- thing...
  -- | Pipeline.Expr.list lst_expr =>
  --   Murϕ.Expr.integerConst 0

  | Pipeline.Expr.some_term term =>
    let term_trans_info :=
      assn_expr_to_term_translation_info expr_trans_info term
    let ret_val :=
    ast_term_to_murphi_expr term_trans_info
    dbg_trace s!"DSL Expr -> Term: ({term})"

    dbg_trace s!"DSL Expr -> Term Murphi: ({ret_val})"
    ret_val

  | Pipeline.Expr.not_equal term1 term2 =>
    let murphi_not_equal_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "!=" expr_trans_info

    murphi_not_equal_expr

  | Pipeline.Expr.equal term1 term2 =>
    let murphi_equal_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "=" expr_trans_info

    murphi_equal_expr

  | Pipeline.Expr.geq term1 term2 =>
    let murphi_greater_equal_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 ">=" expr_trans_info

    murphi_greater_equal_expr

  | Pipeline.Expr.leq term1 term2 =>
    let murphi_less_equal_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "<=" expr_trans_info

    murphi_less_equal_expr

  | Pipeline.Expr.less_than term1 term2 =>
    let murphi_less_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "<" expr_trans_info

    murphi_less_expr

  | Pipeline.Expr.greater_than term1 term2 =>
    let murphi_greater_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 ">" expr_trans_info

    murphi_greater_expr

  | Pipeline.Expr.div term1 term2 =>
    let murphi_div_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "/" expr_trans_info

    murphi_div_expr

  | Pipeline.Expr.mul term1 term2 =>
    let murphi_mul_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "*" expr_trans_info

    murphi_mul_expr

  | Pipeline.Expr.sub term1 term2 =>
    let murphi_sub_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "-" expr_trans_info

    murphi_sub_expr

  | Pipeline.Expr.binand term1 term2 =>
    let murphi_sub_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "&" expr_trans_info

    murphi_sub_expr

  | Pipeline.Expr.binor term1 term2 =>
    let murphi_sub_expr :=
      ast_binop_to_murphi_binop_expr term1 term2 "|" expr_trans_info

    murphi_sub_expr

-- no direct xor translation in Murphi,
-- must expand (a ^ b)
-- into ((!a) & b) | (a & (!b))
  -- | Pipeline.Expr.binxor term1 term2 =>
  --   let murphi_sub_expr :=
  --     ast_binop_to_murphi_binop_expr term1 term2 "^" expr_trans_info

  --   murphi_sub_expr
    -- Check for the function_call


  | _ =>
    dbg_trace "These things don't map to"
    dbg_trace "Murphi directly...."
    dbg_trace "So, we leave this for later..."
    dbg_trace "Since we don't even use these now..?"
    Murϕ.Expr.integerConst 0

  -- Going to ignore bit wise operations..
  -- They don't seem to have them in Murphi
  -- and I don't think we'll use them at the moment..
  -- | Pipeline.Expr.rightshift _ _
  -- | Pipeline.Expr.leftshift _ _
  -- | Pipeline.Expr.binxor _ _

-- ========= Helper Function ==========
partial def recursive_await_when_search
(lst_stmts : List Pipeline.Statement)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: (List Pipeline.Statement)
:=
  dbg_trace s!"Recursive await-when search stmts: ({lst_stmts})"
  let lst_of_lst_stmts :=
  lst_stmts.map (
    λ stmt =>
      match stmt with
      | Statement.await _ lst_stmts =>
        let ret_val :=
        recursive_await_when_search lst_stmts func_name curr_ctrler_name
        -- needed to do this explicitly so
        -- Lean4 will type check :D
        ret_val
      | Statement.when qual_name /- lst_ident -/ _ /- stmt' -/ _ =>
        -- stmt is the block of code, of course
        -- lst_ident would be the arguments of the when stmt
        -- qual_name should be the structure name and func
        -- If we've found one with a matching
        -- (a) func name and
        -- (b) dest struct name
        -- then we can return this in a list form
        match qual_name with
        | QualifiedName.mk lst_ident =>
          let contains_func := lst_ident.contains func_name
          let contains_ctrler := lst_ident.contains curr_ctrler_name
          let contains_func_from_ctrler :=
          and contains_func contains_ctrler
          dbg_trace s!"When, list_ident: ({lst_ident})"++
          s!"\ncontains_func: ({contains_func}), func_name: ({func_name})"++
          s!"\ncontains_ctrler: ({contains_ctrler}), curr_ctrler_name: ({curr_ctrler_name})"

          if contains_func_from_ctrler
          then
            [stmt]
          else
            []
      -- AZ NOTE:
      -- just a thought...
      -- is there any case where there are multiple 
      -- await-when statments for one function call from
      -- another structure?
      -- Maybe if there's an if statement,
      -- but in this case, we can say the developer should
      -- put the if-statement inside the await-when, not outside

      -- AZ NOTE: the rest of these cases are just
      -- cases of nesting, to recursively search
      -- for the await-when
      | Statement.block lst =>
        let ret_val :=
        recursive_await_when_search lst func_name curr_ctrler_name
        ret_val
      | Statement.conditional_stmt cond => 
        match cond with
        | Conditional.if_else_statement expr stmt1 stmt2 =>
          let ret_val :=
          recursive_await_when_search [stmt1, stmt2] func_name curr_ctrler_name 
          ret_val
        | Conditional.if_statement expr stmt =>
          let ret_val :=
          recursive_await_when_search [stmt] func_name curr_ctrler_name 
          ret_val
      | Statement.listen_handle stmt lst => 
        let ret_val :=
        recursive_await_when_search [stmt] func_name curr_ctrler_name
        ret_val
      | _ => []
  )
  let lst_of_stmts := List.join lst_of_lst_stmts
  lst_of_stmts

partial def find_when_and_state_name_from_transition
(trans_list : List Pipeline.Description)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: Pipeline.Statement × StateName
:=
  let when_with_matching_func_and_src_ctrler_name :=
  trans_list.map (
    λ trans =>
      -- get the transition stmts, find stmts
      -- which 
      match trans with
      | Description.state ident stmt =>
        let ident : StateName := ident
        match stmt with
        | Statement.block lst_stmts =>
          dbg_trace s!"first When stmt from find when stmt: ({stmt})"
          let when_blks := recursive_await_when_search lst_stmts func_name curr_ctrler_name
          let when_blks_and_idents := ZipWithList when_blks ident
          when_blks_and_idents
        | _ => dbg_trace "stmt under transition should be blk!"
          []
      | _ => dbg_trace "wasn't passed a transition?"
        []
  )
  let when_stmts_lst := List.join when_with_matching_func_and_src_ctrler_name
  let when_stmt :=
  match when_stmts_lst with
  | [one_stmt] => one_stmt
  | _::_ =>
  dbg_trace "found multiple matching when stmts?"
  default
  | [] =>
  dbg_trace "found no matching when stmts?"
  default

  when_stmt

partial def find_when_from_transition
(trans_list : List Pipeline.Description)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: Pipeline.Statement
:=
  let when_with_matching_func_and_src_ctrler_name :=
  trans_list.map (
    λ trans =>
      -- get the transition stmts, find stmts
      -- which 
      match trans with
      | Description.state state_name stmt =>
        match stmt with
        | Statement.block lst_stmts =>
          dbg_trace s!"first When stmt from find when state_name: ({state_name}) stmt: ({stmt})"
          let when_blk :=
          recursive_await_when_search lst_stmts func_name curr_ctrler_name
          when_blk
        | _ => dbg_trace "stmt under transition should be blk!"
          []
      | _ => dbg_trace "wasn't passed a transition?"
        []
  )
  let when_stmts_lst := List.join when_with_matching_func_and_src_ctrler_name
  let when_stmt :=
  match when_stmts_lst with
  | [one_stmt] => one_stmt
  | h::t =>
  dbg_trace "found multiple matching when stmts?"
  default
  | [] =>
  dbg_trace "found no matching when stmts?"
  default

  when_stmt

partial def find_when_from_transition?
(trans_list : List Pipeline.Description)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: Option Pipeline.Statement
:=
  let when_with_matching_func_and_src_ctrler_name :=
  trans_list.map (
    λ trans =>
      -- get the transition stmts, find stmts
      -- which 
      match trans with
      | Description.state state_name stmt =>
        match stmt with
        | Statement.block lst_stmts =>
          dbg_trace s!"first When stmt from find when state_name: ({state_name}) stmt: ({stmt})"
          let when_blk :=
            recursive_await_when_search lst_stmts func_name curr_ctrler_name
          when_blk
        | _ => dbg_trace "stmt under transition should be blk!"
          []
      | _ => dbg_trace "wasn't passed a transition?"
        []
  )
  let when_stmts_lst := List.join when_with_matching_func_and_src_ctrler_name
  let when_stmt :=
    match when_stmts_lst with
    | [one_stmt] => some one_stmt
    | _::_ =>
      dbg_trace "found multiple matching when stmts?"
      default
    | [] =>
      dbg_trace "found no matching when stmts?"
      none

  when_stmt

partial def find_state_name_matching_when_msg
(trans_list : List Pipeline.Description)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: StateName
:=
  let state_name_with_matching_func_and_src_ctrler_name :=
  trans_list.map (
    λ trans =>
      -- get the transition stmts, find stmts
      -- which 
      match trans with
      | Description.state state_name stmt =>
        match stmt with
        | Statement.block lst_stmts =>
          dbg_trace s!"first When stmt from find when stmt: ({stmt})"
          let when_blk :=
          recursive_await_when_search lst_stmts func_name curr_ctrler_name
          if when_blk.length > 0 then
            some state_name
          else
            none
        | _ => dbg_trace "stmt under transition should be blk!"
          none
      | _ => dbg_trace "wasn't passed a transition?"
        none
  )
  
  let no_none := state_name_with_matching_func_and_src_ctrler_name.filter (·.isSome)
  match no_none with
  | [some state_name] => state_name
  | _ :: _ => dbg_trace "found multiple matching when stmts?"
    default
  | [] => dbg_trace "found no matching when stmts?"
    default

-- partial def ast_expr_to_murphi_expr_for_a_fifo_buffer
-- (expr : Pipeline.Expr)
-- :=
--   0

---======= MAYBE I DON'T NEED THIS? ========
-- partial def ast_stmt_to_murphi_stmts_for_a_fifo_buffer
-- (stmt_and_ctrlers_lst :
-- Pipeline.Statement × (List controller_info))
-- -- ( stmt : Pipeline.Statement )
-- -- ( ctrlers_lst : List controller_info )
-- -- dest_ctrler_name
-- (dest_ctrler_name : Identifier)
-- (entry_id : Murϕ.Designator)
-- -- FIFO assume we can build
-- -- <structure>.entries[<entry_num>].<state_var>

-- -- : [Murϕ.Statement]
-- :=
--   let stmt := stmt_and_ctrlers_lst.1
--   let ctrlers_lst := stmt_and_ctrlers_lst.2

--   match stmt with
--   | Statement.labelled_statement label stmt => (
--     ast_stmt_to_murphi_stmts_for_a_fifo_buffer (
--       stmt, ctrlers_lst) dest_ctrler_name entry_id
--   )
--   | Statement.variable_declaration typed_ident =>
--     -- AZ NOTE: must ignore declarations,
--     -- since they go in the decl list,
--     -- and not the stmt list
--     []
--   | Statement.value_declaration typed_ident expr =>
--     let assned_var's_name :=
--     match typed_ident with
--     | TypedIdentifier.mk tiden ident =>
--       ident

--     let designator :=
--     Designator.mk dest_ctrler_name [
--       Sum.inl "entries",
--       Sum.inr (Murϕ.Expr.designator entry_id),
--       Sum.inl assned_var's_name
--     ]

--     -- Which murphi expr AST is the right match up? 
--     -- Must match DSL Expr to Murphi Expr.
--     -- Perhaps I should make a func for this :)
--     let murphi_expr :=
--     -- TODO: Implement this!!!!!
--       ast_expr_to_murphi_expr_for_a_fifo_buffer expr
    
--     let murphi_assignment_stmt :=
--     Murϕ.Statement.assignment designator murphi_expr

--     [murphi_assignment_stmt]
--     0

-- ===== transition & listen/handle -> murphi if stmt ======
partial def state_listen_handle_to_murphi_if_stmt
( trans_and_func : trans_and_expected_func )
( args_to_map_to : List Pipeline.Expr )
:
-- List of (if Condition, and List of if cond stmts)
List (Murϕ.Expr × lst_stmts_decls)
:=
  let trans : Pipeline.Description := trans_and_func.trans
  let expected_func : Identifier := trans_and_func.expected_func
  let expected_struct : Identifier := trans_and_func.expected_struct
  -- search for this "listen-handle stmt in the trans"
  -- this should be a 'top-level' stmt as well...

-- remember overall goal is to make a Murphi if stmt..
-- but these will return any list of murphi stmt code
-- that's in the handle block... (if there is any)

  let stmt_blk := match trans with
  | Pipeline.Description.state ident stmt =>
  dbg_trace s!"State: ({ident}), stmt block: ({stmt})"
  stmt
  | _ => dbg_trace "TODO: throw an error here!"
    default

  let stmt_lst := match stmt_blk with
  | Pipeline.Statement.block lst_stmt => lst_stmt
  | _ => dbg_trace "TODO: throw an error here!"
    default

  let listen_handle_blk_lst : List Pipeline.Statement :=
  stmt_lst.filter (
    λ stmt =>
      match stmt with
      | Pipeline.Statement.listen_handle _ _ => true
      | _ => false
  )

  let how_many_listen_handle_blks : how_many_found :=
  match listen_handle_blk_lst with
  | [_] => how_many_found.one
  | [] => how_many_found.nothing
  | _::_ => how_many_found.two_or_more

  match how_many_listen_handle_blks with
  | how_many_found.nothing =>
  -- don't need to make anything,
  -- This will just result in going to
  -- the else case for the state;
  -- which will do nothing
    []
  | how_many_found.one =>
    -- construct a murphi expr condition on the transition state
    -- and an empty statement list

    /-
    1. Check for listen-handle stmt
    1.a) check if it has a handle stmt which contains a specific
         func name/struct pair, if it does, gen the if-cond stuff
    
    2. If cond stuff is just
    2.a) A murphi cond which checks if the
         structure is on a given state/transition
    2.b) The translated stmts in the handle blk;
         Translated in the same way as a Func call
         Should have a src struct; dest struct info
    -/

    /-
    2. Check for specific handle code!
    -/
    let listen_handle : Pipeline.Statement := 
      match listen_handle_blk_lst with
      | [one] => one
      | _ => dbg_trace "shouldn't get here based on prev. enum"
        -- TODO: should throw
        default
    
    -- get the list of handle blks?
    let handle_blks : List Pipeline.HandleBlock :=
      match listen_handle with
      | Pipeline.Statement.listen_handle _ lst_handle_blks =>
        lst_handle_blks
      | _ => dbg_trace "shouldn't get here based on prev. filter func"
        -- TODO: throw
        default

    let filtered_handle_blks : List Pipeline.HandleBlock := 
      handle_blks.filter (
        λ handle_blk =>
          let qual_name : Pipeline.QualifiedName :=
          match handle_blk with
          | Pipeline.HandleBlock.mk qual_name' _ _ =>
            qual_name'
          
          let lst_qual_name_idents : List Identifier :=
          match qual_name with
          | Pipeline.QualifiedName.mk lst_idents =>
            lst_idents

          if ( and
          (lst_qual_name_idents.contains expected_func)
          (lst_qual_name_idents.contains expected_struct)
          ) then
            true
          else
            false
      )

    -- Now, if there was a match
    -- map it, if not, return []
    -- If there were 2 matches,
    -- then throw.. (TODO)

    let matching_handle_cases : how_many_found :=
    match filtered_handle_blks with
    | [] => how_many_found.nothing
    | [_] => how_many_found.one
    | _ :: _ => how_many_found.two_or_more

    match matching_handle_cases with
    | how_many_found.nothing =>
      []
    | how_many_found.one =>
      -- convert into exec code!
      let func's_handle_blk : Pipeline.HandleBlock :=
      -- filtered_handle_blks[0]!
      match filtered_handle_blks with
      | [one] => one
      | _ => dbg_trace "shouldn't reach here!"
        -- TODO: throw!
        -- make an empty one for now
        -- could probably use this for 'deriving Inhabited"
        Pipeline.HandleBlock.mk (Pipeline.QualifiedName.mk [""]) [""] (Pipeline.Statement.block [])
        

      --with the handle blk, match to access it's parts
      let (args, stmt_blk') : ( List Identifier × Pipeline.Statement ) :=
        match func's_handle_blk with
        | Pipeline.HandleBlock.mk qual_name lst_ident stmts_blk =>
          (lst_ident, stmts_blk)

      let assign_map_to_args : List Pipeline.Statement :=
        match AssignSrcToDestVars args args_to_map_to with
        | .ok arg_stmts => arg_stmts
        | .error msg =>
          dbg_trace s!"Error Assigning Message Passing Args. Message Name: ({expected_func}). Dest Ctrler: ({expected_struct}): Error Message: ({msg})"
          default

      dbg_trace s!"Message Name: ({expected_func}). Dest Ctrler: ({expected_struct}) Assigning Args: ({assign_map_to_args})"
      
      -- let stmt_blk := match stmt_blk'.pre_append_to_block assign_map_to_args with
      --   | .ok pre_appended => pre_appended
      --   | .error msg =>
      --     dbg_trace s!"Error Pre-Appending Passed Message Args. Message Name: ({expected_func}). Dest Ctrler: ({expected_struct}): Error Message: ({msg})"
      --     default

      -- dbg_trace s!"Message Name: ({expected_func}). Dest Ctrler: ({expected_struct}) Updated Stmt Blk: ({stmt_blk})"
      let parent_trans_info := trans_and_func.parent_trans_info
      
      let assign_trans_info : stmt_translation_info := {
        stmt := assign_map_to_args.to_block
        lst_ctrlers := parent_trans_info.lst_ctrlers,
        ctrler_name := parent_trans_info.ctrler_name,
        src_ctrler := parent_trans_info.src_ctrler,
        lst_src_args := parent_trans_info.lst_src_args,
        func := parent_trans_info.func
        is_await := parent_trans_info.is_await
        entry_keyword_dest := parent_trans_info.entry_keyword_dest,
        trans_obj := parent_trans_info.trans_obj,
        specific_murphi_dest_expr := parent_trans_info.specific_murphi_dest_expr,
        lst_decls := parent_trans_info.lst_decls,
        is_rhs := parent_trans_info.is_rhs,
        use_specific_dest_in_transition := parent_trans_info.use_specific_dest_in_transition
        curr_ctrler_designator_idx := parent_trans_info.curr_ctrler_designator_idx
        lhs_var_is_just_default := parent_trans_info.lhs_var_is_just_default
        translate_entry_or_ctrler := parent_trans_info.translate_entry_or_ctrler
      }
      let assign_input_args : lst_stmts_decls :=
        ast_stmt_to_murphi_stmts assign_trans_info

      -- provide the translation info;
      -- the args and stmt_blk
-- stmt : Pipeline.Statement
-- lst_ctrlers : List controller_info
-- ctrler_name : Identifier
-- -- when statement stuff
-- src_ctrler : Option Identifier
-- lst_src_args : Option (List Identifier)
-- func : Option Identifier
-- is_await : await_or_not_state
-- entry_keyword_dest : Option Identifier
-- trans_obj : Description
      let stmt_trans_info : stmt_translation_info := {
        stmt := stmt_blk',
        lst_ctrlers := trans_and_func.stmt_trans_info.lst_ctrlers,
        -- set the ctrler we're translating for to
        -- just this structure actually.
        -- so we can use the same ctrler name.
        ctrler_name := trans_and_func.stmt_trans_info.ctrler_name,
        -- is there a src ctrler?
        -- well, we still have args in the handle blk, so 
        -- we want the args to be from the "src" ctrler,
        -- i.e. ROB if doing a squash to LQ
        -- and so I assume the caller has provided this..?
        -- or I set the manually here.
        src_ctrler := trans_and_func.stmt_trans_info.src_ctrler, -- expected_struct,
        lst_src_args := match args with
        | [] => Option.none
        | _ => Option.some args
        ,
        func := expected_func,
        is_await := trans_and_func.stmt_trans_info.is_await,
        -- don't think we need this here, but...
        entry_keyword_dest := trans_and_func.dest_ctrler_name,
        trans_obj := trans_and_func.trans,
        specific_murphi_dest_expr := trans_and_func.specific_murphi_dest_expr,
        lst_decls := trans_and_func.stmt_trans_info.lst_decls,
        is_rhs := trans_and_func.stmt_trans_info.is_rhs,
        use_specific_dest_in_transition := trans_and_func.stmt_trans_info.use_specific_dest_in_transition
        curr_ctrler_designator_idx := trans_and_func.curr_ctrler_designator_idx
        lhs_var_is_just_default := trans_and_func.stmt_trans_info.lhs_var_is_just_default
        translate_entry_or_ctrler := trans_and_func.stmt_trans_info.translate_entry_or_ctrler
      }
      dbg_trace "## BEGIN THE PASSED SPECIFIC ACCESSOR"
      dbg_trace trans_and_func.specific_murphi_dest_expr
      dbg_trace "##  END THE PASSED SPECIFIC ACCESSOR"
      -- TODO NOTE: THIS MUST BE TESTED!

      let if_blk_stmts' : lst_stmts_decls := ast_stmt_to_murphi_stmts stmt_trans_info
      let if_blk_stmts : lst_stmts_decls := {
        stmts := assign_input_args.stmts ++ if_blk_stmts'.stmts
        decls := assign_input_args.decls ++ if_blk_stmts'.decls
      }

      -- Okay, this means I need to know how to index into the
      -- current entry as well!
      -- This could be something like 
      -- LQ.entries[curr_idx]
      -- Must do sth like pass it in through the struct
      -- so we don't need to re-deduce this!

      let trans_name : Identifier :=
      match trans with
      | Description.state ident stmt => ident
      | _ => dbg_trace "Shouldn't be another Description"
        default
      -- TODO NOTE: Finish this;
      -- i.e.
      -- 1. add the indexing info to the struct from the calling func!
      -- 2. finish the expr
      -- 3. make a foldl or something that will actually construct the
      -- Murphi if statement!
      -- A FUTURE TODO:
      -- once we have different structure types,
      -- the dest structure may not have entries, and we
      -- may not need the .entries part

      let ctrler_state_term := match trans_and_func.stmt_trans_info.translate_entry_or_ctrler with
        | .ctrler => [trans_and_func.stmt_trans_info.ctrler_name, DSLKeyword.state].to_term
        | .entry => ( DSLKeyword.state : Identifier ).to_term
      let state_check_expr := equal
        ctrler_state_term trans_name.to_term

      let state_check_trans_info : expr_translation_info := {
        expr := state_check_expr,
        lst_ctrlers := trans_and_func.stmt_trans_info.lst_ctrlers,
        -- set the ctrler we're translating for to
        -- just this structure actually.
        -- so we can use the same ctrler name.
        ctrler_name := trans_and_func.stmt_trans_info.ctrler_name,
        -- is there a src ctrler?
        -- well, we still have args in the handle blk, so 
        -- we want the args to be from the "src" ctrler,
        -- i.e. ROB if doing a squash to LQ
        -- and so I assume the caller has provided this..?
        -- or I set the manually here.
        src_ctrler := trans_and_func.stmt_trans_info.src_ctrler, -- expected_struct,
        lst_src_args := match args with
        | [] => Option.none
        | _ => Option.some args
        ,
        func := expected_func,
        is_await := trans_and_func.stmt_trans_info.is_await,
        -- don't think we need this here, but...
        entry_keyword_dest := trans_and_func.dest_ctrler_name,
        trans_obj := trans_and_func.trans,
        specific_murphi_dest_expr := trans_and_func.specific_murphi_dest_expr,
        lst_decls := trans_and_func.stmt_trans_info.lst_decls,
        is_rhs := trans_and_func.stmt_trans_info.is_rhs,
        use_specific_dest_in_transition := trans_and_func.stmt_trans_info.use_specific_dest_in_transition
        curr_ctrler_designator_idx := trans_and_func.curr_ctrler_designator_idx
        lhs_var_is_just_default := trans_and_func.stmt_trans_info.lhs_var_is_just_default
        translate_entry_or_ctrler := trans_and_func.stmt_trans_info.translate_entry_or_ctrler
      }

      let ctrler_name_ : String := trans_and_func.dest_ctrler_name.append "_"
      let dest_struct_entry := trans_and_func.specific_murphi_dest_expr
      let murphi_entry_is_on_state_cond : Murϕ.Expr :=
        ast_expr_to_murphi_expr state_check_trans_info
        -- match trans_and_func.stmt_trans_info.translate_entry_or_ctrler with
        -- | .entry =>
        --   [murϕ| next_state .core_[ j ] .£ctrler_name_ .entries[ £dest_struct_entry ] .state = £trans_name]
        -- | .ctrler =>
        --   [murϕ| next_state .core_[ j ] .£ctrler_name_ .state = £trans_name]

      [(murphi_entry_is_on_state_cond, if_blk_stmts)]
    | how_many_found.two_or_more =>
      -- defining the same handle case twice?
      -- should throw an error...
      default

  | how_many_found.two_or_more =>
    -- must have found more than one!
    -- TODO: Later, we should check each listen handle blk?
    -- panic! "TODO: Replace with throw... multiple listen-handles in 1 transition.."
    -- NOTE: I suppose this is unsupported 
    -- right now;
    -- since the code needs to listen specifically at
    -- specific points, but the Murphi rule/transitions
    -- are atomic, so this requires a decomposition into
    -- subsection transitions which have just 1 listen/handle
      []
  
partial def ctrler_trans_handle_stmts_to_murphi_if_stmt
(stmt_trans_info : stmt_translation_info)
(ctrler_to_translate_handle_blks : Identifier)
(queue_idx : Murϕ.Expr)
(dest_ctrler_name : Identifier)

(expected_func : Identifier)
(expected_struct : Identifier)

(murphi_when_stmts : Option (List Murϕ.Statement))
(if_in_when_state : Option Murϕ.Expr)
(args_to_map_to : List Pipeline.Expr)
(parent_trans_info : stmt_translation_info)
:
-- List Murϕ.Statement -- if stmt..
lst_stmts_decls
:=
  -- let stmt := stmt_trans_info.stmt
  let ctrlers_lst := stmt_trans_info.lst_ctrlers
  let ctrler_name := stmt_trans_info.ctrler_name

  -- when statement stuff (handling nested scopes?
  -- or rather, clojures?)

  -- let src_ctrler := stmt_trans_info.src_ctrler
  -- let lst_src_args := stmt_trans_info.lst_src_args
  -- let func_name : Option Identifier := stmt_trans_info.func

  -- search the ctrlers_lst, for the ld struct name
  let ld_ctrler_lst : List controller_info :=
  ctrlers_lst.filter (
    λ ctrler =>
    -- find the one with a matching name
    if ctrler.name == ctrler_to_translate_handle_blks
    then
      true
    else
      false
  )
  -- check for transition's and their specific
  -- listen/handle
  -- construct if / else stmt which does something
  -- based on each state's / transitions listen/await
  -- stmts
  let this_ctrler : Ctrler :=
  -- dbg_trace "===== ROB SQUASH api gen ====="
    get_ctrler_matching_name ctrler_to_translate_handle_blks ctrlers_lst

  let ctrler_squash_idx := queue_idx
  -- Murϕ.Expr.designator (Murϕ.Designator.mk "squash_ld_id" [])
  let c_type := match this_ctrler.type with
    | .ok t => t
    | .error msg =>
      dbg_trace s!"Couldn't get ctrler type in state if case translation: Msg: ({msg})"
      default
  let entry_or_ctrler_translation : entry_or_ctrler :=
    c_type.entry_or_ctrler
  let states_to_search : List Description :=
    if this_ctrler.init_trans.isSome then
      this_ctrler.transition_list.get!
    else if this_ctrler.ctrler_init_trans.isSome then
      this_ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({this_ctrler})"
        default

  -- let parent_trans_info' : stmt_translation_info := {
  --   stmt := stmt_trans_info.stmt,
  --   lst_ctrlers := stmt_trans_info.lst_ctrlers,
  --   ctrler_name := ctrler_to_translate_handle_blks,
  --   src_ctrler := expected_struct,
  --   lst_src_args := stmt_trans_info.lst_src_args,
  --   func := stmt_trans_info.func,
  --   is_await := stmt_trans_info.is_await,
  --   entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
  --   trans_obj := stmt_trans_info.trans_obj,
  --   specific_murphi_dest_expr := Option.some ctrler_squash_idx -- stmt_trans_info.specific_murphi_dest_expr,
  --   lst_decls := stmt_trans_info.lst_decls,
  --   is_rhs := stmt_trans_info.is_rhs,
  --   use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
  --   curr_ctrler_designator_idx := Option.some ctrler_squash_idx -- stmt_trans_info.curr_ctrler_designator_idx
  --   lhs_var_is_just_default := stmt_trans_info.lhs_var_is_just_default
  --   translate_entry_or_ctrler := entry_or_ctrler_translation
  -- }
  let stmt_trans_info' : stmt_translation_info := {
    stmt := stmt_trans_info.stmt,
    lst_ctrlers := stmt_trans_info.lst_ctrlers,
    ctrler_name := ctrler_to_translate_handle_blks,
    src_ctrler := expected_struct,
    lst_src_args := stmt_trans_info.lst_src_args,
    func := stmt_trans_info.func,
    is_await := stmt_trans_info.is_await,
    entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
    trans_obj := stmt_trans_info.trans_obj,
    specific_murphi_dest_expr := Option.some ctrler_squash_idx -- stmt_trans_info.specific_murphi_dest_expr,
    lst_decls := stmt_trans_info.lst_decls,
    is_rhs := stmt_trans_info.is_rhs,
    use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
    curr_ctrler_designator_idx := Option.some ctrler_squash_idx -- stmt_trans_info.curr_ctrler_designator_idx
    lhs_var_is_just_default := stmt_trans_info.lhs_var_is_just_default
    translate_entry_or_ctrler := entry_or_ctrler_translation
  }
  let handle_trans_info_lst : List trans_and_expected_func :=
  states_to_search.map (
  λ trans' =>
  {
    expected_func := expected_func,
    expected_struct := expected_struct,
    trans := trans',
    parent_trans_info := parent_trans_info
    stmt_trans_info := stmt_trans_info',
    dest_ctrler_name := ctrler_to_translate_handle_blks --dest_ctrler_name,
    specific_murphi_dest_expr := ctrler_squash_idx
    curr_ctrler_designator_idx := stmt_trans_info'.curr_ctrler_designator_idx
    -- lst_decls := stmt_trans_info.lst_decls
  }
  )

  let trans_handle_squash_list : List (Murϕ.Expr × (lst_stmts_decls)) :=
    handle_trans_info_lst.map (state_listen_handle_to_murphi_if_stmt · args_to_map_to)
    |>.join

  dbg_trace "##### BEGIN match handle block #####"
  dbg_trace ctrler_to_translate_handle_blks
  dbg_trace trans_handle_squash_list.length
  dbg_trace "##### END match handle block #####"
  -- This would be the if stmt
  -- to do handle ROB Squash signals
  let error_msg := s!"Controller is not on an expected state for a msg: ({expected_func}) from: ({expected_struct}) to: ({ctrler_name})";
  let error_if_not_on_expected_state := Murϕ.Statement.errorstmt error_msg

  let trans_handle_squash_if_stmt : lst_stmts_decls :=
  match trans_handle_squash_list with
  | [] => --dbg_trace ""
    -- Nothing found, nothing to generate?
    match murphi_when_stmts, if_in_when_state with
    | none, none =>
      empty_stmt_decl_lsts
    | some when_stmts, some if_cond_expr =>
      let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_cond_expr when_stmts [] [error_if_not_on_expected_state];
      ⟨ [ murphi_if_stmt ], [] ⟩ 
    | _, _ => dbg_trace "ERROR, when stmts and if cond expr aren't either both none or some"
      default
  | [one] => 
    let murphi_if_condition : Murϕ.Expr := one.1
    let murphi_true_cond_stmts : lst_stmts_decls := one.2

    match murphi_when_stmts, if_in_when_state with
    | none, none =>
      let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt murphi_if_condition murphi_true_cond_stmts.stmts [] [error_if_not_on_expected_state];
      ⟨ [ murphi_if_stmt ], murphi_true_cond_stmts.decls ⟩ 
    | some when_stmts, some if_cond_expr =>
      let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_cond_expr when_stmts [(murphi_if_condition, murphi_true_cond_stmts.stmts)] [error_if_not_on_expected_state];
      ⟨ [ murphi_if_stmt ], murphi_true_cond_stmts.decls ⟩ 
    | _, _ => dbg_trace "ERROR, when stmts and if cond expr aren't either both none or some"
      default
    -- let stmts_decls : lst_stmts_decls := {
    --   stmts := [
    --     [murϕ|
    --     if (£murphi_if_condition) then
    --     £murphi_true_cond_stmts.stmts
    --     endif
    --     ]
    --   ],
    --   decls := murphi_true_cond_stmts.decls
    -- }
  | h :: t => 
    let len_2_if_conds : Bool :=
    trans_handle_squash_list.length == 2
    let more_than_2_if_conds : Bool :=
    trans_handle_squash_list.length > 2

    let first_if_cond := trans_handle_squash_list[0]!
    let if_condition_0 := first_if_cond.1
    let true_cond_stmts_0 : lst_stmts_decls := first_if_cond.2

    let snd_if_cond := trans_handle_squash_list[1]!
    let if_condition_1 := snd_if_cond.1
    let true_cond_stmts_1 : lst_stmts_decls := snd_if_cond.2

    if len_2_if_conds then
      match murphi_when_stmts, if_in_when_state with
      | none, none =>
        let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_condition_0 true_cond_stmts_0.stmts [(if_condition_1, true_cond_stmts_1.stmts)] [error_if_not_on_expected_state];
        ⟨ [ murphi_if_stmt ], true_cond_stmts_0.decls ++ true_cond_stmts_1.decls⟩ 
      | some when_stmts, some if_cond_expr =>
        let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_cond_expr when_stmts [(if_condition_0, true_cond_stmts_0.stmts), (if_condition_1, true_cond_stmts_1.stmts)] [error_if_not_on_expected_state];
        ⟨ [ murphi_if_stmt ], true_cond_stmts_0.decls ++ true_cond_stmts_1.decls ⟩ 
      | _, _ => dbg_trace "ERROR, when stmts and if cond expr aren't either both none or some"
        default
      -- let stmts_decls : lst_stmts_decls := {
      --   stmts := [
      --     [murϕ|
      --     if (£if_condition_0) then
      --       £true_cond_stmts_0.stmts
      --     elsif (£if_condition_1) then
      --       £true_cond_stmts_1.stmts
      --     -- else
      --       -- actually, could be on other states,
      --       -- so don't error here!
      --       -- error "Unexpected case in this transition";
      --     endif
      --     ]
      --   ],
      --   decls := true_cond_stmts_0.decls ++ true_cond_stmts_1.decls
      -- }
      -- stmts_decls
    else
    if more_than_2_if_conds then
      let stmts_form : List (Murϕ.Expr × List Murϕ.Statement) :=
        t.map (
          λ tuple =>
            let murphi_expr := tuple.1
            let murphi_stmts := tuple.2.stmts
            (murphi_expr, murphi_stmts)
        )
      let murphi_if_stmt : Murϕ.Statement :=
      Murϕ.Statement.ifstmt if_condition_0 true_cond_stmts_0.stmts stmts_form []

      let decls : List Murϕ.Decl := List.join (
        t.map (
          λ tuple =>
            let murphi_decls := tuple.2.decls
            murphi_decls
        )
      )

      match murphi_when_stmts, if_in_when_state with
      | none, none =>
        let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_condition_0 true_cond_stmts_0.stmts stmts_form [error_if_not_on_expected_state];
        ⟨ [ murphi_if_stmt ], true_cond_stmts_0.decls ++ decls⟩ 
      | some when_stmts, some if_cond_expr =>
        let murphi_if_stmt : Murϕ.Statement := Murϕ.Statement.ifstmt if_cond_expr when_stmts (stmts_form ++ [(if_condition_0, true_cond_stmts_0.stmts)] ) [error_if_not_on_expected_state];
        ⟨ [ murphi_if_stmt ], true_cond_stmts_0.decls ++ decls ⟩ 
      | _, _ => dbg_trace "ERROR, when stmts and if cond expr aren't either both none or some"
        default
      -- let stmts_decls : lst_stmts_decls := {
      --   stmts := [murphi_if_stmt],
      --   decls := true_cond_stmts_0.decls ++ decls
      -- }
      -- stmts_decls
    else
      panic! "Should have caught other cases before this one?"
  
  trans_handle_squash_if_stmt

partial def get_ctrler_first_state -- get that "await_creation" state
(ctrler_name : String)
(ctrlers : List controller_info)
: String -- first state name
:=
  let dest_ctrler : controller_info :=
    get_ctrler_matching_name ctrler_name ctrlers

  let entry_or_ctrler_translation : entry_or_ctrler :=
    if dest_ctrler.init_trans.isSome then
      entry_or_ctrler.entry
    else if dest_ctrler.ctrler_init_trans.isSome then
      entry_or_ctrler.ctrler
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
        default
  let states_to_search : List Description :=
    if dest_ctrler.init_trans.isSome then
      dest_ctrler.transition_list.get!
    else if dest_ctrler.ctrler_init_trans.isSome then
      dest_ctrler.ctrler_trans_list.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
        default
  let init_transition : String :=
    if dest_ctrler.init_trans.isSome then
      dest_ctrler.init_trans.get!
    else if dest_ctrler.ctrler_init_trans.isSome then
      dest_ctrler.ctrler_init_trans.get!
    else
      dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
        default
  -- let init_state_stmt : Pipeline.Statement := 
    -- get_transition_stmt dest_ctrler.init_trans
  let initialization_state_list : List Description :=
    states_to_search.filter (λ state : Description =>
      match state with
      | Description.state name stmt => name == init_transition
      | _ => false
    )
  let initialization_state : Description :=
    match initialization_state_list with
    | [init_state] => init_state
    | [] =>
      let msg : String := "Didn't find any initial state in the state list" ++
      s!"\nIn init state: ({dest_ctrler.init_trans})\nState List: ({dest_ctrler.init_trans})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
    | _ :: _ => 
      let msg : String := "Found multiple initial states in the state list" ++
      s!"\nIn init state: ({dest_ctrler.init_trans})\nState List: ({dest_ctrler.init_trans})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
  let initialization_state_stmt : Pipeline.Statement :=
  match initialization_state with
    | .state _ stmt => stmt
    | _ =>
    let msg : String := "Somehow got a Pipeline.Description that isn't a state" ++
    s!"\nIn state: ({initialization_state})\nState List: ({dest_ctrler.init_trans})"
    -- throw msg
    dbg_trace s!"({msg})"
    default

  let first_state_name_list : List String := 
    get_stmts_with_transitions initialization_state_stmt
  let first_state_name : String :=
    match first_state_name_list with
    | [first_state] => first_state
    | [] =>
      let msg : String := "Didn't find any initial state in the init state" ++
      s!"\nIn init state: ({dest_ctrler.init_trans})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
    | _ :: _ => 
      let msg : String := "Found multiple initial states in the init state" ++
      s!"\nIn init state: ({dest_ctrler.init_trans})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
  let dest_ctrler_'await_insert'_state : String := first_state_name

  /-
  2. The matching when block in the other structure
  -/
  -- Get the state first...
  let first_state_list : List Pipeline.Description :=
    states_to_search.filter ( λ state : Description =>
      match state with
      | Description.state name stmt => name == first_state_name
      | _ => false
      )
  let first_state : Pipeline.Description := match first_state_list with
  | [state] => state
  | [] => 
      let msg : String := "Didn't find the first state in the state list" ++
      s!"\nFirst state: ({first_state_name})\nState List: ({dest_ctrler.transition_list})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
  | _ :: _ => 
      let msg : String := "Found multiple first state in the state list" ++
      s!"\nFirst state: ({first_state_name})\nState List: ({dest_ctrler.transition_list})"
      -- throw msg
      dbg_trace s!"({msg})"
      default
  
  match first_state with
  | Description.state name _ => name
  | _ =>
    let msg : String := "Shouldn't have found a Description that isn't a state\n"++
    s!"first_state: ({first_state})"
    -- throw msg
    dbg_trace s!"({msg})"
    default

partial def ast_stmt_stray_expr_to_murphi_expr
(stmt_trans_info : stmt_translation_info)
-- (expr : Pipeline.Expr)
-- (lst_ctrlers : List controller_info)
-- (curr_ctrler_name : Identifier) -- "string"

  -- When statement stuff
-- (src_ctrler : Option Identifier)
-- (lst_src_args : Option List Identifier)
:
-- List Murϕ.Statement
lst_stmts_decls
:=
  let stmt := stmt_trans_info.stmt
  let ctrlers_lst := stmt_trans_info.lst_ctrlers
  let ctrler_name := stmt_trans_info.ctrler_name

  -- when statement stuff (handling nested scopes?
  -- or rather, clojures?)
  let src_ctrler := stmt_trans_info.src_ctrler
  let lst_src_args := stmt_trans_info.lst_src_args
  let func_name : Option Identifier := stmt_trans_info.func
  -- If it isn't a term -> func call, with
  -- 2 qualified param names, then we can just call 
  -- the ast_expr_to_murphi_expr actually!
  let is_await := stmt_trans_info.is_await
    dbg_trace "***** BEGIN stmt, stray_expr *****"
    dbg_trace stmt
    dbg_trace "***** END stmt, stray expr *****"
  match stmt with
  | Statement.stray_expr expr =>
    match expr with
    -- I don't want to bother with nested scopes
    -- where "insert" -> When -> "insert/API"
    -- scopes need to be considered
    -- Thus, flatten things
    | Pipeline.Expr.some_term term =>
      match term with
      | Pipeline.Term.function_call qual_name lst_expr =>
        -- This means we found a structure func call!!
        let qual_name_list :=
          match qual_name with
          | QualifiedName.mk lst_idents =>
          lst_idents
        let qual_name_len   := qual_name_list.length

        let len_1_qual_name := qual_name_len == 1
        let len_2_qual_name := qual_name_len == 2
        let len_more_than_2_qual_name : Bool
                            := qual_name_len > 2

        dbg_trace "***** BEGIN stray_expr, qual_name *****"
        dbg_trace qual_name_list
        dbg_trace "*** expr args"
        dbg_trace lst_expr
        dbg_trace "***** END stray_expr *****"

              -- Define some stuff I use later....
              -- too many branching paths...
              let entries := "entries"
              let ruleset_entry_elem_idx := "i"
              let entry_idx_designator :=
              Murϕ.Expr.designator (
                Designator.mk ruleset_entry_elem_idx []
              )
              let ruleset_core_elem_idx := "j"
              let core_idx_designator :=
              Murϕ.Expr.designator (
                Designator.mk ruleset_core_elem_idx []
              )
        -- if equal to 2, then put handle the mapping
        if len_2_qual_name
        then
          -- read the name, check what the
          -- 1. Dest structure is
          -- 2. the function call API
          dbg_trace "== this was len 2 qual name check'd =="
          let dest_ctrler_name := qual_name_list[0]!
          let api_func_name := qual_name_list[1]!

          if (and (api_func_name == "send_load_request")
          (dest_ctrler_name == "memory_interface"))
          then

            if 2 == lst_expr.length then
              let dsl_term_phys_addr : Pipeline.Expr := lst_expr[0]!
              let dsl_term_inst_seq_num : Pipeline.Expr := lst_expr[1]!

              let phys_addr_trans_expr : expr_translation_info :=
                assn_stmt_to_expr_translation_info stmt_trans_info dsl_term_phys_addr
              let inst_seq_num_trans_expr : expr_translation_info :=
                assn_stmt_to_expr_translation_info stmt_trans_info dsl_term_inst_seq_num

              let murphi_phys_addr_expr := ast_expr_to_murphi_expr phys_addr_trans_expr
              let murphi_inst_seq_num_trans_expr := ast_expr_to_murphi_expr inst_seq_num_trans_expr

              let assn_out_msg : List Murϕ.Statement := [murϕ|
                next_state .core_[ j ] .mem_interface_ .out_msg .addr := £murphi_phys_addr_expr;
                next_state .core_[ j ] .mem_interface_ .out_msg .r_w := read;
                next_state .core_[ j ] .mem_interface_ .out_msg .valid := true;
                next_state .core_[ j ] .mem_interface_ .out_msg .dest := mem;
                next_state .core_[ j ] .mem_interface_ .out_msg .dest_id := j;
                next_state .core_[ j ] .mem_interface_ .out_msg .seq_num := £murphi_inst_seq_num_trans_expr;
                next_state .core_[ j ] .mem_interface_ .out_busy := true;
              ]
              dbg_trace s!"memory_interface->send_load_request() API: ({assn_out_msg})"

              let stmts_decls : lst_stmts_decls := {
                stmts := assn_out_msg,
                decls := []
              }
              stmts_decls
            else
              dbg_trace s!"Error, send_load_request API expr args list is not = 2? Args: ({lst_expr})"
              panic! s!"Error, send_load_request API expr args list is not = 2? Args: ({lst_expr})"
            
          else
          if (and (api_func_name == "send_store_request")
          (dest_ctrler_name == "memory_interface"))
          then

            -- let this_ctrler : controller_info :=
            -- dbg_trace "===== mem_interface api gen ====="
            --   get_ctrler_matching_name ctrler_name ctrlers_lst
            -- let ctrler_ordering :=
            --   get_ctrler_elem_ordering this_ctrler

            -- -- Old stuff, maybe use this to check if the dest is a controller.
            -- let is_indexable : Bool :=
            --   IndexableCtrlerTypesStrings.contains ctrler_ordering

            -- New version
            let dsl_term_phys_addr : Pipeline.Term := Pipeline.Term.var ("phys_addr")
            let dsl_term_write_value : Pipeline.Term := Pipeline.Term.var ("write_value")
            let dsl_term_inst_seq_num : Pipeline.Term := Pipeline.Term.qualified_var (QualifiedName.mk ["instruction", "seq_num"])

            let phys_addr_trans_term : term_translation_info :=
              assn_stmt_to_term_translation_info stmt_trans_info dsl_term_phys_addr
            let write_value_trans_term : term_translation_info :=
              assn_stmt_to_term_translation_info stmt_trans_info dsl_term_write_value
            let inst_seq_num_trans_term : term_translation_info :=
              assn_stmt_to_term_translation_info stmt_trans_info dsl_term_inst_seq_num

            let murphi_phys_addr_expr := ast_term_to_murphi_expr phys_addr_trans_term
            let murphi_write_value_expr := ast_term_to_murphi_expr write_value_trans_term
            let murphi_inst_seq_num_trans_expr := ast_term_to_murphi_expr inst_seq_num_trans_term
            -- 
            let assn_out_msg : List Murϕ.Statement := [murϕ|
              next_state .core_[ j ] .mem_interface_ .out_msg .addr := £murphi_phys_addr_expr;
              next_state .core_[ j ] .mem_interface_ .out_msg .r_w := write;
              next_state .core_[ j ] .mem_interface_ .out_msg .value := £murphi_write_value_expr;
              next_state .core_[ j ] .mem_interface_ .out_msg .valid := true;
              next_state .core_[ j ] .mem_interface_ .out_msg .dest := mem;
              next_state .core_[ j ] .mem_interface_ .out_msg .dest_id := j;
              next_state .core_[ j ] .mem_interface_ .out_msg .seq_num := £murphi_inst_seq_num_trans_expr;
              next_state .core_[ j ] .mem_interface_ .out_busy := true;
            ]
            dbg_trace s!"memory_interface->send_store_request() API: ({assn_out_msg})"

            let stmts_decls : lst_stmts_decls := {
              stmts := assn_out_msg,
              decls := []
            }
            stmts_decls

          else
          if (and (api_func_name == "write")
          (dest_ctrler_name == "reg_file"))
          then
            dbg_trace "== Translating reg_file write API =="
            let write_val_expr := lst_expr[0]!
            let write_val_trans_expr : expr_translation_info :=
              assn_stmt_to_expr_translation_info stmt_trans_info write_val_expr
            let write_val_murphi_expr := ast_expr_to_murphi_expr write_val_trans_expr
            let reg_idx_expr := lst_expr[1]!
            let reg_idx_trans_expr : expr_translation_info :=
              assn_stmt_to_expr_translation_info stmt_trans_info reg_idx_expr
            let reg_idx_murphi_expr := ast_expr_to_murphi_expr reg_idx_trans_expr

            let reg_write_stmt : List Murϕ.Statement :=
            [murϕ| next_state .core_[j] .rf_ .rf[ £reg_idx_murphi_expr ] := £write_val_murphi_expr; ]

            let stmts_decls : lst_stmts_decls := {
              stmts := reg_write_stmt,
              -- AZ TODO: Have expr translation gen decls..?
              -- No, it should temporarily use next_state.<sth>
              decls := [
                -- Murϕ.Decl.var ["inst"] (Murϕ.TypeExpr.previouslyDefined "INST")
              ]
            }
            stmts_decls
          else
          if (api_func_name == "head_search_squash")
          then
            -- AZ TODO:
            -- Copy over the other setup stuff as well
            -- Specifically about translating the Condtion
            -- Expr to a Murphi Expr
            -- and the max count thing

            -- TODO: replace the lst_exprs with whatever the
            -- args list thing is here
            dbg_trace "== Translating head_search_squash API =="
            let match_cond : Pipeline.Expr := lst_expr[0]!
            let match_cond_trans_info : expr_translation_info :=  {
            expr := match_cond
            lst_ctrlers := stmt_trans_info.lst_ctrlers,
            -- The "main ctrler to translate with"
            -- is it the 
            ctrler_name := stmt_trans_info.ctrler_name,
            src_ctrler := stmt_trans_info.src_ctrler,
            lst_src_args := stmt_trans_info.lst_src_args,
            func := stmt_trans_info.func,
            is_await := stmt_trans_info.is_await,
            entry_keyword_dest := Option.some dest_ctrler_name,
            trans_obj := stmt_trans_info.trans_obj,
            specific_murphi_dest_expr := none,
            lst_decls := stmt_trans_info.lst_decls,
            is_rhs := stmt_trans_info.is_rhs,
            use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition
            curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
            lhs_var_is_just_default := false
            translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
            }

-- (
-- assn_term_to_expr_translation_info term_trans_info match_cond)

            -- The Condition Expr
            let murphi_match_cond_expr := ast_expr_to_murphi_expr match_cond_trans_info

            -- 
            let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])

            -- AZ TODO:
            -- Identify the units which store the
            -- loads and stores while they speculatively
            -- execute
            -- SPECULATIVE LOAD UNIT
            -- search for controllers which process loads
            -- have ctrler which store loads, and 
            -- has a transition to process loads
            -- let speculative_ld_unit_name := "LQ"
            let speculative_ld_unit_name := "LSQ"
            -- SPECULATIVE STORE UNIT
            -- let speculative_st_unit_name := "SQ"
            let speculative_st_unit_name := "LSQ"
              -- NOTE: Things to consider:
              -- (1) Is the ctrler/struct a queue?
              --     i.e. how do we access an in-flight load
              -- (2) Do we need to stop a speculative action?
              --     i.e. it either has a state machine or
              --     we need to signal to it not to do something

            -- AZ TODO:
            -- Filter the list of transitions for this structure
            -- Find each state's listen/handle pair
            -- Find each pair's handle case for a ROB Squash() API
            -- Call

            -- search the ctrlers_lst, for the ld struct name
            let squash_ld_id :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "squash_ld_id" [])
            let expected_func := "squash"
            let expected_struct := "ROB" -- current ctrler

            let if_stmt_trans_info : stmt_translation_info := {
              stmt := stmt_trans_info.stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := stmt_trans_info.ctrler_name,
              src_ctrler := stmt_trans_info.src_ctrler,
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
            }

            let ld_trans_handle_squash_if_stmt : lst_stmts_decls := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              if_stmt_trans_info) speculative_ld_unit_name squash_ld_id (
              dest_ctrler_name) expected_func expected_struct none none
              lst_expr stmt_trans_info
            )
            dbg_trace "===== BEGIN LQ Handle finder ====="
            dbg_trace ld_trans_handle_squash_if_stmt.stmts
            dbg_trace ld_trans_handle_squash_if_stmt.decls
            dbg_trace "===== END LQ Handle finder ====="

            let squash_st_id :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "squash_st_id" [])
            let expected_func := "squash"
            let expected_struct := "ROB"

            let st_trans_handle_squash_if_stmt : lst_stmts_decls := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              stmt_trans_info) speculative_st_unit_name squash_st_id (
              dest_ctrler_name) expected_func expected_struct none none
              lst_expr stmt_trans_info
            )

            dbg_trace "===== BEGIN SQ Handle finder ====="
            dbg_trace st_trans_handle_squash_if_stmt.stmts
            dbg_trace st_trans_handle_squash_if_stmt.decls
            dbg_trace "===== END SQ Handle finder ====="

            -- 
            let search_load_ctrler_func_call : Identifier := "search_" ++ speculative_ld_unit_name ++ "_seq_num_idx"
            let search_store_ctrler_func_call : Identifier := "search_" ++ speculative_st_unit_name ++ "_seq_num_idx"

            let dest_ctrler_name_ := String.join [dest_ctrler_name, "_"]


            let speculative_ld_unit_name_ := speculative_ld_unit_name.append "_"
            let speculative_st_unit_name_ := speculative_st_unit_name.append "_"
            -- These structures may or may not exist depending 
            -- on the μarch
            -- Thus TODO: Check if these structures exist and
            -- gen template code accordingly...

            let speculative_ld_unit_idx_type := speculative_ld_unit_name.append "_idx_t"
            let speculative_st_unit_idx_type := speculative_st_unit_name.append "_idx_t"
            let ctrler_idx_t := dest_ctrler_name.append "_idx_t"
            let decls : List Murϕ.Decl :=
              [murϕ_var_decls| var squash_ld_id : £speculative_ld_unit_idx_type; ] ++
              [murϕ_var_decls| var squash_st_id : £speculative_st_unit_idx_type; ] ++
            [
              (Murϕ.Decl.var ["loop_break"] (Murϕ.TypeExpr.previouslyDefined "boolean")),
              (Murϕ.Decl.var ["difference"] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
              (Murϕ.Decl.var ["entry_idx"] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
              (Murϕ.Decl.var ["offset"] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
              (Murϕ.Decl.var ["curr_idx"] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
              (Murϕ.Decl.var ["violating_seq_num"] (Murϕ.TypeExpr.previouslyDefined "ROB_count_t")),
              (Murϕ.Decl.var ["rob_idx"] (Murϕ.TypeExpr.previouslyDefined "ROB_idx_t")),
              (Murϕ.Decl.var ["squash_diff"] (Murϕ.TypeExpr.previouslyDefined "ROB_idx_t")),
              (Murϕ.Decl.var ["squash_offset"] (Murϕ.TypeExpr.previouslyDefined "ROB_count_t")),
              (Murϕ.Decl.var ["rob"] (Murϕ.TypeExpr.previouslyDefined "ROB")),
              (Murϕ.Decl.var ["squash_idx"] (Murϕ.TypeExpr.previouslyDefined "ROB_idx_t")),
              (Murϕ.Decl.var ["curr_rob_inst"] (Murϕ.TypeExpr.previouslyDefined "INST"))
            ]
            let overall_murphi_head_search_squash_template : List Murϕ.Statement :=
            [murϕ|
            rob := next_state .core_[j] .ROB_;
            loop_break := false;
            if next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0 then
              loop_break := true;
            endif;

            entry_idx := next_state .core_[j] .£dest_ctrler_name_ .head;
            --# (2) loop to tail searching for:
            --# if plus 1 is outside this range, this should be caught
            --# by difference check
            difference := ( ( (next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name) - 1 ) - entry_idx ) % £dest_num_entries_const_name;
            offset := 0;
            --#if (difference != 0) then
            while ( (offset <= difference) & (loop_break = false)
                    & (difference >= 0)
                  ) do
              --# do the search
              curr_idx := ( entry_idx + offset ) % £dest_num_entries_const_name;
              --# (3) a load entry that's in a state where it's
              --# already done a read AND has a matching phys addr

              -- TODO: could implement both decl & stmt generation
              -- 
              -- dest_ctrler_entry := next_state .core_[j] .£dest_ctrler_name_ .entries[curr_idx];

              -- AZ TODO:
              -- Need a way to translate it so we get a list of states which come
              -- after the memory read?
              -- Wrote down the general steps on my OneNote Drive...
              -- but save this for when i have some time... just implement a simpler
              -- version first!

              -- Commented out, but in general is similar to what
              -- we want in the end...
              -- already_read_mem := !(
              --                       ( dest_ctrler_entry.ld_state = await_fwd_check)
              --                       |
              --                       ( dest_ctrler_entry.ld_state = await_scheduled)
              --                     );
              -- phys_addr_match := dest_ctrler_entry.phys_addr = £dest_ctrler_name_ .phys_addr;

              --# (4) if match, then reset it's state to before
              --# it actually tried to check for a fwding st
              --# (don't have any inst/scoreboard squashing to do)
              -- AZ TODO NOTE:
              -- Would use the previous check for something like this..
              -- if ( already_read_mem & phys_addr_match ) then

              -- TODO NOTE:
              -- Tuesday Aug 23;
              -- 1) if this match is found, then set a flag
              -- that the loop can be exited

              -- 2) perform the ROB Squash here....
              -- I suppose ideally this would trigger
              -- a separate transition..
              -- But handle this here:
              -- a) Record this load's seq_num
              -- b) have the ROB squash each subsequent inst
              -- Either by squash by seq_num;
              -- Or it finds each specific load?
              -- c) Implement it simply in Murphi by
              -- Looping over ROB entries;
              -- finding them in a structure;
              -- and checking for listen-handle
              -- code of each transition/state
              -- and translate it into murphi to exec
              -- a large (if) switch case stmt
              if (
                £murphi_match_cond_expr
                ) then
                -- Should implement a) in 1 line...
                violating_seq_num := next_state .core_[j] .£dest_ctrler_name_ .entries[curr_idx] .instruction .seq_num;

                -- Beginning of trying to implement
                -- part b) in 2 b)
                -- TODO: Fill the inside of the if stmt with
                -- part c) and also implement part c)
                -- generation in the above code on 
                -- line 3001
                rob_idx := (search_rob_seq_num_idx(rob,
                                                  violating_seq_num)
                                                  -- comment out +1
                                                  -- if don't want to
                                                  -- skip this elem
                                                  -- in ROB
                                                  -- + 1
                            ) % (CORE_INST_NUM + 1);
                            -- minus 1 to get the actual tail. rob_tail is an insertion point
                squash_diff := ((rob.tail + (CORE_INST_NUM + 1) - 1) - rob_idx) % ( CORE_INST_NUM + 1);
                squash_offset := 0;
                while (
                    (squash_offset <= squash_diff)
                    &
                    -- also if equal to 0, so it also resets the last elem as well..
                    -- remember to have stores handle squash signals as well..
                    (squash_diff >= 0)
                  ) do
                  --# squash
                  squash_idx := (rob_idx + squash_offset) % (CORE_INST_NUM + 1);
                  --# Check if Ld or St
                  curr_rob_inst := rob .entries[squash_idx] .instruction;

                  -- squash eitehr a ld or st
                  -- inst would have op field
                  -- NOTE: Assumption is that
                  -- if the inst is in the ROB
                  -- it's in-flight / speculative
                  if (curr_rob_inst.op = ld) then
                    -- NOTE: As a part of part b)
                    -- identify the unit which stores in-flight
                    -- insts
                    -- But maybe do a simpler version for now
                    --# if ld, then copy above
                    -- TODO: Auto gen the search func per queue struct we generate...
                    squash_ld_id := £search_load_ctrler_func_call(next_state .core_[j] .£speculative_ld_unit_name_ , curr_rob_inst.seq_num);

                    £ld_trans_handle_squash_if_stmt.stmts
                  elsif (curr_rob_inst.op = st) then
                    --#
                    squash_st_id := £search_store_ctrler_func_call(next_state .core_[j] .£speculative_st_unit_name_, curr_rob_inst.seq_num);

                    £st_trans_handle_squash_if_stmt.stmts
                  else
                    -- in the future, handle other inst types
                    error "inst should be either ld or st";
                  endif;

                  squash_offset := squash_offset + 1;
                end;


                --# don't bother doing any sophisticated rollback
                --# or squashing for now
                --# UPDATE STATE

                -- next_state .core_[j] .£dest_ctrler_name .entries[curr_idx] := dest_ctrler_entry;

                --# reset other entries after, checking the ROB
                --# get this entry's index in the ROB,
                --# and iterate to the tail of the ROB.
                --#put "INIT ROB_IDX SET:\n";
                --#put ld_entry.instruction.seq_num;

                --# NOTE IMPORTANT! exit from loop!
                loop_break := true;
              endif;

              -- if (offset != ( £dest_num_entries_const_name )) then
              if (offset != difference) then
                offset := offset + 1;
                -- if (( entry_idx + offset ) % £dest_num_entries_const_name) = next_state .core_[j] .£dest_ctrler_name_ .tail then
                --   --#
                --   loop_break := true;
                -- endif;
              else
                loop_break := true;
              endif;
            end;

            next_state .core_[j] .ROB_ := rob;
            ]
            -- []
            let stmts_decls : lst_stmts_decls := {
              stmts := overall_murphi_head_search_squash_template,
              decls := (decls ++
              ld_trans_handle_squash_if_stmt.decls ++ 
              st_trans_handle_squash_if_stmt.decls)
            }
            stmts_decls
          else
          if (and (api_func_name == "set_executed")
          ((dest_ctrler_name == "rob") || (dest_ctrler_name == "ROB")))
          then
            -- TODO: Handle this case!!!
            -- In the final version which also
            -- generates the ROB, we do the same thing as
            -- the "insert" api code and search for the
            -- dest ctrler (ROB) & use it's when code
            -- But for now, we can just use a template..
            let ctrler_name_ : String := ctrler_name.append "_"

            let curr_ctrler_idx_isSome : Bool := stmt_trans_info.curr_ctrler_designator_idx.isSome
            let specific_murphi_idx_isSome : Bool := stmt_trans_info.specific_murphi_dest_expr.isSome
            let (designator_idx, assigned_var_entry) : ( Option Murϕ.Expr ) × tail_or_entry :=
              if stmt_trans_info.src_ctrler.isSome then
                dbg_trace "src_ctrler is some! ({stmt_trans_info.src_ctrler})"
                if stmt_trans_info.src_ctrler.get! == stmt_trans_info.ctrler_name then
                  dbg_trace "Ctrler & Src ctrler are the same! in Assignment stmt translation"
                  if curr_ctrler_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
                    dbg_trace "Assign curr_ctrler_desig_idx"
                    dbg_trace "curr_ctrler_desig_idx: ({stmt_trans_info.curr_ctrler_designator_idx})"
                    ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
                  else if specific_murphi_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
                    dbg_trace "Assign specific_murphi_dest_expr"
                    dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
                    ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
                  else
                    dbg_trace "Use none for designator idx"
                    (Option.none, tail_or_entry.entry)
                else
                  dbg_trace "Ctrler & Src ctrler are the different! in Assignment stmt translation"
                  dbg_trace "Thus, use curr ctrler_designator since it's the lhs!"
                  if curr_ctrler_idx_isSome then
                    ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
                  else
                    (Option.none, tail_or_entry.entry)
              else
                dbg_trace "No src_ctrler, so just use specific murphi dest expr"
                dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
                if specific_murphi_idx_isSome then
                  ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
                else
                  (Option.none, tail_or_entry.entry)
            let designator : Murϕ.Expr := if designator_idx.isSome then
              designator_idx.get!
              else
              [murϕ| i]
            let set_exec_template : List Murϕ.Statement :=
            [murϕ|
              -- set the is_executed bool in the ROB
              -- AZ NOTE: This leads to a
              -- subtle bug when chained together
              -- with other API templates;
              -- i.e. it overwrites any changes...
              -- rob := Sta.core_[j].rob_;
              -- rob := Sta.core_[j].rob_;
              rob := next_state.core_[j].ROB_;

              --# process msg
              rob_id := search_ROB_seq_num_idx(rob,
                        next_state .core_[j] .£ctrler_name_ .entries[£designator] .instruction .seq_num);
              assert (rob .entries[rob_id].is_executed = false) "why isn't it false?";
              rob .entries[ rob_id ] .is_executed := true;

              -- rob .valid_access_msg := false;

              next_state .core_[j] .ROB_ := rob;
            ]
            let set_exec_decls : List Murϕ.Decl := [
              (Murϕ.Decl.var ["rob"] (Murϕ.TypeExpr.previouslyDefined "ROB") ),
              -- (Murϕ.Decl.var ["rob_id"] (Murϕ.TypeExpr.previouslyDefined "inst_idx_t") )
              (Murϕ.Decl.var ["rob_id"] (Murϕ.TypeExpr.previouslyDefined "ROB_idx_t") )
            ]

            let stmts_decls : lst_stmts_decls := {
            stmts := set_exec_template,
            decls := set_exec_decls
            }
            stmts_decls
          else
          if (and (api_func_name == "set_unexecuted")
          ( (dest_ctrler_name == "rob") || (dest_ctrler_name == "ROB")  ))
          then
            dbg_trace "@@@@@ FOUND SET_UNEXECUTED"
            let ctrler_name_ : String := ctrler_name.append "_"

            let curr_ctrler_idx_isSome : Bool := stmt_trans_info.curr_ctrler_designator_idx.isSome
            let specific_murphi_idx_isSome : Bool := stmt_trans_info.specific_murphi_dest_expr.isSome
            let (designator_idx, assigned_var_entry) : ( Option Murϕ.Expr ) × tail_or_entry :=
              if stmt_trans_info.src_ctrler.isSome then
                dbg_trace "src_ctrler is some! ({stmt_trans_info.src_ctrler})"
                if stmt_trans_info.src_ctrler.get! == stmt_trans_info.ctrler_name then
                  dbg_trace "Ctrler & Src ctrler are the same! in Assignment stmt translation"
                  if curr_ctrler_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
                    dbg_trace "Assign curr_ctrler_desig_idx"
                    dbg_trace "curr_ctrler_desig_idx: ({stmt_trans_info.curr_ctrler_designator_idx})"
                    ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
                  else if specific_murphi_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
                    dbg_trace "Assign specific_murphi_dest_expr"
                    dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
                    ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
                  else
                    dbg_trace "Use none for designator idx"
                    (Option.none, tail_or_entry.entry)
                else
                  dbg_trace "Ctrler & Src ctrler are the different! in Assignment stmt translation"
                  dbg_trace "Thus, use curr ctrler_designator since it's the lhs!"
                  if curr_ctrler_idx_isSome then
                    ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
                  else
                    (Option.none, tail_or_entry.entry)
              else
                dbg_trace "No src_ctrler, so just use specific murphi dest expr"
                dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
                if specific_murphi_idx_isSome then
                  ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
                else
                  (Option.none, tail_or_entry.entry)
            let designator : Murϕ.Expr := if designator_idx.isSome then
              designator_idx.get!
              else
              [murϕ| i]
            let set_exec_template : List Murϕ.Statement :=
            [murϕ|
              -- set the is_executed bool in the ROB
              -- AZ NOTE: This leads to a
              -- subtle bug when chained together
              -- with other API templates;
              -- i.e. it overwrites any changes...
              -- rob := Sta.core_[j].rob_;
              rob := next_state.core_[j].ROB_;

              --# process msg
              rob_id := search_ROB_seq_num_idx(rob,
                        next_state .core_[j] .£ctrler_name_ .entries[£designator] .instruction .seq_num);
              assert (rob .entries[rob_id] .is_executed = true) "why isn't it true?";
              -- rob .is_executed[rob_id] := false;
              rob .entries[rob_id] .is_executed := false;

              -- rob .valid_access_msg := false;

              next_state .core_[j] .ROB_ := rob;
            ]
            let set_exec_decls : List Murϕ.Decl := [
              (Murϕ.Decl.var ["rob"] (Murϕ.TypeExpr.previouslyDefined "ROB") ),
              -- (Murϕ.Decl.var ["rob_id"] (Murϕ.TypeExpr.previouslyDefined "inst_idx_t") )
              (Murϕ.Decl.var ["rob_id"] (Murϕ.TypeExpr.previouslyDefined "ROB_idx_t") )
            ]

            let stmts_decls : lst_stmts_decls := {
            stmts := set_exec_template,
            decls := set_exec_decls
            }
            stmts_decls
          else
          if (and (api_func_name == "write")
          (dest_ctrler_name == "reg_file"))
          then
-- structure expr_translation_info where
-- expr : Pipeline.Expr
-- lst_ctrlers : List controller_info
-- ctrler_name : Identifier
-- -- when statement stuff
-- src_ctrler : Option Identifier
-- lst_src_args : Option (List Identifier)
-- func : Option Identifier
-- is_await : await_or_not_state
            dbg_trace "THIS CASE IS REACHED.."

            -- dbg_trace "== Assuming there's a dest_reg and write_val expr =="
            let dest_reg_expr_trans_info : expr_translation_info :=
            assn_stmt_to_expr_translation_info stmt_trans_info (lst_expr[0]!)
            let dest_reg_expr := ast_expr_to_murphi_expr dest_reg_expr_trans_info

            let write_val_expr_trans_info : expr_translation_info :=
            assn_stmt_to_expr_translation_info stmt_trans_info (lst_expr[1]!)

            let write_val_expr := ast_expr_to_murphi_expr write_val_expr_trans_info

            let murphi_reg_file_assign :=
            [murϕ|
            next_state .core_[i] .rf_ .rf[ £dest_reg_expr ] := £write_val_expr ]
            -- Murϕ.Statement.assignment murphi_designator write_val_expr

            -- Then build the expr, maybe with the metaprogramming
            -- environment
            -- []
            let stmts_decls : lst_stmts_decls := {
              stmts := [murphi_reg_file_assign],
              decls := []
            }
            stmts_decls
          else if ((api_func_name == "insert")) then
            -- Use a "insert" template, and add the await when from the dest ctrler
            -- in to the middle...

            /-
            1. Name of the "await insert" state
            -/
            let dest_ctrler : controller_info :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst

            -- let entry_or_ctrler_translation : entry_or_ctrler :=
            --   if dest_ctrler.init_trans.isSome then
            --     entry_or_ctrler.entry
            --   else if dest_ctrler.ctrler_init_trans.isSome then
            --     entry_or_ctrler.ctrler
            --   else
            --     dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
            --       default
            -- let states_to_search : List Description :=
            --   if dest_ctrler.init_trans.isSome then
            --     dest_ctrler.transition_list.get!
            --   else if dest_ctrler.ctrler_init_trans.isSome then
            --     dest_ctrler.ctrler_trans_list.get!
            --   else
            --     dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
            --       default

            -- let init_state_stmt : Pipeline.Statement := 
              -- get_transition_stmt dest_ctrler.init_trans
            let initialization_state_list : List Description :=
              dest_ctrler.transition_list.get!.filter (λ state : Description =>
                match state with
                | Description.state name stmt => name == dest_ctrler.init_trans
                | _ => false
              )
            let initialization_state : Description :=
              match initialization_state_list with
              | [init_state] => init_state
              | [] =>
                let msg : String := "Didn't find any initial state in the state list" ++
                s!"\nIn init state: ({dest_ctrler.init_trans})\nState List: ({dest_ctrler.init_trans})"
                -- throw msg
                dbg_trace s!"({msg})"
                default
              | _ :: _ => 
                let msg : String := "Found multiple initial states in the state list" ++
                s!"\nIn init state: ({dest_ctrler.init_trans})\nState List: ({dest_ctrler.init_trans})"
                -- throw msg
                dbg_trace s!"({msg})"
                default
            let initialization_state_stmt : Pipeline.Statement :=
            match initialization_state with
              | .state _ stmt => stmt
              | _ =>
              let msg : String := "Somehow got a Pipeline.Description that isn't a state" ++
              s!"\nIn state: ({initialization_state})\nState List: ({dest_ctrler.init_trans})"
              -- throw msg
              dbg_trace s!"({msg})"
              default

            let first_state_name_list : List String := 
              get_stmts_with_transitions initialization_state_stmt
            let first_state_name : String :=
              match first_state_name_list with
              | [first_state] => first_state
              | [] =>
                let msg : String := "Didn't find any initial state in the init state" ++
                s!"\nIn init state: ({dest_ctrler.init_trans})"
                -- throw msg
                dbg_trace s!"({msg})"
                default
              | _ :: _ => 
                let msg : String := "Found multiple initial states in the init state" ++
                s!"\nIn init state: ({dest_ctrler.init_trans})"
                -- throw msg
                dbg_trace s!"({msg})"
                default
            let dest_ctrler_'await_insert'_state : String := first_state_name

            /-
            2. The matching when block in the other structure
            -/
            -- Get the state first...
            let first_state_list : List Pipeline.Description :=
              dest_ctrler.transition_list.get!.filter ( λ state : Description =>
                match state with
                | Description.state name stmt => name == first_state_name
                | _ => false
                )
            let first_state : Pipeline.Description := match first_state_list with
            | [state] => state
            | [] => 
                let msg : String := "Didn't find the first state in the state list" ++
                s!"\nFirst state: ({first_state_name})\nState List: ({dest_ctrler.transition_list})"
                -- throw msg
                dbg_trace s!"({msg})"
                default
            | _ :: _ => 
                let msg : String := "Found multiple first state in the state list" ++
                s!"\nFirst state: ({first_state_name})\nState List: ({dest_ctrler.transition_list})"
                -- throw msg
                dbg_trace s!"({msg})"
                default

            -- Get the when-statements from the state's stmts...
            let when_stmt : Pipeline.Statement :=
              find_when_from_transition first_state_list "insert" ctrler_name
            dbg_trace s!"first_state_list: ({first_state_list})"
            dbg_trace s!"ctrler_name: ({ctrler_name})"
            dbg_trace s!"When stmt for 'insert' API: ({when_stmt})"
            -- Convert to Murphi Stmt
            let ctrler_curr_idx : String := dest_ctrler_name.append "_curr_idx"
            let murphi_dest_idx_expr : Murϕ.Expr := [murϕ| £ctrler_curr_idx]

            let actual_when_stmt : Pipeline.Statement :=
              find_when_stmt_from_transition first_state_list "insert" ctrler_name

            let when_stmt_args : List String :=
              match (actual_when_stmt.get_when_stmt_src_args) with
              | .error msg =>
                let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'
              | .ok lst_args => lst_args

            let when_stmt_src_ctrler : String :=
              match get_when_stmt_src_ctrler actual_when_stmt with
              | .ok src_ctrler => src_ctrler
              | .error msg =>
                let msg' : String := s!"Error getting when stmt src_ctrler in 'insert' API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'

            let when_stmt_trans_info : stmt_translation_info := {
              stmt := when_stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
              --            want to set this to the SQ somehow...
              src_ctrler := 
              dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
              dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
              -- stmt_trans_info.src_ctrler,
              dbg_trace s!"Translate with src ctrler: ({ctrler_name})"

              -- if stmt_trans_info.src_ctrler.isSome then
              --   stmt_trans_info.src_ctrler.get!
              -- else
              --   Option.some ctrler_name,
              if stmt_trans_info.src_ctrler.isNone then
                Option.some when_stmt_src_ctrler
              else
                if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                  Option.some stmt_trans_info.ctrler_name
                else
                  Option.some stmt_trans_info.ctrler_name
              ,
              lst_src_args :=
                if stmt_trans_info.lst_src_args.isSome then
                  stmt_trans_info.lst_src_args
                else
                  -- THe list of args from the func call
                  match when_stmt_args with
                  | [] => Option.none
                  | _ => Option.some when_stmt_args
                ,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := Option.some dest_ctrler_name,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr :=
                if stmt_trans_info.curr_ctrler_designator_idx.isSome then
                  stmt_trans_info.curr_ctrler_designator_idx
                else
                  stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx :=
                if stmt_trans_info.src_ctrler.isNone then
                  murphi_dest_idx_expr
                else
                  if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                    stmt_trans_info.curr_ctrler_designator_idx
                  else
                    Option.some murphi_dest_idx_expr
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            -- TODO: Test the translation, I suspect I may need to set the
            -- sepcific_murphi_dest_expr to this "i" index...
            let murphi_when_stmts_decls : lst_stmts_decls :=
              ast_stmt_to_murphi_stmts when_stmt_trans_info
            let murphi_when_stmt : List Murϕ.Statement :=
              murphi_when_stmts_decls.stmts
            dbg_trace s!"Translated when stmts: ({murphi_when_stmt})"

            let dest_num_entries_const_name : String := dest_ctrler_name ++ "_NUM_ENTRIES_CONST"

            let ctrler_loop_break : String := dest_ctrler_name.append "_loop_break"
            let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
            let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
            let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
            let ctrler_offset : String :=      dest_ctrler_name.append "_offset"
              -- This one is defined above
            -- let ctrler_curr_idx : String :=    /- dest_ctrler_name.append -/ "_curr_idx"

            let dsl_valid_asn_true := var_asn_var ["valid"] "true"
            let valid_to_true_trans_info : stmt_translation_info := {
              stmt := dsl_valid_asn_true,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name,
              src_ctrler := 
                if stmt_trans_info.src_ctrler.isNone then
                  Option.some when_stmt_src_ctrler
                else
                  if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                    Option.some stmt_trans_info.ctrler_name
                  else
                    Option.some stmt_trans_info.ctrler_name
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := Option.some dest_ctrler_name,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := 
                if stmt_trans_info.curr_ctrler_designator_idx.isSome then
                  stmt_trans_info.curr_ctrler_designator_idx
                else
                  stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx :=
                if stmt_trans_info.src_ctrler.isNone then
                  murphi_dest_idx_expr
                else
                  if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                    stmt_trans_info.curr_ctrler_designator_idx
                  else
                    Option.some murphi_dest_idx_expr
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let murphi_asn_true := ast_stmt_to_murphi_stmts valid_to_true_trans_info

            let dest_ctrler_idx_t : String := dest_ctrler_name ++ "_idx_t";
            let dest_ctrler_name_ : String := dest_ctrler_name ++ "_";
            let stmts : List Murϕ.Statement := [murϕ| 

              £ctrler_loop_break := false;
              if next_state .core_[j] .£dest_ctrler_name_ .num_entries = £dest_num_entries_const_name then
                £ctrler_loop_break := true;
              endif;

              £ctrler_entry_idx := 0;
              --# (1) loop to tail searching for:
              --# if plus 0 is outside this range, this should be caught
              --# by difference check
              -- NOTE: the -1 is because tail is actually 1 more than the actual tail, so it acts as an "insert" location
              £ctrler_found_entry := false;
              -- difference := ( ( (£dest_num_entries_const_name - 1 + £dest_num_entries_const_name) - 1 ) - £ctrler_entry_idx ) % £dest_num_entries_const_name;
              -- £ctrler_difference := (£dest_num_entries_const_name - 1 );
              £ctrler_offset := 0;
              --#if (difference != -1) then
              while ( (£ctrler_offset < £dest_num_entries_const_name) & (£ctrler_loop_break = false) & (£ctrler_found_entry = false)
                      -- & (£ctrler_difference >= 0)
                    ) do
                --# do the search
                £ctrler_curr_idx := ( £ctrler_entry_idx + £ctrler_offset ) % £dest_num_entries_const_name;
                if (next_state .core_[j] .£dest_ctrler_name_ .entries[ £ctrler_curr_idx ].state = £dest_ctrler_'await_insert'_state) then
                  -- CHECKPOINT TODO: put the translated await-when block of the dest ctrler here
                  £murphi_when_stmt;
                  £murphi_asn_true.stmts;
                  £ctrler_found_entry := true;
                end;
                if (£ctrler_offset < £dest_num_entries_const_name) then
                  £ctrler_offset := £ctrler_offset + 1;
                -- else
                --   £ctrler_loop_break := true;
                endif;
              end;
              next_state .core_[j] .£dest_ctrler_name_ .num_entries := (next_state .core_[j] .£dest_ctrler_name_ .num_entries + 1);
              if (£ctrler_found_entry = false) then
                error "Couldn't find an empty entry to insert into";
              end;
              -- Can't use for loop since we can't break...
              -- for k : £dest_ctrler_idx_t do
              --   if (next_state .core[j] .£dest_ctrler_name_ .entries[ k ].state = £dest_ctrler_'await_insert'_state) then
              --     £murphi_when_stmt
              --   end;
              -- endfor;
            ]

            let ctrler_idx_t : String := dest_ctrler_name ++ "_idx_t"
            let ctrler_count_t : String := dest_ctrler_name ++ "_count_t"
            let decls : List Murϕ.Decl := 
              -- [murϕ_decl| ctrler : £dest_ctrler_name]
              -- (Murϕ.Decl.var ["rob"] (Murϕ.TypeExpr.previouslyDefined "ROB") ),
              [murϕ_var_decls| var £ctrler_loop_break : boolean;] ++
              [murϕ_var_decls| var £ctrler_entry_idx : £ctrler_idx_t] ++
              [murϕ_var_decls| var £ctrler_found_entry : boolean] ++
              -- [murϕ_var_decls| var £ctrler_difference : £ctrler_count_t] ++
              [murϕ_var_decls| var £ctrler_offset : £ctrler_count_t] ++
              [murϕ_var_decls| var £ctrler_curr_idx : £ctrler_idx_t] ++
              murphi_when_stmts_decls.decls

            let stmts_decls : lst_stmts_decls := {
              stmts := stmts,
              decls := decls ++ murphi_asn_true.decls
            }
            dbg_trace s!"SB when insert: ({murphi_when_stmts_decls})"
            dbg_trace s!"insert API call translated: ({stmts_decls})"
            stmts_decls
            -- CHECKPOINT TODO:
          else if (api_func_name == "remove_head") then
            -- remove the head entry of a controller
            let first_state_name : String := get_ctrler_first_state dest_ctrler_name ctrlers_lst;

            let dest_ctrler_ : String := dest_ctrler_name.append "_"
            let dest_ctrler_entries_const : String := dest_ctrler_name.append "_NUM_ENTRIES_CONST"

            -- let entry_or_ctrler_translation : entry_or_ctrler :=
            --   if dest_ctrler.init_trans.isSome then
            --     entry_or_ctrler.entry
            --   else if dest_ctrler.ctrler_init_trans.isSome then
            --     entry_or_ctrler.ctrler
            --   else
            --     dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
            --       default
            -- let states_to_search : List Description :=
            --   if dest_ctrler.init_trans.isSome then
            --     dest_ctrler.transition_list.get!
            --   else if dest_ctrler.ctrler_init_trans.isSome then
            --     dest_ctrler.ctrler_trans_list.get!
            --   else
            --     dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
            --       default

            let dest_ctrler : controller_info :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst
            let init_stmt_except : Except String Pipeline.Statement := get_init_state_stmts dest_ctrler.init_trans.get! dest_ctrler.transition_list.get!
            let init_stmt : Pipeline.Statement := match init_stmt_except with
            | .ok stmt => stmt
            | .error msg =>
              dbg_trace s!"Error when getting init state stmt: ({msg})"
                -- TODO: throw!
              default

            let init_stmt_without_transition_except : Except String Pipeline.Statement :=
              return_blk_without_transitions init_stmt
            let init_stmt_without_transition : Pipeline.Statement :=
            match init_stmt_without_transition_except with
            | .ok stmt => stmt
            | .error msg =>
              dbg_trace s!"Error when removing transition stmts in init blk: ({msg})"
                -- TODO: throw!
              default

            -- let init_stmt_trans_info := assn_stmt_to_stmt_translation_info stmt_trans_info init_stmt
            -- let curr_head_ : String := "curr_head_"
              dbg_trace s!"Translate remove_head() call for ctrler: ({dest_ctrler_name})"
            let murphi_expr_curr_head_ : Murϕ.Expr := [murϕ| next_state .core_[j] .£dest_ctrler_ .head]
            let init_stmt_trans_info := {
              stmt := init_stmt_without_transition,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name,
              -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
              src_ctrler := Option.some dest_ctrler_name,
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := Option.some murphi_expr_curr_head_,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
              curr_ctrler_designator_idx := Option.some murphi_expr_curr_head_
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            dbg_trace "About to init a queue's head entries!"
            let murphi_init_stmts_decls : lst_stmts_decls := ast_stmt_to_murphi_stmts init_stmt_trans_info
            dbg_trace s!"Init stmts: ({murphi_init_stmts_decls.stmts})"

            let murphi_stmts : List Murϕ.Statement := [murϕ|
              -- curr_head_ := Sta .core_[j] .£dest_ctrler_ .head;
              £murphi_init_stmts_decls.stmts;
              next_state .core_[j] .£dest_ctrler_ .entries[ £murphi_expr_curr_head_ ].state := £first_state_name;
              next_state .core_[j] .£dest_ctrler_ .head := ((£murphi_expr_curr_head_ + 1) % £dest_ctrler_entries_const);
              next_state .core_[j] .£dest_ctrler_ .num_entries := (next_state .core_[j] .£dest_ctrler_ .num_entries - 1);
            ]

            let dest_ctrler_name_idx_t : String := dest_ctrler_name.append "_idx_t"

            let murphi_decls : List Murϕ.Decl :=
              -- [murϕ_var_decl| £curr_head_ : £dest_ctrler_name_idx_t ] ++
              murphi_init_stmts_decls.decls
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmts,
              decls := murphi_decls
            }
            stmts_decls
          else if (api_func_name == "remove_key") then
            -- remove the entry matching a unique "key" / "state var" of a controller
            -- TODO: Write the murphi code to search the structure's elements,
            -- and check if the key matches
            -- Use for loop, and error if multiple hits found?

            let first_state_name : String := get_ctrler_first_state dest_ctrler_name ctrlers_lst;

            let dest_ctrler_ : String := dest_ctrler_name.append "_"
            let dest_ctrler_entries_const : String := dest_ctrler_name.append "_NUM_ENTRIES_CONST"

            let dest_ctrler : Ctrler :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst
            let init_stmt_except : Except String Pipeline.Statement := get_init_state_stmts dest_ctrler.init_trans.get! dest_ctrler.transition_list.get!
            let init_stmt : Pipeline.Statement := match init_stmt_except with
            | .ok stmt => stmt
            | .error msg =>
              dbg_trace s!"Error when getting init state stmt: ({msg})"
                -- TODO: throw!
              default

            -- let init_stmt_without_transition_except : Except String Pipeline.Statement :=
            --   return_blk_without_transitions init_stmt
            -- let init_stmt_without_transition : Pipeline.Statement :=
            -- match init_stmt_without_transition_except with
            -- | .ok stmt => stmt
            -- | .error msg =>
            --   dbg_trace s!"Error when removing transition stmts in init blk: ({msg})"
            --     -- TODO: throw!
            --   default

            --   dbg_trace s!"Translate remove_key() call for ctrler: ({dest_ctrler_name})"
            -- let init_stmt_trans_info := {
            --   stmt := init_stmt_without_transition,
            --   lst_ctrlers := stmt_trans_info.lst_ctrlers,
            --   ctrler_name := dest_ctrler_name,
            --   -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
            --   src_ctrler := Option.some dest_ctrler_name,
            --   lst_src_args := stmt_trans_info.lst_src_args,
            --   func := stmt_trans_info.func,
            --   is_await := stmt_trans_info.is_await,
            --   entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
            --   trans_obj := stmt_trans_info.trans_obj,
            --   specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr --Option.some murphi_expr_curr_head_,
            --   lst_decls := stmt_trans_info.lst_decls,
            --   is_rhs := stmt_trans_info.is_rhs,
            --   use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
            --   curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx,--Option.some murphi_expr_curr_head_
            --   lhs_var_is_just_default := false
            --   translate_entry_or_ctrler := entry_or_ctrler.entry
            -- }
            -- dbg_trace "About to init a queue's head entries!"
            -- let murphi_init_stmts_decls : lst_stmts_decls := ast_stmt_to_murphi_stmts init_stmt_trans_info
            -- dbg_trace s!"Init stmts: ({murphi_init_stmts_decls.stmts})"

            -- NOTE: Stuff I'm using to implement remove_key(key)
            -- Add key var
            let key_arg := match lst_expr with
              | key :: /- args -/ _ => key
              | _ => dbg_trace "Error: remove_key() should have at least 1 arg!"
                default
            let key_expr_trans_info : expr_translation_info := {
              expr := key_arg,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := ctrler_name, -- dest_ctrler_name,
              -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
              src_ctrler := stmt_trans_info.src_ctrler, -- Leave as src ctrler, so it uses the current ctrler's state vars if they're in scope
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr, --Option.some murphi_expr_curr_head_,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx, --Option.some murphi_expr_curr_head_
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let murphi_key_arg := ast_expr_to_murphi_expr key_expr_trans_info

            -- TODO: Find the key var from the Unordered Queue if it exists!
            let key_var := match dest_ctrler.key with
              | .ok key => key
              | .error msg =>
                dbg_trace s!"Error when getting key var: ({msg}) of ctrler: ({ctrler_name})"
                default
            let remove_dest_ctrler_idx : String := "remove_".append <| dest_ctrler_name.append "_idx"
            let remove_dest_key_murϕ_expr : Murϕ.Expr :=
              [murϕ_expr| £remove_dest_ctrler_idx]
            let key_var_expr_trans_info : expr_translation_info := {
              expr := key_var,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := ctrler_name,
              -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
              src_ctrler := some dest_ctrler_name, -- Use the dest_Ctrler, as we compare if the key from {scope/ctrler} is the dest_ctrler's key
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := some remove_dest_key_murϕ_expr, --  stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx, --Option.some murphi_expr_curr_head_
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let murphi_key_var := ast_expr_to_murphi_expr key_var_expr_trans_info

            let dest_ctrler : Ctrler :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst

            let states_to_search : List Description :=
              match dest_ctrler.states with
              | .ok states => states
              | .error msg =>
                dbg_trace s!"Error when getting states: ({msg}) of ctrler: ({dest_ctrler_name})"
                default

            dbg_trace s!"===== Begin When Info ====="
            dbg_trace s!"search for when stmt for 'arbitrary' ({api_func_name})"
            dbg_trace s!"States to search: ({states_to_search})"
            dbg_trace s!"dest_ctrler: ({dest_ctrler})"
            dbg_trace s!"curr_ctrler: ({ctrler_name})"
            dbg_trace s!"===== End When Info ====="

            let when_stmt : Pipeline.Statement :=
              -- NOTE to self: could make an option type, so when stmt is not necessary
              find_when_from_transition states_to_search api_func_name ctrler_name

            let when_stmt_args : List String :=
              -- ← when_stmt.when_stmt_arguments
              --   |>.throw
              match (when_stmt.when_stmt_arguments) with
              | .error msg =>
                let msg' : String := s!"Error getting when stmt args in 'remove_key' API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'
              | .ok lst_args => lst_args

            let entry_or_ctrler_translation : entry_or_ctrler :=
              match dest_ctrler.entry_or_ctrler_translation with
              | .ok entry_ctrler => entry_ctrler
              | .error msg =>
                dbg_trace s!"Error determining if Ctrler translation is entry or ctrler: ({msg})"
                default

            -- Convert to Murphi Stmt
            let (murphi_when_stmt_decls, murphi_when_stmt_stmts)
              : /- Option -/ (List Decl) × /- Option -/ (List Murϕ.Statement) :=
            (
              let when_stmt_trans_info : stmt_translation_info := {
                stmt := when_stmt,
                lst_ctrlers := stmt_trans_info.lst_ctrlers,
                ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
                --            want to set this to the SQ somehow...
                src_ctrler := 
                dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
                dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
                if stmt_trans_info.src_ctrler.isNone then
                  Option.some ctrler_name
                else
                  Option.some stmt_trans_info.ctrler_name,
                lst_src_args :=
                  if stmt_trans_info.lst_src_args.isSome then
                    if stmt_trans_info.lst_src_args.get!.length > 0 then
                      stmt_trans_info.lst_src_args
                    else
                      match when_stmt_args with
                      | [] => Option.none
                      | _ => Option.some when_stmt_args
                  else
                    -- THe list of args from the func call
                    match when_stmt_args with
                    | [] => Option.none
                    | _ => Option.some when_stmt_args
                  ,
                func := stmt_trans_info.func,
                is_await := stmt_trans_info.is_await,
                entry_keyword_dest := Option.some dest_ctrler_name,
                trans_obj := stmt_trans_info.trans_obj,
                -- Swap curr ctrler designator & specific murphi desig
                -- since we just call in the opposite order
                specific_murphi_dest_expr := remove_dest_key_murϕ_expr,
                lst_decls := stmt_trans_info.lst_decls,
                is_rhs := stmt_trans_info.is_rhs,
                -- By setting these fields, I assume we'll specifically mean to use this with 
                -- controllers with multiple elements, and thus we need to use 'tail_search'
                -- which indexes the dest with 'curr_idx'
                use_specific_dest_in_transition := true -- stmt_trans_info.use_specific_dest_in_transition
                curr_ctrler_designator_idx := remove_dest_key_murϕ_expr, -- stmt_trans_info.specific_murphi_dest_expr
                lhs_var_is_just_default := false
                translate_entry_or_ctrler := entry_or_ctrler_translation
              }
              dbg_trace s!"Arbitrary msg translation. when stmt trans info: ({when_stmt_trans_info})"
              -- TODO: Test the translation, I suspect I may need to set the
              -- sepcific_murphi_dest_expr to this "i" index...
              let murphi_when_stmts_decls : lst_stmts_decls :=
                ast_stmt_to_murphi_stmts when_stmt_trans_info
              -- let murphi_when_stmt : List Murϕ.Statement :=
              --   murphi_when_stmts_decls.stmts
              
              dbg_trace s!"Translated when stmts: ({murphi_when_stmts_decls.stmts})"
              (/- some-/ murphi_when_stmts_decls.decls, /- some -/ murphi_when_stmts_decls.stmts)
            )

            let remove_key_dest_already_found := "remove_key_dest_already_found"
            let already_found_decl := [murϕ_var_decl|  £remove_key_dest_already_found : boolean]

            let dest_ctrler_idx_t : String := dest_ctrler_name.append "_idx_t"
            -- let remove_dest_ctrler_idx : String := "remove_".append <| dest_ctrler_name.append "_idx"
            let murphi_stmts : List Murϕ.Statement := [murϕ|
              -- for each, look and check if key arg matches the entry's key state var

              £remove_key_dest_already_found := false;
              for £remove_dest_ctrler_idx : £dest_ctrler_idx_t do
                if next_state .core_[j] .£dest_ctrler_ .entries[£remove_dest_ctrler_idx].valid then
                  -- if next_state .core_[j] .£dest_ctrler_ .entries[£remove_dest_ctrler_idx].£key_var = £murphi_key_arg then
                  if £murphi_key_var = £murphi_key_arg then
                    if £remove_key_dest_already_found then
                      error "Error: Found multiple entries with same key in remove_key API func";
                    elsif ! £remove_key_dest_already_found then
                      -- remove this entry
                      next_state .core_[j] .£dest_ctrler_ .entries[£remove_dest_ctrler_idx] .valid := false;
                      next_state .core_[j] .£dest_ctrler_ .num_entries := next_state .core_[j] .£dest_ctrler_ .num_entries - 1;
                      next_state .core_[j] .£dest_ctrler_ .entries[£remove_dest_ctrler_idx] .state := £first_state_name;
                      -- £remove_key_dest_already_found := true;
                      £murphi_when_stmt_stmts
                    else
                      error "Unreachable.. Just to make Murphi metaprogramming parser parse the stmts...";
                    endif;
                  endif;
                endif;
              endfor;

              -- £murphi_when_stmts_decls.stmts;
              -- next_state .core_[j] .£dest_ctrler_ .tail := ( Sta .core_[j] .£dest_ctrler_ .tail + 1 ) % (£dest_num_entries_const);
              -- next_state .core_[j] .£dest_ctrler_ .num_entries := Sta .core_[j] .£dest_ctrler_ .num_entries + 1;
            ]

            -- let dest_ctrler_name_idx_t : String := dest_ctrler_name.append "_idx_t"

            let murphi_decls : List Murϕ.Decl :=
              -- [murϕ_var_decl| £curr_head_ : £dest_ctrler_name_idx_t ] ++
              -- murphi_init_stmts_decls.decls
              []
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmts,
              decls := already_found_decl ++ murphi_when_stmt_decls
            }
            stmts_decls
          else if (api_func_name == "insert_tail") then
            -- TODO: move murphi code here...
            let dest_ctrler_ : String := dest_ctrler_name.append "_"
            let dest_ctrler : controller_info :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst

            let entry_or_ctrler_translation : entry_or_ctrler :=
              if dest_ctrler.init_trans.isSome then
                entry_or_ctrler.entry
              else if dest_ctrler.ctrler_init_trans.isSome then
                entry_or_ctrler.ctrler
              else
                dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
                  default
            let states_to_search : List Description :=
              if dest_ctrler.init_trans.isSome then
                dest_ctrler.transition_list.get!
              else if dest_ctrler.ctrler_init_trans.isSome then
                dest_ctrler.ctrler_trans_list.get!
              else
                dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
                  default

            let when_stmt : Pipeline.Statement :=
              find_when_from_transition states_to_search "insert_tail" ctrler_name
            dbg_trace s!"dest_ctrler states: ({dest_ctrler.transition_list})"
            dbg_trace s!"ctrler_name: ({ctrler_name})"
            dbg_trace s!"When stmt for 'insert_tail' API: ({when_stmt})"
            let actual_when_stmt : Pipeline.Statement :=
              find_when_stmt_from_transition states_to_search "insert" ctrler_name

            let when_stmt_args : List String :=
              match (actual_when_stmt.get_when_stmt_src_args) with
              | .error msg =>
                let msg' : String := s!"Error getting when stmt args in 'insert_tail' API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'
              | .ok lst_args => lst_args

            let when_stmt_src_ctrler : String :=
              match get_when_stmt_src_ctrler actual_when_stmt with
              | .ok src_ctrler => src_ctrler
              | .error msg =>
                let msg' : String := s!"Error getting when stmt src_ctrler in 'insert_tail' API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'
            -- Convert to Murphi Stmt

            let murphi_dest_idx_expr : Murϕ.Expr := [murϕ| next_state .core_[j] .£dest_ctrler_ .tail]
            let when_stmt_trans_info : stmt_translation_info := {
              stmt := when_stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
              --            want to set this to the SQ somehow...
              src_ctrler := 
              -- dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
              -- dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
              -- stmt_trans_info.src_ctrler
              if stmt_trans_info.src_ctrler.isNone then
                Option.some ctrler_name
              else
                if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                  Option.some stmt_trans_info.ctrler_name
                else
                  Option.some stmt_trans_info.ctrler_name
              ,
              lst_src_args :=
                if stmt_trans_info.lst_src_args.isSome then
                  stmt_trans_info.lst_src_args
                else
                  -- THe list of args from the func call
                  match when_stmt_args with
                  | [] => Option.none
                  | _ => Option.some when_stmt_args
                ,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := Option.some dest_ctrler_name,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := murphi_dest_idx_expr
              lhs_var_is_just_default := false
              --translate_entry_or_ctrler := entry_or_ctrler_translation
              -- Insert should really be for a entry-controller ( a ctrler w/ entries)
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            dbg_trace s!"Insert_Tail translate when stmt: ({when_stmt_trans_info})"
            -- TODO: Test the translation, I suspect I may need to set the
            -- sepcific_murphi_dest_expr to this "i" index...
            let murphi_when_stmts_decls : lst_stmts_decls :=
              ast_stmt_to_murphi_stmts when_stmt_trans_info
            let murphi_when_stmt : List Murϕ.Statement :=
              murphi_when_stmts_decls.stmts
            dbg_trace s!"Translated when stmts: ({murphi_when_stmt})"

            let dest_num_entries_const : String := dest_ctrler_name.append "_NUM_ENTRIES_CONST"
            let murphi_stmts : List Murϕ.Statement := [murϕ|
              -- NOTE: assert is nice to have. maybe consider generating one later...
              -- assert curr_tail_entry .state = await_creation "to insert, load should be awaiting creation";

              -- update the state accordingly...
              £murphi_when_stmts_decls.stmts;

              --# NOTE: Auto generate the standard "insert" book keeping part
              -- insert inst is not so 'standard'
              -- curr_tail_entry .instruction := inst;
              next_state .core_[j] .£dest_ctrler_ .tail := ( next_state .core_[j] .£dest_ctrler_ .tail + 1 ) % (£dest_num_entries_const);
              next_state .core_[j] .£dest_ctrler_ .num_entries := next_state .core_[j] .£dest_ctrler_ .num_entries + 1;
            ]
            let murphi_decls : List Murϕ.Decl := [] ++ murphi_when_stmts_decls.decls
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmts,
              decls := murphi_decls
            }
            stmts_decls
          else if (api_func_name == "insert_key") then
            -- TODO: move murphi code here...
            let dest_ctrler_ : String := dest_ctrler_name.append "_"
            let dest_ctrler : Ctrler :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst

            -- let entry_or_ctrler_translation : entry_or_ctrler :=
            --   dest_ctrler.entry_or_ctrler_translation

            let states_to_search : List Description :=
              match dest_ctrler.states with
              | .ok states => states
              | .error msg =>
                dbg_trace s!"Error getting states of ctrler ({dest_ctrler_name}) in insert_key api translation: ({msg})"
                default
            let when_stmt : Pipeline.Statement :=
              find_when_from_transition states_to_search "insert_key" ctrler_name
            dbg_trace s!"dest_ctrler states: ({dest_ctrler.transition_list})"
            dbg_trace s!"ctrler_name: ({ctrler_name})"
            dbg_trace s!"When stmt for 'insert_key API: ({when_stmt})"

            let when_stmt_args : List String :=
              match (when_stmt.get_when_stmt_src_args) with
              | .error msg =>
                let msg' : String := s!"Error getting when stmt args in 'insert_key API func: ({msg})"
                dbg_trace msg'
                panic! msg'
              | .ok lst_args => lst_args
            
            let when_stmt_arg_exprs : List Pipeline.Expr :=
              match (when_stmt.get_when_stmt_src_arg_exprs) with
              | .error msg =>
                let msg' : String := s!"Error getting when stmt arg exprs in 'insert_key API func: ({msg})"
                dbg_trace msg'
                panic! msg'
              | .ok lst_arg_exprs => lst_arg_exprs
            let when_stmt_vars_matched_to_args : Pipeline.Statement :=
              match when_stmt.map_rhs_vars_src_to_dest when_stmt_arg_exprs lst_expr with
              | .ok when_stmt' => when_stmt'
              | .error msg =>
                let msg' : String := s!"Error getting when stmt vars matched to args in 'insert_key API func: ({msg})"
                dbg_trace msg'
                panic! msg'

            let when_stmt_src_ctrler : String :=
              match get_when_stmt_src_ctrler when_stmt with
              | .ok src_ctrler => src_ctrler
              | .error msg =>
                let msg' : String := s!"Error getting when stmt src_ctrler in 'insert_key API func: ({msg})"
                dbg_trace msg'
                -- default
                panic! msg'
            -- Convert to Murphi Stmt

            -- let calling_ctrler_args := sorry

            dbg_trace s!"Translate ({dest_ctrler_name}).insert_key({lst_expr}) from ({ctrler_name})"
            dbg_trace s!"when_stmt: ({when_stmt})"
            dbg_trace s!"when_stmt_vars_matched_to_args: ({when_stmt_vars_matched_to_args})"

            -- When-stmt translated for inserting into an existing key-matched entry
            let dest_ctrler_key_check_idx := dest_ctrler_name.append "_key_check_idx"
            let insert_existing_entry_murphi_dest_idx_expr : Murϕ.Expr := [murϕ| £dest_ctrler_key_check_idx]
            let insert_existing_entry_when_stmt_trans_info : stmt_translation_info := {
              stmt := when_stmt_vars_matched_to_args, -- when_stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
              --            want to set this to the SQ somehow...
              src_ctrler := 
              -- dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
              -- dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
              -- stmt_trans_info.src_ctrler
              if stmt_trans_info.src_ctrler.isNone then
                Option.some ctrler_name
              else
                if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                  Option.some stmt_trans_info.ctrler_name
                else
                  Option.some stmt_trans_info.ctrler_name
              ,
              lst_src_args :=
                if stmt_trans_info.lst_src_args.isSome then
                  stmt_trans_info.lst_src_args
                else
                  -- THe list of args from the func call
                  match when_stmt_args with
                  | [] => Option.none
                  | _ => Option.some when_stmt_args
                ,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := Option.some dest_ctrler_name,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := insert_existing_entry_murphi_dest_idx_expr
              lhs_var_is_just_default := false
              --translate_entry_or_ctrler := entry_or_ctrler_translation
              -- Insert should really be for a entry-controller ( a ctrler w/ entries)
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let insert_existing_entry_murphi_when_stmts_decls : lst_stmts_decls :=
              ast_stmt_to_murphi_stmts insert_existing_entry_when_stmt_trans_info
            let insert_existing_entry_murphi_when_stmt : List Murϕ.Statement :=
              insert_existing_entry_murphi_when_stmts_decls.stmts

            -- When-stmt translated for inserting into an empty entry
            let ctrler_curr_idx : String := dest_ctrler_name.append "_curr_idx"
            let murphi_dest_idx_expr : Murϕ.Expr := [murϕ| £ctrler_curr_idx]
            let insert_unused_entry_when_stmt_trans_info : stmt_translation_info := {
              stmt := when_stmt_vars_matched_to_args, -- when_stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
              --            want to set this to the SQ somehow...
              src_ctrler := 
              -- dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
              -- dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
              -- stmt_trans_info.src_ctrler
              if stmt_trans_info.src_ctrler.isNone then
                Option.some ctrler_name
              else
                if (stmt_trans_info.ctrler_name) != when_stmt_src_ctrler then
                  Option.some stmt_trans_info.ctrler_name
                else
                  Option.some stmt_trans_info.ctrler_name
              ,
              lst_src_args :=
                if stmt_trans_info.lst_src_args.isSome then
                  stmt_trans_info.lst_src_args
                else
                  -- THe list of args from the func call
                  match when_stmt_args with
                  | [] => Option.none
                  | _ => Option.some when_stmt_args
                ,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := Option.some dest_ctrler_name,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := murphi_dest_idx_expr
              lhs_var_is_just_default := false
              --translate_entry_or_ctrler := entry_or_ctrler_translation
              -- Insert should really be for a entry-controller ( a ctrler w/ entries)
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            dbg_trace s!"Insert_key (into unused entry) translate when stmt: ({insert_unused_entry_when_stmt_trans_info})"
            -- TODO: Test the translation, I suspect I may need to set the
            -- sepcific_murphi_dest_expr to this "i" index...
            let insert_unused_entry_murphi_when_stmts_decls : lst_stmts_decls :=
              ast_stmt_to_murphi_stmts insert_unused_entry_when_stmt_trans_info
            let insert_unused_entry_murphi_when_stmt : List Murϕ.Statement :=
              insert_unused_entry_murphi_when_stmts_decls.stmts
            dbg_trace s!"Insert_key (into unused entry) translated when stmts: ({insert_unused_entry_murphi_when_stmt})"

            -- Don't have to add var decls for these two
            let dest_ctrler_idx_t := dest_ctrler_name.append "_idx_t"
              let dest_ctrler_name_ := dest_ctrler_name.append "_"

            -- add var decls for these
            let ctrler_loop_break : String := dest_ctrler_name.append "_loop_break"
            let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
            let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
            let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
            let ctrler_offset : String :=      dest_ctrler_name.append "_offset"

            -- Could get first_state_name as 'the state of the when stmt that expects the msg' instead?  to make it easier to get?
            let first_state_name : String :=
              match dest_ctrler.init_trans_dest with
              | .ok state_name => state_name
              | .error msg =>
                let msg' : String := s!"Error getting dest_ctrler.init_trans_dest in 'insert_key API func: ({msg})"
                dbg_trace msg'
                default
                -- panic! msg'
            let dest_ctrler_'await_insert'_state : String := first_state_name

            -- TODO NOTE: Generate another version of the murphi_when_stmts_decls , where the stmts
            -- are generated with the while loop's index variable for accessing the dest_ctrler RHS

            -- let couldn't_insert_unused_error_msg := s!"Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into"

            let key_arg := match lst_expr with
              | key :: /- args -/ _ => key
              | _ => dbg_trace "Error: insert_key() should have at least 1 arg!"
                default
            let key_expr_trans_info : expr_translation_info := {
              expr := key_arg,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := ctrler_name, -- dest_ctrler_name,
              -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
              src_ctrler := stmt_trans_info.src_ctrler, -- Leave as src ctrler, so it uses the current ctrler's state vars if they're in scope
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr, --Option.some murphi_expr_curr_head_,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx, --Option.some murphi_expr_curr_head_
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let murphi_key_arg := ast_expr_to_murphi_expr key_expr_trans_info

            -- TODO: Find the key var from the Unordered Queue if it exists!
            let key_var := match dest_ctrler.key with
              | .ok key => key
              | .error msg =>
                dbg_trace s!"Error when getting key var: ({msg}) of ctrler: ({ctrler_name})"
                default
            -- let dest_ctrler_key_check_idx := dest_ctrler_name.append "_key_check_idx"
            let insert_dest_key_murϕ_expr : Murϕ.Expr :=
              [murϕ_expr| £dest_ctrler_key_check_idx]
            let key_var_expr_trans_info : expr_translation_info := {
              expr := key_var,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := ctrler_name,
              -- NOTE TO SELF: src_ctrler is used to indicate use a specific designator index name..
              src_ctrler := some dest_ctrler_name, -- Use the dest_Ctrler, as we compare if the key from {scope/ctrler} is the dest_ctrler's key
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := some insert_dest_key_murϕ_expr, --  stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition,
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx, --Option.some murphi_expr_curr_head_
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := entry_or_ctrler.entry
            }
            let murphi_key_var := ast_expr_to_murphi_expr key_var_expr_trans_info

            let insert_key_check_found := "insert_key_check_found"
            let found_double_key_check := "found_double_key_check"
            let dest_num_entries_const_name : String := dest_ctrler_name.append "_NUM_ENTRIES_CONST"
            let murphi_stmts : List Murϕ.Statement := [murϕ|
              -- === BEGIN New Code ===
              -- Start with If stmt, to check if there is space in the dest_ctrler??
              -- Or try to generate stmts to check if the structure is full?
              -- Then (1) check if there's a valid entry with the same key, if there is, insert into that one
              insert_key_check_found := false;
              found_double_key_check := false;
              for £dest_ctrler_key_check_idx : £dest_ctrler_idx_t do
                if (next_state .core_[j] .£dest_ctrler_ .entries[£dest_ctrler_key_check_idx] .valid) then
                  if (£murphi_key_var = £murphi_key_arg) then
                    -- Update the entry
                    £insert_existing_entry_murphi_when_stmt;
                    if insert_key_check_found = false then
                      insert_key_check_found := true;
                    else
                      found_double_key_check := true;
                    endif;
                  endif;
                endif;
              endfor;

              if found_double_key_check = true then
                error "Found two entries with the same key? Was this intentional?" ;
              elsif insert_key_check_found = false then
                £ctrler_loop_break := false;
                if next_state .core_[j] .£dest_ctrler_name_ .num_entries = £dest_num_entries_const_name then
                  £ctrler_loop_break := true;
                endif;

                £ctrler_entry_idx := 0;
                --# (1) loop to tail searching for:
                --# if plus 0 is outside this range, this should be caught
                --# by difference check
                -- NOTE: the -1 is because tail is actually 1 more than the actual tail, so it acts as an "insert" location
                £ctrler_found_entry := false;
                -- difference := ( ( (£dest_num_entries_const_name - 1 + £dest_num_entries_const_name) - 1 ) - £ctrler_entry_idx ) % £dest_num_entries_const_name;
                £ctrler_difference := (£dest_num_entries_const_name - 1 );
                £ctrler_offset := 0;
                --#if (difference != -1) then
                while ( (£ctrler_offset <= £ctrler_difference) & (£ctrler_loop_break = false) & (£ctrler_found_entry = false)
                        & (£ctrler_difference >= 0)
                      ) do
                  --# do the search
                  £ctrler_curr_idx := ( £ctrler_entry_idx + £ctrler_offset ) % £dest_num_entries_const_name;
                  if (next_state .core_[j] .£dest_ctrler_name_ .entries[ £ctrler_curr_idx ] .valid = false) then
                    if (next_state .core_[j] .£dest_ctrler_name_ .entries[ £ctrler_curr_idx ] .state = £dest_ctrler_'await_insert'_state) then
                      -- CHECKPOINT TODO: put the translated await-when block of the dest ctrler here
                      £insert_unused_entry_murphi_when_stmt;
                      next_state .core_[j] .£dest_ctrler_name_ .entries[ £ctrler_curr_idx ].valid := true;
                      £ctrler_found_entry := true;
                    end;
                  endif;
                  if (£ctrler_offset != £ctrler_difference) then
                    £ctrler_offset := £ctrler_offset + 1;
                  else
                    £ctrler_loop_break := true;
                  endif;
                end;
                next_state .core_[j] .£dest_ctrler_name_ .num_entries := (next_state .core_[j] .£dest_ctrler_name_ .num_entries + 1);
                if (£ctrler_found_entry = false) then
                  error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                end;
                -- next_state .core_[j] .£dest_ctrler_ .num_entries := Sta .core_[j] .£dest_ctrler_ .num_entries + 1;
                -- Find previous code for insert into unordered queue to insert into first available entry
              endif;
              -- Else (2) Take an unused value to insert into
              -- === END New Code ===
            ]
            let ctrler_idx_t : String := dest_ctrler_name ++ "_idx_t"
            let decls : List Murϕ.Decl := 
              [murϕ_var_decls| var £insert_key_check_found : boolean;] ++
              [murϕ_var_decls| var £found_double_key_check : boolean;] ++
              -- [murϕ_decl| ctrler : £dest_ctrler_name]
              -- (Murϕ.Decl.var ["rob"] (Murϕ.TypeExpr.previouslyDefined "ROB") ),
              [murϕ_var_decls| var £ctrler_loop_break : boolean;] ++
              [murϕ_var_decls| var £ctrler_entry_idx : £ctrler_idx_t] ++
              [murϕ_var_decls| var £ctrler_found_entry : boolean] ++
              [murϕ_var_decls| var £ctrler_difference : £ctrler_idx_t] ++
              [murϕ_var_decls| var £ctrler_offset : £ctrler_idx_t] ++
              [murϕ_var_decls| var £ctrler_curr_idx : £ctrler_idx_t] ++
              insert_unused_entry_murphi_when_stmts_decls.decls ++
              insert_existing_entry_murphi_when_stmts_decls.decls

            let murphi_decls : List Murϕ.Decl := decls ++
              insert_unused_entry_murphi_when_stmts_decls.decls ++ 
              insert_existing_entry_murphi_when_stmts_decls.decls
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmts,
              decls := murphi_decls
            }
            stmts_decls
          else if (api_func_name == "squash") then
            dbg_trace s!"DBG: translating squash api:({term})"
            -- Just get the handle code
            -- Murϕ.Expr.designator (Murϕ.Designator.mk "squash_ld_id" [])
            let expected_func := "squash"
            let expected_struct := ctrler_name

            let if_stmt_trans_info : stmt_translation_info := {
              stmt := stmt_trans_info.stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name,--stmt_trans_info.ctrler_name,
              src_ctrler := stmt_trans_info.ctrler_name, -- stmt_trans_info.src_ctrler,
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
            }

            let dest_ctrler : Ctrler := get_ctrler_matching_name dest_ctrler_name stmt_trans_info.lst_ctrlers
            let dest_type := match dest_ctrler.type with
              | .ok c_type => c_type
              | .error msg => 
                dbg_trace s!"Couldn't get Ctrler type in Squash API translation. MSG: ({msg})"
                default

            let ctrler_curr_idx : String := dest_ctrler_name.append "_squash_curr_idx"
            dbg_trace s!"**search for squash handlers for ctrler: ({dest_ctrler_name}) from ctrler: ({ctrler_name})"
            let ctrler_idx : String := dest_ctrler_name.append "_idx_t"
            -- let squash_idx : Murϕ.Expr := [murϕ| £ctrler_idx]
            let ctrler_squash_idx : String := dest_ctrler_name.append "_squash_idx"

            let ctrler_squash_designator_expr : Murϕ.Expr :=
              match dest_type with
              | .Unordered =>
                [murϕ| £ctrler_squash_idx]
              | .FIFO =>
                [murϕ| £ctrler_curr_idx]
              | .BasicCtrler => default

            let state_handle_squash_if_stmt : lst_stmts_decls := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              if_stmt_trans_info) dest_ctrler_name ctrler_squash_designator_expr (
              dest_ctrler_name) expected_func expected_struct none none
              lst_expr stmt_trans_info
            )
            let squash_handle_by_state : List Murϕ.Statement := state_handle_squash_if_stmt.stmts

            let dest_ctrler_ : String := dest_ctrler_name.append "_"
            -- place into murphi statements
            -- TODO NOTE: Should do this based on the structure type!
            -- i.e. if it's a structure or a queue to reset state with.. etc.
            let (squash_stmts, squash_decls) : List Murϕ.Statement × List Murϕ.Decl :=
              match dest_type with
              | .Unordered =>
                let dest_ctrler_name_ := dest_ctrler_name.append "_"
                ([murϕ|
                  for £ctrler_squash_idx : £ctrler_idx do
                    -- Forstmt error if not as 1 stmt?
                    if next_state .core_[j] .£dest_ctrler_name_ .entries[£ctrler_squash_idx].valid then
                      £squash_handle_by_state
                    endif;
                  endfor;
                ], [])
              | .BasicCtrler =>
                (squash_handle_by_state, [])
              | .FIFO =>
                let ctrler_while_break : String := dest_ctrler_name.append "_while_break"
                let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
                let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
                let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
                let ctrler_offset : String :=      dest_ctrler_name.append "_offset"
                -- let ctrler_curr_idx : String := dest_ctrler_name.append "_curr_idx"
                let dest_ctrler_name_ := dest_ctrler_name.append "_"
                let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])

                let squash_remove_count := dest_ctrler_name.append "_squash_remove_count"
                let s_decls :=
                  let ctrler_idx_t := dest_ctrler_name.append "_idx_t"
                  let ctrler_count_t := dest_ctrler_name.append "_count_t"
                  [murϕ_var_decls| var £ctrler_while_break : boolean] ++
                  [murϕ_var_decls| var £ctrler_found_entry : boolean] ++
                  [murϕ_var_decls| var £ctrler_entry_idx : £ctrler_idx_t] ++
                  [murϕ_var_decls| var £ctrler_difference : £ctrler_count_t] ++
                  [murϕ_var_decls| var £ctrler_offset : £ctrler_count_t] ++
                  [murϕ_var_decls| var £ctrler_curr_idx : £ctrler_idx_t] ++
                  [murϕ_var_decls| var £squash_remove_count : £ctrler_count_t]
                  -- [murϕ_var_decls| var £prev_entry_squashed : boolean]

                let s_stmts :=
                  [murϕ|
                    £ctrler_while_break := false;
                    £ctrler_found_entry := false;
                    if (next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0) then
                      £ctrler_while_break := true;
                    endif;
                    £ctrler_entry_idx := next_state .core_[j] .£dest_ctrler_name_ .head;
                    -- if ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) > £dest_num_entries_const_name then
                    --   £ctrler_difference := ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) % £dest_num_entries_const_name;
                    -- else
                    --   £ctrler_difference := ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head );
                    -- endif;
                    £ctrler_difference := next_state .core_[j] .£dest_ctrler_name_ .num_entries;
                    £ctrler_offset := 0;
                    £squash_remove_count := 0;
                    while ( (£ctrler_offset < £ctrler_difference) & (£ctrler_while_break = false) & ( £ctrler_found_entry = false ) ) do
                      £ctrler_curr_idx := ( £ctrler_entry_idx + £ctrler_offset ) % £dest_num_entries_const_name;
                      if true then
                        £squash_handle_by_state
                      endif;
                      if (£ctrler_offset < £ctrler_difference) then
                        £ctrler_offset := £ctrler_offset + 1;
                      -- else
                      --   £ctrler_while_break := true;
                      endif;
                    end;
                    next_state .core_[j] .£dest_ctrler_name_ .tail := (next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - £squash_remove_count ) % £dest_num_entries_const_name;
                    next_state .core_[j] .£dest_ctrler_name_ .num_entries := (next_state .core_[j] .£dest_ctrler_name_ .num_entries - £squash_remove_count );
                    ]
                (s_stmts, s_decls)

            let stmts_decls : lst_stmts_decls := {
              stmts := squash_stmts
              decls := state_handle_squash_if_stmt.decls ++ squash_decls
            }
            stmts_decls
          -- NOTE: probably don't need a <ctrler>.remove() api?
          -- Since ctrler entries generally do remove() when they get removed..?
          -- else if (api_func_name == "remove") then
          else /- Default case: Simply find the matching when-stmt & handle blks & then translate it here... -/
            dbg_trace "Arbitrary message passing translation"
            dbg_trace s!"Term (func_call): ({term})"
            dbg_trace s!"Arbitrary Message Name: ({api_func_name})"
            dbg_trace s!"Dest ctrler Name: ({dest_ctrler_name})"

            -- TODO: Replace "insert" with the api_func_name
            -- TODO: Check if the translation options below for the when stmt are correct!
            -- TODO: move murphi code here...
            let dest_ctrler : controller_info :=
              get_ctrler_matching_name dest_ctrler_name ctrlers_lst

            let entry_or_ctrler_translation : entry_or_ctrler :=
              if dest_ctrler.init_trans.isSome then
                entry_or_ctrler.entry
              else if dest_ctrler.ctrler_init_trans.isSome then
                entry_or_ctrler.ctrler
              else
                dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
                  default
            let states_to_search : List Description :=
              if dest_ctrler.init_trans.isSome then
                dest_ctrler.transition_list.get!
              else if dest_ctrler.ctrler_init_trans.isSome then
                dest_ctrler.ctrler_trans_list.get!
              else
                dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"
                  default

            dbg_trace s!"===== Begin When Info ====="
            dbg_trace s!"search for when stmt for 'arbitrary' ({api_func_name})"
            dbg_trace s!"States to search: ({states_to_search})"
            dbg_trace s!"dest_ctrler: ({dest_ctrler})"
            dbg_trace s!"curr_ctrler: ({ctrler_name})"
            dbg_trace s!"===== End When Info ====="

            let when_stmt : Option Pipeline.Statement :=
              find_when_from_transition? states_to_search api_func_name ctrler_name
            dbg_trace s!"dest_ctrler_name: ({dest_ctrler_name})"
            dbg_trace s!"ctrler_name: ({ctrler_name})"
            dbg_trace s!"When stmt for 'arbitrary' ({api_func_name}) API: ({when_stmt})"
            dbg_trace s!"States to search: ({states_to_search})"
            dbg_trace s!"dest_ctrler: ({dest_ctrler})"

            let when_stmt_args : List String :=
              match when_stmt with
              | none => []
              | some when_stmt =>
                match (when_stmt.get_when_stmt_src_args) with
                | .error msg =>
                  let msg' : String := s!"Error getting when stmt args in 'Arbitrary' API func: ({msg})"
                  dbg_trace msg'
                  -- default
                  panic! msg'
                | .ok lst_args => lst_args

            -- Convert to Murphi Stmt
            let (murphi_when_stmt_decls?, murphi_when_stmt_stmts?) : Option (List Decl) × Option (List Murϕ.Statement) :=
              match when_stmt with
              | some when_stmt =>
                let when_stmt_trans_info : stmt_translation_info := {
                  stmt := when_stmt,
                  lst_ctrlers := stmt_trans_info.lst_ctrlers,
                  ctrler_name := dest_ctrler_name, --stmt_trans_info.ctrler_name,
                  --            want to set this to the SQ somehow...
                  src_ctrler := 
                  dbg_trace s!"src_ctrler: ({stmt_trans_info.src_ctrler})"
                  dbg_trace s!"stmt_trans_info: ({stmt_trans_info})"
                  if stmt_trans_info.src_ctrler.isNone then
                    Option.some ctrler_name
                  else
                    Option.some stmt_trans_info.ctrler_name,
                  lst_src_args :=
                    if stmt_trans_info.lst_src_args.isSome then
                      if stmt_trans_info.lst_src_args.get!.length > 0 then
                        stmt_trans_info.lst_src_args
                      else
                        match when_stmt_args with
                        | [] => Option.none
                        | _ => Option.some when_stmt_args
                    else
                      -- THe list of args from the func call
                      match when_stmt_args with
                      | [] => Option.none
                      | _ => Option.some when_stmt_args
                    ,
                  func := stmt_trans_info.func,
                  is_await := stmt_trans_info.is_await,
                  entry_keyword_dest := Option.some dest_ctrler_name,
                  trans_obj := stmt_trans_info.trans_obj,
                  -- Swap curr ctrler designator & specific murphi desig
                  -- since we just call in the opposite order
                  specific_murphi_dest_expr := stmt_trans_info.curr_ctrler_designator_idx,
                  lst_decls := stmt_trans_info.lst_decls,
                  is_rhs := stmt_trans_info.is_rhs,
                  -- By setting these fields, I assume we'll specifically mean to use this with 
                  -- controllers with multiple elements, and thus we need to use 'tail_search'
                  -- which indexes the dest with 'curr_idx'
                  use_specific_dest_in_transition := true -- stmt_trans_info.use_specific_dest_in_transition
                  curr_ctrler_designator_idx := stmt_trans_info.specific_murphi_dest_expr -- murphi_dest_idx_expr
                  lhs_var_is_just_default := false
                  translate_entry_or_ctrler := entry_or_ctrler_translation
                }
                dbg_trace s!"Arbitrary msg translation. when stmt trans info: ({when_stmt_trans_info})"
                -- TODO: Test the translation, I suspect I may need to set the
                -- sepcific_murphi_dest_expr to this "i" index...
                let murphi_when_stmts_decls : lst_stmts_decls :=
                  ast_stmt_to_murphi_stmts when_stmt_trans_info
                -- let murphi_when_stmt : List Murϕ.Statement :=
                --   murphi_when_stmts_decls.stmts
                
                dbg_trace s!"Translated when stmts: ({murphi_when_stmts_decls.stmts})"
                (some murphi_when_stmts_decls.decls, some murphi_when_stmts_decls.stmts)
              | none => 
                (none, none)

            -- Handle blocks?
            -- find_handle_blks_matching_msg states_to_search api_func_name ctrler_name
            let expected_func := api_func_name
            let expected_struct := ctrler_name

            let if_stmt_trans_info : stmt_translation_info := {
              stmt := stmt_trans_info.stmt,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := dest_ctrler_name,--stmt_trans_info.ctrler_name,
              src_ctrler := stmt_trans_info.ctrler_name, -- stmt_trans_info.src_ctrler,
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := true
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
            }

            let ctrler_idx : String := dest_ctrler_name.append "_idx_t"
            -- let squash_idx : Murϕ.Expr := [murϕ| £ctrler_idx]
            -- TODO: Replace the squash idx by the context's idx as usual
            let ctrler_squash_idx : String := dest_ctrler_name.append "_idx"
            let ctrler_squash_idx_expr : Murϕ.Expr := [murϕ| £ctrler_squash_idx]

            let state_when_is_in : StateName :=
              find_state_name_matching_when_msg states_to_search api_func_name ctrler_name
            let dest_ctrler_name_ := dest_ctrler_name.append "_"
            let ctrler_is_on_state_check : Murϕ.Expr :=
              dbg_trace s!"Translating Ctrler is on state check Murphi Expr, state when is in: ({state_when_is_in})"
              match dest_ctrler.type with
              | .ok type_ => match type_ with
                | .BasicCtrler =>
                  dbg_trace s!"***Translation Msg to BasicCtrler: Dest Ctrler Name: ({dest_ctrler_name_}) | State when is in: ({state_when_is_in})"
                  [murϕ| next_state .core_[ j ] .£dest_ctrler_name_ .state = £state_when_is_in] 
                | .Unordered =>
                  if let some specific_ctrler_desig := stmt_trans_info.specific_murphi_dest_expr then
                    [murϕ| next_state .core_[ j ] .£dest_ctrler_name_ .entries[ £specific_ctrler_desig ] .state = £state_when_is_in] 
                  else
                    dbg_trace s!"Error, curr_ctrler_designator_idx is none, but dest_ctrler is a Queue type (Unordered)"
                    default
                | .FIFO =>
                  if let some specific_ctrler_desig := stmt_trans_info.specific_murphi_dest_expr then
                    [murϕ| next_state .core_[ j ] .£dest_ctrler_name_ .entries[ £specific_ctrler_desig ] .state = £state_when_is_in] 
                  else
                    dbg_trace s!"Error, curr_ctrler_designator_idx is none, but dest_ctrler is a Queue type (FIFO)"
                    default
              | .error msg =>
                dbg_trace s!"Error while translating arbitrary message to Murϕ: ({msg})"
                default

            let state_handle_squash_if_stmt : lst_stmts_decls := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              if_stmt_trans_info) dest_ctrler_name ctrler_squash_idx_expr (
              dest_ctrler_name) expected_func expected_struct
              (murphi_when_stmt_stmts?) (some ctrler_is_on_state_check)
              lst_expr stmt_trans_info
            )
            let squash_handle_by_state : List Murϕ.Statement := state_handle_squash_if_stmt.stmts

            let murphi_when_stmt_decls : List Decl :=
              match murphi_when_stmt_decls? with
              | some murphi_when_stmt_decls =>
                murphi_when_stmt_decls
              | none => []
            let stmts_decls : lst_stmts_decls := {
              stmts := squash_handle_by_state,
              decls := murphi_when_stmt_decls ++ state_handle_squash_if_stmt.decls
            }
            stmts_decls
        else
        if len_1_qual_name
        then
        -- dbg_trace "doesn't make sense.. replace this with Except throw"

          if ( qual_name_list[0]! == "remove" ) then
            let curr_ctrler_name_ : String := ctrler_name.append "_"

            -- name of func call is just "is_head"
            -- then translate term as curr_ctrler.head == i
            -- NOTE: Add a line to set valid to false
            let dsl_valid_asn_false := var_asn_var ["valid"] "false"
            let valid_to_false_trans_info : stmt_translation_info := {
              stmt := dsl_valid_asn_false,
              lst_ctrlers := stmt_trans_info.lst_ctrlers,
              ctrler_name := stmt_trans_info.ctrler_name,--stmt_trans_info.ctrler_name,
              src_ctrler := stmt_trans_info.ctrler_name, -- stmt_trans_info.src_ctrler,
              lst_src_args := stmt_trans_info.lst_src_args,
              func := stmt_trans_info.func,
              is_await := stmt_trans_info.is_await,
              entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
              trans_obj := stmt_trans_info.trans_obj,
              specific_murphi_dest_expr := stmt_trans_info.curr_ctrler_designator_idx --stmt_trans_info.specific_murphi_dest_expr,
              lst_decls := stmt_trans_info.lst_decls,
              is_rhs := stmt_trans_info.is_rhs,
              use_specific_dest_in_transition := false
              curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
              lhs_var_is_just_default := false
              translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
            }
            let murphi_asn_false := ast_stmt_to_murphi_stmts valid_to_false_trans_info
            let murphi_stmt : List Murϕ.Statement := [murϕ|
              next_state .core_[j] .£curr_ctrler_name_ .num_entries := (next_state .core_[j] .£curr_ctrler_name_ .num_entries - 1);
              £murphi_asn_false.stmts
            ]
            -- murphi_expr
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmt,
              decls := [] ++ murphi_asn_false.decls
            }
            stmts_decls
          else if ( qual_name_list[0]! == "squash_remove" ) then
            -- Use with FIFOs only
            let squash_remove_count := ctrler_name.append "_squash_remove_count"
            let murphi_stmt : List Murϕ.Statement := [murϕ|
              £squash_remove_count := £squash_remove_count + 1;
            ]
            -- murphi_expr
            let stmts_decls : lst_stmts_decls := {
              stmts := murphi_stmt,
              decls := []
            }
            stmts_decls
          else
            dbg_trace "Found a function stmt that doesn't match an API name"
            dbg_trace "Also we don't match user-written functions yet."
            dbg_trace s!"The stmt: ({stmt})"
            dbg_trace s!"The expr: ({expr})"
            dbg_trace s!"The term: ({term})"
            dbg_trace s!"Function found: ({qual_name_list})"
            empty_stmt_decl_lsts
        -- empty_stmt_decl_lsts
        else
          -- len more than 2 qual name...
          -- we don't have that at the moment?
          -- should throw error
          -- just return default
          dbg_trace "doesn't make sense.. replace this with Except throw"
          dbg_trace "Got a function name with a long qual name, > 2 parts"
          dbg_trace s!"The stmt: ({stmt})"
          dbg_trace s!"The expr: ({expr})"
          dbg_trace s!"DEBUG: Qual name functions: The function: ({term})"
          empty_stmt_decl_lsts
         

      | _ =>
        -- just call the term translation normally?
        -- can't, this is returns some expr?
        dbg_trace "What was i just passed?? should be a stray expr?"
        dbg_trace s!"The stmt: ({stmt})"
        dbg_trace s!"The expr: ({expr})"
        dbg_trace s!"DEBUG: Handling functions in the DSL: The Term: ({term})"
        empty_stmt_decl_lsts
      -- Murϕ.Expr.integerConst 0
    | _ =>
      -- let expr_trans_info :=
      -- assn_stmt_to_expr_translation_info stmt_trans_info expr
      -- let ret_val :=
      -- ast_expr_to_murphi_expr expr_trans_info

      -- ret_val

      -- let expr_trans_info :=
      --   assn_stmt_to_expr_translation_info stmt_trans_info expr
      -- let murphi_expr :=
      --   ast_expr_to_murphi_expr expr_trans_info
      -- let murphi_stmt :=
      --   Murϕ.

      -- AZ NOTE: I don't think there is anything this
      -- would map to?
          
      dbg_trace "What was i just passed?? should be a stray expr?"
      dbg_trace s!"The stmt: ({stmt})"
      dbg_trace s!"DEBUG: Unexpected expr type: The expr: ({expr})"
      empty_stmt_decl_lsts
  | _ =>
    dbg_trace "passed in sth that's not a stray_expr"
    dbg_trace s!"DEBUG: Unexpected stmt type: The stmt: ({stmt})"
    empty_stmt_decl_lsts

partial def api_term_func_to_murphi_func
-- ( term : Pipeline.Term )
( term_trans_info : term_translation_info )
-- The statements in the await block
-- try to match with when statements
-- use when stmt identifiers
( stmts_in_await : List Pipeline.Statement)
: lst_stmts_decls
:=
  let term : Pipeline.Term := term_trans_info.term
  -- also get the current struct name... etc.
  let ctrlers_lst : Ctrlers := term_trans_info.lst_ctrlers
  let ctrler_name := term_trans_info.ctrler_name

  -- when statement stuff (handling nested scopes?
  -- or rather, clojures?)
  let src_ctrler := term_trans_info.src_ctrler
  let lst_src_args := term_trans_info.lst_src_args
  let func_name : Option Identifier := term_trans_info.func
  -- If it isn't a term -> func call, with
  -- 2 qualified param names, then we can just call 
  -- the ast_expr_to_murphi_expr actually!
  let is_await := term_trans_info.is_await

  -- let dsl_func_info : (QualifiedName × List Pipeline.Expr) :=
  let (qual_name, lst_exprs) : (QualifiedName × List Pipeline.Expr) :=
  match term with
  | Pipeline.Term.function_call qual_name lst_exprs =>
    -- translate this specific call..
    -- don't use the the call for other stray exprs
    (qual_name, lst_exprs)
  | _ => dbg_trace "this would be an error"
    default
  -- let qual_name : QualifiedName := dsl_func_info.1
  let lst_names : List Identifier := match qual_name with
  | QualifiedName.mk lst_idents => lst_idents

  -- dbg_trace "== trying to get list of api names out of a func call Qual name =="
  let api_name : Identifier := lst_names[1]!

  let dest_ctrler_name : Identifier := lst_names[0]!
  -- let lst_exprs : List Pipeline.Expr := dsl_func_info.2

  -- Extract info, gen the murphi func code
  -- this is mostly setting up the Murphi Template
  let is_tail_search : Bool := lst_names.contains "tail_search"
  let is_head_search : Bool := lst_names.contains "head_search"
  let is_unordered_search : Bool := lst_names.contains "search"
  let is_search_all : Bool := lst_names.contains "search_all"

  dbg_trace "&&&&& BEGIN is await function tail_search? &&&&&"
  dbg_trace is_tail_search
  dbg_trace "&&&&& END is await function tail_search? &&&&&"

  if is_tail_search then
    -- AZ TODO: This we know should have the one expr,
    -- we could techinically use Except and 'throw' here
    -- if the len isn't 1
    -- dbg_trace "== trying to get expr from func call arg list =="
    -- let curr_idx := Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])
    let ctrler_curr_idx : String := dest_ctrler_name.append "_curr_idx"
    let murphi_ctrler_curr_idx : Murϕ.Expr := [murϕ| £ctrler_curr_idx]

    let match_cond : Pipeline.Expr := lst_exprs[0]!
    let match_cond_trans_info : expr_translation_info :=  {
      expr := match_cond
      lst_ctrlers := term_trans_info.lst_ctrlers,
      -- The "main ctrler to translate with"
      -- is it the 
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler, -- Option.some dest_ctrler_name, -- 
      lst_src_args := Option.some [], -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := term_trans_info.use_specific_dest_in_transition
      -- TODO: Double check what is this?
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx --term_trans_info.specific_murphi_dest_expr 
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search match cond: ({match_cond_trans_info})"

    -- (
    -- assn_term_to_expr_translation_info term_trans_info match_cond)

    let murphi_match_cond_expr := ast_expr_to_murphi_expr match_cond_trans_info
    dbg_trace "&&&&& BEGIN match if cond &&&&&"
    dbg_trace murphi_match_cond_expr
    dbg_trace "&&&&& END match if cond &&&&&"

    -- AZ TODO: Identify the "search success case"
    -- and the "search failure case"
    -- translate them accordingly
    -- take the stmts in await, should be 2, the 2 whens?
    let when_stmts : List Pipeline.Statement := stmts_in_await.filter (
      λ stmt =>
        match stmt with
        | Pipeline.Statement.when _ _ _ =>
          true
        | _ => false
    )
    dbg_trace "&&&&& BEGIN when_stmts &&&&&"
    dbg_trace when_stmts
    dbg_trace "&&&&& END when_stmts &&&&&"
        -- qual name is probably the structure -- ... list idents is the args list!
        -- so take the qual name and check for search_success / fail
    let when_search_success_list : List Pipeline.Statement := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName :=
      match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error here?"
        default

      let is_search_success : Bool :=
      match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => (
      lst_idents.contains "search_success")

      is_search_success
    )
    let when_search_fail_list := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName := match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error?"
        default
      let is_search_fail : Bool := match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => lst_idents.contains "search_fail"
      is_search_fail
    )
    dbg_trace "&&&&& BEGIN when_success &&&&&"
    dbg_trace when_search_success_list
    dbg_trace "&&&&& END when_success &&&&&"
    dbg_trace "&&&&& BEGIN when_fail &&&&&"
    dbg_trace when_search_fail_list
    dbg_trace "&&&&& END when_fail &&&&&"
    -- dbg_trace "== assuming there are when search success/fail stmts found! =="
    let when_search_success := when_search_success_list[0]!
    let when_search_fail := when_search_fail_list[0]!

    dbg_trace s!"When ctrler: ({term_trans_info.ctrler_name})\n"++
      s!"When src_ctrler: ({dest_ctrler_name})"

    let when_success_stmt_args : List String :=
      match (when_search_success.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args
    let when_fail_stmt_args : List String :=
      match (when_search_fail.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args

    let when_search_success_trans_info : stmt_translation_info := {
      stmt := when_search_success,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_success_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- i.e. use this for lhs
      -- (ex. lhs = rhs ==> next_state.LQ.entry[desig_idx].lhs, ... rhs)
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx -- Option.some murphi_ctrler_curr_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search search success: ({when_search_success_trans_info})"

    let when_search_fail_trans_info : stmt_translation_info := {
      stmt := when_search_fail,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_fail_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := term_trans_info.specific_murphi_dest_expr,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- search fail isn't in the loop, so we don't use the murphi_ctrler_curr_idx
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search search fail: ({when_search_fail_trans_info})"

    dbg_trace "(((***((( BEGIN TAIL SEARCH WHEN TRANSLATION ))))))"
    let when_search_success_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_success_trans_info
    let when_search_fail_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_fail_trans_info
    dbg_trace "(((***((( END TAIL SEARCH WHEN TRANSLATION ))))))"

    dbg_trace "&&&&& BEGIN Murϕ tail_search when_success &&&&&"
    dbg_trace s!"tail_search when_success: ({when_search_success})"
    dbg_trace when_search_success_murphi_stmts.stmts
    dbg_trace when_search_success_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ tail_search when_success &&&&&"
    dbg_trace "&&&&& BEGIN Murϕ tail_search when_fail &&&&&"
    dbg_trace s!"tail_search when_fail: ({when_search_fail})"
    dbg_trace when_search_fail_murphi_stmts.stmts
    dbg_trace when_search_fail_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ tail_search when_fail &&&&&"

    -- ex. SQ_NUM_ETNRIES_CONST
    let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])

    -- AZ NOTE: Use this point to build a different template
    -- based on the specific API call...
    -- Or maybe earlier, but i'm not 100% sure
    -- what the "common code" segments are just yet...

    -- TODO: Give them slightly more unique names, so they don't collide..
    -- This affects the curr_idx designator that will need to be adjusted as well
    let ctrler_while_break : String := dest_ctrler_name.append "_while_break"
    let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
    let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
    let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
    let ctrler_offset : String :=      dest_ctrler_name.append "_offset"
      -- This one is defined above
    -- let ctrler_curr_idx : String :=    /- dest_ctrler_name.append -/ "_curr_idx"

    let ctrler_idx_t := dest_ctrler_name.append "_idx_t"
    let ctrler_count_t := dest_ctrler_name.append "_count_t"
    let decls : List Murϕ.Decl := [
      (Murϕ.Decl.var [ctrler_while_break] (Murϕ.TypeExpr.previouslyDefined "boolean")),
      (Murϕ.Decl.var [ctrler_found_entry] (Murϕ.TypeExpr.previouslyDefined "boolean")),
      -- [murϕ_var_decls|
      --   var found_entry : boolean
      -- ],
      (Murϕ.Decl.var [ctrler_entry_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
      (Murϕ.Decl.var [ctrler_difference] (Murϕ.TypeExpr.previouslyDefined ctrler_count_t)),
      (Murϕ.Decl.var [ctrler_offset] (Murϕ.TypeExpr.previouslyDefined ctrler_count_t)),
      (Murϕ.Decl.var [ctrler_curr_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t))
      -- [murϕ_var_decls|
      --   var curr_idx : £ctrler_idx_t
      -- ]
    ]
    let dest_ctrler_name_ := dest_ctrler_name.append "_"
    let overall_murphi_tail_search_template : List Murϕ.Statement :=
      -- AZ TODO: introduce a type for the ACCESS_HASH
      -- or ACCESS_TAIL and just set it at the beginning

      -- [murϕ| next_state := Sta]
      -- [murϕ|  sq := Sta.core_[j].lsq_.sq_],
      -- [murϕ|  lq := Sta.core_[j].lsq_.lq_],
      [murϕ|
        £ctrler_while_break := false;
        £ctrler_found_entry := false;
        if (next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0) then
          £ctrler_while_break := true;
        endif;
        -- AZ TODO:
        -- no, can just map the condition check

      -- [murϕ|  if (£dest_ctrler_name .sq_msg_enum = £dest_ctrler_name_ACCESS_HASH) then
      --     st_idx := find_st_idx_of_seq_num(£dest_ctrler_name,
      --                                      £dest_ctrler_name .st_seq_num);
      --   elsif (£dest_ctrler_name .sq_msg_enum = SQ_ACCESS_TAIL) then
      --     st_idx := (£dest_ctrler_name .sq_tail + ( SQ_ENTRY_NUM + 1) - 1) % ( SQ_ENTRY_NUM + 1 );
      --   endif],
        £ctrler_entry_idx := (next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - 1) % £dest_num_entries_const_name ;
        -- if ( £ctrler_entry_idx + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) > £dest_num_entries_const_name then
        --   £ctrler_difference := ( £ctrler_entry_idx + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) % £dest_num_entries_const_name;
        -- else
        --   £ctrler_difference := ( £ctrler_entry_idx + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) ;
        -- endif;
        £ctrler_difference := next_state .core_[j] .£dest_ctrler_name_ .num_entries;
        £ctrler_offset := 0;
        while ( (£ctrler_offset < £ctrler_difference) & (£ctrler_while_break = false) & ( £ctrler_found_entry = false ) ) do
          £ctrler_curr_idx := ( £ctrler_entry_idx + £dest_num_entries_const_name - £ctrler_offset ) % £dest_num_entries_const_name;
          if (
            -- AZ TODO:
            -- THIS IS WHERE TO TRANSLATE THE "API ARGS LIST"
            -- INTO CONDITIONS
            -- Keeping the code here as an example of
            -- what to expect kind of

            -- £dest_ctrler_name_ .entries[curr_idx] .phys_addr
            --   =
            --   £dest_ctrler_name_ .phys_addr
            £murphi_match_cond_expr

              ) then
    
            -- AZ TODO:
            -- This should be replaced with "this" ctrler's
            -- code
            -- Note that "entry" indicates access to the dest ctrler's
            -- fields
            -- So that we can even use entry in the when block...
            £when_search_success_murphi_stmts.stmts;

            -- value := £dest_ctrler_name_ .entries[curr_idx] .write_value;
    
            £ctrler_found_entry := true;
          endif;
    
          -- This is not really necessary
          if (£ctrler_offset < £ctrler_difference) then
            £ctrler_offset := £ctrler_offset + 1;
          -- else
          --   £ctrler_while_break := true;
          endif;
        end;
        if (£ctrler_found_entry = false) then
          £when_search_fail_murphi_stmts.stmts
        endif;
    ]


    -- 0
    let stmts_decls : lst_stmts_decls := {
      stmts := overall_murphi_tail_search_template,
      decls := (
        when_search_success_murphi_stmts.decls ++
        when_search_fail_murphi_stmts.decls ++
        decls
      )
    }
    stmts_decls
    -- default
  else if is_head_search then
    -- AZ TODO: This we know should have the one expr,
    -- we could techinically use Except and 'throw' here
    -- if the len isn't 1
    -- dbg_trace "== trying to get expr from func call arg list =="
    -- let curr_idx := Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])
    let ctrler_curr_idx : String := dest_ctrler_name.append "_curr_idx"
    let murphi_ctrler_curr_idx : Murϕ.Expr := [murϕ| £ctrler_curr_idx]

    let match_cond : Pipeline.Expr := lst_exprs[0]!
    let match_cond_trans_info : expr_translation_info :=  {
      expr := match_cond
      lst_ctrlers := term_trans_info.lst_ctrlers,
      -- The "main ctrler to translate with"
      -- is it the 
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler, -- Option.some dest_ctrler_name, -- 
      lst_src_args := Option.some [], -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := term_trans_info.use_specific_dest_in_transition
      -- TODO: Double check what is this?
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx --term_trans_info.specific_murphi_dest_expr 
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Head_Search match cond: ({match_cond_trans_info})"

    -- (
    -- assn_term_to_expr_translation_info term_trans_info match_cond)

    let murphi_match_cond_expr := ast_expr_to_murphi_expr match_cond_trans_info
    dbg_trace "&&&&& BEGIN match if cond &&&&&"
    dbg_trace murphi_match_cond_expr
    dbg_trace "&&&&& END match if cond &&&&&"

    -- AZ TODO: Identify the "search success case"
    -- and the "search failure case"
    -- translate them accordingly
    -- take the stmts in await, should be 2, the 2 whens?
    let when_stmts : List Pipeline.Statement := stmts_in_await.filter (
      λ stmt =>
        match stmt with
        | Pipeline.Statement.when _ _ _ =>
          true
        | _ => false
    )
    dbg_trace "&&&&& BEGIN when_stmts &&&&&"
    dbg_trace when_stmts
    dbg_trace "&&&&& END when_stmts &&&&&"
        -- qual name is probably the structure -- ... list idents is the args list!
        -- so take the qual name and check for search_success / fail
    let when_search_success_list : List Pipeline.Statement := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName :=
      match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error here?"
        default

      let is_search_success : Bool :=
      match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => (
      lst_idents.contains "search_success")

      is_search_success
    )
    let when_search_fail_list := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName := match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error?"
        default
      let is_search_fail : Bool := match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => lst_idents.contains "search_fail"
      is_search_fail
    )
    dbg_trace "&&&&& BEGIN when_success &&&&&"
    dbg_trace when_search_success_list
    dbg_trace "&&&&& END when_success &&&&&"
    dbg_trace "&&&&& BEGIN when_fail &&&&&"
    dbg_trace when_search_fail_list
    dbg_trace "&&&&& END when_fail &&&&&"
    -- dbg_trace "== assuming there are when search success/fail stmts found! =="
    let when_search_success := when_search_success_list[0]!
    let when_search_fail := when_search_fail_list[0]!

    dbg_trace s!"When ctrler: ({term_trans_info.ctrler_name})\n"++
      s!"When src_ctrler: ({dest_ctrler_name})"

    let when_success_stmt_args : List String :=
      match (when_search_success.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args
    let when_fail_stmt_args : List String :=
      match (when_search_fail.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args

    let when_search_success_trans_info : stmt_translation_info := {
      stmt := when_search_success,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_success_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- i.e. use this for lhs
      -- (ex. lhs = rhs ==> next_state.LQ.entry[desig_idx].lhs, ... rhs)
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx -- Option.some murphi_ctrler_curr_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Head_Search search success: ({when_search_success_trans_info})"

    let when_search_fail_trans_info : stmt_translation_info := {
      stmt := when_search_fail,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_fail_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- search fail isn't in the loop, so we don't use the murphi_ctrler_curr_idx
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Head_Search search fail: ({when_search_fail_trans_info})"

    dbg_trace "(((***((( BEGIN Head SEARCH WHEN TRANSLATION ))))))"
    let when_search_success_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_success_trans_info
    let when_search_fail_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_fail_trans_info
    dbg_trace "(((***((( END Head SEARCH WHEN TRANSLATION ))))))"

    dbg_trace "&&&&& BEGIN Murϕ head_search when_success &&&&&"
    dbg_trace s!"head_search when_success: ({when_search_success})"
    dbg_trace when_search_success_murphi_stmts.stmts
    dbg_trace when_search_success_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ head_search when_success &&&&&"
    dbg_trace "&&&&& BEGIN Murϕ head_search when_fail &&&&&"
    dbg_trace s!"head_search when_fail: ({when_search_fail})"
    dbg_trace when_search_fail_murphi_stmts.stmts
    dbg_trace when_search_fail_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ head_search when_fail &&&&&"

    -- ex. SQ_NUM_ETNRIES_CONST
    let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])

    -- AZ NOTE: Use this point to build a different template
    -- based on the specific API call...
    -- Or maybe earlier, but i'm not 100% sure
    -- what the "common code" segments are just yet...

    -- TODO: Give them slightly more unique names, so they don't collide..
    -- This affects the curr_idx designator that will need to be adjusted as well
    let ctrler_while_break : String := dest_ctrler_name.append "_while_break"
    let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
    let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
    let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
    let ctrler_offset : String :=      dest_ctrler_name.append "_offset"
      -- This one is defined above
    -- let ctrler_curr_idx : String :=    /- dest_ctrler_name.append -/ "_curr_idx"

    let ctrler_idx_t := dest_ctrler_name.append "_idx_t"
    let ctrler_count_t := dest_ctrler_name.append "_count_t"
    let decls : List Murϕ.Decl := [
      (Murϕ.Decl.var [ctrler_while_break] (Murϕ.TypeExpr.previouslyDefined "boolean")),
      (Murϕ.Decl.var [ctrler_found_entry] (Murϕ.TypeExpr.previouslyDefined "boolean")),
      -- [murϕ_var_decls|
      --   var found_entry : boolean
      -- ],
      (Murϕ.Decl.var [ctrler_entry_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
      (Murϕ.Decl.var [ctrler_difference] (Murϕ.TypeExpr.previouslyDefined ctrler_count_t)),
      (Murϕ.Decl.var [ctrler_offset] (Murϕ.TypeExpr.previouslyDefined ctrler_count_t)),
      (Murϕ.Decl.var [ctrler_curr_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t))
      -- [murϕ_var_decls|
      --   var curr_idx : £ctrler_idx_t
      -- ]
    ]
    let dest_ctrler_name_ := dest_ctrler_name.append "_"
    let overall_murphi_head_search_template : List Murϕ.Statement :=
      -- AZ TODO: introduce a type for the ACCESS_HASH
      -- or ACCESS_head and just set it at the beginning

      -- [murϕ| next_state := Sta]
      -- [murϕ|  sq := Sta.core_[j].lsq_.sq_],
      -- [murϕ|  lq := Sta.core_[j].lsq_.lq_],
      [murϕ|
        £ctrler_while_break := false;
        £ctrler_found_entry := false;
        if (next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0) then
          £ctrler_while_break := true;
        endif;
        -- AZ TODO:
        -- no, can just map the condition check

      -- [murϕ|  if (£dest_ctrler_name .sq_msg_enum = £dest_ctrler_name_ACCESS_HASH) then
      --     st_idx := find_st_idx_of_seq_num(£dest_ctrler_name,
      --                                      £dest_ctrler_name .st_seq_num);
      --   elsif (£dest_ctrler_name .sq_msg_enum = SQ_ACCESS_head) then
      --     st_idx := (£dest_ctrler_name .sq_head + ( SQ_ENTRY_NUM + 1) - 1) % ( SQ_ENTRY_NUM + 1 );
      --   endif],
        £ctrler_entry_idx := next_state .core_[j] .£dest_ctrler_name_ .head;
        -- if ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) > £dest_num_entries_const_name then
        --   £ctrler_difference := ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) % £dest_num_entries_const_name;
        -- else
        --   £ctrler_difference := ( next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head );
        -- endif;
        £ctrler_difference := next_state .core_[j] .£dest_ctrler_name_ .num_entries;
        £ctrler_offset := 0;
        while ( (£ctrler_offset < £ctrler_difference) & (£ctrler_while_break = false) & ( £ctrler_found_entry = false ) ) do
          £ctrler_curr_idx := ( £ctrler_entry_idx + £ctrler_offset ) % £dest_num_entries_const_name;
          if (
            -- AZ TODO:
            -- THIS IS WHERE TO TRANSLATE THE "API ARGS LIST"
            -- INTO CONDITIONS
            -- Keeping the code here as an example of
            -- what to expect kind of

            -- £dest_ctrler_name_ .entries[curr_idx] .phys_addr
            --   =
            --   £dest_ctrler_name_ .phys_addr
            £murphi_match_cond_expr

              ) then
    
            -- AZ TODO:
            -- This should be replaced with "this" ctrler's
            -- code
            -- Note that "entry" indicates access to the dest ctrler's
            -- fields
            -- So that we can even use entry in the when block...
            £when_search_success_murphi_stmts.stmts;

            -- value := £dest_ctrler_name_ .entries[curr_idx] .write_value;
    
            £ctrler_found_entry := true;
          endif;
    
          -- This is not really necessary
          if (£ctrler_offset != £ctrler_difference) then
            £ctrler_offset := £ctrler_offset + 1;
          -- else
          --   £ctrler_while_break := true;
          endif;
        end;
        if (£ctrler_found_entry = false) then
          £when_search_fail_murphi_stmts.stmts
        endif;
    ]


    -- 0
    let stmts_decls : lst_stmts_decls := {
      stmts := overall_murphi_head_search_template,
      decls := (
        when_search_success_murphi_stmts.decls ++
        when_search_fail_murphi_stmts.decls ++
        decls
      )
    }
    stmts_decls
    -- default
  else if is_unordered_search then
    let dest_ctrler_name_ : String := dest_ctrler_name.append "_"
    let dest_ctrler_iter : String := dest_ctrler_name.append "_iter"
    let dest_ctrler_idx_t : String := dest_ctrler_name.append "_idx_t"
    let dest_ctrler_iter_desig_expr : Murϕ.Expr := [murϕ_expr| £dest_ctrler_iter ]
    dbg_trace "$$$$$ BEGIN Desig Iter"
      dbg_trace dest_ctrler_iter_desig_expr
    dbg_trace "$$$$$ END Desig Iter"

    -- (API Argument 1)
    let match_cond : Pipeline.Expr := lst_exprs[0]!
    let match_cond_trans_info : expr_translation_info :=  {
      expr := match_cond
      lst_ctrlers := term_trans_info.lst_ctrlers,
      -- The "main ctrler to translate with"
      -- is it the 
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler,
      lst_src_args := term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := dest_ctrler_iter_desig_expr,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := term_trans_info.use_specific_dest_in_transition
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    let condition : Murϕ.Expr := ast_expr_to_murphi_expr match_cond_trans_info

    let (lst_idents, exprs) : (List Identifier × List Pipeline.Expr) := match (lst_exprs[1]!) with
    | Pipeline.Expr.some_term term =>
      match term with
      | Term.function_call qual_name exprs =>
        match qual_name with
        | QualifiedName.mk lst_idents => (lst_idents, exprs)
      | _ => dbg_trace "throw!"
        default
    | _ => dbg_trace "throw!"
      default

    let less_or_greater : String := if lst_idents[0]! == "min" then "<"
      else --lst_idents[0]! == "max"
        ">"

    -- (API Argument 2)
    let match_overall_cond : Pipeline.Expr := exprs[0]!
    let match_overall_cond_trans_info : expr_translation_info :=  {
    expr := match_overall_cond
    lst_ctrlers := term_trans_info.lst_ctrlers,
    -- The "main ctrler to translate with"
    -- is it the 
    ctrler_name := term_trans_info.ctrler_name,
    src_ctrler := term_trans_info.src_ctrler,
    lst_src_args := term_trans_info.lst_src_args,
    func := term_trans_info.func,
    is_await := term_trans_info.is_await,
    entry_keyword_dest := Option.some dest_ctrler_name,
    trans_obj := term_trans_info.trans_obj,
    specific_murphi_dest_expr :=  dest_ctrler_iter_desig_expr,
    lst_decls := term_trans_info.lst_decls,
    is_rhs := term_trans_info.is_rhs,
    use_specific_dest_in_transition := term_trans_info.use_specific_dest_in_transition
    curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
    lhs_var_is_just_default := false
    translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    let overall_condition : Murϕ.Expr := ast_expr_to_murphi_expr match_overall_cond_trans_info

    let update_found_idx_expr : Murϕ.Expr := Murϕ.Expr.binop less_or_greater overall_condition [murϕ_expr| found_element]
    -- [murϕ_expr| (£overall_condition) £less_or_greater found_element]

    let when_stmts : List Pipeline.Statement := stmts_in_await.filter (
      λ stmt =>
        match stmt with
        | Pipeline.Statement.when _ _ _ =>
          true
        | _ => false
    )
    dbg_trace "&&&&& BEGIN when_stmts &&&&&"
    dbg_trace when_stmts
    dbg_trace "&&&&& END when_stmts &&&&&"
        -- qual name is probably the structure -- ... list idents is the args list!
        -- so take the qual name and check for search_success / fail
    let when_search_success_list : List Pipeline.Statement := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName :=
      match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error here?"
        default

      let is_search_success : Bool :=
      match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => (
      lst_idents.contains "search_success")

      is_search_success
    )
    let when_search_fail_list := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName := match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error?"
        default
      let is_search_fail : Bool := match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => lst_idents.contains "search_fail"
      is_search_fail
    )
    dbg_trace "&&&&& BEGIN when_success &&&&&"
    dbg_trace when_search_success_list
    dbg_trace "&&&&& END when_success &&&&&"
    dbg_trace "&&&&& BEGIN when_fail &&&&&"
    dbg_trace when_search_fail_list
    dbg_trace "&&&&& END when_fail &&&&&"
    -- dbg_trace "== assuming there are when search success/fail stmts found! =="
    let when_search_success := when_search_success_list[0]!
    let when_search_fail := when_search_fail_list[0]!

    let when_search_success_trans_info : stmt_translation_info := {
      stmt := when_search_success,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler,
      lst_src_args := term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := [murϕ_expr| found_idx],
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }

    let when_search_fail_trans_info : stmt_translation_info := {
      stmt := when_search_fail,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler,
      lst_src_args := term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := [murϕ_expr| found_idx],
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }

    dbg_trace "(((***((( BEGIN TAIL SEARCH WHEN TRANSLATION ))))))"
    let when_search_success : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_success_trans_info
    let when_search_fail : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_fail_trans_info
    dbg_trace "(((***((( END TAIL SEARCH WHEN TRANSLATION ))))))"
    -- Setup the code £
    let murphi_template_stmts := [murϕ_statements|
  found_entry := false;
  for £dest_ctrler_iter : £dest_ctrler_idx_t do
    -- TODO: Put an if condition which checks if the seq_num if invalid (i.e. is 0)
    if (next_state .core_[ j ] .£dest_ctrler_name_ .entries[ £dest_ctrler_iter ] .valid) then
    -- if seq_num is invalid, don't do anything.
      -- TODO: £condition (API Argument 1)
      if ( £condition ) then
        -- TODO: The first time we find a match, we write a value to the recorded value we check
        if found_entry = false then
          -- TODO: £overall_condition (API Argument 2)
          found_element := (£overall_condition);
          -- TODO: We can record the element idx that has the closest value here..
          found_idx := £dest_ctrler_iter;
        else
          -- check if it meets the "overall" condition?
          -- TODO: This is the "overall condition" or the second argument to our generic Unordered buffer/set container search function
          -- TODO: £overall_condition (API Argument 2) < found_entry 
          -- if (£overall_condition) < found_element then
          if £update_found_idx_expr then
            found_element := (£overall_condition);
            found_idx := £dest_ctrler_iter;
          endif;
        endif;
        -- TODO: NOTE: Move this into an if block that does something based on
        -- if an element was found...
        -- next_state.core_[ j ].LQ_.entries[ i ].state := write_result;
        found_entry := true;
      endif;
    endif;
  endfor;

  if (found_entry = false) then
    -- TODO: £when_fail
    £when_search_fail.stmts
    -- the metaprogramming doesn't allow for justparam with else at the moment
  elsif (found_entry = true) then
    -- NOTE: This is what this would look like... 
    -- TODO: Remember to use found_idx for the accessor in the translation 
    -- TODO: £when_success 
    £when_search_success.stmts
  endif;
    ]
 
    let decls : List Murϕ.Decl := [
      (Murϕ.Decl.var ["found_entry"] (Murϕ.TypeExpr.previouslyDefined "boolean")),
      (Murϕ.Decl.var ["found_element"] (Murϕ.TypeExpr.previouslyDefined "inst_count_t")),
      (Murϕ.Decl.var ["found_idx"] (Murϕ.TypeExpr.previouslyDefined dest_ctrler_idx_t))
    ]

    let stmts_decls : lst_stmts_decls := {
      stmts := murphi_template_stmts,
      decls := (
        when_search_success.decls ++
        when_search_fail.decls ++
        decls
      )
    }
    stmts_decls
  else if is_search_all then
    let ctrler_curr_idx : String := dest_ctrler_name.append "_search_all_curr_idx"
    let murphi_ctrler_curr_idx : Murϕ.Expr := [murϕ| £ctrler_curr_idx]

    -- Msg APi is : ctrler.search_all(inst.op == expected_type, etc..)
    let match_cond : Pipeline.Expr := lst_exprs[0]!
    let match_cond_trans_info : expr_translation_info :=  {
      expr := match_cond
      lst_ctrlers := term_trans_info.lst_ctrlers,
      -- The "main ctrler to translate with"
      -- is it the 
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := term_trans_info.src_ctrler, -- Option.some dest_ctrler_name, -- 
      lst_src_args := Option.some [], -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := term_trans_info.use_specific_dest_in_transition
      -- TODO: Double check what is this?
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx --term_trans_info.specific_murphi_dest_expr 
      lhs_var_is_just_default := false
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search match cond: ({match_cond_trans_info})"

    -- (
    -- assn_term_to_expr_translation_info term_trans_info match_cond)

    let murphi_match_cond_expr := ast_expr_to_murphi_expr match_cond_trans_info
    dbg_trace "&&&&& BEGIN match if cond &&&&&"
    dbg_trace murphi_match_cond_expr
    dbg_trace "&&&&& END match if cond &&&&&"

    -- AZ TODO: Identify the "search success case"
    -- and the "search failure case"
    -- translate them accordingly
    -- take the stmts in await, should be 2, the 2 whens?
    let when_stmts : List Pipeline.Statement := stmts_in_await.filter (
      λ stmt =>
        match stmt with
        | Pipeline.Statement.when _ _ _ =>
          true
        | _ => false
    )
    dbg_trace "&&&&& BEGIN when_stmts &&&&&"
    dbg_trace when_stmts
    dbg_trace "&&&&& END when_stmts &&&&&"
        -- qual name is probably the structure -- ... list idents is the args list!
        -- so take the qual name and check for search_success / fail
    let when_search_success_list : List Pipeline.Statement := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName :=
      match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error here?"
        default

      let is_search_success : Bool :=
      match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => (
      lst_idents.contains "search_success")

      is_search_success
    )
    let when_search_fail_list := when_stmts.filter (
      λ when_stmt =>
      let qual_name_from_when : QualifiedName := match when_stmt with
      | Pipeline.Statement.when qual_name _ _ => qual_name
      | _ => dbg_trace "place an error?"
        default
      let is_search_fail : Bool := match qual_name_from_when with
      | Pipeline.QualifiedName.mk lst_idents => lst_idents.contains "search_fail"
      is_search_fail
    )
    dbg_trace "&&&&& BEGIN when_success &&&&&"
    dbg_trace when_search_success_list
    dbg_trace "&&&&& END when_success &&&&&"
    dbg_trace "&&&&& BEGIN when_fail &&&&&"
    dbg_trace when_search_fail_list
    dbg_trace "&&&&& END when_fail &&&&&"
    -- dbg_trace "== assuming there are when search success/fail stmts found! =="
    let when_search_success := when_search_success_list[0]!
    let when_search_fail := when_search_fail_list[0]!

    dbg_trace s!"When ctrler: ({term_trans_info.ctrler_name})\n"++
      s!"When src_ctrler: ({dest_ctrler_name})"

    let when_success_stmt_args : List String :=
      match (when_search_success.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args
    let when_fail_stmt_args : List String :=
      match (when_search_fail.get_when_stmt_src_args) with
      | .error msg =>
        let msg' : String := s!"Error getting when stmt args in 'insert' API func: ({msg})"
        dbg_trace msg'
        -- default
        panic! msg'
      | .ok lst_args => lst_args

    let when_search_success_trans_info : stmt_translation_info := {
      stmt := when_search_success,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_success_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := Option.some murphi_ctrler_curr_idx,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- i.e. use this for lhs
      -- (ex. lhs = rhs ==> next_state.LQ.entry[desig_idx].lhs, ... rhs)
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx -- Option.some murphi_ctrler_curr_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search search success: ({when_search_success_trans_info})"

    let when_search_fail_trans_info : stmt_translation_info := {
      stmt := when_search_fail,
      lst_ctrlers := term_trans_info.lst_ctrlers,
      ctrler_name := term_trans_info.ctrler_name,
      src_ctrler := Option.some dest_ctrler_name, -- term_trans_info.src_ctrler,
      lst_src_args := Option.some when_fail_stmt_args -- term_trans_info.lst_src_args,
      func := term_trans_info.func,
      is_await := term_trans_info.is_await,
      entry_keyword_dest := Option.some dest_ctrler_name,
      trans_obj := term_trans_info.trans_obj,
      specific_murphi_dest_expr := term_trans_info.specific_murphi_dest_expr,
      lst_decls := term_trans_info.lst_decls,
      is_rhs := term_trans_info.is_rhs,
      use_specific_dest_in_transition := false
      -- search fail isn't in the loop, so we don't use the murphi_ctrler_curr_idx
      curr_ctrler_designator_idx := term_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := true
      translate_entry_or_ctrler := term_trans_info.translate_entry_or_ctrler
    }
    dbg_trace s!"Tail_Search search fail: ({when_search_fail_trans_info})"

    dbg_trace "(((***((( BEGIN TAIL SEARCH WHEN TRANSLATION ))))))"
    let when_search_success_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_success_trans_info
    let when_search_fail_murphi_stmts : lst_stmts_decls := ast_stmt_to_murphi_stmts when_search_fail_trans_info
    dbg_trace "(((***((( END TAIL SEARCH WHEN TRANSLATION ))))))"

    dbg_trace "&&&&& BEGIN Murϕ tail_search when_success &&&&&"
    dbg_trace s!"tail_search when_success: ({when_search_success})"
    dbg_trace when_search_success_murphi_stmts.stmts
    dbg_trace when_search_success_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ tail_search when_success &&&&&"
    dbg_trace "&&&&& BEGIN Murϕ tail_search when_fail &&&&&"
    dbg_trace s!"tail_search when_fail: ({when_search_fail})"
    dbg_trace when_search_fail_murphi_stmts.stmts
    dbg_trace when_search_fail_murphi_stmts.decls
    dbg_trace "&&&&& END Murϕ tail_search when_fail &&&&&"

    -- ex. SQ_NUM_ETNRIES_CONST
    let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])

    -- AZ NOTE: Use this point to build a different template
    -- based on the specific API call...
    -- Or maybe earlier, but i'm not 100% sure
    -- what the "common code" segments are just yet...

    -- TODO: Give them slightly more unique names, so they don't collide..
    -- This affects the curr_idx designator that will need to be adjusted as well
    let ctrler_while_break : String := dest_ctrler_name.append "_while_break"
    let ctrler_found_entry : String := dest_ctrler_name.append "_found_entry"
    let ctrler_entry_idx : String :=   dest_ctrler_name.append "_entry_idx"
    let ctrler_difference : String :=  dest_ctrler_name.append "_difference"
    let ctrler_offset : String :=      dest_ctrler_name.append "_offset"
      -- This one is defined above
    -- let ctrler_curr_idx : String :=    /- dest_ctrler_name.append -/ "_curr_idx"

    let dest_ctrler : Ctrler := match ctrlers_lst.ctrler_from_name dest_ctrler_name with
      | .ok ctrler => ctrler
      | .error msg' =>
        dbg_trace s!"Error getting dest ctrler in 'search_all' API func: ({msg'})"
        default
    let dest_ctrler_type : CtrlerType := match dest_ctrler.type with
      | .ok ctrler_type => ctrler_type
      | .error msg' =>
        dbg_trace s!""
        default

    let ctrler_idx_t := dest_ctrler_name.append "_idx_t"
    let decls : List Murϕ.Decl :=
      match dest_ctrler_type with
      | .FIFO => 
        [
          (Murϕ.Decl.var [ctrler_while_break] (Murϕ.TypeExpr.previouslyDefined "boolean")),
          (Murϕ.Decl.var [ctrler_found_entry] (Murϕ.TypeExpr.previouslyDefined "boolean")),
          -- [murϕ_var_decls|
          --   var found_entry : boolean
          -- ],
          (Murϕ.Decl.var [ctrler_entry_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
          (Murϕ.Decl.var [ctrler_difference] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
          (Murϕ.Decl.var [ctrler_offset] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t)),
          (Murϕ.Decl.var [ctrler_curr_idx] (Murϕ.TypeExpr.previouslyDefined ctrler_idx_t))
          -- [murϕ_var_decls|
          --   var curr_idx : £ctrler_idx_t
          -- ]
        ]
      | .Unordered =>
        []
      | .BasicCtrler =>
        dbg_trace s!"Error: This API Msg Shouldn't be used with a Basic Ctrler?"
        default
         
    let dest_ctrler_name_ := dest_ctrler_name.append "_"
    let overall_murphi_tail_search_template : List Murϕ.Statement :=
      match dest_ctrler_type with
      | .FIFO =>
        [
          -- AZ TODO: introduce a type for the ACCESS_HASH
          -- or ACCESS_TAIL and just set it at the beginning

          -- [murϕ| next_state := Sta]
          -- [murϕ|  sq := Sta.core_[j].lsq_.sq_],
          -- [murϕ|  lq := Sta.core_[j].lsq_.lq_],
          [murϕ|  £ctrler_while_break := false],
          [murϕ|  £ctrler_found_entry := false],
          [murϕ|  if (next_state .core_[j] .£dest_ctrler_name_ .num_entries = 0) then
              £ctrler_while_break := true;
            endif],
            -- AZ TODO:
            -- no, can just map the condition check

          -- [murϕ|  if (£dest_ctrler_name .sq_msg_enum = £dest_ctrler_name_ACCESS_HASH) then
          --     st_idx := find_st_idx_of_seq_num(£dest_ctrler_name,
          --                                      £dest_ctrler_name .st_seq_num);
          --   elsif (£dest_ctrler_name .sq_msg_enum = SQ_ACCESS_TAIL) then
          --     st_idx := (£dest_ctrler_name .sq_tail + ( SQ_ENTRY_NUM + 1) - 1) % ( SQ_ENTRY_NUM + 1 );
          --   endif],
          [murϕ|  £ctrler_entry_idx := (next_state .core_[j] .£dest_ctrler_name_ .tail + £dest_num_entries_const_name - 1) % £dest_num_entries_const_name ],
          [murϕ|  £ctrler_difference := ( £ctrler_entry_idx + £dest_num_entries_const_name - next_state .core_[j] .£dest_ctrler_name_ .head ) % £dest_num_entries_const_name],
          [murϕ|  £ctrler_offset := 0],
          [murϕ|   while ( (£ctrler_offset <= £ctrler_difference) & (£ctrler_while_break = false) /- & ( £ctrler_found_entry = false )-/ ) do
              £ctrler_curr_idx := ( £ctrler_entry_idx + £dest_num_entries_const_name - £ctrler_offset ) % £dest_num_entries_const_name;
              if (
                -- AZ TODO:
                -- THIS IS WHERE TO TRANSLATE THE "API ARGS LIST"
                -- INTO CONDITIONS
                -- Keeping the code here as an example of
                -- what to expect kind of

                -- £dest_ctrler_name_ .entries[curr_idx] .phys_addr
                --   =
                --   £dest_ctrler_name_ .phys_addr
                £murphi_match_cond_expr

                  ) then
    
                -- AZ TODO:
                -- This should be replaced with "this" ctrler's
                -- code
                -- Note that "entry" indicates access to the dest ctrler's
                -- fields
                -- So that we can even use entry in the when block...
                £when_search_success_murphi_stmts.stmts

                -- value := £dest_ctrler_name_ .entries[curr_idx] .write_value;
    
                -- £ctrler_found_entry := true;
              endif;
    
              -- This is not really necessary
              if (£ctrler_offset != £ctrler_difference) then
                £ctrler_offset := £ctrler_offset + 1;
              else
                £ctrler_while_break := true;
              endif;
            end]
          --   ,
          -- [murϕ|
          --   if (£ctrler_found_entry = false) then
          --     £when_search_fail_murphi_stmts.stmts
          --   endif]
        ]
      | .Unordered =>
        let dest_ctrler_idx_t : String := dest_ctrler_name.append "_idx_t";
        [murϕ_statements|
  -- found_entry := false;
  for £ctrler_curr_idx : £dest_ctrler_idx_t do
    -- TODO: Put an if condition which checks if the seq_num if invalid (i.e. is 0)
    if (next_state .core_[ j ] .£dest_ctrler_name_ .entries[ £ctrler_curr_idx ] .valid = true) then
    -- if entry is invalid, don't do anything.
      -- TODO: £condition (API Argument 1)
      if ( £murphi_match_cond_expr ) then
        -- TODO: The first time we find a match, we write a value to the recorded value we check
          £when_search_success_murphi_stmts.stmts
      endif;
    endif;
  endfor;

  -- if (found_entry = false) then
  --   -- TODO: £when_fail
  --   £when_search_fail.stmts
  --   -- the metaprogramming doesn't allow for justparam with else at the moment
  -- elsif (found_entry = true) then
  --   -- NOTE: This is what this would look like... 
  --   -- TODO: Remember to use found_idx for the accessor in the translation 
  --   -- TODO: £when_success 
  --   £when_search_success.stmts
  -- endif;
    ]
      | .BasicCtrler =>
        dbg_trace s!"Error: This API Msg Shouldn't be used with a Basic Ctrler?"
        default


    -- 0
    let stmts_decls : lst_stmts_decls := {
      stmts := overall_murphi_tail_search_template,
      decls := (
        when_search_success_murphi_stmts.decls ++
        when_search_fail_murphi_stmts.decls ++
        decls
      )
    }
    stmts_decls
    -- default
  else
    -- TODO: THROW!
    dbg_trace "The API Term Function didn't match any expected API names!"
    empty_stmt_decl_lsts

partial def dsl_type_to_murphi_type_string
( dsl_type : Identifier )
: String
:=
  -- Types that one may use in the DSL.. & it's Murphi Test Harness "Equivalent" (for now that is)
  -- DSL        |    Murphi Test Harness
  -- -----------------------------------
  -- address    |    addr_idx_t
  -- u32        |    val_t
  -- packet     |    N/A
  -- seq_num    |    inst_idx_t
  let murphi_type_name : ID :=
  if dsl_type == "address" then
    "addr_idx_t"
  else if dsl_type == "u32" then
    "val_t"
  else if dsl_type == "seq_num" then
    "inst_count_t"
  else if dsl_type == "inst" then
    "INST"
  else
    let msg : String :=
      "ERROR: ===== ENCOUNTERED UNEXPECTED DSL TYPE ====="++
      s!"The type is: ({dsl_type})"
    panic! msg

  murphi_type_name

partial def murphi_type_to_null
(murphi_type : String)
: Murϕ.Expr
:=
  if murphi_type == "addr_idx_t" then
    Murϕ.Expr.integerConst 0
  else if murphi_type == "val_t" then
    Murϕ.Expr.integerConst 0
  else if murphi_type == "inst_count_t" then
    Murϕ.Expr.integerConst 0
  else
    let msg : String :=
      "ERROR: ===== ENCOUNTERED UNEXPECTED Murphi TYPE =====\n"++
      s!"The type is: ({murphi_type})"
    panic! msg
  

-- AZ TODO: Implement these 2 functions!!!
partial def ast_stmt_to_murphi_stmts
(stmt_trans_info : stmt_translation_info)
:
-- (List Murϕ.Statement)
lst_stmts_decls
:=
  dbg_trace s!"stmt_to_translate: ({stmt_trans_info.stmt})"
  let stmt := stmt_trans_info.stmt
  let ctrlers_lst := stmt_trans_info.lst_ctrlers
  let ctrler_name := stmt_trans_info.ctrler_name

  -- when statement stuff (handling nested scopes?
  -- or rather, clojures?)
  let src_ctrler := stmt_trans_info.src_ctrler
  let lst_src_args := stmt_trans_info.lst_src_args
  let func_name : Option Identifier := stmt_trans_info.func
  let await_or_not := stmt_trans_info.is_await

  match stmt with
  | Statement.labelled_statement label stmt =>
    let new_translation_info := assn_stmt_to_stmt_translation_info stmt_trans_info stmt

    -- let stmt_lst : List Murϕ.Statement := ast_stmt_to_murphi_stmts new_translation_info
    let stmt_lst : lst_stmts_decls := ast_stmt_to_murphi_stmts new_translation_info

    stmt_lst
  | Statement.variable_declaration typed_ident =>
    -- AZ NOTE: must ignore declarations,
    -- since they go in the decl list,
    -- and not the stmt list
    -- Well.. i kinda have the other func for this..
    -- but i still need the decl list for manually returning the 
    -- template func decls, so i can avoid doing type inference...
    let lsts : lst_stmts_decls := { stmts := [], decls := []}
    lsts
  | Statement.value_declaration typed_ident expr =>
    -- dbg_trace "$$$$$$$ CHECK FOR NULL $$$$$$$"
  -- AZ NOTE: In this case there's no way this is
  -- a controller state var, leave the code as-is

  -- Actually no, we need to do the Designator generation
  -- For Exprs in assignment as well
  -- 

  -- NOTE TO SELF: I suppose it's really just the
  -- var assignment cases to look for.

    -- AZ NOTE: I think we will need to pass in
    -- the list of all ctrlers
    -- For this so that I can get other ctrler's info

    -- But for this, we need to make a Murϕ stmt &
    -- also produce a Decl for this
-- inductive Designator
-- | mk : ID → List (ID ⊕ Expr) → Designator
    let assned_var's_name :=
    match typed_ident with
    | TypedIdentifier.mk tiden ident =>
      ident

    let designator :=
    Designator.mk assned_var's_name []

    -- Which murphi expr AST is the right match up? 
    -- Must match DSL Expr to Murphi Expr.
    -- Perhaps I should make a func for this :)
    let expr_trans_info : expr_translation_info := {
      expr := expr,
      ctrler_name := stmt_trans_info.ctrler_name,
      lst_ctrlers := stmt_trans_info.lst_ctrlers,
      src_ctrler := stmt_trans_info.src_ctrler
      lst_src_args := stmt_trans_info.lst_src_args
      func := stmt_trans_info.func
      is_await := stmt_trans_info.is_await
      entry_keyword_dest := stmt_trans_info.entry_keyword_dest
      trans_obj := stmt_trans_info.trans_obj
      specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr
      lst_decls := stmt_trans_info.lst_decls
      is_rhs := true
      use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition
      curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := stmt_trans_info.lhs_var_is_just_default
      translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
    }
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info
    
    let is_null_expr : Bool := (
      match expr with
      | Pipeline.Expr.some_term term =>
        match term with
        | Term.var ident =>
          -- I think this is marked as a Var, rather than a const 
          if ident == "NULL" then
            dbg_trace "$$$$$$$ FOUND NULL VAR $$$$$$$"
            true
          else false
        | _ => false
      | _ => false
    )

    if is_null_expr then
      -- translate, identify the type
      let dsl_type : Identifier := (
        match typed_ident with
        | TypedIdentifier.mk tiden ident => tiden
      )

      let murphi_type_expr := dsl_type_to_murphi_type_string dsl_type
      let murphi_null := murphi_type_to_null murphi_type_expr

      let murphi_assignment_stmt :=
      Murϕ.Statement.assignment designator murphi_null

      let lsts : lst_stmts_decls := {
        stmts := [murphi_assignment_stmt],
        decls := []
      }
      dbg_trace s!"val decl translated: ({lsts.stmts})"
      lsts
    else

      let murphi_assignment_stmt :=
      Murϕ.Statement.assignment designator murphi_expr

      let lsts : lst_stmts_decls := {
        stmts := [murphi_assignment_stmt],
        decls := []
      }
      dbg_trace s!"val decl translated: ({lsts.stmts})"
      lsts

  | Statement.return_stmt expr =>
    let expr_trans_info :=
      assn_stmt_to_expr_translation_info stmt_trans_info expr
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info

    let murphi_return_stmt :=
    Murϕ.Statement.returnstmt murphi_expr

    let lsts : lst_stmts_decls := {
      stmts := [murphi_return_stmt],
      decls := []
    }
    lsts

  | Statement.block lst_stmts =>
    -- let num_stmts := lst_stmts.length
    -- let num_stmts_of_ctrlers_lst :=
    --   List.replicate num_stmts ctrlers_lst
    -- let zipped_stmts_and_ctrlers :=
    --   lst_stmts.zip num_stmts_of_ctrlers_lst
    let stmt_trans_info_lst :=
      lst_stmts.map (
        λ stmt =>
          assn_stmt_to_stmt_translation_info stmt_trans_info stmt
      )
    
    let murphi_stmts_lst_lst :=
    stmt_trans_info_lst.map ast_stmt_to_murphi_stmts

    -- let empty_stmt_decl_lsts : lst_stmts_decls := {
    --   stmts := [],
    --   decls := []
    -- }
    let murphi_stmts_lst := murphi_stmts_lst_lst.foldl (
      λ lsts_lst lsts =>
        let new_lsts : lst_stmts_decls := {
          stmts := lsts_lst.stmts ++ lsts.stmts,
          decls := lsts_lst.decls ++ lsts.decls
        }
        new_lsts
    ) empty_stmt_decl_lsts

    murphi_stmts_lst

  /-
  The stray expr case is special..
  we need to make sure we correctly translate
  this "insert" operation

  This also applies to the memory interface call
  -/

  | Statement.stray_expr _ =>

  let lst_murphi_stmts : lst_stmts_decls :=
  ast_stmt_stray_expr_to_murphi_expr stmt_trans_info

  lst_murphi_stmts

  | Statement.await none lst_stmts =>
    -- nothing to do here, we're awaiting on
    -- another structure to do something
    -- []
    empty_stmt_decl_lsts

  /-
  Listen & Handle...
  do we really need this at the moment?
  I can't imagine it at the moment...

  Either way, first just recursively call this
  for the Listen block's stmts!
  -/
  | Statement.listen_handle stmt handle_blk =>
    let stmt_trans_info' :=
      assn_stmt_to_stmt_translation_info stmt_trans_info stmt
    let murphi_stmt :=
    ast_stmt_to_murphi_stmts stmt_trans_info'

    -- note, ignoring the handle block for now

    -- AZ FUTURE TODO:
    -- AZ NOTE: handle_block should be "simple"
    -- to translate later..
    -- It should just be a state check from when
    -- another structure attempts to manipulate
    -- an entry, it must check if this entry is on
    -- this state, for if it has any
    -- "handle block actions" it needs to do
    murphi_stmt

  /-
  Conditional 
  Should be 1 for 1 between the DSL and Murphi
  -/
  | Statement.conditional_stmt conditional =>
  dbg_trace "!!!!! BEGIN Conditional !!!!!"
  dbg_trace stmt
  dbg_trace "!!!!! END Conditional !!!!!"
    dbg_trace s!"Conditional Stmt: ({stmt})"
    match conditional with
    | Conditional.if_else_statement expr stmt1 stmt2 =>
      dbg_trace s!"(if_else) Condition's Expr: ({expr})"
      -- map to Murphi
      -- This mapping is kind of simple
      -- Perhaps recursively checking the stmts
      -- would help map to a flatter Murphi structure
      let expr_trans_info := 
        assn_stmt_to_expr_translation_info stmt_trans_info expr
      let murphi_expr := ast_expr_to_murphi_expr expr_trans_info

      let stmt_trans_info1 := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt1
      let murphi_stmt1 : lst_stmts_decls :=
      ast_stmt_to_murphi_stmts stmt_trans_info1

      let stmt_trans_info2 := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt2
      let murphi_stmt2 : lst_stmts_decls  :=
      ast_stmt_to_murphi_stmts stmt_trans_info2

      let murphi_if_stmt :=
      Murϕ.Statement.ifstmt murphi_expr murphi_stmt1.stmts [] murphi_stmt2.stmts

      dbg_trace "!!!!! BEGIN generated if-else -> if stmt !!!!!"
      dbg_trace murphi_if_stmt
      dbg_trace "!!!!! END generated if-else -> if stmt !!!!!"

      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_if_stmt],
        decls := murphi_stmt1.decls ++ murphi_stmt2.decls
        }
      stmts_decls
    | Conditional.if_statement expr stmt =>
      dbg_trace s!"(if) Condition's Expr: ({expr})"
      let expr_trans_info := 
        assn_stmt_to_expr_translation_info stmt_trans_info expr
      let murphi_expr := ast_expr_to_murphi_expr expr_trans_info
      dbg_trace s!"translated to Murphi Expr: ({murphi_expr})"

      let stmt_trans_info' := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt
      let murphi_stmt :=
      ast_stmt_to_murphi_stmts stmt_trans_info'

      let murphi_if_stmt :=
      Murϕ.Statement.ifstmt murphi_expr murphi_stmt.stmts [] []
      dbg_trace "!!!!! BEGIN generated if-stmt -> if stmt !!!!!"
      dbg_trace murphi_if_stmt
      dbg_trace "!!!!! END generated if-stmt -> if stmt !!!!!"

      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_if_stmt],
        decls := murphi_stmt.decls
      }
      stmts_decls

  /-
  Variable assignment
  This should be 1 to 1 between the DSL and Murphi
  * Caveat: I'm leaving the annoying part of having to
  provide the initial state assignment to Declared Vars
  done by the Decl generation part

  No wait, there is the case of
  if something is a struct, then we need to
  check the qualified name to see if the
  dest var is a part of a struct

  Then note that for the Decl generation,
  we must take the left most ID (or just the ID
  in the designator basically) and generate stmt
  to init this
  -/
  | Statement.variable_assignment qual_name expr =>
  -- So, what needs to be done:
  /-
  Check if the qual name is a state var
  -- (a) this is the case of len 1 qual_name list
  -- (b) if there's a len 1 list, check if it
  -- matches any state var entries
  -- Upon a match, we generate the full Designator

  -- The full designator depends on if the
  -- designator is a buffer or not
  -- if it is, then we do the
  -- <designator>.entries[<entry>] thing
  -- though figuring out <entry> is difficult
  -/
    let unwrapped_func_name :=
    if func_name.isSome then
      func_name.get!
    else
      -- panic! "func name wasn't provided?"
      -- var assignment won't always be assigned a func name
      ""

    let tail_entry : tail_or_entry :=
    -- if unwrapped_func_name == "insert"
    -- then tail_or_entry.tail
    -- else
    if (
        ( stmt_trans_info.specific_murphi_dest_expr.isSome ) ||
        (stmt_trans_info.curr_ctrler_designator_idx.isSome)
        --||
        -- (term_trans_info.is_rhs && specific_murphi_dest_expr_is_some) ||
        -- ( ident == "curr_state" )
        ) then
      dbg_trace "CUSTOM ENTRY"
      tail_or_entry.custom_entry
    else
      dbg_trace "BASIC ENTRY"
      tail_or_entry.entry

        let bool_thing : Bool :=
        if ctrler_name == "" then
        dbg_trace "===== BLANK STRING CTRLER NAME ====="
        false
        else
        true
    let lst_idents := match qual_name with
    | Pipeline.QualifiedName.mk lst_idents => lst_idents

    -- let assigned_var_entry : tail_or_entry :=
    --   if (stmt_trans_info.curr_ctrler_designator_idx.isSome ||
    --     stmt_trans_info.specific_murphi_dest_expr.isSome)
    --     then
    --     tail_or_entry.custom_entry
    --   else
    --     tail_or_entry.entry

    let curr_ctrler_idx_isSome : Bool := stmt_trans_info.curr_ctrler_designator_idx.isSome
    let specific_murphi_idx_isSome : Bool := stmt_trans_info.specific_murphi_dest_expr.isSome

    dbg_trace s!"Translate Assgn Stmt curr_ctrler_designator_idx: ({stmt_trans_info.curr_ctrler_designator_idx})"
    dbg_trace s!"Translate Assgn Stmt specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
    let (designator_idx, assigned_var_entry) : ( Option Murϕ.Expr ) × tail_or_entry :=
      if stmt_trans_info.src_ctrler.isSome then
        dbg_trace "src_ctrler is some! ({stmt_trans_info.src_ctrler})"
        if stmt_trans_info.src_ctrler.get! == stmt_trans_info.ctrler_name then
          dbg_trace "Ctrler & Src ctrler are the same! in Assignment stmt translation"
          if curr_ctrler_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
            dbg_trace "Assign curr_ctrler_desig_idx"
            dbg_trace "curr_ctrler_desig_idx: ({stmt_trans_info.curr_ctrler_designator_idx})"
            ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
          else if specific_murphi_idx_isSome && !stmt_trans_info.lhs_var_is_just_default then
            dbg_trace "Assign specific_murphi_dest_expr"
            dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
            ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
          else
            dbg_trace "Use none for designator idx"
            (Option.none, tail_or_entry.entry)
        else
          dbg_trace "Ctrler & Src ctrler are the different! in Assignment stmt translation"
          dbg_trace "Thus, use curr ctrler_designator since it's the lhs!"
          -- if specific_murphi_idx_isSome then
          --   ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
          -- else
          if curr_ctrler_idx_isSome then
            ( stmt_trans_info.curr_ctrler_designator_idx, tail_or_entry.custom_entry )
          else
            (Option.none, tail_or_entry.entry)
      else
        dbg_trace "No src_ctrler, so just use specific murphi dest expr"
        dbg_trace "specific_murphi_dest_expr: ({stmt_trans_info.specific_murphi_dest_expr})"
        if specific_murphi_idx_isSome then
          ( stmt_trans_info.specific_murphi_dest_expr, tail_or_entry.custom_entry )
        else
          (Option.none, tail_or_entry.entry)
      -- if stmt_trans_info.curr_ctrler_designator_idx.isSome then
      --   dbg_trace "Assign curr_ctrler_desig_idx"
      --   stmt_trans_info.curr_ctrler_designator_idx
      -- -- else if stmt_trans_info.specific_murphi_dest_expr.isSome then
      -- --   stmt_trans_info.specific_murphi_dest_expr
      -- else if stmt_trans_info.specific_murphi_dest_expr.isSome then
      --   dbg_trace "Assign specific_murphi_dest_expr"
      --   stmt_trans_info.specific_murphi_dest_expr
      -- else
      --   dbg_trace "Assigning a var in a var assignment stmt w/ basic idx"
      --   Option.none
    let murphi_var_name_designator :=
      match qual_name with
      | QualifiedName.mk lst_idents =>
        list_ident_to_murphi_designator_ctrler_var_check
        lst_idents ctrlers_lst ctrler_name assigned_var_entry --tail_entry
        designator_idx --stmt_trans_info.specific_murphi_dest_expr
        stmt_trans_info.translate_entry_or_ctrler

-- AZ TODO CHECKPOINT:
-- make this ast_expr_to_murphi_expr also
-- add the structure name stuff..
    -- let expr_trans_info := 
    --   assn_stmt_to_expr_translation_info stmt_trans_info expr
    let expr_trans_info : expr_translation_info := {
    expr := expr,
    ctrler_name := stmt_trans_info.ctrler_name,
    lst_ctrlers := stmt_trans_info.lst_ctrlers,
    src_ctrler := stmt_trans_info.src_ctrler
    lst_src_args := stmt_trans_info.lst_src_args
    func := stmt_trans_info.func
    is_await := stmt_trans_info.is_await
    entry_keyword_dest := stmt_trans_info.entry_keyword_dest
    trans_obj := stmt_trans_info.trans_obj
    specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr
    lst_decls := stmt_trans_info.lst_decls
    is_rhs := true
    use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition
    curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
    lhs_var_is_just_default := stmt_trans_info.lhs_var_is_just_default
    translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
    }
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info
    
    let is_null_expr : Bool := (
      match expr with
      | Pipeline.Expr.some_term term =>
        match term with
        | Term.var ident =>
          -- I think this is marked as a Var, rather than a const 
          if ident == "NULL" then
            true
          else false
        | _ => false
      | _ => false
    )

    if is_null_expr then
      -- match the var's type with an AST type, then get
      -- the Murphi type, the get the Murphi type's NULL value (like 0, or "")
      let this_ctrler : controller_info :=
        get_ctrler_matching_name ctrler_name ctrlers_lst
      
      let state_vars_to_use : List TypedIdentifier :=
        if this_ctrler.init_trans.isSome then
          this_ctrler.state_vars.get!
        else if this_ctrler.ctrler_init_trans.isSome then
          this_ctrler.ctrler_state_vars.get!
        else
          dbg_trace "ERROR, ctrler doesn't have entry or ctrler transition info? ({this_ctrler})"
            default

      let lst_state_var_idents : List (String × String) := state_vars_to_use.map (
        λ t_ident =>
          match t_ident with
          | TypedIdentifier.mk tident ident => (tident, ident)
      )

      let match_state_var : List String := List.join (
      lst_state_var_idents.map (
        λ idents_lst =>
          if idents_lst.2 == lst_idents[0]! then
            let ident_type : String := idents_lst.1
            [ident_type, idents_lst.2]
          else
            []
        )
      )


      if match_state_var.length > 0 then
        let dsl_type := match_state_var[0]!
        let murphi_type_expr := dsl_type_to_murphi_type_string dsl_type
        let murphi_null := murphi_type_to_null murphi_type_expr

        let murphi_assn_stmt :=
        Murϕ.Statement.assignment murphi_var_name_designator murphi_null
    
        -- let murphi_decl :=
        -- Murϕ.Decl.var 
        let stmts_decls : lst_stmts_decls := {
          stmts := [murphi_assn_stmt],
          decls := []
        }
        dbg_trace s!"(Var len > 0) The Stmts gen'd from a AST var assignment: ({stmts_decls.stmts})"
        stmts_decls
      else
        let murphi_assn_stmt :=
          Murϕ.Statement.assignment murphi_var_name_designator murphi_expr
    
        let stmts_decls : lst_stmts_decls := {
          stmts := [murphi_assn_stmt],
          decls := []
        }
        dbg_trace s!"(Var len <= 0) The Stmts gen'd from a AST var assignment: ({stmts_decls.stmts})"
        stmts_decls
    else
      let murphi_assn_stmt :=
        Murϕ.Statement.assignment murphi_var_name_designator murphi_expr
    
      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_assn_stmt],
        decls := []
      }
      dbg_trace s!"The Stmts gen'd from a AST var assignment: ({stmts_decls.stmts})"
      stmts_decls

  -- 0

  | Statement.transition ident =>
    -- for this, we check our state thing
    -- see if this is an await state
    -- if this is an await state, return []
    -- else we translate this to a state update
    -- to the identifier
    let murphi_stmt :=
    match await_or_not with
    | await_or_not_state.await => 
      -- return nothing
      -- []
      empty_stmt_decl_lsts
    | await_or_not_state.not_await => 
      -- translate ident into updating
      -- this structure's entry state
      -- ident
      let this_ctrler : controller_info :=
    -- dbg_trace "===== dsl transition to murphi translation ====="
        get_ctrler_matching_name ctrler_name ctrlers_lst

      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_indexable : Bool :=
        IndexableCtrlerTypesStrings.contains ctrler_ordering

      if is_indexable
      then
      -- we access the specific entry i for the rule

      let ruleset_core_elem_idx := "j"
      let core_idx_designator :=
      Murϕ.Expr.designator (
        Designator.mk ruleset_core_elem_idx []
      )

      -- let ctrler_id := this_ctrler.name

      let entries := "entries"

      let tail_entry :=
        if stmt_trans_info.curr_ctrler_designator_idx.isSome ||
          stmt_trans_info.specific_murphi_dest_expr.isSome ||
          stmt_trans_info.use_specific_dest_in_transition then
        dbg_trace "CUSTOM ENTRY"
        tail_or_entry.custom_entry
        else
        dbg_trace "BASIC ENTRY"
        tail_or_entry.entry
      -- AZ TODO: Use the specific entry thing
      -- let ruleset_entry_elem_idx := "i"
      let queue_idx :=
        match tail_entry with
        | tail_or_entry.entry => "i"
        | tail_or_entry.custom_entry =>
          if stmt_trans_info.use_specific_dest_in_transition then
            ""
          else
            "i"
        | tail_or_entry.tail => "tail"

      dbg_trace s!"Tail Entry to use: ({tail_entry})"
      dbg_trace s!"use_specific_dest_in_transition: ({stmt_trans_info.use_specific_dest_in_transition})"
      dbg_trace s!"Specific designator dest: ({stmt_trans_info.curr_ctrler_designator_idx})"
      -- dbg_trace s!": ({})"

      let entry_idx_designator :=
      -- Murϕ.Expr.designator (
      --   Designator.mk ruleset_entry_elem_idx []
      -- )
      match tail_entry with
        | tail_or_entry.tail =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk "next_state" [
        -- entries
        -- Assume the buffer entries are
        -- referred to as 'i'
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (ctrler_name.append "_"),
        Sum.inl queue_idx
        ])
        | tail_or_entry.entry =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk queue_idx [])
        | tail_or_entry.custom_entry =>
        dbg_trace "SPECIFIC MURPHI EXPR, for transition state!!"
        dbg_trace stmt_trans_info.specific_murphi_dest_expr

        if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.curr_ctrler_designator_idx.isSome then
          stmt_trans_info.curr_ctrler_designator_idx.get!
        else if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.specific_murphi_dest_expr.isSome then
          stmt_trans_info.specific_murphi_dest_expr.get!
        else
          Murϕ.Expr.designator (
          Murϕ.Designator.mk queue_idx [])

      let state := "state"

      let current_structure_entry_state :=
        -- Murϕ.Expr.designator (
          Designator.mk (
            -- Example in comments
            -- core_
            "next_state"
          )
          [
            -- Example in comments
            Sum.inl "core_",
            -- core_[i]
            Sum.inr core_idx_designator,
            -- core_[i].LQ
            Sum.inl (ctrler_name.append "_"),
            -- core_[i].LQ.entries
            Sum.inl entries,
            -- core_[i].LQ.entries[j]
            Sum.inr entry_idx_designator,
            -- core_[i].LQ.entries[j].state
            Sum.inl state
          ]
        -- )

      -- want to assign the state the ident

      let murphi_state_assn : Murϕ.Statement :=
        Murϕ.Statement.assignment
        current_structure_entry_state
        (Murϕ.Expr.designator (
          Murϕ.Designator.mk ident []
        ) )

      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_state_assn],
        decls := []
      }
      stmts_decls
      else
      -- AZ TODO NOTE: Consider the case of a unit
        -- If this isn't a FIFO / buffer structure
        -- Then do we just assign the unit's state?
        -- []
        -- empty_stmt_decl_lsts
        let ctrler_name_ : String := ctrler_name.append "_";
        let stmts_decls : lst_stmts_decls := {
          -- Should just be ctrler
          stmts := [murϕ| next_state .core_[j] .£ctrler_name_ .state := £ident;],
          decls := []
        }
        stmts_decls
    murphi_stmt
  -- TODO: Fill in these cases,
  -- These kinda go hand in hand,
  -- Since
  -- Await should be somewhat simple?
  | Statement.await term lst_stmts =>
    -- So, this term is the func call...
    -- List of Stmts is the code block within...
      -- But this should just have when stmts...
    /-
    Two Main Tasks Here:
    1. Generate the Murphi code (template probably)
       to perform the function {search + fwd, search + squash, etc.}
    -- Function should do sth
    2. The function may have a certain return case {search success, fail, etc.}
    -- We generate these cases by matching what the function returned with
    -/
    -- 1. March term with a function call (must fix the API names...)
    -- , use the args for any parameters of the function

    -- 2. in the function template code, translate and insert the
    -- when conditions in the right place..
    -- Could also check the number of when stmts and template function
    -- cases and throw an error if needed

    if term.isSome
    then
      let extracted_term := if term.isSome then
      term.get!
      else
      panic! "Term was blank?? But none case would have been above?"

      -- TODO: Pick out the dest structure name to the
      -- code will be used with
      -- Also gen any parameterized args accordingly
      let term_trans_info := assn_stmt_to_term_translation_info stmt_trans_info extracted_term
      -- TODO: Actually handle the Option term

      let murphi_tail_search_template : lst_stmts_decls :=
      api_term_func_to_murphi_func term_trans_info lst_stmts

      murphi_tail_search_template
    else
      empty_stmt_decl_lsts

  | Statement.when qual_name lst_ident stmt' =>
  -- TODO : Implement this case
    -- match when_stmt with
    -- | Pipeline.Statement.when qual_name lst_ident stmt =>
    let qual_name_list :=
    match qual_name with
    | QualifiedName.mk lst_idents =>
      lst_idents
    -- i.e. ctrler & function
    let qual_name_len_2 := qual_name_list.length == 2

    let dest_ctrler_name := qual_name_list[0]?

    let sanity_check :=
    if qual_name_len_2
    then
      dbg_trace "translating insert func!"
      dbg_trace "PASS: qualified name, is len 2!"
      true
    else
      dbg_trace "translating insert func!"
      dbg_trace "FAIL: qualified name, is not len 2!"
      false
    
    dbg_trace "== This was also len 2 checked! =="
    let struct_name : Identifier := qual_name_list[0]!
    let when_func_name : Identifier := qual_name_list[1]!
    let struct_name_sanity := struct_name == ctrler_name
    let when_func_name_sanity := when_func_name == func_name

    let struct_sanity_check :=
    if struct_name_sanity
    then
      dbg_trace "translating insert func!"
      dbg_trace "PASS: first identifier is the curr_ctrler_name"
      true
    else
      dbg_trace "translating insert func!"
      dbg_trace "FAIL: first identifier is not the curr_ctrler_name"
      false

    let func_sanity_check :=
    if when_func_name_sanity
    then
      dbg_trace "translating insert func!"
      dbg_trace "PASS: second identifier is the 'insert' func"
      true
    else
      dbg_trace "translating insert func!"
      dbg_trace "FAIL: second identifier is not the 'insert' func"
      false

    dbg_trace "(((((( BEGIN SPECIFIC MURPHI EXPR ))))))"
    dbg_trace stmt_trans_info.specific_murphi_dest_expr
    dbg_trace "(((((( END SPECIFIC MURPHI EXPR ))))))"

    -- After any sanity messages, try to map the stmts
    -- Create the required info object:
    let trans_info : stmt_translation_info := {
      -- info
      -- stmt_translation_info.mk
      stmt := stmt',
      lst_ctrlers := stmt_trans_info.lst_ctrlers,
      ctrler_name := stmt_trans_info.ctrler_name,
      src_ctrler := dest_ctrler_name,
      lst_src_args :=
      if stmt_trans_info.lst_src_args.isSome then
        stmt_trans_info.lst_src_args
      else
        lst_ident,
      func := stmt_trans_info.func,
      is_await := stmt_trans_info.is_await,
      entry_keyword_dest := stmt_trans_info.entry_keyword_dest,
      trans_obj := stmt_trans_info.trans_obj,
      specific_murphi_dest_expr := stmt_trans_info.specific_murphi_dest_expr,
      lst_decls := stmt_trans_info.lst_decls,
      is_rhs := stmt_trans_info.is_rhs
      use_specific_dest_in_transition := stmt_trans_info.use_specific_dest_in_transition
      curr_ctrler_designator_idx := stmt_trans_info.curr_ctrler_designator_idx
      lhs_var_is_just_default := stmt_trans_info.lhs_var_is_just_default
      translate_entry_or_ctrler := stmt_trans_info.translate_entry_or_ctrler
    }

    -- let murphi_stmts : List Murϕ.Statement :=
    let murphi_stmts : lst_stmts_decls :=
    ast_stmt_to_murphi_stmts trans_info

    murphi_stmts
    -- map the stmt (stmt blk) to Murphi stmts,
    -- but also consider that it's assigned vars
    -- should be generated with the ctrler's designators

    -- The Decl gen process shouldn't be affected, since
    -- the desginators will start with the structure
    -- as the decl to generate...
    -- So i think this should be ok...

    -- TODO: This should also be translated by a 
    -- function which will explicitly take the
    -- dest structure name as an input arg, so it
    -- can translate it and reference it's entry tail
    -- as needed

  -- | _ => dbg_trace "shouldn't get another stmt type"
  --   -- []
  --   empty_stmt_decl_lsts
  | Statement.stall _ =>
    dbg_trace "Unimplemented (stall (expr)). Just using an Await statement with tail_search for now."
    default
  | Statement.reset ident =>
    -- NOTE: Copied from Statement.transition
    let murphi_stmt :=
    match await_or_not with
    | await_or_not_state.await => 
      -- return nothing
      empty_stmt_decl_lsts
    | await_or_not_state.not_await => 
      -- translate ident into updating
      -- this structure's entry state
      -- ident
      let this_ctrler : controller_info :=
    -- dbg_trace "===== dsl transition to murphi translation ====="
        get_ctrler_matching_name ctrler_name ctrlers_lst

      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_indexable : Bool :=
        IndexableCtrlerTypesStrings.contains ctrler_ordering

      if is_indexable
      then
      -- we access the specific entry i for the rule

      let ruleset_core_elem_idx := "j"
      let core_idx_designator :=
      Murϕ.Expr.designator (
        Designator.mk ruleset_core_elem_idx []
      )

      -- let ctrler_id := this_ctrler.name

      let entries := "entries"

      let tail_entry :=
        if stmt_trans_info.curr_ctrler_designator_idx.isSome ||
          stmt_trans_info.specific_murphi_dest_expr.isSome ||
          stmt_trans_info.use_specific_dest_in_transition then
        dbg_trace "CUSTOM ENTRY"
        tail_or_entry.custom_entry
        else
        dbg_trace "BASIC ENTRY"
        tail_or_entry.entry
      -- AZ TODO: Use the specific entry thing
      -- let ruleset_entry_elem_idx := "i"
      let queue_idx :=
        match tail_entry with
        | tail_or_entry.entry => "i"
        | tail_or_entry.custom_entry =>
          if stmt_trans_info.use_specific_dest_in_transition then
            ""
          else
            "i"
        | tail_or_entry.tail => "tail"

      let entry_idx_designator :=
      -- Murϕ.Expr.designator (
      --   Designator.mk ruleset_entry_elem_idx []
      -- )
      match tail_entry with
        | tail_or_entry.tail =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk "next_state" [
        -- entries
        -- Assume the buffer entries are
        -- referred to as 'i'
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (ctrler_name.append "_"),
        Sum.inl queue_idx
        ])
        | tail_or_entry.entry =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk queue_idx [])
        | tail_or_entry.custom_entry =>
        dbg_trace "SPECIFIC MURPHI EXPR, for transition state!!"
        dbg_trace stmt_trans_info.specific_murphi_dest_expr

        if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.curr_ctrler_designator_idx.isSome then
          stmt_trans_info.curr_ctrler_designator_idx.get!
        else if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.specific_murphi_dest_expr.isSome then
          stmt_trans_info.specific_murphi_dest_expr.get!
        else
          Murϕ.Expr.designator (
          Murϕ.Designator.mk queue_idx [])

      let state := "state"

      let current_structure_entry_state :=
        -- Murϕ.Expr.designator (
          Designator.mk (
            -- Example in comments
            -- core_
            "next_state"
          )
          [
            -- Example in comments
            Sum.inl "core_",
            -- core_[i]
            Sum.inr core_idx_designator,
            -- core_[i].LQ
            Sum.inl (ctrler_name.append "_"),
            -- core_[i].LQ.entries
            Sum.inl entries,
            -- core_[i].LQ.entries[j]
            Sum.inr entry_idx_designator,
            -- core_[i].LQ.entries[j].state
            Sum.inl state
          ]
        -- )

      -- want to assign the state the ident

      let murphi_state_assn : Murϕ.Statement :=
        Murϕ.Statement.assignment
        current_structure_entry_state
        (Murϕ.Expr.designator (
          Murϕ.Designator.mk ident []
        ) )

      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_state_assn],
        decls := []
      }
      stmts_decls
      else
      -- AZ TODO NOTE: Consider the case of a unit
        -- If this isn't a FIFO / buffer structure
        -- Then do we just assign the unit's state?
        -- []
        let ctrler_name_ : String := ctrler_name.append "_";
        let stmts_decls : lst_stmts_decls := {
          -- Should just be ctrler
          stmts := [murϕ| next_state .core_[j] .£ctrler_name_ .state := £ident;],
          decls := []
        }
        stmts_decls
    murphi_stmt
  | Statement.complete ident =>
    -- NOTE: Copied from Statement.transition
    let murphi_stmt :=
    match await_or_not with
    | await_or_not_state.await => 
      -- return nothing
      empty_stmt_decl_lsts
    | await_or_not_state.not_await => 
      -- translate ident into updating
      -- this structure's entry state
      -- ident
      let this_ctrler : controller_info :=
    -- dbg_trace "===== dsl transition to murphi translation ====="
        get_ctrler_matching_name ctrler_name ctrlers_lst

      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_indexable : Bool :=
        IndexableCtrlerTypesStrings.contains ctrler_ordering

      if is_indexable
      then
      -- we access the specific entry i for the rule

      let ruleset_core_elem_idx := "j"
      let core_idx_designator :=
      Murϕ.Expr.designator (
        Designator.mk ruleset_core_elem_idx []
      )

      -- let ctrler_id := this_ctrler.name

      let entries := "entries"

      let tail_entry :=
        if stmt_trans_info.curr_ctrler_designator_idx.isSome ||
          stmt_trans_info.specific_murphi_dest_expr.isSome ||
          stmt_trans_info.use_specific_dest_in_transition then
        dbg_trace "CUSTOM ENTRY"
        tail_or_entry.custom_entry
        else
        dbg_trace "BASIC ENTRY"
        tail_or_entry.entry
      -- AZ TODO: Use the specific entry thing
      -- let ruleset_entry_elem_idx := "i"
      let queue_idx :=
        match tail_entry with
        | tail_or_entry.entry => "i"
        | tail_or_entry.custom_entry =>
          if stmt_trans_info.use_specific_dest_in_transition then
            ""
          else
            "i"
        | tail_or_entry.tail => "tail"

      let entry_idx_designator :=
      -- Murϕ.Expr.designator (
      --   Designator.mk ruleset_entry_elem_idx []
      -- )
      match tail_entry with
        | tail_or_entry.tail =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk "next_state" [
        -- entries
        -- Assume the buffer entries are
        -- referred to as 'i'
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl (ctrler_name.append "_"),
        Sum.inl queue_idx
        ])
        | tail_or_entry.entry =>
        Murϕ.Expr.designator (
        Murϕ.Designator.mk queue_idx [])
        | tail_or_entry.custom_entry =>
        dbg_trace "SPECIFIC MURPHI EXPR, for transition state!!"
        dbg_trace stmt_trans_info.specific_murphi_dest_expr

        if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.curr_ctrler_designator_idx.isSome then
          stmt_trans_info.curr_ctrler_designator_idx.get!
        else if stmt_trans_info.use_specific_dest_in_transition &&
          stmt_trans_info.specific_murphi_dest_expr.isSome then
          stmt_trans_info.specific_murphi_dest_expr.get!
        else
          Murϕ.Expr.designator (
          Murϕ.Designator.mk queue_idx [])

      let state := "state"

      let current_structure_entry_state :=
        -- Murϕ.Expr.designator (
          Designator.mk (
            -- Example in comments
            -- core_
            "next_state"
          )
          [
            -- Example in comments
            Sum.inl "core_",
            -- core_[i]
            Sum.inr core_idx_designator,
            -- core_[i].LQ
            Sum.inl (ctrler_name.append "_"),
            -- core_[i].LQ.entries
            Sum.inl entries,
            -- core_[i].LQ.entries[j]
            Sum.inr entry_idx_designator,
            -- core_[i].LQ.entries[j].state
            Sum.inl state
          ]
        -- )

      -- want to assign the state the ident

      let murphi_state_assn : Murϕ.Statement :=
        Murϕ.Statement.assignment
        current_structure_entry_state
        (Murϕ.Expr.designator (
          Murϕ.Designator.mk ident []
        ) )

      let stmts_decls : lst_stmts_decls := {
        stmts := [murphi_state_assn],
        decls := []
      }
      stmts_decls
      else
      -- AZ TODO NOTE: Consider the case of a unit
        -- If this isn't a FIFO / buffer structure
        -- Then do we just assign the unit's state?
        -- []
        let ctrler_name_ : String := ctrler_name.append "_";
        let stmts_decls : lst_stmts_decls := {
          -- Should just be ctrler
          stmts := [murϕ| next_state .core_[j] .£ctrler_name_ .state := £ident;],
          decls := []
        }
        stmts_decls
    murphi_stmt

end -- END mutually recursive func region --

--===== Convert idents list (qualified name) to MurϕExpr =====
def qualified_name_to_murphi_expr
(lst_idents : List Identifier)
: Except String Murϕ.Expr
:= do
  let is_insert := lst_idents.contains "insert"
  let is_mem_access := lst_idents.contains "send_memory_request"

  let ruleset_core_elem_idx := "i"
  let core_idx_designator :=
  Murϕ.Expr.designator (
    Designator.mk ruleset_core_elem_idx []
  )

  if is_insert
  then
  -- this is a fifo_access_guard_
  -- since it goes and checks an entry...
    -- dbg_trace "== trying to get ctrler from list of idents in qual name translation =="
    let dest_ctrler := lst_idents[0]!
    let insert_func := lst_idents[1]!

    let num_entries := "num_entries"

    -- now build up the conditional
    let dest_structure_entry_count :=
      Murϕ.Expr.designator (
        Designator.mk (
          -- Example in comments
          -- core_
          "next_state"
        )
        [
          -- Example in comments
          Sum.inl "core_",
          -- core_[i]
          Sum.inr core_idx_designator,
          -- core_[i].LQ
          Sum.inl dest_ctrler,
          -- core_[i].LQ.num_entries
          Sum.inl num_entries
        ]
      )

    let current_state_expr :=
    Murϕ.Expr.designator (
      Designator.mk
      (String.join [dest_ctrler,"_NUM_ENTRIES_CONST"])
      []
    )
    let num_entries_of_dest_not_full :=
    Murϕ.Expr.binop (
      "<"
    ) dest_structure_entry_count current_state_expr

    return num_entries_of_dest_not_full
  else
  if is_mem_access
  then

    -- dbg_trace "== trying to get ctrler_name from lst of idents! in qual name trans =="
    let dest_ctrler := lst_idents[0]!
    -- let dest_ctrler := "mem_interface_"
    let mem_access_func := lst_idents[1]!

    let out_busy := "out_busy"

    -- check that out_busy is false
    let out_msg_busy_designator :=
      Murϕ.Expr.designator (
        Designator.mk (
          -- Example in comments
          -- core_
          "next_state"
        )
        [
          -- Example in comments
          Sum.inl "core_",
          -- core_[i]
          Sum.inr core_idx_designator,
          -- core_[i].mem_interface_
          -- Sum.inl dest_ctrler,
          Sum.inl "mem_interface_",
          -- core_[i].mem_interface_.out_busy
          Sum.inl out_busy
        ]
      )

    let murphi_false_expr :=
      Murϕ.Expr.negation out_msg_busy_designator
    -- let must_out_not_busy_murphi_expr :=
    --   Murϕ.Expr.binop
    --   "="
    --   out_msg_busy_designator
    --   ()
    return murphi_false_expr
  else
    throw s!"Input isn't insert or mem_access, how did it reach this?"

-- =========== Same, but use Sta instead of next_state for Guard ===
def qualified_name_to_sta_murphi_expr
(lst_idents : List Identifier)
: Except String Murϕ.Expr
:= do
  let is_insert := lst_idents.contains "insert"
  let is_mem_access := or (lst_idents.contains "send_load_request") (lst_idents.contains "send_store_request")
  let is_set_executed := lst_idents.contains "set_executed"

  let ruleset_core_elem_idx := "j"
  let core_idx_designator :=
  Murϕ.Expr.designator (
    Designator.mk ruleset_core_elem_idx []
  )

  if is_insert
  then
  -- this is a fifo_access_guard_
  -- since it goes and checks an entry...
    -- dbg_trace "== trying to get ctrler from list of idents in qual name translation =="
    let dest_ctrler := lst_idents[0]!
    let insert_func := lst_idents[1]!

    let dest_ctrler_ : String := dest_ctrler.append "_"
    -- let num_entries := "num_entries"

    -- -- now build up the conditional
    -- let dest_structure_entry_count :=
    --   Murϕ.Expr.designator (
    --     Designator.mk (
    --       -- Example in comments
    --       -- core_
    --       "Sta"
    --     )
    --     [
    --       -- Example in comments
    --       Sum.inl "core_",
    --       -- core_[i]
    --       Sum.inr core_idx_designator,
    --       -- core_[i].LQ
    --       Sum.inl dest_ctrler,
    --       -- core_[i].LQ.num_entries
    --       Sum.inl num_entries
    --     ]
    --   )

    -- let current_state_expr :=
    -- Murϕ.Expr.designator (
    --   Designator.mk
    --   (String.join [dest_ctrler,"_NUM_ENTRIES_CONST"])
    --   []
    -- )
    -- let num_entries_of_dest_not_full :=
    -- Murϕ.Expr.binop (
    --   "<"
    -- ) dest_structure_entry_count current_state_expr

    let dest_ctrler_num_entries_const : String :=
      (String.join [dest_ctrler,"_NUM_ENTRIES_CONST"])
    let num_entries_of_dest_not_full : Murϕ.Expr := [murϕ|
      Sta . core_[j] .£dest_ctrler_ .num_entries < £dest_ctrler_num_entries_const
    ]

    return num_entries_of_dest_not_full
  else
  if is_mem_access
  then

    -- dbg_trace "== trying to get ctrler_name from lst of idents! in qual name trans =="
    let dest_ctrler := lst_idents[0]!
    -- let dest_ctrler := "mem_interface_"
    let mem_access_func := lst_idents[1]!

    let out_busy := "out_busy"

    -- check that out_busy is false
    let out_msg_busy_designator :=
      Murϕ.Expr.designator (
        Designator.mk (
          -- Example in comments
          -- core_
          "Sta"
        )
        [
          -- Example in comments
          Sum.inl "core_",
          -- core_[i]
          Sum.inr core_idx_designator,
          -- core_[i].mem_interface_
          -- Sum.inl dest_ctrler,
          Sum.inl "mem_interface_",
          -- core_[i].mem_interface_.out_busy
          Sum.inl out_busy
        ]
      )

    let murphi_false_expr :=
      Murϕ.Expr.negation out_msg_busy_designator
    -- let must_out_not_busy_murphi_expr :=
    --   Murϕ.Expr.binop
    --   "="
    --   out_msg_busy_designator
    --   ()
    return murphi_false_expr
  else
    throw s!"Input isn't insert or mem_access, how did it reach this?"
--========= Convert DSL Decl and Decl Assn Stmts to Decls =========
-- TODO: Check what I did to Decl Assn Stmts

-- Moved to AnalysisHelpers.lean for use in EmitMurphi.lean

-- def dsl_type_to_murphi_type
-- ( dsl_type : Identifier )
-- : Murϕ.TypeExpr
-- :=
--   -- Types that one may use in the DSL.. & it's Murphi Test Harness "Equivalent" (for now that is)
--   -- DSL        |    Murphi Test Harness
--   -- -----------------------------------
--   -- address    |    addr_idx_t
--   -- u32        |    val_t
--   -- packet     |    N/A
--   -- seq_num    |    inst_idx_t
--   let murphi_type_name : ID :=
--   if dsl_type == "address" then
--     "addr_idx_t"
--   else if dsl_type == "u32" then
--     "val_t"
--   else if dsl_type == "seq_num" then
--     "inst_idx_t"
--   else
--     panic! "ERROR: ===== ENCOUNTERED UNEXPECTED DSL TYPE ====="

--   let murphi_type_expr : Murϕ.TypeExpr :=
--   Murϕ.TypeExpr.previouslyDefined murphi_type_name

--   murphi_type_expr


-- structure decl_gen_info where
-- stmt : Pipeline.Statement
-- lst_ctrlers : List controller_info


structure decl_and_init_list where
decl_list : List Murϕ.Decl
init_list : List Murϕ.Statement
trans' : Pipeline.Description
ctrler_names : List Identifier
lst_ctrlers : List controller_info
curr_ctrler_name : Identifier

abbrev DeclInitM := StateT decl_and_init_list Id

partial def ast_decl_assn_decl_to_murphi_decl
-- ( stmt_ctrlers : decl_gen_info)
(stmt : Pipeline.Statement)
: DeclInitM (List Murϕ.Decl)
:= do
  let lst_ctrlers : List controller_info := (← get).lst_ctrlers

  -- The other stuff, but i don't use this here..
  -- { trans := trans, init_list := [], decl_list := simple_ast_murphi_decls, ctrler_names := lst_ctrler_names}
  let trans' := (← get).trans'
  let init_list := (← get).init_list
  let decl_list := (← get).decl_list
  let ctrler_names := (← get).ctrler_names
  let curr_ctrler_name := (← get).curr_ctrler_name

  -- let stmt : Pipeline.Statement := stmt_ctrlers.stmt
  -- match stmt; check for decl & assn decl; convert to Murϕ.Decl
-- inductive Statement
-- | labelled_statement : Label → Statement → Statement
-- | variable_declaration : TypedIdentifier → Statement
-- | value_declaration : TypedIdentifier →  Expr → Statement
-- | variable_assignment : QualifiedName  → Expr → Statement
-- | conditional_stmt : Conditional → Statement
-- | listen_handle : Statement → List HandleBlock → Statement
-- | await : Option Term → List Statement → Statement -- here the AST is "imprecise" (when could be a different inductive type)
-- | when :  QualifiedName → List Identifier → Statement → Statement
-- | transition : Identifier → Statement
-- | stray_expr : Expr → Statement
-- | block : /- { -/ List Statement /- } -/ → Statement
-- | return_stmt : Expr → Statement

  -- let new_decls : List Murϕ.Decl :=
  match stmt with
  | Pipeline.Statement.variable_declaration typed_ident =>
    let (typed, ident) :=
    match typed_ident with
    | TypedIdentifier.mk typed ident => (typed, ident)

    -- use ident for the Decl name
    -- map type (typed) to a type for the decl
-- inductive Decl
--   | const : ID → Expr → Decl
--   | type  : ID → TypeExpr → Decl
--   | var   : List ID → TypeExpr → Decl
    let murphi_type_expr : Murϕ.TypeExpr := dsl_type_to_murphi_type typed
    
    return [Murϕ.Decl.var [ident] murphi_type_expr]
  | Pipeline.Statement.value_declaration typed_ident _ =>
    let (typed, ident) :=
    match typed_ident with
    | TypedIdentifier.mk typed ident => (typed, ident)

    let murphi_type_expr : Murϕ.TypeExpr := dsl_type_to_murphi_type typed
    
    return [Murϕ.Decl.var [ident] murphi_type_expr]
  | .labelled_statement _ stmt =>
  -- let murphi_stmts_decls_monad := murphi_stmts_to_murphi_decls lst_murphi_stmt
  -- let murphi_stmts_decls := murphi_stmts_decls_monad.run
  --   { trans := trans, init_list := [], decl_list := simple_ast_murphi_decls, ctrler_names := lst_ctrler_names} |>.run.2
  -- let (lst_murphi_decls, murphi_inits) := (murphi_stmts_decls.decl_list, murphi_stmts_decls.init_list)
    let ast_decl_translation ← ast_decl_assn_decl_to_murphi_decl stmt
    return ast_decl_translation
  | .conditional_stmt condition =>
    match condition with
    | Pipeline.Conditional.if_else_statement _ stmt1 stmt2 =>
      let ast_decl_translation1 ← ast_decl_assn_decl_to_murphi_decl stmt1
      let ast_decl_translation2 ← ast_decl_assn_decl_to_murphi_decl stmt2
      return (ast_decl_translation1 ++ ast_decl_translation2)
    | .if_statement _ stmt =>
      let ast_decl_translation ← ast_decl_assn_decl_to_murphi_decl stmt
      return ast_decl_translation
  | .listen_handle stmt _ => ast_decl_assn_decl_to_murphi_decl stmt
  | .await none _ => return []
  | .await term stmt_list =>
    --  let await_stmts_decls :=
    --  List.join (stmt_list.map ast_decl_assn_decl_to_murphi_decl)

    -- let await_stmts ← stmt_list.map ast_decl_assn_decl_to_murphi_decl
    let await_stmts ← stmt_list.mapM ast_decl_assn_decl_to_murphi_decl

    -- let lst_lst_decls : List (List Murϕ.Decl) := await_stmt_monads.map
    --   λ monad =>
    --     monad.run {trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
    --     |>.run.1
    let lst_decls : (List Murϕ.Decl) := List.join await_stmts

-- inductive Term
-- | negation: Term → Term
-- | logical_negation: Term → Term
-- | binary_negation: Term → Term
-- | var : Identifier → Term -- variable is a lean keyword...
-- | qualified_var : QualifiedName → Term -- variable is a lean keyword...
-- | const : Const → Term -- constant is a lean keyword...
-- | function_call : QualifiedName → /- ( -/ List Expr  /- ) -/ → Term
-- | expr : Expr → Term

    --  let func_decls :=

    let q_name :=
     match term with
     | Pipeline.Term.function_call q_name _ => q_name
     | _ => dbg_trace "Throw!" -- throw! 
       panic! "Shouldn't get here.."
    let q_name_list :=
    match q_name with
    | QualifiedName.mk lst_ident => lst_ident
    
    let func_ctrler_name : Identifier := q_name_list[0]!
    let matching_ctrler : controller_info := get_ctrler_matching_name func_ctrler_name lst_ctrlers
      
    let func_name := q_name_list[1]!
    -- with the matching ctrler, search thru it's transitions for a matching
    -- when stmt
    let matching_when : Pipeline.Statement := 
      if matching_ctrler.init_trans.isSome then
        find_when_from_transition matching_ctrler.transition_list.get! func_name curr_ctrler_name
      else if matching_ctrler.ctrler_init_trans.isSome then
        find_when_from_transition matching_ctrler.ctrler_trans_list.get! func_name curr_ctrler_name
      else
        dbg_trace "ERROR, matching ctrler doesn't have entry or ctrler transition info? ({matching_ctrler})"
          default
        
    let when_stmts :=
    match matching_when with
    | Pipeline.Statement.when _ _ stmt => stmt
    | _ => dbg_trace "should be a when stmt!?"
      let msg : String :=
        "Should have just found a when stmt! why did we get something else?" ++
        s!"Found: ({matching_when})"
      panic! msg

    let ast_decl_translation ← ast_decl_assn_decl_to_murphi_decl when_stmts

    -- await_stmts_decls
    return (lst_decls ++ ast_decl_translation)
       
  | .when _ _ stmt =>
    let ast_decl_translation ← ast_decl_assn_decl_to_murphi_decl stmt
    return ast_decl_translation
  -- ast_decl_assn_decl_to_murphi_decl stmt
  | .stray_expr expr =>
    let term_from_expr :=
    match expr with
    | Pipeline.Expr.some_term term => term
    | _ => dbg_trace "This shouldn't happen, malformed AST. TODO: Throw?"
      panic! "Should Throw here..."

    let q_name :=
     match term_from_expr with
     | Pipeline.Term.function_call q_name _ => q_name
     | _ => dbg_trace "Throw!" -- throw! 
       panic! "Shouldn't get here.."
    let q_name_list :=
    match q_name with
    | QualifiedName.mk lst_ident => lst_ident
    
    let func_ctrler_name : Identifier := q_name_list[0]!
    let matching_ctrler : controller_info := get_ctrler_matching_name func_ctrler_name lst_ctrlers
      
    let func_name := q_name_list[1]!
    -- with the matching ctrler, search thru it's transitions for a matching
    -- when stmt
    let matching_when : Pipeline.Statement :=
      if matching_ctrler.init_trans.isSome then
        find_when_from_transition matching_ctrler.transition_list.get! func_name curr_ctrler_name
      else if matching_ctrler.ctrler_init_trans.isSome then
        find_when_from_transition matching_ctrler.ctrler_trans_list.get! func_name curr_ctrler_name
      else
        dbg_trace "ERROR, matching ctrler doesn't have entry or ctrler transition info? ({matching_ctrler})"
          default
    let when_stmts :=
    match matching_when with
    | Pipeline.Statement.when _ _ stmt => stmt
    | _ => dbg_trace "should be a when stmt!?"
      panic! "Should have just found a when stmt! why did we get something else?"

    let ast_decl_translation ← ast_decl_assn_decl_to_murphi_decl when_stmts

    return ast_decl_translation
  | .block stmt_lst =>
    let await_stmts ← stmt_lst.mapM ast_decl_assn_decl_to_murphi_decl
    let lst_decls : (List Murϕ.Decl) := List.join await_stmts

    return lst_decls
    -- List.join (stmt_lst.map ast_decl_assn_decl_to_murphi_decl)
  | _ => return []

--========= Convert Murphi Stmts to Decls =========

-- A func to check exprs for Designators in Murphi

def Decl.getIDs : Decl → List ID
| .const id _ => [id]
| .type id _ => [id]
| .var ids _ => ids

def Designator.getID : Designator → ID
| .mk id _ => id

def gen_decl_from_stmt_and_append
(_ : Unit)
(murphi_stmt : Murϕ.Statement)
: DeclInitM Unit
:= do
let decl_list := (← get).decl_list
-- TODO: get the list of stmts, find any decls,
-- record the decls and their types in a list
-- Generate the decls!
-- map decls from "dsl" types to Murphi types...
-- Need to define a manual table of translations

-- Later, check if the root qual name var is in this list already or not

-- inductive Statement
--   | assignment : Designator → Expr → Statement
--   | ifstmt : Expr → List Statement → List (Expr × List Statement) → List Statement → Statement
--   | switchstmt : Expr → List (List Expr × List Statement) → List Statement → Statement
--   | forstmt : Quantifier → List Statement → Statement
--   | whilestmt : Expr → List Statement → Statement
--   | aliasstmt : List Alias → List Statement → Statement
--   | proccall : ID → List Expr → Statement
--   | clearstmt : Designator → Statement
--   | errorstmt : String → Statement
--   | assertstmt : Expr → String → Statement
--   | putstmtexp : Expr → Statement
--   | putstmtstr : String → Statement
--   | returnstmt : Option Expr → Statement
--   deriving Inhabited
-- process the murphi stmt
let ctrler_names := (← get).ctrler_names
let decl_init_tuple : (List Murϕ.Decl) × (List Murϕ.Statement)
:=
match murphi_stmt with
| Murϕ.Statement.assignment designator _ =>
  -- Check if the first part of the designator has
  -- been previously declared in the Decl list
  -- 1) If not, add it to the list; gen an init statement and return that
  -- 2) If it has, we just return the original list...

  /- 1) -/
  let assign_var_is_declared : Bool
  := (decl_list.map (Decl.getIDs)).join.contains (Designator.getID designator)

  let new_decl_init_list : (List Murϕ.Decl) × (List Murϕ.Statement)
  :=
    if !assign_var_is_declared then
      -- undeclared, so we declare and init it if necessary
      /-
      1. Declare the var.
      How to determine the type?
      a) If the var is a struct defined in a core_,
      then it's the struct type
      ex.
      LQ.entries[i].value := 0
      -- can check if the list is empty or not?
      -- or just check if the name is a defined structure's
      -- name.
      -- This means we also carry around the list of structure names
      b) Otherwise, we'd have to check if this has been defined somewhere

      c) Or if there's no list in the Designator, can try to figure out
      the expr's type, but this seems like a lot of work...

      So if we were to generate Decls for aribtrarily translated Murphi
      code, how would be do so easily?
      -/
      /-
      2. Init the var.
      Init it if it's something we
      directly reference..
      -/
      -- TODO: implement

      -- 1. One case: it's a Ctrler!
      -- Check if it is a ctrler!,
      -- if it is, then create a new init & decl!
      let desig_id := (Designator.getID designator)
      let is_ctrler := ctrler_names.contains desig_id

      if is_ctrler then
        -- gen both new_init and decl
        let decl_type := Murϕ.TypeExpr.previouslyDefined desig_id
        let decl : Murϕ.Decl := Murϕ.Decl.var [desig_id] decl_type

        -- Sta.core_[j].<ctrler_name>
        -- Define j
        let j_desig := Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])
        let init_desig : Murϕ.Expr :=
        Murϕ.Expr.designator (
          Murϕ.Designator.mk "next_state" [
          Sum.inl "core_",
          Sum.inr j_desig,
          Sum.inl (desig_id.append "_")
          ]
        )
        let desig_id_desig : Murϕ.Designator := Murϕ.Designator.mk desig_id []

        let init : Murϕ.Statement := Murϕ.Statement.assignment desig_id_desig init_desig

        let new_inits := [init]
        let new_decls := [decl]
        (new_decls, new_inits)
      else
        -- 2. another case: it's a func var...
        -- we should create a decl for this..
        -- But the type requires us to search
        -- in the stmts for any called ctrler funcs
        -- and see if we can match the stmt/ctrler name there
        dbg_trace "TODO: HANDLE THIS CASE!"
        dbg_trace "TODO: Either find the decl before hand, or after"
        dbg_trace "Though the decl translation func used before this¬"
        dbg_trace "Should have gotten them all..."
        dbg_trace "This should catch any vars that weren't handled somehow"

        let new_inits := []
        let new_decls := []
        (new_decls, new_inits)
    else
      -- declared, so we just return the original lists
      -- and the fold which uses this func continues
      -- to the next Murphi stmt
      ([], [])
  new_decl_init_list
--   | ifstmt : Expr → List Statement → List (Expr × List Statement) → List Statement → Statement
--   | switchstmt : Expr → List (List Expr × List Statement) → List Statement → Statement
--   | forstmt : Quantifier → List Statement → Statement
--   | whilestmt : Expr → List Statement → Statement
--   | aliasstmt : List Alias → List Statement → Statement
--   | proccall : ID → List Expr → Statement
--   | clearstmt : Designator → Statement
--   | errorstmt : String → Statement
--   | assertstmt : Expr → String → Statement
--   | putstmtexp : Expr → Statement
--   | putstmtstr : String → Statement
--   | returnstmt : Option Expr → Statement
| _ => ([], [])
modify λ { trans' := trans', init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name } =>
  { trans' := trans', init_list := init_list ++ decl_init_tuple.2, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name,
    decl_list := decl_list ++ decl_init_tuple.1, ctrler_names := ctrler_names }
return ()

-- TODO:
def murphi_stmts_to_murphi_decls
-- use a monad, need a struct
-- input is list of stmts
( stmts : List Murϕ.Statement)
: DeclInitM Unit
:= do
  -- get relevant info
  -- let lst_ctrlers : List controller_info := (← get).lst_ctrlers

  -- The other stuff, but i don't use this here..
  -- { trans := trans, init_list := [], decl_list := simple_ast_murphi_decls, ctrler_names := lst_ctrler_names}
  -- let trans := (← get).trans
  -- let init_list := (← get).init_list
  -- let decl_list := (← get).decl_list
  -- let ctrler_names := (← get).ctrler_names
  -- let curr_ctrler_name := (← get).curr_ctrler_name

  -- foldl thru the stmts list, match stmt with | case
  -- translate line by line
  -- if blk, can recursively call
  --   if recursive, then still same; add decls & init to list..

  -- α : Unit, β : List Pipeline.Statement
  let _ ← List.foldlM gen_decl_from_stmt_and_append () stmts

  -- TODO: implement
  return ()

def gen_next_state_update
(lst_decls : List Murϕ.Decl)
(lst_ctrlers : List Identifier)
: List Murϕ.Statement
:=
  -- if decl name in lst of decls matches a name of ctrler
  -- then generate a next_state := <ctrler_name>
  let lst_decl_ids := lst_decls.map (
    λ decl =>
      let lst_ids :=
      match decl with
      | Murϕ.Decl.var lst_id _ => lst_id
      | _ => dbg_trace "Shouldn't be other decl.."
        panic! "TODO: Throw"
      
      let id := lst_ids[0]!
      id
  )

  let lst_ctrler_updates : List Murϕ.Statement :=
  List.join (
    lst_decl_ids.map (
      λ id =>
        if lst_ctrlers.contains id then
          [[murϕ| next_state := £id]]
        else
          []
    )
  )
  lst_ctrler_updates

--========== Check if desig has i =========
def check_desig_for_i_expr 
(desig : Murϕ.Designator)
: List Bool
:=
  List.join (
  match desig with
  | Murϕ.Designator.mk _ sum_lst =>
    sum_lst.map (
      λ elem =>
        match elem with
        | Sum.inr type_expr =>
          match type_expr with
          | Murϕ.Expr.designator desig' =>
            match desig' with
            | .mk id _ =>
              if id == "i" then
                [true]
              else
                []
          | _ => []
        | _ => []
    )
  )

partial def check_expr_for_i
( expr : Murϕ.Expr )
: List Bool
:=
-- inductive Expr
--   | designator : Designator → Expr
--   | integerConst : Int → Expr
--   | call : ID → List Expr → Expr
--   | universal : Quantifier → Expr → Expr
--   | existential : Quantifier → Expr → Expr
--   | binop : BinOp → Expr → Expr → Expr
--   | negation : Expr → Expr
--   | conditional : Expr → Expr → Expr → Expr
  match expr with
  | Murϕ.Expr.designator desig =>
    check_desig_for_i_expr desig
  | .call _ lst_expr =>
    List.join (lst_expr.map check_expr_for_i)
  | .binop _ expr1 expr2 =>
    (check_expr_for_i expr1) ++ (check_expr_for_i expr2)
  | .negation expr => check_expr_for_i expr
  | .conditional expr1 expr2 expr3 => (
      (check_expr_for_i expr1) ++
      (check_expr_for_i expr2) ++
      (check_expr_for_i expr3)
    )
  | _ => []

partial def does_stmt_have_desig_i
(stmt : Murϕ.Statement)
: Bool
:=
-- inductive Statement
--   | assignment : Designator → Expr → Statement
--   | ifstmt : Expr → List Statement → List (Expr × List Statement) → List Statement → Statement
--   | switchstmt : Expr → List (List Expr × List Statement) → List Statement → Statement
--   | forstmt : Quantifier → List Statement → Statement
--   | whilestmt : Expr → List Statement → Statement
--   | aliasstmt : List Alias → List Statement → Statement
--   | proccall : ID → List Expr → Statement
--   | clearstmt : Designator → Statement
--   | errorstmt : String → Statement
--   | assertstmt : Expr → String → Statement
--   | putstmtexp : Expr → Statement
--   | putstmtstr : String → Statement
--   | returnstmt : Option Expr → Statement
--   | undefine : ID → Statement
  let bool_list : List Bool :=
  match stmt with
  | Murϕ.Statement.assignment desig expr =>
    let desig_has_i : List Bool :=
    check_desig_for_i_expr desig
    let expr_has_i : List Bool :=
    check_expr_for_i expr

    desig_has_i ++ expr_has_i
  | .ifstmt expr lst_stmt1 lst_expr_stmt lst_stmt2 =>
    let expr_bool := check_expr_for_i expr
    let stmt1_bool := lst_stmt1.map does_stmt_have_desig_i
    let lst_expr_stmt_bool : List Bool := List.join ( lst_expr_stmt.map (
      λ elem =>
        let expr_bool' : List Bool := check_expr_for_i elem.1
        let stmts' : List Bool := elem.2.map does_stmt_have_desig_i
        expr_bool' ++ stmts'
    ))
    let stmt2_bool := lst_stmt2.map does_stmt_have_desig_i

    expr_bool ++ stmt1_bool ++ lst_expr_stmt_bool ++ stmt2_bool
  | .switchstmt expr lst_expr_stmt lst_stmt =>
    let expr_bool := check_expr_for_i expr
    let lst_expr_stmt_bool : List Bool := List.join ( lst_expr_stmt.map (
      λ elem =>
        let expr_bool' : List Bool := List.join (elem.1.map check_expr_for_i)
        let stmts' : List Bool := elem.2.map does_stmt_have_desig_i
        expr_bool' ++ stmts'
    ))
    let stmt_bool := lst_stmt.map does_stmt_have_desig_i
    expr_bool ++ lst_expr_stmt_bool ++ stmt_bool

  | .forstmt _ lst_stmt =>
    let stmt_bool := lst_stmt.map does_stmt_have_desig_i
    stmt_bool

  | .whilestmt expr lst_stmt =>
    let expr_bool := check_expr_for_i expr
    let stmt_bool := lst_stmt.map does_stmt_have_desig_i
    expr_bool ++ stmt_bool
  | .proccall _ lst_expr =>
    List.join (lst_expr.map check_expr_for_i)
  | .returnstmt expr =>
    match expr with
    | .some expr' =>
      check_expr_for_i expr'
    | .none =>
      []
  | _ => []
  
  match bool_list with
  | [] => false
  | _ => true

def find_designator_with_expr_i
(stmts : List Murϕ.Statement)
: Bool
:=
  let res : List Bool := stmts.map does_stmt_have_desig_i
  match res with
  | [] => false
  | _ => true

def check_if_decl_is_defined
( decls : List Murϕ.Decl )
( decl : Murϕ.Decl )
: List Murϕ.Decl
:=
  let decl_names := List.join (
  decls.map (
    fun decl' =>
      Decl.getIDs decl'
  ))
  let decl_name := ( Decl.getIDs decl )[0]!

  if decl_names.contains decl_name then
    decls
  else
    decls.concat decl

def remove_duplicate_murphi_decl
( decls : List Murϕ.Decl )
: List Murϕ.Decl
:=
  let unique_decls : List Murϕ.Decl :=
  decls.foldl check_if_decl_is_defined []

  unique_decls

def dsl_trans_ctrler_to_murphi
(trans_info : dsl_trans_info)
: Except String (List Murϕ.Rule)
:= do
  -- check if ctrler is a
  -- (1) entry-state-machine type or
  -- (2) ctrler-state-machine type
  let ctrler_name := trans_info.ctrler_name
  let trans := trans_info.trans
  let ctrler_lst := trans_info.ctrler_lst
  
  let filtered_ctrlers := 
  ctrler_lst.filter (
    λ ctrler =>
      -- match if ctrler name
      -- is the struct name
      ctrler.name == ctrler_name
  )

  let ctrler : controller_info ← 
  match filtered_ctrlers with
  | [one] => pure one
  | h :: t =>
    let msg : String := s!"multiple ctrlers with the same name? ({filtered_ctrlers})"
    throw msg
  | [] =>
    let msg : String := s!"no ctrlers with this name? ({filtered_ctrlers})"
    throw msg

  -- AZ NOTE CHECKPOINT:

  -- Need a name for the ruleset elem idx
  let ruleset_core_elem_idx := "j"

  -- Need to get the core number enum type
  let cores_t :=
    TypeExpr.previouslyDefined "cores_t"

  let current_state_name := 
    get_transition_name trans

  let dest_state_name := 
    get_dest_transition_names trans

  let await_state_name :=
    does_transition_have_await trans

  let rule_name :=
    if await_state_name then
      String.join ["AWAIT ",ctrler.name, " ", current_state_name]
    else
      String.join [ctrler.name, " ", current_state_name, " ===> ", dest_state_name]

  -- ======= Transition Analysis ========
  
  /-
  Things we need for a transition!
  1. Expr : the rule guard.
  2. Decls : What variables do we need in Murphi?
  3. Operational Code : The DSL code as Murphi code
  -/

  /- How to get 1. (rule guard) -/
  /-
  1. Conditions to put in the rule guard for a transition
  
  a. The for an entry, it is in a certain state
  (This transition name)

  b. If this structure does something like "insert" into
  another structure, the dest structure must have
  available entries

  These should be the only two things required
  in a Murphi model which doesn't do msg passing
  -/

  /-
  1. Ensure we're on this state if we exec
  this transition
  -/
  let ctrler_name_ : String := ctrler.name.append "_"
  let entry_is_at_state_expr := [murϕ|
    Sta .core_[j] .£ctrler_name_ .state = £current_state_name
  ]

  /-
  2. do a check on the transition, if we insert into
  another structure
  (ex. SQ -> SB after commit signal)
  -/

  -- should be similar to other searches,
  -- but this time instead of for transitions,
  -- for insert function calls
  let trans_stmt_blk :=
    get_transition_stmt trans

  -- TODO: Adjustment to this,
  -- search if there's an insert API call
  -- or if there's a memory interface call
  -- (a) insert also gens the tail := tail + 1
  -- (b) memory_interface just gens the packet stuff
  -- maybe we don't need a "build packet" library API

  -- since we can consider there could be multiple
  -- insert actions to take

  let calls_which_can_guard :=
    get_api_with_guard_function_calls trans_stmt_blk

  dbg_trace "------ BEGIN CHECK list of GUARDS ------"
  dbg_trace calls_which_can_guard
  dbg_trace "------ END CHECK list of GUARDS ------"
  -- for each of these func calls (list of idents)
  -- we want to get their guard,
  -- i.e. put this "insert" or "memory-access"
    -- specific code into a separate function,
  -- and finally use something like a foldl to 
  -- put together the guard condition

  let exception_murphi_guard_exprs := 
    calls_which_can_guard.map qualified_name_to_sta_murphi_expr
  dbg_trace "------ BEGIN GUARD EXPRS------"
  dbg_trace exception_murphi_guard_exprs
  dbg_trace "------ END GUARD EXPRS ------"

  let murphi_guard_exprs : List Murϕ.Expr := 
    exception_murphi_guard_exprs.map (
      λ excpt =>
        match excpt with
        -- TODO: This is bad, but I don't want
        -- to refactor right now, maybe later..
        | .error msg => Murϕ.Expr.designator (Murϕ.Designator.mk "BAD!!!" [])
        | .ok    murphi => murphi
    )

  --========== This is the guard condition =============
  let guard_cond :=
    List.foldl (
      λ mur_expr1 mur_expr2 =>
        -- and the exprs!
      Murϕ.Expr.binop (
        "&"
      ) mur_expr1 mur_expr2
    ) entry_is_at_state_expr murphi_guard_exprs


  /-
  3. Operational Code, DSL to Murphi
  -/


  let stmt_trans_info : stmt_translation_info := {
    stmt := trans_stmt_blk,
    lst_ctrlers := ctrler_lst,
    ctrler_name := ctrler_name,
    src_ctrler := none,
    lst_src_args := none,
    func := none,
    is_await := if await_state_name
    then await_or_not_state.await
    else await_or_not_state.not_await
    entry_keyword_dest := none
    trans_obj := trans
    specific_murphi_dest_expr := none
    lst_decls := []
    is_rhs := false
    use_specific_dest_in_transition := false
    curr_ctrler_designator_idx := none
    lhs_var_is_just_default := false
    translate_entry_or_ctrler := entry_or_ctrler.ctrler
  }

  let murphi_stmts_decls : lst_stmts_decls :=
  -- AZ TODO: Implement the AST Stmts => Murphi Stmts fn
    -- AZ TODO: Use the struct!
    ast_stmt_to_murphi_stmts stmt_trans_info
  -- The murphi stmts for the transition body
  let lst_murphi_stmt : List Murϕ.Statement := murphi_stmts_decls.stmts
  let decls_from_translation : List Murϕ.Decl := murphi_stmts_decls.decls
    
  -- List of ctrler names ( identifiers )
  let lst_ctrler_names : List Identifier := 
  ctrler_lst.map λ ctrler => ctrler.name

  let ast_decl_to_murphi_decl_monad : DeclInitM (List Murϕ.Decl) := ast_decl_assn_decl_to_murphi_decl trans_stmt_blk
  let simple_ast_murphi_decls : List Murϕ.Decl := ast_decl_to_murphi_decl_monad.run
    { trans' := trans, init_list := [], decl_list := [], ctrler_names := lst_ctrler_names,
      lst_ctrlers := ctrler_lst, curr_ctrler_name := ctrler_name} |>.run.1

  let murphi_stmts_decls_monad := murphi_stmts_to_murphi_decls lst_murphi_stmt
  let murphi_stmts_decls := murphi_stmts_decls_monad.run {
    trans' := trans, init_list := [],
    decl_list := simple_ast_murphi_decls ++ decls_from_translation,
    ctrler_names := lst_ctrler_names,
      lst_ctrlers := ctrler_lst, curr_ctrler_name := ctrler_name} |>.run.2
  let (lst_murphi_decls, murphi_inits) := (murphi_stmts_decls.decl_list, murphi_stmts_decls.init_list)
  -- AZ TODO: Implement the Murphi stmts -> Decls fn
  -- TODO NOTE: There are default vars to decl, like
  -- next_state of type (Sta or state)
  -- TODO NOTE: This should also gen the initialization
  -- assignment stmts for the Decls
  -- TODO NOTE: Use a Monad if necessary
    -- murphi_stmts_to_murphi_decls lst_murphi_stmt
    -- ([],[])

  -- TODO: Thursday Evening:
  -- Pre-pend the murphi_inits to the lst_murphi_stmt
  -- Post-pend next_state for all structures in the Decl list
  -- Post-pend Sta := next_state

  let all_decls : List Murϕ.Decl :=
    -- add the next state, which is of type STATE...
    lst_murphi_decls ++ [Murϕ.Decl.var ["next_state"] (Murϕ.TypeExpr.previouslyDefined "STATE")]
  let prepared_murphi_decls : List Murϕ.Decl :=
    remove_duplicate_murphi_decl all_decls

  let update_next_state : List Murϕ.Statement := 
    -- Convert any declared decls of ctrlers into update
    -- Though I think i could have avoided this by simply generating next_state.core_[j].<ctrler>
    gen_next_state_update prepared_murphi_decls lst_ctrler_names

  let prepared_murphi_stmts : List Murϕ.Statement :=
    [[murϕ| next_state := Sta]] ++ murphi_inits ++ lst_murphi_stmt ++ update_next_state ++ [[murϕ| Sta := next_state]]

  dbg_trace "===== BEGIN TRANSLATION INFO ====="
  dbg_trace "=== ctrler ==="
  dbg_trace ctrler_name
  dbg_trace "=== transition ==="
  dbg_trace trans
  dbg_trace "=== lst murphi stmt ==="
    dbg_trace lst_murphi_stmt
  dbg_trace "===== END TRANSLATION INFO ====="

  -- let is_a_per_entry_rule : Bool :=
  --   -- find_designator_with_expr_i lst_murphi_stmt
  --   false
  
  let rule_core_quantifier : Murϕ.Quantifier :=
      (
        Quantifier.simple
        -- ID
        (ruleset_core_elem_idx)
        -- TypeExpr
        (cores_t)
      )

  let list_rule_quantifiers :=
  -- if is_a_per_entry_rule then
  --   let ruleset_buffer_idx := "i"
  --   let ctrler_idx :=
  --     TypeExpr.previouslyDefined (ctrler_name.append "_idx_t")

  --   [rule_core_quantifier].concat (
  --     Quantifier.simple ruleset_buffer_idx ctrler_idx
  --   )
  -- else
    [rule_core_quantifier]
  -- ======= After the analysis ======
  let murphi_core_ruleset :=
    Rule.ruleset -- List of quantifier, List of rule
    -- List of Quantifier (our TypeExpr of cores)
    list_rule_quantifiers
    [
      (
        Rule.simplerule
        -- Option String
        ---- Good investment:
        ---- Should build a good name
        ---- btn the state transitions
        ---- Await states get AWAIT appended
        rule_name
        -- Option Expr
        guard_cond
        -- List Decl
        prepared_murphi_decls
        -- List Statement
        prepared_murphi_stmts
      )
    ]
    -- List of Rule

  if lst_murphi_stmt.length == 0 then
    return []
  else
    return [murphi_core_ruleset]
  

--=========== DSL AST to Murphi AST =============
def dsl_trans_entry_descript_to_murphi_rule
(trans_info : dsl_trans_info)
-- (ctrler_and_trans : (List controller_info) × Description)
-- (ctrler : controller_info)
-- (trans : Description) -- Description.state

-- all other controllers, if we need to gen
-- something like "insert" code

-- (lst_ctrlers : List controller_info)
: List Murϕ.Rule
:=
  let ctrler_name := trans_info.ctrler_name
  let trans := trans_info.trans
  let ctrler_lst := trans_info.ctrler_lst
  
  let filtered_ctrlers := 
  ctrler_lst.filter (
    λ ctrler =>
      -- match if ctrler name
      -- is the struct name
      ctrler.name == ctrler_name
  )
  -- let ctrler_except : Except String controller_info :=
  -- do
  -- match filtered_ctrlers with
  -- | [one] => return one
  -- | h :: t => throw "multiple ctrlers with the same name?"
  -- | [] => throw "no ctrlers with this name?"

  -- let ctrler : controller_info := ctrler_except.get
  let ctrler : controller_info :=
  match filtered_ctrlers with
  | [one] => one
  | h :: t => dbg_trace "multiple ctrlers with the same name?"
    default
  | [] => dbg_trace "no ctrlers with this name?"
    default


  -- ======= String Name Setup =======
  /-
  First we can create the required Murphi Rules
  Decide if we need a Ruleset for
  1. cores
  2. entries

  1. Core are required anyways
  2. Entries are required for per entry transitions.
  All entry transitions should require them

  Only Controller transitions do not require entry
  transitions, since they are just for the
  controller/unit
  -/
  -- Need a name for the ruleset elem idx
  let ruleset_core_elem_idx := "j"

  -- Need to get the core number enum type
  let cores_t :=
    TypeExpr.previouslyDefined "cores_t"

  let current_state_name := 
    get_transition_name trans

  let dest_state_name := 
    get_dest_transition_names trans

  let await_state_name :=
    does_transition_have_await trans

  let rule_name :=
    if await_state_name
    then
      String.join ["AWAIT ",ctrler.name, " ", current_state_name]
    else
      String.join [ctrler.name, " ", current_state_name, " ===> ", dest_state_name]

  -- ======= Transition Analysis ========
  
  /-
  Things we need for a transition!
  1. Expr : the rule guard.
  2. Decls : What variables do we need in Murphi?
  3. Operational Code : The DSL code as Murphi code
  -/

  /- How to get 1. (rule guard) -/
  /-
  1. Conditions to put in the rule guard for a transition
  
  a. The for an entry, it is in a certain state
  (This transition name)

  b. If this structure does something like "insert" into
  another structure, the dest structure must have
  available entries

  These should be the only two things required
  in a Murphi model which doesn't do msg passing
  -/

  /-
  1. Ensure we're on this state if we exec
  this transition
  -/
  let ctrler_name_ : String := ctrler.name.append "_"
  let entry_is_at_state_expr := [murϕ|
    Sta .core_[j] .£ctrler_name_ .entries[i] .state = £current_state_name
  ]
  /-
  2. do a check on the transition, if we insert into
  another structure
  (ex. SQ -> SB after commit signal)
  -/

  -- should be similar to other searches,
  -- but this time instead of for transitions,
  -- for insert function calls
  let trans_stmt_blk :=
    get_transition_stmt trans

  -- TODO: Adjustment to this,
  -- search if there's an insert API call
  -- or if there's a memory interface call
  -- (a) insert also gens the tail := tail + 1
  -- (b) memory_interface just gens the packet stuff
  -- maybe we don't need a "build packet" library API

  -- since we can consider there could be multiple
  -- insert actions to take

  let calls_which_can_guard :=
    get_api_with_guard_function_calls trans_stmt_blk

  dbg_trace "------ BEGIN CHECK list of GUARDS ------"
  dbg_trace calls_which_can_guard
  dbg_trace "------ END CHECK list of GUARDS ------"
  -- for each of these func calls (list of idents)
  -- we want to get their guard,
  -- i.e. put this "insert" or "memory-access"
    -- specific code into a separate function,
  -- and finally use something like a foldl to 
  -- put together the guard condition

  let exception_murphi_guard_exprs := 
    calls_which_can_guard.map qualified_name_to_sta_murphi_expr
  dbg_trace "------ BEGIN GUARD EXPRS------"
  dbg_trace exception_murphi_guard_exprs
  dbg_trace "------ END GUARD EXPRS ------"

  let murphi_guard_exprs : List Murϕ.Expr := 
    exception_murphi_guard_exprs.map (
      λ excpt =>
        match excpt with
        -- TODO: This is bad, but I don't want
        -- to refactor right now, maybe later..
        | .error msg => Murϕ.Expr.designator (Murϕ.Designator.mk "BAD!!!" [])
        | .ok    murphi => murphi
    )

  --========== This is the guard condition =============
  let guard_cond :=
    List.foldl (
      λ mur_expr1 mur_expr2 =>
        -- and the exprs!
      Murϕ.Expr.binop (
        "&"
      ) mur_expr1 mur_expr2
    ) entry_is_at_state_expr murphi_guard_exprs


  /-
  3. Operational Code, DSL to Murphi
  -/


  let stmt_trans_info : stmt_translation_info := {
    stmt := trans_stmt_blk,
    lst_ctrlers := ctrler_lst,
    ctrler_name := ctrler_name,
    src_ctrler := none,
    lst_src_args := none,
    func := none,
    is_await := if await_state_name
    then await_or_not_state.await
    else await_or_not_state.not_await
    entry_keyword_dest := none
    trans_obj := trans
    specific_murphi_dest_expr := none
    lst_decls := []
    is_rhs := false
    use_specific_dest_in_transition := false
    curr_ctrler_designator_idx := none
    lhs_var_is_just_default := false
    translate_entry_or_ctrler := entry_or_ctrler.entry
  }

  let murphi_stmts_decls : lst_stmts_decls :=
  -- AZ TODO: Implement the AST Stmts => Murphi Stmts fn
    -- AZ TODO: Use the struct!
    ast_stmt_to_murphi_stmts stmt_trans_info
  -- The murphi stmts for the transition body
  let lst_murphi_stmt : List Murϕ.Statement := murphi_stmts_decls.stmts
  let decls_from_translation : List Murϕ.Decl := murphi_stmts_decls.decls
    
  -- List of ctrler names ( identifiers )
  let lst_ctrler_names : List Identifier := 
  ctrler_lst.map λ ctrler => ctrler.name

  let ast_decl_to_murphi_decl_monad : DeclInitM (List Murϕ.Decl) := ast_decl_assn_decl_to_murphi_decl trans_stmt_blk
  let simple_ast_murphi_decls : List Murϕ.Decl := ast_decl_to_murphi_decl_monad.run
    { trans' := trans, init_list := [], decl_list := [], ctrler_names := lst_ctrler_names,
      lst_ctrlers := ctrler_lst, curr_ctrler_name := ctrler_name} |>.run.1

  let murphi_stmts_decls_monad := murphi_stmts_to_murphi_decls lst_murphi_stmt
  let murphi_stmts_decls := murphi_stmts_decls_monad.run {
    trans' := trans, init_list := [],
    decl_list := simple_ast_murphi_decls ++ decls_from_translation,
    ctrler_names := lst_ctrler_names,
      lst_ctrlers := ctrler_lst, curr_ctrler_name := ctrler_name} |>.run.2
  let (lst_murphi_decls, murphi_inits) := (murphi_stmts_decls.decl_list, murphi_stmts_decls.init_list)
  -- AZ TODO: Implement the Murphi stmts -> Decls fn
  -- TODO NOTE: There are default vars to decl, like
  -- next_state of type (Sta or state)
  -- TODO NOTE: This should also gen the initialization
  -- assignment stmts for the Decls
  -- TODO NOTE: Use a Monad if necessary
    -- murphi_stmts_to_murphi_decls lst_murphi_stmt
    -- ([],[])

  -- TODO: Thursday Evening:
  -- Pre-pend the murphi_inits to the lst_murphi_stmt
  -- Post-pend next_state for all structures in the Decl list
  -- Post-pend Sta := next_state

  let all_decls : List Murϕ.Decl :=
    -- add the next state, which is of type STATE...
    lst_murphi_decls ++ [Murϕ.Decl.var ["next_state"] (Murϕ.TypeExpr.previouslyDefined "STATE")]
  let prepared_murphi_decls : List Murϕ.Decl :=
    remove_duplicate_murphi_decl all_decls

  let update_next_state : List Murϕ.Statement := 
    -- Convert any declared decls of ctrlers into update
    -- Though I think i could have avoided this by simply generating next_state.core_[j].<ctrler>
    gen_next_state_update prepared_murphi_decls lst_ctrler_names

  let prepared_murphi_stmts : List Murϕ.Statement :=
    [[murϕ| next_state := Sta]] ++ murphi_inits ++ lst_murphi_stmt ++ update_next_state ++ [[murϕ| Sta := next_state]]

  dbg_trace "===== BEGIN TRANSLATION INFO ====="
  dbg_trace "=== ctrler ==="
  dbg_trace ctrler_name
  dbg_trace "=== transition ==="
  dbg_trace trans
  dbg_trace "=== lst murphi stmt ==="
    dbg_trace lst_murphi_stmt
  dbg_trace "===== END TRANSLATION INFO ====="

  let is_a_per_entry_rule : Bool :=
    find_designator_with_expr_i lst_murphi_stmt
  
  let rule_core_quantifier : Murϕ.Quantifier :=
      (
        Quantifier.simple
        -- ID
        (ruleset_core_elem_idx)
        -- TypeExpr
        (cores_t)
      )

  let list_rule_quantifiers :=
  if is_a_per_entry_rule then
    let ruleset_buffer_idx := "i"
    let ctrler_idx :=
      TypeExpr.previouslyDefined (ctrler_name.append "_idx_t")

    [rule_core_quantifier].concat (
      Quantifier.simple ruleset_buffer_idx ctrler_idx
    )
  else
    [rule_core_quantifier]
  -- ======= After the analysis ======
  let murphi_core_ruleset :=
    Rule.ruleset -- List of quantifier, List of rule
    -- List of Quantifier (our TypeExpr of cores)
    list_rule_quantifiers
    [
      (
        Rule.simplerule
        -- Option String
        ---- Good investment:
        ---- Should build a good name
        ---- btn the state transitions
        ---- Await states get AWAIT appended
        rule_name
        -- Option Expr
        guard_cond
        -- List Decl
        prepared_murphi_decls
        -- List Statement
        prepared_murphi_stmts
      )
    ]
    -- List of Rule

  if lst_murphi_stmt.length == 0 then
    []
  else
    [murphi_core_ruleset]

--- ==== AST tests =====

def ex0000 : Identifier := "hullo"
def ex0002 : Term := Term.var ex0000
def ex0003 : Pipeline.Expr := Expr.some_term ex0002

-- === Statement
def ex0004 : Pipeline.Statement := Statement.stray_expr ex0003

-- === Conditional
def ex0005 : Conditional := Conditional.if_else_statement ex0003 ex0004 ex0004

-- === await
def ex0006 : Pipeline.Statement := Statement.await none [ex0004]

-- === descriptions
def ex0007 : Description := Description.controller "example_structure" ex0006

-- === AST with 1 description!
def ex0008 : AST := AST.structure_descriptions [ ex0007 ]

def ex1000 : List Description := ast0002_get_controllers ex0008
#eval ex1000
-- Empty list because there are no assignment statements in there?
#eval ast0004 ex1000
