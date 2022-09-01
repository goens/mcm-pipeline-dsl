-- import PipelineDsl

import PipelineDsl.Murphi

import PipelineDsl.AST
-- import PipelineDsl.Transformation

-- start at the top of the AST
-- (1) Collect the state_queues or "controllers"
-- (2) Match a state_queue/controller to an entry description
--     Tells us what variables the entry has
-- (3) Match state_queue/controller to transitions
-- (4) break up transitions at await

-- TODO: Write Objects to represent Murphi code as well
-- (5) Convert entries into records
-- (6) Convert controllers into Murphi records
--     consisting of an array of entries
-- (7) Convert transitions into Rules
--     

-- pick structure_descriptions, to get controllers


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
  | Description.transition a _ => a
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

structure controller_info where
  -- Name, like LQ, SQ, SB, etc.
  name : Identifier
  -- The controller description, probably some info here...
  controller_descript : Description
  -- The entry description, probably some info here...
  entry_descript : Description
  -- The init transition
  init_trans : Identifier
  -- Entry vars, like seq_num, ld_seq_num, inst, read_value
  -- NOTE: leave for now, figure out tomorrow
  -- Or translate from the entry_descript
  state_vars : List TypedIdentifier
  -- list of transitions this structure takes
  -- should be: Description.transition
  transition_list : List Description
deriving Inhabited

instance : ToString controller_info := ⟨
  λ i =>
    "===controller===\n" ++
    "NAME: " ++ toString i.name ++ "\n" ++
    "CONTROLLER_DESCRIPTION: " ++ toString i.controller_descript ++ "\n" ++
    "ENTRY_DESCRIPT: " ++ toString i.entry_descript ++ "\n" ++
    "INIT_TRANS: " ++ toString i.init_trans ++ "\n" ++
    "STATE_VARS: " ++ toString i.state_vars ++ "\n" ++
    "TRANSITION_LIST: " ++ toString i.transition_list ++ "\n=== End Controller ===\n\n"
  ⟩ 

inductive how_many_found
| nothing
| one
| two_or_more


structure dsl_trans_info where
ctrler_name: Identifier
ctrler_lst : List controller_info
trans : Description -- Description.transition

inductive tail_or_entry
| tail : tail_or_entry
| entry : tail_or_entry
| custom_entry : tail_or_entry

inductive await_or_not_state
| await : await_or_not_state
| not_await : await_or_not_state

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
-- assign var?

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

structure trans_and_expected_func where
expected_func : Identifier
expected_struct : Identifier
trans : Pipeline.Description
stmt_trans_info : stmt_translation_info
dest_ctrler_name : Identifier
-- dest_ctrler_entry : Murϕ.Expr
-- NOTE: This is for translating and 
-- using a specific entry name with a
-- ctrler which has entries
specific_murphi_dest_expr : Murϕ.Expr

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
)
--- =========== CUT FROM TRANSFORMATION ================
def filter_lst_of_stmts_for_ordering_asn
(lst_stmts : List Statement)
:=
  List.filter (
    λ stmt => 
      match stmt with
      -- | Statement.variable_assignment qual_name expr =>
      --   match qual_name with
      --   | QualifiedName.mk lst_idents' =>
      --     if (lst_idents'.contains "element_ordering")
      --       then true
      --       else false
      | Statement.value_declaration typed_ident expr =>
        match typed_ident with
        | TypedIdentifier.mk tident ident =>
          if (
            or
            (tident == "element_ordering")
            (ident == "ordering")
          )
          then true
          else false
      | _ => false
        
  )
  lst_stmts

def get_val_decl_stmt_var
(stmt : Statement)
:= 
  match stmt with
  | Statement.value_declaration typed_ident expr =>
  -- | Statement.variable_assignment qual_name expr =>
    match expr with
    | Expr.some_term term =>
      match term with
      | Term.var ident =>
        ident
      | _ => dbg_trace "Error: unexpected Term"
      default
    | _ => dbg_trace "Error: unexpected Expr"
      default
  | _ => dbg_trace "Error: unexpected Stmt"
    -- dbg_trace "BEGIN Stmt:\n"
    -- dbg_trace stmt
    -- dbg_trace "END Stmt:\n"
    default

def get_ordering_from_ctrler_descript
(ctrler_descript : Description)
:= 
  match ctrler_descript with
  | Description.controller ident stmt =>
    match stmt with
    | Statement.block lst_stmts =>
      let ordering_stmt_lst := (
        filter_lst_of_stmts_for_ordering_asn
        lst_stmts
      )
      -- should only be 1 stmt for ordering
      let ordering_stmt :=
        match ordering_stmt_lst with
        | [one_stmt] => one_stmt
        | _ => dbg_trace "Error: unexpected List size?"
          -- dbg_trace "List:\n"
          -- dbg_trace ordering_stmt_lst
          -- dbg_trace "List_stmts:\n"
          -- dbg_trace lst_stmts
          default
      -- as an Identifier
      let ordering_type :=
        get_val_decl_stmt_var ordering_stmt

      ordering_type
    | _ => dbg_trace "Error: unexpected stmt in order search"
      default
  | _ => dbg_trace "Error: unexpected ctrler in order search"
    default

def get_ctrler_elem_ordering
(ctrler : controller_info)
:=
  let ctrler_description := ctrler.controller_descript
  let ctrler_ordering :=
    get_ordering_from_ctrler_descript (
      ctrler_description
    )
  ctrler_ordering

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
structure murphi_transitions where
  transitions : List Description

def ast0021_empty_controller : controller_info :=
  {name := default, controller_descript := default, entry_descript := default, init_trans := default, state_vars := default, transition_list := default}

def ast0022_set_controller_name ( name : Identifier ) (ctrl : controller_info) : controller_info :=
  {name := name, controller_descript := ctrl.controller_descript, entry_descript := ctrl.entry_descript, init_trans := ctrl.init_trans, state_vars := ctrl.state_vars, transition_list := ctrl.transition_list}

def ast0024_set_entry_descript (ctrl : controller_info) ( descript : Description ) : controller_info :=
  {name := ctrl.name, controller_descript := ctrl.controller_descript, entry_descript := descript, init_trans := ctrl.init_trans, state_vars := ctrl.state_vars, transition_list := ctrl.transition_list}

def ast0025_set_entry_descript ( ctrl_and_entry : controller_info × Description ) :=
  ast0024_set_entry_descript ctrl_and_entry.1 ctrl_and_entry.2 

def ast0026_set_controller_init (ctrl : controller_info) ( trans : Identifier ) : controller_info :=
  {name := ctrl.name, controller_descript := ctrl.controller_descript, entry_descript := ctrl.entry_descript, init_trans := trans, state_vars := ctrl.state_vars, transition_list := ctrl.transition_list}

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

def ast0030_set_controller_descript (ctrl : controller_info) ( descript : Description ) : controller_info :=
  {name := ctrl.name, controller_descript := descript, entry_descript := ctrl.entry_descript, init_trans := ctrl.init_trans, state_vars := ctrl.state_vars, transition_list := ctrl.transition_list}

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
  -- Don't join list, for each one we try to find transitions
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

def ast0035_ctrl_obj_set_vars (ctrl : controller_info) : controller_info :=
  {name := ctrl.name, controller_descript := ctrl.controller_descript, entry_descript := ctrl.entry_descript, init_trans := ctrl.init_trans, state_vars := ast0032_get_entry_vars ctrl.entry_descript, transition_list := ctrl.transition_list}

partial def get_stmts_with_transitions
(stmt : Statement)
:=
          -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
          -- dbg_trace stmt
          -- dbg_trace "==END GET-TRANSITIONS ==\n"

  match stmt with
  | Statement.transition ident => [ident]
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
    | Description.transition iden stmt =>
      iden == trans_name
    | _ => false
  )
  ).map
  -- current node (in a list) now we find the transition stmts inside the
  -- matching transitions, these transition identifiers are
  -- the "child nodes"
  (
    λ transit => match transit with
    | Description.transition iden stmt =>
          
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
          | _ => false
        )
      | Statement.await _ await_lst => await_lst
      | Statement.when qname ident_list stmt => [stmt]
      | Statement.transition iden2 => [stmt]
      | Statement.conditional_stmt cond => [stmt]
      | Statement.listen_handle stmt1 lst => [stmt]
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
          | Description.transition iden1 stmt =>
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
        | Description.transition iden stmt => true
        | _ => false
    )

def ast0036_ctrl_obj_find_trans
-- (ctrl : controller_info)
-- (all_transitions : List Description)
(ctrl_and_all_trans : controller_info × List Description)
: controller_info :=
  {name := ctrl_and_all_trans.1.name, controller_descript := ctrl_and_all_trans.1.controller_descript, entry_descript := ctrl_and_all_trans.1.entry_descript, init_trans := ctrl_and_all_trans.1.init_trans, state_vars := ctrl_and_all_trans.1.state_vars, transition_list := ast0039_trans_ident_to_list (ast0038_trans_ident_to_trans_list ctrl_and_all_trans.1.init_trans ctrl_and_all_trans.2 []) ctrl_and_all_trans.2}

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
--   | Description.transition iden stmt =>
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

-- Do a BB traversal
-- def ast0047_traverse_stmt_ast
-- ()
-- ()

-- -- Info/State for the ast0046_transition_to_bb func
-- structure splitting_info where
--   -- (Actually should be Description.transition)
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
-- return the Description.transition
: (Description)
:=
  -- Create a Description.transition
  Description.transition
  identifier
  (Statement.block lst_stmts)

-- Aux function for recursive fn ast0046
-- is likely required:
-- got to handle reading things in a subcase

-- def ast0046_examine_statements
-- (lst_stmts : List Statement)
-- --( visit_nested : splitting_info )
-- (lst_transitions : List Description)
-- (top_transition_ident : Identifier)
-- (nested_stmts : List Statement)
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

--   -- want to extract
--   -- match lst_stmts with
--   -- | Statement.block lst_stmts =>
--   --   List.foldl
--   --   ()
--   --   ()
--   --   ()

--   List.foldl
--   (
--     λ checked next_stmt =>
--     -- [base case]
--     if (
--       -- if the next_stmt is the head of the transition
--       -- and an await, then continue to next stmt
--       and
--       (
--         and
--         (
--           -- next_stmt is await
--           match next_stmt with
--           | Statement.await lst_stmts => true
--           | _ => false
--         )
--         -- and the list of checked stmts
--         -- is empty (await is the head)
--         (lst_stmts.length == 0)
--       )
--       -- ensure we're also not nested
--       (nested_stmts.length == 0)
--     )
--     -- if await is the head stmt
--     -- then continue!
--     -- But we will add this to the
--     -- previously checked nodes
--     then --add_stmt_to_checked_list checked next_stmt
--       checked.cons next_stmt
--     else
--     -- [next inductive step]
--     -- now check if we encounter an await later
--     -- and this await is not nested
--     if (
--       and
--       (
--         and
--         (
--           -- NOTE: there's a difference between
--           -- a list of transitions to return,
--           -- and the level of nesting

--           -- I will need another list state
--           -- var for nested stmts

--           -- next_stmt is await
--           (
--             match next_stmt with
--             | Statement.await lst_stmts => true
--             | _ => false
--           )
--           -- and the list of checked stmts
--           -- is not empty (this await is the head)
--         )
--         (lst_stmts.length != 0)
--       )
--       -- we have not nested into a stmt block
--       (nested_stmts.length == 0)
--     )
--     then
--       none
--     -- AZ CHECKPOINT:
--     -- Thought up to here
--     -- Just putting the "cases"
--     -- here for now, fill them in later

--     -- TODO: 
--     -- actually write in the TODO cases

--       -- split the transition
--       -- (1) take above items 
--       -- (2) separate the below not yet checked items
--       -- (can get them with removing common items
--       -- from the initially provided list)

--       -- If this is the first transition made,
--       -- use the same original name, so
--       -- transitions targeting this one will
--       -- reach this
--     else
--     if (lst_transitions.length == 0)
--       then -- build new trans w/ original name
--         -- recursively call this func
--         -- on the created transition's
--         -- statements list
--         [
--           create_transition_from_lst_stmts
--           lst_stmts
--           top_transition_ident
--           ,
--           create_transition_from_lst_stmts
--           -- get the remaining stmts in this thing
--           (
--           lst_stmts.filter
--           (
--             -- return the remaining unchecked elems
--             λ elem =>
--               List.notElem
--               elem lst_stmts
--           )
--           )
--           -- generate a clever/useful identifier name
--           (
--             -- base top-transition ident
--             top_transition_ident
--             ++ 
--             -- suffix
--             (
--               match next_stmt with
--               | Statement.await lst_stmts => 
--                 lst_stmts.map
--                 (
--                   λ stmt' =>
--                   match stmt' with
--                   | Statement.when
--                     qname lst_iden stmt
--                     =>
--                     match qname with
--                     | lst_ident' =>
--                       [List.intercalate lst_ident']
--                   | _ => []
--                 )
--               | _ => []
--             ).intercalate
--           )
--         ]
--     else
--     -- [next inductive step]
--     -- now check if we encounter an await later
--     -- and this await is nested
--     -- (this case comes from when we try to handle
--     -- stmt blocks)
--     if (
--       and
--       (
--         and
--         (
--           -- NOTE: there's a difference between
--           -- a list of transitions to return,
--           -- and the level of nesting

--           -- I will need another list state
--           -- var for nested stmts

--           -- next_stmt is await
--           (
--             match next_stmt with
--             | Statement.await lst_stmts => true
--             | _ => false
--           )
--           -- and the list of checked stmts
--           -- is not empty (this await is the head)
--         )
--         (lst_stmts.length != 0)
--       )
--       -- we have not nested into a stmt block
--       (nested_stmts.length != 0)
--     )
--     then
--       none
--     else
--     -- [other case]
--     -- handle stmts which can nest into sub-stmts
--     if (
--       -- next_stmt is a conditional
--       (
--         match next_stmt with
--         | Statement.conditional_stmt cond => true
--         | _ => false
--       )
--     )
--     then
--       none
--       -- recursively call this fn on the
--       -- nested stmt block
--       -- Also remember to update the
--       -- nesting info in the
--       -- checked var

--       -- Do the recursive call by
--       -- matching into the stmts list
--       -- i.e. we don't have an if
--       -- case here in this lambda func
--       -- for block
--     else
--     if (
--       -- next_stmt is a when?
--       (
--         match next_stmt with
--         | Statement.when qname lst_iden stmt => true
--         | _ => false
--       )
--     )
--     then
--       none
--     else
--       -- if neither of these cases
--       -- then we just add the stmt to the checked list
--       -- and continue!
--       --add_stmt_to_checked_list checked next_stmt
--       checked.cons next_stmt

--   )
--   -- initial list of stmts to visit
--   -- and depth
--   -- so w
--   (
--     -- match visit_nested.lst_stmts with
--     match lst_stmts with
--     | h::t => [h]
--       -- create_splitting_info_with_lst_stmts [h] visit_nested
--     | [] => [] --create_splitting_info_with_lst_stmts [] visit_nested
--   )
--   (
--     -- match visit_nested.lst_stmts with
--     match lst_stmts with
--     | h::t => t
--       -- create_splitting_info_with_lst_stmts t visit_nested
--     | [] => []
--   )

--   -- To get this to work with a foldl:
--   -- The function returns a tuple/structure of 2 items:
--     -- the transition we'll return in place of this one
--     -- and the stmts we've nested into in order to search for an await
--   -- We try to process the current stmt in the transition
--   -- check if it's an await, and there are statements ahead of this!
--     -- if it's an await, and we aren't nested, we can split this block
--       -- How do we handle nested awaits?
--         -- Do we recursively call this fn?
--         -- Or do we do "iteration to a fixed point"?
--       -- i think we'll recursively call this
--       -- Not important either way
--   -- check if it's a nestable stmt:
--     -- Conditional stmt,
--     -- Await that is the first stmt,
--     -- When stmt
--     -- listen_statement,
--     -- Block stmt (duh!)

--     -- Nestable statements mean we recurse a layer
--     -- recursing a layer means we check for awaits again
--     -- Checking for awaits means we check if an await is:
--     -- (1) At the top of a transition (in this case no)
--     -- (2) Not at the top of a transition
--     -- (3) if the Await is nested
--     -- If it's nested then we return a new transition List
--     -- which must split & consider all nested parts and statements
--   -- record if not either way to our list of stmts? or just the nesting?
--     -- heh. just the nesting :)
      

-- def ast0047_access_transition_info
-- (descript : Description)
-- :=
--   match descript with
--   | Description.transition iden stmt =>
--     match stmt with
--     | Statement.block lst_stmt' =>
--       ast0046_examine_statements [stmt]
--     -- want to error if this is not a block
--     -- first stmt in transition should be a block!
--     | _ => []
--   | _ => []

-- Tie ast0010 (entries / names / identifiers)
-- and ast0013 entry first transition
-- into a controller_into struct
def ast0019_controller_info (ast : AST) :=
  -- ast0020_combine_controller_lists (ast0010_get_entries ast) (ast0013_map_entries (ast0010_get_entries ast))
  -- First get entries, then entry names
  (
  ast0041_list_ctrl_find_trans
  -- Arg1
  (
  (
  (
  (
  (
  (
  (
    (
      (
        List.join
        ((ast0010_get_entries ast).map ast0023_entry_to_name)
      ).map
    -- Now this makes "controller_info" objects from the names
    ast0020_controllers_from_ident_list
    ).zip
    -- Then add the entry AST objs to the controller
    -- First zip the list of controllers & entry AST objs
    (ast0010_get_entries ast)
  ).map
  -- Then map the tuple list to a fn to add the entry Description info
  ast0025_set_entry_descript
  -- Now zip this with the init transition name
  ).zip
  (ast0013_map_entries (ast0010_get_entries ast))
  ).map
  -- and map it to add the init_transition name to the controller
  ast0027_set_controller_init
  ).zip
  -- Zip with the controller info
  (ast0029_get_controllers ast)
  ).map
  -- map to add it to the controller description
  ast0031_set_controller_descript
  ).map
  ast0035_ctrl_obj_set_vars
  )
  -- Arg2
  (ast0040_get_trans ast)
  -- Now it has a: name, ctrl descript, entry discript
  -- Still need: state vars, transition list
  -- So: (1) Write func to check Controller obj to extract state vars
  -- from the entry
  -- (2) get the transition list by some kind of tree search
  -- Transition Description objs collected by ast0041 func
  )
  -- So now that we have transition objects,
  -- Start doing the to Murphi conversions
  -- Things we need for Murphi:
  -- (1) Constants (from Description.controller)
  -- i.e. num of elems in a buffer
  -- This is used in the records to specify num of entries
  -- (2) Records (from state vars of the structures)
  -- a buffer of some number of entries
  -- An instance of these records will also be added to the "core"
  -- (3) Transitions (from the Description.transition objects)
  -- This requires a more involved translation algo
  -- (a) init transition: an amalgamation of all controller's init trans
  -- (b) other transitions: This is where we do things like split
  -- transitions at await to have an awaiting state in Murphi,
  -- and transitions to 
  
  -- Start the constants
  -- Then Records
  -- > also generate the variables to do the searchs in the controllers
  -- > like search younger than, etc.
  -- > Should be something that's expected?

open Murϕ in
structure ctrler_decl_entry_decl_const_decl where
-- Decl.type ctrl.name (TypeExpr.record, ID/String TypeExpr.record)
ctrler_decl : Decl
entry_decl : Decl -- Decl.type, ID/String TypeExpr.record
const_decl_lst : List Decl -- Decl.const, ID/String Expr.integerConst
range_enum_decl : List Decl -- 

open Murϕ in
instance : ToString ctrler_decl_entry_decl_const_decl := ⟨
  λ i =>
    "=== Controller, Entry, Const, and Entry Range Decls ===\n" ++
    "<<<MURPHI CONTROLLER DECL>>>\n" ++ toString i.ctrler_decl ++ "\n\n" ++
    "<<<MURPHI ENTRY DECL>>>\n" ++ toString i.entry_decl ++ "\n\n" ++
    "<<<MURPHI CONST DECL LST>>>\n" ++ toString i.const_decl_lst ++ "\n\n" ++
    "<<<MURPHI ENUM DECL>>>\n" ++ toString i.range_enum_decl ++
    "\n=== End Controller Defn Decls ===\n\n"
  ⟩ 

open /-Murphi-/Murϕ in
def ast0048_generate_controller_murphi_record
( ctrl : controller_info )
:=
  -- TODO: read the entry description
  -- build the per entry record of state vars
  -- read the transition names
  -- build allowable states murphi enum
  -- Also add state for searching
  let murphi_decls_lst :=
  -- NOTE: Lean likes it when the parentheses
  -- starts immediately on the same line as map
  -- nightly-2022-07-13
  ctrl.state_vars.map (
    λ dsl_typed_ident => 
    match dsl_typed_ident with
    | TypedIdentifier.mk tiden ident =>
      Decl.var [ident] (TypeExpr.previouslyDefined tiden)
  )
  -- This is a record of what an entry looks like
  let murphi_entry_record := TypeExpr.record murphi_decls_lst
  -- make the controller record with a given num of entries
  let murphi_entry_record_decl_name := (String.join [ctrl.name, "_entry_values"])
  -- NOTE: key item to return for code gen
  let murphi_entry_record_decl :=
    Decl.type
      murphi_entry_record_decl_name
      murphi_entry_record
  -- Get the num of entries in a controller
  let num_entries :=
    match ctrl.controller_descript with
    | Description.controller ident stmt =>
      match stmt with
      | Statement.block lst_stmt =>
        let num_entries_stmt :=
        lst_stmt.filter (
          λ stmt => match stmt with
          | Statement.value_declaration typed_iden expr =>
            match typed_iden with
            | TypedIdentifier.mk tiden iden =>
              if iden == "num_entries"
                then true
                else false
          | _ => false
        )
        let num_entries_value_decl :=
          match num_entries_stmt with
          | [one] => one
          -- Shouldn't have more than one, or an empty list?
          | _ => dbg_trace "FAIL! No entries number for controller"
          default
        let num_entries' :=
          match num_entries_value_decl with
          | Statement.value_declaration typed_iden expr =>
            match expr with
            | Expr.some_term term =>
              match term with
              | Term.const cnst =>
                match cnst with
                | Const.num_lit num => num
                | _ => dbg_trace "FAIL!"
                  default
              | _ => default
            | _ => default
          | _ => default
        num_entries'
      -- another bad case
      | _ => default
    -- another bad case
    | _ => default
  let num_entries_range_enum := Nat.sub num_entries 1

  -- ========== Ctrler Num Entries =============
  let ctrler_num_entries_const_name := (String.join [ctrl.name, "_NUM_ENTRIES_CONST"])
  let ctrler_num_entries_const :=
    Decl.const 
    ctrler_num_entries_const_name
    (Expr.integerConst num_entries)

  let ctrler_num_entries_name_designator :=
    Designator.mk ctrler_num_entries_const_name []

  let ctrler_entries_count :=
    TypeExpr.integerSubrange
    (Expr.integerConst 0)
    (Expr.designator ctrler_num_entries_name_designator)
  let ctrler_entries_count_decl_name :=
    (String.join [ctrl.name, "_COUNT_ENUM"])
  -- NOTE: Another decl to return
  let ctrler_entries_count_decl :=
    Decl.type (
      ctrler_entries_count_decl_name
    )
    ctrler_entries_count

  -- ========== Ctrler Entries Enum =============
  let ctrler_num_entries_range_enum_const_name := (String.join [ctrl.name, "_NUM_ENTRIES_ENUM_CONST"])
  let ctrler_num_entries_range_enum_const :=
    Decl.const 
    ctrler_num_entries_range_enum_const_name
    (Expr.integerConst num_entries_range_enum)

  let ctrler_entries_enum_name_designator :=
    Designator.mk ctrler_num_entries_range_enum_const_name []

  -- Build Decl for the range of values the entry can take
  -- We should also build a constant to reference this
  -- controller's upper bound on num of entries
  let ctrler_entries_range :=
    TypeExpr.integerSubrange
    (Expr.integerConst 0)
    (Expr.designator ctrler_entries_enum_name_designator)
  let ctrler_entries_range_decl_name :=
    (String.join [ctrl.name, "_ENTRIES_ENUM"])
  -- NOTE: Another decl to return
  let ctrler_entries_range_decl :=
    Decl.type (
      ctrler_entries_range_decl_name
    )
    ctrler_entries_range

  -- Now we can build a Decl for the controller record
  let murphi_ctrler_record_name := String.join [ctrl.name, "_entries"]
  let murphi_ctrler_record :=
    Decl.var [ctrl.name] (
      TypeExpr.record [
        -- The array of entries, which is also a record
        Decl.var [
          -- Name of this array of entries
          murphi_ctrler_record_name
          ]
          (
            -- and the array entries and number of entries
            TypeExpr.array
            (
              TypeExpr.previouslyDefined
              ctrler_entries_range_decl_name
            )
            (
              TypeExpr.previouslyDefined
              murphi_entry_record_decl_name
            )
          ),
        -- Head, tail, and num_entries counter
        Decl.var ["head"] (
          TypeExpr.previouslyDefined
          ctrler_entries_range_decl_name
          ),
        Decl.var ["tail"] (
          TypeExpr.previouslyDefined
          ctrler_entries_range_decl_name
          ),
        Decl.var ["num_entries"] (
          TypeExpr.previouslyDefined
          ctrler_entries_count_decl_name
          )
        -- NOTE: Don't do msg buffers!
      ]
    )

  -- We must also send back the supporting Decl's
  -- for translation/code generation
  let ctrler_entry_const_decls : ctrler_decl_entry_decl_const_decl := {
    ctrler_decl := murphi_ctrler_record,
    entry_decl := murphi_entry_record_decl,
    const_decl_lst := [
      ctrler_num_entries_range_enum_const,
      ctrler_num_entries_const
      ],
    range_enum_decl := [ctrler_entries_range_decl, ctrler_entries_count_decl]
    }
  ctrler_entry_const_decls

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
(trans : Description) -- Description.transition
:=
  match trans with
  | Description.transition ident stmt =>
    ident
  | _ => dbg_trace "Didn't pass in a transition?"
    default

def get_transition_stmt
(trans : Description) -- Description.transition
:=
  match trans with
  | Description.transition ident stmt =>
    stmt
  | _ => dbg_trace "Didn't pass in a transition?"
    default

-- Go copy in relevant code from the DFS
def get_dest_transition_names
(trans : Description) -- Description.transition
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
  | Statement.await _ lst_stmt1 =>
  -- List.join (lst_stmt1.map get_stmts_with_transitions)
    [true]
  | Statement.when qname list_idens stmt => check_if_transition_stmt_blk_has_an_await stmt
  -- | Statement.listen_handle  => 
  | _ => []

-- probably can reuse some code to do this check
def does_transition_have_await
(trans : Description) -- Description.transition
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

          if (or
          (lst_ident.contains "insert")
          (lst_ident.contains "send_memory_request")
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
  let this_ctrler : controller_info :=
    -- dbg_trace "===== list_ident_to_murphi_designator_ctrler_var_check ====="
    get_ctrler_matching_name ctrler_name lst_ctrlers
  let this_ctrler_state_vars := this_ctrler.state_vars
  let state_var_idents : List Identifier :=
  this_ctrler_state_vars.map (
    λ t_ident =>
      match t_ident with
      | TypedIdentifier.mk tiden ident =>
        ident
  )

  let designator : Murϕ.Designator :=
  match qual_name_idents with
  | [one_ident] =>
    let ident_matches_state_var :=
    ident_matches_ident_list state_var_idents one_ident 

    -- if ident_matches_state_var
    -- then we should check the designator
    -- and generate the name using the
    -- sth.name.whatever.longer.name

    if ident_matches_state_var
    then
      -- If this matches then i should
      -- check if this var comes from
      -- a fifo structure to index into
      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_fifo : Bool :=
        ctrler_ordering == "FIFO"
      -- AZ CHECKPOINT TODO:
      -- finish this

      if is_fifo
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
            Sum.inl ctrler_name,
            Sum.inl idx
          ])
        | tail_or_entry.entry =>
          Murϕ.Expr.designator (
          Murϕ.Designator.mk idx [])
        | tail_or_entry.custom_entry =>
          specific_murphi_dest_extracted

        let murphi_designator :=
        Murϕ.Designator.mk "next_state" [
          -- entries
          Sum.inl "core_",
          Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
          Sum.inl ctrler_name,
          Sum.inl "entries",
          Sum.inr fifo_idx_expr,
          Sum.inl one_ident
        ]
        murphi_designator
      else
        dbg_trace "WHAT CTRLER STRUCTURE IS NOT FIFO"
        Murϕ.Designator.mk one_ident []
    else
      Murϕ.Designator.mk one_ident []

  | h::t =>
    let ident_matches_ident_list :=
    ident_matches_ident_list state_var_idents h

    -- This is for the second check,
    -- if "entry" is a term identifier,
    -- translate this as <ctrler>.entries[curr_idx]
    -- AZ NOTE: it's curr_idx simply because that's
    -- what the search API (for LQ -> SQ or SQ -> LQ searches)
    -- uses for indexing to the element it's searching for!
    let ident_is_entry : Bool :=
      h == "entry"

    if ident_matches_ident_list
    then
      -- If this matches then i should
      -- check if this var comes from
      -- a fifo structure to index into
      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_fifo : Bool :=
        ctrler_ordering == "FIFO"
      -- AZ CHECKPOINT TODO:
      -- is there a case where I want to
      -- replace "i" with "<structure>.tail?"

      if is_fifo
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
            Sum.inl ctrler_name,
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
          Sum.inl ctrler_name,
          Sum.inl "entries",
          Sum.inr fifo_tail_expr,
          Sum.inl h
        ] (list_ident_to_murphi_ID t)

        let murphi_designator :=
        Murϕ.Designator.mk "next_state" sum_list
        -- Murϕ.Designator.

        murphi_designator
      else
        dbg_trace "WHAT CTRLER STRUCTURE ISN'T FIFO?"
        Murϕ.Designator.mk h (list_ident_to_murphi_ID t)
    else
    if ident_is_entry
    then
      -- head entry h, is entry, translate to
      -- <ctrler>.entries[curr_idx]

      -- already have ctrler_name
      let entries := "entries"
      let curr_idx_designator_expr :=
      Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])

      let sum_list : List (String ⊕ Murϕ.Expr)
      := List.append [
        -- entries
        Sum.inl "core_",
        Sum.inr (Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])),
        Sum.inl ctrler_name,
        Sum.inl entries,
        Sum.inr curr_idx_designator_expr
      ] (list_ident_to_murphi_ID t)

      -- AZ TODO: Use dest ctrler!
      -- TODO Tuesday, Aug 16, 2022
      let murphi_designator :=
      -- Murϕ.Designator.mk dest_ctrler sum_list
      Murϕ.Designator.mk "next_state" sum_list
      -- Murϕ.Designator.

      murphi_designator
    else
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

  match term with
  | Term.negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let translation :=
    Murϕ.Expr.negation (ast_term_to_murphi_expr term'_trans_info)
    translation
  | Term.logical_negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let translation :=
    Murϕ.Expr.negation (ast_term_to_murphi_expr term'_trans_info)
    -- ast_term_to_murphi_expr term'
    translation
  | Term.binary_negation term' =>
    let term'_trans_info := assn_term_to_term_translation_info term_trans_info term'
    let ret_val :=
    ast_term_to_murphi_expr term'_trans_info
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

    if !is_src_ctrler_none
    then
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
      
      if ident_in_args
      then
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

        let murphi_designator : Designator :=
        list_ident_to_murphi_designator_ctrler_var_check (
          [ident]
        ) (lst_ctrlers) src_ctrler_extracted (tail_or_entry.entry) none

        let murphi_expr_designator : Murϕ.Expr := 
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
      else
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
        let tail_or_entry_or_custom :=
        if specific_murphi_dest_expr_is_some then
          tail_or_entry.custom_entry
        else
          tail_or_entry.entry

        let murphi_designator := (
          list_ident_to_murphi_designator_ctrler_var_check
          [ident]
          lst_ctrlers
          -- Note that curr_ctrler is likely
          -- the dest ctrler
          curr_ctrler_name
          tail_or_entry_or_custom
          -- will if none naturally if it is none
          term_trans_info.specific_murphi_dest_expr
        )
        let murphi_expr_designator :=
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
    else
      let specific_murphi_dest_expr_is_some : Bool :=
      term_trans_info.specific_murphi_dest_expr.isSome

      let tail_or_entry_or_custom :=
      if specific_murphi_dest_expr_is_some then
        -- dbg_trace "THIS IS UNIMPLEMENTED?"
        tail_or_entry.custom_entry
      else
        tail_or_entry.entry

      let murphi_designator := (
        list_ident_to_murphi_designator_ctrler_var_check
        [ident]
        lst_ctrlers
        curr_ctrler_name
        tail_or_entry_or_custom
        -- will if none naturally if it is none
        term_trans_info.specific_murphi_dest_expr
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
      src_ctrler == none

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
      list_ident_to_murphi_designator_ctrler_var_check (
        lst_ident
      ) (lst_ctrlers) (entry_keyword_dest) (tail_or_entry.entry) none

      let murphi_expr_designator : Murϕ.Expr := 
      Murϕ.Expr.designator murphi_designator

      murphi_expr_designator
    else
    if !is_src_ctrler_none
    then
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

      let lst_src_args_extracted :=
        if lst_src_args.isSome
        then
          lst_src_args.get!
        else
          panic! "calling func didn't provide the list of src ctrler args!"
      let ident_in_args : Bool :=
        lst_src_args_extracted.contains ident
      
      if ident_in_args
      then
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
        let murphi_designator : Designator :=
        list_ident_to_murphi_designator_ctrler_var_check (
          lst_ident
        ) (lst_ctrlers) src_ctrler_extracted (tail_or_entry.entry) none

        let murphi_expr_designator : Murϕ.Expr := 
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
      else
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

        let murphi_designator := (
          list_ident_to_murphi_designator_ctrler_var_check
          lst_ident lst_ctrlers curr_ctrler_name
          tail_or_entry_or_custom
          term_trans_info.specific_murphi_dest_expr
          -- Note that curr_ctrler is likely
          -- the dest ctrler
        )
        let murphi_expr_designator :=
        Murϕ.Expr.designator murphi_designator

        murphi_expr_designator
    else


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

      let murphi_designator := (
        list_ident_to_murphi_designator_ctrler_var_check
        lst_ident
        lst_ctrlers
        curr_ctrler_name
        tail_or_entry_or_custom
        term_trans_info.specific_murphi_dest_expr
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
    dbg_trace "WARNING: we didn't really use DSL Funcs,
    and I'm not bothering with a good translation"
    
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
      Murϕ.Expr.integerConst 0
      -- TODO: handle this case. Should be more or less (if it was done with a monad :D)
   --| Term.expr exp => -- ast_expr_to_murphi_expr exp
  | Term.expr expr =>
    let expr_trans_info := assn_term_to_expr_translation_info term_trans_info expr
    let pipeline_expr := ast_expr_to_murphi_expr expr_trans_info
    pipeline_expr
  -- panic! "unimplemented case (nested terms/expressions)"


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

  | _ => dbg_trace "These things don't map to"
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
  -- | Pipeline.Expr.binor _ _
  -- | Pipeline.Expr.binand _ _

-- ========= Helper Function ==========
partial def recursive_await_when_search
(lst_stmts : List Pipeline.Statement)
(func_name : Identifier)
(curr_ctrler_name : Identifier)
: (List Pipeline.Statement)
:=
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
      | Statement.when qual_name lst_ident stmt =>
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
      | Description.transition ident stmt =>
        match stmt with
        | Statement.block lst_stmts =>
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
:
-- List of (if Condition, and List of if cond stmts)
List (Murϕ.Expr × List Murϕ.Statement)
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
  | Pipeline.Description.transition ident stmt =>
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
  | [one] => how_many_found.one
  | [] => how_many_found.nothing
  | h::t => how_many_found.two_or_more

  match how_many_listen_handle_blks with
  | how_many_found.nothing =>
  -- Don't need to make anything,
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
      let args_and_stmts : ( List Identifier × Pipeline.Statement ) :=
        match func's_handle_blk with
        | Pipeline.HandleBlock.mk qual_name lst_ident stmts_blk =>
          (lst_ident, stmts_blk)

      let args : List Identifier := args_and_stmts.1
      let stmt_blk : Pipeline.Statement := args_and_stmts.2

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
        stmt := stmt_blk,
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
        src_ctrler := expected_struct,
        lst_src_args := args,
        func := expected_func,
        is_await := trans_and_func.stmt_trans_info.is_await,
        -- Don't think we need this here, but...
        entry_keyword_dest := trans_and_func.dest_ctrler_name,
        trans_obj := trans_and_func.trans,
        specific_murphi_dest_expr := trans_and_func.specific_murphi_dest_expr
      }
      -- TODO NOTE: THIS MUST BE TESTED!

      let if_blk_stmts := ast_stmt_to_murphi_stmts stmt_trans_info

      -- Okay, this means I need to know how to index into the
      -- current entry as well!
      -- This could be something like 
      -- LQ.entries[curr_idx]
      -- Must do sth like pass it in through the struct
      -- so we don't need to re-deduce this!

      let trans_name : Identifier :=
      match trans with
      | Description.transition ident stmt => ident
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
      let dest_struct_entry := trans_and_func.specific_murphi_dest_expr
      let murphi_entry_is_on_state_cond : Murϕ.Expr :=
        [murϕ|
        £trans_and_func.dest_ctrler_name .entries[ £dest_struct_entry ] .state = £trans_name]

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
: List Murϕ.Statement -- if stmt..
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
  let this_ctrler : controller_info :=
  -- dbg_trace "===== ROB SQUASH api gen ====="
    get_ctrler_matching_name ctrler_name ctrlers_lst

  let ctrler_squash_idx := queue_idx
  -- Murϕ.Expr.designator (Murϕ.Designator.mk "squash_ld_id" [])

  let handle_trans_info_lst : List trans_and_expected_func :=
  this_ctrler.transition_list.map (
  λ trans' =>
  {
    expected_func := expected_func,
    expected_struct := expected_struct,
    trans := trans',
    stmt_trans_info := stmt_trans_info,
    dest_ctrler_name := dest_ctrler_name,
    specific_murphi_dest_expr := ctrler_squash_idx
  }
  )

  let trans_handle_squash_list : List (Murϕ.Expr × (List Murϕ.Statement)) :=
  List.join (handle_trans_info_lst.map state_listen_handle_to_murphi_if_stmt)

  -- This would be the if stmt
  -- to do handle ROB Squash signals
  let trans_handle_squash_if_stmt : List Murϕ.Statement :=
  match trans_handle_squash_list with
  | [] => --dbg_trace ""
    -- Nothing found, nothing to generate?
    []
  | [one] => 
    let murphi_if_condition := one.1
    let murphi_true_cond_stmts := one.2
    [
    [murϕ|
    if (£murphi_if_condition) then
    £murphi_true_cond_stmts
    endif
    ]
    ]
  | h :: t => 
    let len_2_if_conds : Bool :=
    trans_handle_squash_list.length == 2
    let more_than_2_if_conds : Bool :=
    trans_handle_squash_list.length > 2

    let first_if_cond := trans_handle_squash_list[0]!
    let if_condition_0 := first_if_cond.1
    let true_cond_stmts_0 := first_if_cond.2

    let snd_if_cond := trans_handle_squash_list[1]!
    let if_condition_1 := snd_if_cond.1
    let true_cond_stmts_1 := snd_if_cond.2

    if len_2_if_conds then
      [
      [murϕ|
      if (£if_condition_0) then
        £true_cond_stmts_0
      elsif (£if_condition_1) then
        £true_cond_stmts_1
      -- else
        -- actually, could be on other states,
        -- so don't error here!
        -- error "Unexpected case in this transition";
      endif
      ]
      ]
    else
    if more_than_2_if_conds then
      let murphi_if_stmt : Murϕ.Statement :=
      Murϕ.Statement.ifstmt if_condition_0 true_cond_stmts_0 t []

      [murphi_if_stmt]
    else
      panic! "Should have caught other cases before this one?"
  
  trans_handle_squash_if_stmt

partial def ast_stmt_stray_expr_to_murphi_expr
(stmt_trans_info : stmt_translation_info)
-- (expr : Pipeline.Expr)
-- (lst_ctrlers : List controller_info)
-- (curr_ctrler_name : Identifier) -- "string"

  -- When statement stuff
-- (src_ctrler : Option Identifier)
-- (lst_src_args : Option List Identifier)
:
List Murϕ.Statement
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
    -- I Don't want to bother with nested scopes
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

          -- AZ TODO: Also need to do the "stall" code
          -- Now understand which function is this?
          if api_func_name == "insert"
          then
            -- structure insert sth
            -- Need some template/boilerplate code
            -- Things we need for this template are:
            /-
            1. not the decls? since we're generating them
            separately?
            2. The check on the strucutre? (but this
            has been done as a guard in earlier code)
            (i think? check later)
            3. The actual insert code.. could generate a
            function, but this isn't necessary.
            for now just generate the steps required..

            i.e (for a FIFO)
            (1) get the dest structure's tail,
            (2)
            and insert whatever element at it,
            executing the destination block's "when"
            stmt.
            (3) update next_tail to
            (tail + 1) % (<structure>_ENTRY_COUNT)
            or whatever the constant was called.
            And the num_entries to num_entries + 1.

            AZ NOTE: This changes depending on the
            buffer type. i.e. FIFO is fifo insert,
            hash is.. hash.
            -/

            /-
            But the layout of this is:
            (1) get <new_dest_struct>.tail
            (2) do the "when" stmt code.
            Check the stmts inside the when block.
            ideally they'll have some stmts which
            assign to their state vars
            (example:)
            var = sth // translate into
            <new_dest_struct>.entries[tail].var = sth

            But we'll need to match the assignment
            statement's destination variables to the
            entry's state vars list.
            If they belong, then we use the
            <new_dest_struct>.entries[tail].var
            murphi designator in the assignment.
            Otherwise normal translation..

            If the user declares variables, then
            we must omit the type here, and
            generate the decl in the decl generation
            step.

            Probably want a version of the translation
            function to check all vars, even in the
            exprs, if they belong to the state vars
            
            (3) I forgot what i was going to say.

            (4) Update the overall state, i.e. all
            variables we've generated for state

            How:
            After updating any "state" variables,
            or structure state vars, do
            next_state.<designator>
              := <new_dest_struc>.<designator>
            -/

            /- Starting with item (1) -/
            -- match the struct name
            -- put in helper func? to find the controller
            -- use filter to identify it?

            let dest_ctrler_tail_designator :=
            -- Get a struct name
            Murϕ.Designator.mk dest_ctrler_name [Sum.inl "tail"]

            /- For item (2) -/
            -- Take a look at the other structure's
            -- when stmt code,
            -- (a) to generate code for the insert process

            -- I should also know what the name of the current
            -- structure is, so I can access this current
            -- entry's vars as well
            -- (b) to refer to this structure's vars

            -- also, the structure's arguments for the function
            -- call as well, since the func args will likely
            -- be used within the Dest structure's await-when
            -- block as well.
            -- (b) use this structure's vars where needed

            -- (c) we must still remember to generate the
            -- dest structure's accessing designators for (a)


            let ctrler_lst_with_name :=
            ctrlers_lst.filter (
              λ ctrler =>
                -- match if ctrler name
                -- is the struct name
                ctrler.name == dest_ctrler_name
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
              dbg_trace dest_ctrler_name
              -- dbg_trace ctrlers_lst
              default

            -- First search for the structure/ctrler
            -- transitions.
            -- find this await-when statement
            -- waiting on this function
            -- and translate those stmts
            let when_stmt :=
            find_when_from_transition dest_ctrler.transition_list "insert" ctrler_name

            let when_stmt_murphi_stmts :=
            -- Qual name is likely still
            -- in list order <structure>.function

            -- Use the lst_ident to as a part of
            -- any vars of the "calling" structure
            -- (src structure, instead of dest_structure)

            -- this sounds like I should write a 
            -- separate ast_stmt_to_murphi_stmt func
            -- Or have this func take optional args
            -- like src ctrler, dest ctrler,
            -- and src ctrler local var ident list

            -- Okay, so then the variables that need to be
            -- properly translated are:
            -- (a) the stmts in the when block
            -- need to be done with the dest ctrler in mind
            
            -- (b) any vars in the lst_idents
            -- are from the src ctrler
            -- do we know if there's a defn
            -- for these vars?

            -- I think we just need to
            -- translate these vars specifically
            -- with the src_ctrler in mind

            -- I can double check the qual_name structure
            -- matches the src ctrler
            match when_stmt with
            | Pipeline.Statement.when qual_name lst_ident stmt =>
              let qual_name_list :=
              match qual_name with
              | QualifiedName.mk lst_idents =>
                lst_idents
              let qual_name_len_2 := qual_name_list.length == 2

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


              -- After any sanity messages, try to map the stmts
              -- Create the required info object:
              let trans_info : stmt_translation_info := (
                -- info
                stmt_translation_info.mk
                stmt
                ctrlers_lst
                dest_ctrler_name
                struct_name
                lst_ident
                (Option.some api_func_name)
                (await_or_not_state.not_await)
                none
                stmt_trans_info.trans_obj
                none
              )

              let murphi_stmts : List Murϕ.Statement :=
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

            | _ => dbg_trace "shouldn't get another stmt type"
              []

            when_stmt_murphi_stmts
            -- AZ CHECKPOINT TODO:
            -- Take this when_stmt, and match it and get it's
            -- lines of code as murphi code,
            -- but translate them differently this time...
            -- (i.e. considering the designators of the different
            -- structures)
            
            -- assume for now we have the matching controller;

            -- match the list of state vars to
            -- any vars in the stmts

          else
          if (and (api_func_name == "send_load_request")
          (dest_ctrler_name == "memory_interface"))
          then
            -- should be from reg_file
            -- This should just use the mem
            -- interface that's manually written
            -- in Murphi
            -- i.e. gen this and use the
            -- pre-exisiting mem-interface API

            -- 3 things to do
            /- 
            1. get the ld_entry or st_entry
            -- this depends on the current entry type...
            -- depends on transition, is it for ld or st?
            -- encode this into the API, so this is not implicit
            -- i.e. send_load_request

            -- Also, must figure out if we index into a queue entry?
            -- or not...
            2. use the helper function to get the out_msg msg created
            3. set the out_busy to true
            -/
            let this_ctrler : controller_info :=
    dbg_trace "===== mem_interface api gen ====="
              get_ctrler_matching_name ctrler_name ctrlers_lst
            let ctrler_ordering :=
              get_ctrler_elem_ordering this_ctrler

            let is_fifo : Bool :=
              ctrler_ordering == "FIFO"
            
            -- if it's fifo, then
            -- index to this rule's entry j
            -- else,
            -- we need to access the entry info in some other way?
            let ld_or_st_inst_info : List Murϕ.Statement :=
            if is_fifo
            then
              -- get info by accessing
              -- <structure>.entries[j]


              let ld_st_entry_designator := (
              Designator.mk (
                -- Example in comments
                -- core_
                "next_state"
              )
              [
                -- Example in comments
                -- Sta.core_
                Sum.inl "core_",
                -- Sta.core_[j]
                Sum.inr core_idx_designator,
                -- Sta.core_[j].LQ
                Sum.inl ctrler_name,
                -- Sta.core_[j].LQ.entries
                Sum.inl entries,
                -- Sta.core_[j].LQ.entries[i]
                Sum.inr entry_idx_designator
              ]
              )

              let ld_st_entry_expr :=
              Murϕ.Expr.designator ld_st_entry_designator

              let ld_st_designator : Murϕ.Designator :=
              Murϕ.Designator.mk "ld_st" []
              let ld_st_entry_stmt : Murϕ.Statement :=
              Murϕ.Statement.assignment ld_st_designator ld_st_entry_expr

              [ld_st_entry_stmt]
            else
              -- TODO:
              -- This case is for something like
              -- the load exec unit in the NoSQ
              []

            let set_core_mem_out_msg : Murϕ.Statement :=
            -- TODO: fix "ambiguous" here
            [murϕ|
            next_state .core_[ j ] .mem_interface_  .out_msg := insert_ld_in_mem_interface( ld_st , j)]

            let msg_out := "msg_out"
            let msg_out_designator : Murϕ.Designator := (
            Designator.mk (
              -- Example in comments
              -- core_
              "next_state"
            )
            [
              -- Example in comments
              Sum.inl "core_",
              -- core_[j]
              Sum.inr core_idx_designator,
              -- core_[j].LQ
              Sum.inl ctrler_name,
              -- core_[j].LQ.entries
              Sum.inl msg_out
            ])

            let func_call_expr :=
            Murϕ.Expr.call "insert_ld_in_mem_interface" [
              -- list of args
              Murϕ.Expr.designator (Murϕ.Designator.mk "ld_st_entry" []),
              Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])
            ]

            -- assign the out_msg the func call
            --   £dest_ctrler_name .out_msg := insert_ld_in_mem_interface(
            --                                       ld_entry,
            --                                       j
            --                                      );
            let assn_msg_out_func_call_stmt : Murϕ.Statement :=
            Murϕ.Statement.assignment msg_out_designator func_call_expr

            let out_busy := "out_busy"
            let out_busy_designator : Murϕ.Designator := (
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
              Sum.inl ctrler_name,
              -- core_[i].LQ.entries
              Sum.inl out_busy
            ])

            let true_designator_expr :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "true" [])
            -- assign the out_busy to true
            --   £dest_ctrler_name .out_busy := false;
            let assn_out_busy_true_stmt : Murϕ.Statement :=
            Murϕ.Statement.assignment msg_out_designator true_designator_expr
            -- let set_core_mem_out_busy :=
            -- [murϕ|
            --   £dest_ctrler_name .out_msg := insert_ld_in_mem_interface(
            --                                       ld_entry,
            --                                       j
            --                                      );
            --   £dest_ctrler_name .out_busy := false;
            -- ]
            let combined_stmts :=
            ld_or_st_inst_info.append [assn_msg_out_func_call_stmt, assn_out_busy_true_stmt]

            combined_stmts
          else
          if (and (api_func_name == "send_store_request")
          (dest_ctrler_name == "memory_interface"))
          then
            let this_ctrler : controller_info :=
    dbg_trace "===== mem_interface api gen ====="
              get_ctrler_matching_name ctrler_name ctrlers_lst
            let ctrler_ordering :=
              get_ctrler_elem_ordering this_ctrler

            let is_fifo : Bool :=
              ctrler_ordering == "FIFO"
            
            -- if it's fifo, then
            -- index to this rule's entry j
            -- else,
            -- we need to access the entry info in some other way?
            let ld_or_st_inst_info : List Murϕ.Statement :=
            if is_fifo
            then
              -- get info by accessing
              -- <structure>.entries[j]


              let ld_st_entry_designator := (
              Designator.mk (
                -- Example in comments
                -- core_
                "next_state"
              )
              [
                -- Example in comments
                -- Sta.core_
                Sum.inl "core_",
                -- Sta.core_[j]
                Sum.inr core_idx_designator,
                -- Sta.core_[j].LQ
                Sum.inl ctrler_name,
                -- Sta.core_[j].LQ.entries
                Sum.inl entries,
                -- Sta.core_[j].LQ.entries[i]
                Sum.inr entry_idx_designator
              ]
              )

              let ld_st_entry_expr :=
              Murϕ.Expr.designator ld_st_entry_designator

              let ld_st_designator : Murϕ.Designator :=
              Murϕ.Designator.mk "ld_st" []
              let ld_st_entry_stmt : Murϕ.Statement :=
              Murϕ.Statement.assignment ld_st_designator ld_st_entry_expr

              [ld_st_entry_stmt]
            else
              -- TODO:
              -- This case is for something like
              -- the load exec unit in the NoSQ
              []

            let set_core_mem_out_msg : Murϕ.Statement :=
            -- TODO: fix "ambiguous" here
            [murϕ|
            next_state .core_[ j ] .mem_interface_ .out_msg := insert_st_in_mem_interface( ld_st , j)]

            let msg_out := "msg_out"
            let msg_out_designator : Murϕ.Designator := (
            Designator.mk (
              -- Example in comments
              -- core_
              "next_state"
            )
            [
              -- Example in comments
              Sum.inl "core_",
              -- core_[j]
              Sum.inr core_idx_designator,
              -- core_[j].LQ
              Sum.inl ctrler_name,
              -- core_[j].LQ.entries
              Sum.inl msg_out
            ])

            let func_call_expr :=
            Murϕ.Expr.call "insert_st_in_mem_interface" [
              -- list of args
              Murϕ.Expr.designator (Murϕ.Designator.mk "ld_st_entry" []),
              Murϕ.Expr.designator (Murϕ.Designator.mk "j" [])
            ]

            -- assign the out_msg the func call
            --   £dest_ctrler_name .out_msg := insert_ld_in_mem_interface(
            --                                       ld_entry,
            --                                       j
            --                                      );
            let assn_msg_out_func_call_stmt : Murϕ.Statement :=
            Murϕ.Statement.assignment msg_out_designator func_call_expr

            let out_busy := "out_busy"
            let out_busy_designator : Murϕ.Designator := (
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
              Sum.inl ctrler_name,
              -- core_[i].LQ.entries
              Sum.inl out_busy
            ])

            let true_designator_expr :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "true" [])
            -- assign the out_busy to true
            --   £dest_ctrler_name .out_busy := false;
            let assn_out_busy_true_stmt : Murϕ.Statement :=
            Murϕ.Statement.assignment msg_out_designator true_designator_expr
            -- let set_core_mem_out_busy :=
            -- [murϕ|
            --   £dest_ctrler_name .out_msg := insert_ld_in_mem_interface(
            --                                       ld_entry,
            --                                       j
            --                                      );
            --   £dest_ctrler_name .out_busy := false;
            -- ]
            let combined_stmts :=
            ld_or_st_inst_info.append [assn_msg_out_func_call_stmt, assn_out_busy_true_stmt]

            combined_stmts

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

            [murϕ| next_state .core[j] .rf_ .rf[ £reg_idx_murphi_expr ] := £write_val_murphi_expr; ]
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
            specific_murphi_dest_expr := none
            }

-- (
-- assn_term_to_expr_translation_info term_trans_info match_cond)

            -- The Condition Expr
            let murphi_match_cond_expr := ast_expr_to_murphi_expr match_cond_trans_info

            -- 
            let dest_num_entries_const_name := (String.join [dest_ctrler_name, "_NUM_ENTRIES_CONST"])
            -- AZ NOTE:
            -- These are probably the main things... check around line 3400 if need other things..

            -- AZ NOTE: 
            -- for the "await" state check;
            -- particularily want to check if we're in
            -- an await state awaiting on response from
            -- another unit, ex. Memory Response, or ROB Commit

            -- "detect in-flight action / await states"
            -- could be a simple check for "await" stmt in
            -- a transition, but we don't need all
            -- transitions...?
            -- Just some specific ones?
            -- That have either have a pair of send-receive
            -- Or are explicitly marked, such as mem access
            -- or ROB response

            -- start by looking at list of transitions..
            -- for the dest struct that is..
            -- General detection algo:
            -- search for transitions with await & none
            -- check that the predecessor block
            -- initiates some request to the await'd structure

            -- Or, well, there's also the option of just
            -- knowing what type of await'ing we're doing;
            -- i.e. ROB Commit API is a asynch in-flight await
            -- i.e. mem reponse API is also a in-flight await
            -- This is probably a decent stop-gap solution to
            -- just get things working without needing to
            -- implement too much analysis

            -- Though there's still the issue of what should we do for the
            -- "squash in-flight action"

            -- So how do we know what the "squash" action is?
            -- In the discussion, Dan & Vijay brought up
            -- perhaps having the user specify the SSP of
            -- wht the squash action is...
            -- Okay, so how to express this...?

            -- AZ TODO:
            -- Identify the units which store the
            -- loads and stores while they speculatively
            -- execute
            -- SPECULATIVE LOAD UNIT
            -- search for controllers which process loads
            -- have ctrler which store loads, and 
            -- has a transition to process loads
            let speculative_ld_unit_name := "LQ"
            -- SPECULATIVE STORE UNIT
            let speculative_st_unit_name := "SQ"

            -- AZ TODO:
            -- Filter the list of transitions for this structure
            -- Find each state's listen/handle pair
            -- Find each pair's handle case for a ROB Squash() API
            -- Call

            -- search the ctrlers_lst, for the ld struct name
            let squash_ld_id :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "squash_ld_id" [])
            let expected_func := "squash"
            let expected_struct := "ROB"

            let ld_trans_handle_squash_if_stmt : List Murϕ.Statement := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              stmt_trans_info) speculative_ld_unit_name squash_ld_id (
              dest_ctrler_name) expected_func expected_struct
            )

            let squash_st_id :=
            Murϕ.Expr.designator (Murϕ.Designator.mk "squash_st_id" [])
            let expected_func := "squash"
            let expected_struct := "ROB"

            let st_trans_handle_squash_if_stmt : List Murϕ.Statement := (
              ctrler_trans_handle_stmts_to_murphi_if_stmt (
              stmt_trans_info) speculative_st_unit_name squash_st_id (
              dest_ctrler_name) expected_func expected_struct
            )

            let overall_murphi_head_search_squash_template : List Murϕ.Statement :=
            [murϕ|
            loop_break := false;
            if next_state .core_[j] .£dest_ctrler_name .num_entries = 0 then
              loop_break := true;
            endif;

            entry_idx := next_state .core_[j] .£dest_ctrler_name .head;
            --# (2) loop to tail searching for:
            --# if plus 1 is outside this range, this should be caught
            --# by difference check
            difference := ( next_state .core_[j] .£dest_ctrler_name .tail + £dest_num_entries_const_name - entry_idx ) % £dest_num_entries_const_name;
            offset := 0;
            --#if (difference != 0) then
            while ( (offset <= difference) & (loop_break = false)
                    & (difference > 0)
                  ) do
              --# Do the search
              curr_idx := ( entry_idx + offset ) % £dest_num_entries_const_name;
              --# (3) a load entry that's in a state where it's
              --# already done a read AND has a matching phys addr
              dest_ctrler_entry := next_state .core_[j] .£dest_ctrler_name .entries[curr_idx];
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
              -- phys_addr_match := dest_ctrler_entry.phys_addr = £dest_ctrler_name .phys_addr;

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
                violating_seq_num := dest_ctrler_entry .instruction .seq_num;

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
                squash_diff := (rob.rob_tail + (CORE_INST_NUM + 1) - rob_idx) % ( CORE_INST_NUM + 1);
                squash_offset := 0;
                while (
                    (squash_offset <= squash_diff)
                    &
                    (squash_diff > 0)
                  ) do
                  --# squash
                  squash_idx := (rob_idx + squash_offset) % (CORE_INST_NUM + 1);
                  --# Check if Ld or St
                  curr_rob_inst := rob.rob_insts[squash_idx];

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
                    squash_ld_id := search_lq_seq_num_idx(£speculative_ld_unit_name , curr_rob_inst.seq_num);
                    -- squash_dest_ctrler_entry := £speculative_ld_unit_name .entries[squash_ld_id];

                    £ld_trans_handle_squash_if_stmt
                    -- if ( squash_dest_ctrler_entry.ld_state = await_fwd_check_search_result ) then
                    --   if (sq.ld_seq_num = squash_dest_ctrler_entry.instruction.seq_num) then
                    --     sq.valid_access_msg := false;
                    --   endif;
                    -- elsif ( squash_dest_ctrler_entry.ld_state = await_sb_fwd_check_response ) then
                    --   if (sb.ld_seq_num = squash_dest_ctrler_entry.instruction.seq_num) then
                    --     sb.valid_access_msg := false;
                    --   endif;
                    -- elsif ( squash_dest_ctrler_entry.ld_state = await_committed ) then
                    --   --# If the executed msg was sent, and not yet accepted!
                    --   if (
                    --       (rob.valid_access_msg = true)
                    --       &
                    --       (rob.seq_num = squash_dest_ctrler_entry.instruction.seq_num)
                    --      ) then
                    --     rob.valid_access_msg := false;
                    --   --# If the executed msg was sent, and processed!
                    --   else
                    --     rob.is_executed[search_rob_seq_num_idx(rob,squash_dest_ctrler_entry.instruction.seq_num)] := false;
                    --   endif;
                    -- endif;

                    -- if ( squash_dest_ctrler_entry.ld_state = await_mem_response ) then
                    --   squash_dest_ctrler_entry.ld_state := squashed_await_mem_response;
                    -- else
                    --   squash_dest_ctrler_entry.ld_state := await_fwd_check;
                    -- endif;

                    -- £dest_ctrler_name .ld_entries[squash_ld_id] := squash_dest_ctrler_entry;

                  elsif (curr_rob_inst.op = st) then
                    --#
                    squash_st_id := search_sq_seq_num_idx(£speculative_st_unit_name, curr_rob_inst.seq_num);
                    -- squash_st_entry := £speculative_st_unit_name .entries[squash_st_id];

                    £st_trans_handle_squash_if_stmt
                    -- --# Shouldn't need to check this case actually
                    -- --# LQ must be busy with an older store's req
                    -- if ( squash_sq_entry.st_state = st_await_lq_squash ) then
                    --   if (£dest_ctrler_name .st_seq_num = squash_sq_entry.instruction.seq_num) then
                    --     £dest_ctrler_name .valid_access_msg := false;
                    --   endif;
                    -- elsif ( squash_sq_entry.st_state = st_await_committed ) then
                    --   --# If the executed msg was sent, and not yet accepted!
                    --   if (
                    --       (rob.valid_access_msg = true)
                    --       &
                    --       (rob.seq_num = squash_sq_entry.instruction.seq_num)
                    --      ) then
                    --     rob.valid_access_msg := false;
                    --   --# If the executed msg was sent, and processed!
                    --   else
                    --     rob.is_executed[search_rob_seq_num_idx(rob,squash_sq_entry.instruction.seq_num)] := false;
                    --   endif;
                    -- endif;

                    -- --# This case doesn't apply to this SQ
                    -- --#if ( squash_sq_entry.st_state = await_mem_response ) then
                    -- --#  squash_sq_entry.st_state := squashed_await_mem_response;
                    -- --#else
                    -- --#  squash_dest_ctrler_entry.ld_state := await_fwd_check;
                    -- --#end;
                    -- --# NOTE: The state to reset to depends on how
                    -- --# accurately we model the values that are used.
                    -- --# i.e. this reset is to stop any insts from using
                    -- --# a value a load incorrectly read.
                    -- --# 
                    -- squash_sq_entry.st_state := st_await_translation;

                    -- sq.sq_entries[squash_sq_id] := squash_sq_entry;
                  else
                    -- in the future, handle other inst types
                    error "inst should be either ld or st";
                  endif;

                  squash_offset := squash_offset + 1;
                end;

                -- if ( dest_ctrler_entry.ld_state = await_fwd_check_search_result ) then
                --   if (sq.ld_seq_num = dest_ctrler_entry.instruction.seq_num) then
                --     sq.valid_access_msg := false;
                --     --# I should probably clear the other fields...
                --   endif;
                -- elsif ( dest_ctrler_entry.ld_state = await_sb_fwd_check_response ) then
                --   if (sb.ld_seq_num = dest_ctrler_entry.instruction.seq_num) then
                --     sb.valid_access_msg := false;
                --     --# I should probably clear the other fields...
                --   endif;
                -- -- AZ TODO:
                -- -- want something like this, but to get this we could
                -- -- filter for transitions which have an await which
                -- -- awaits on the committed signal from the ROB
                -- elsif ( dest_ctrler_entry.ld_state = await_committed ) then
                --   --# If the executed msg was sent, and not yet accepted!
                --   if (
                --       (rob.valid_access_msg = true)
                --       &
                --       (rob.seq_num = dest_ctrler_entry.instruction.seq_num)
                --      ) then
                --     rob.valid_access_msg := false;
                --   --# If the executed msg was sent, and processed!
                --   else
                --     rob.is_executed[search_rob_seq_num_idx(rob,dest_ctrler_entry.instruction.seq_num)] := false;
                --   endif;
                -- endif;

                -- --#put "=== BEGIN ===\n";
                -- --#put dest_ctrler_entry.ld_state;
                -- if ( dest_ctrler_entry.ld_state = await_mem_response ) then
                --   --# set to squashed state
                --   dest_ctrler_entry.ld_state := squashed_await_mem_response;
                --   --#put "squashing\n";
                -- else
                --   dest_ctrler_entry.ld_state := await_fwd_check;
                --   --#put "just reset\n";
                -- endif;

                --#put "==== END ====\n";
                --# Don't bother doing any sophisticated rollback
                --# or squashing for now
                --# UPDATE STATE
                next_state .core_[j] .£dest_ctrler_name .entries[curr_idx] := dest_ctrler_entry;

                --# reset other entries after, checking the ROB
                --# get this entry's index in the ROB,
                --# and iterate to the tail of the ROB.
                --#put "INIT ROB_IDX SET:\n";
                --#put ld_entry.instruction.seq_num;

                --# NOTE IMPORTANT! exit from loop!
                loop_break := true;
              endif;

              if (offset != £dest_num_entries_const_name) then
                offset := offset + 1;
                if (( entry_idx + offset ) % £dest_num_entries_const_name) = next_state .core_[j] .£dest_ctrler_name .tail then
                  --#
                  loop_break := true;
                endif;
              else
                loop_break := true;
              endif;
            end;

            ]
            -- []
            overall_murphi_head_search_squash_template
          else
            -- reg file write?
            -- throw an error for unknown fns?
            -- but just going to return a default
            --
            -- (a) Check the args of the call, to get
            -- the reg number, and value to write...
            -- (b) Then access the core_[i]'s index

            -- let args := lst_expr
            -- assume we've "prepared the exprs" in some
            -- other previous stmts.. and we just use them..
            -- Just confirm the order...
            -- i.e. first arg is the reg idx, and 2nd is the write val
-- structure expr_translation_info where
-- expr : Pipeline.Expr
-- lst_ctrlers : List controller_info
-- ctrler_name : Identifier
-- -- when statement stuff
-- src_ctrler : Option Identifier
-- lst_src_args : Option (List Identifier)
-- func : Option Identifier
-- is_await : await_or_not_state

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
            [murphi_reg_file_assign]
        else
        if len_1_qual_name
        then
        dbg_trace "Doesn't make sense.. replace this with Except throw"
        []
        else
          -- len more than 2 qual name...
          -- we don't have that at the moment?
          -- should throw error
          -- just return default
        dbg_trace "Doesn't make sense.. replace this with Except throw"
          []
         

      | _ =>
        -- just call the term translation normally?
        -- can't, this is returns some expr?
        dbg_trace "What was i just passed?? should be a stray expr?"
        dbg_trace term
        []
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
    []
  | _ =>
  dbg_trace "passed in sth that's not a stray_expr"
  []

partial def api_term_func_to_murphi_func
-- ( term : Pipeline.Term )
( term_trans_info : term_translation_info )
-- The statements in the await block
-- try to match with when statements
-- use when stmt identifiers
( stmts_in_await : List Pipeline.Statement)
:=
  let term : Pipeline.Term := term_trans_info.term
  -- also get the current struct name... etc.
  let ctrlers_lst := term_trans_info.lst_ctrlers
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

  let dsl_func_info : (QualifiedName × List Pipeline.Expr) :=
  match term with
  | Pipeline.Term.function_call qual_name lst_exprs =>
    -- translate this specific call..
    -- don't use the the call for other stray exprs
    (qual_name, lst_exprs)
  | _ => dbg_trace "this would be an error"
    default
  let qual_name : QualifiedName := dsl_func_info.1
  let lst_names : List Identifier := match qual_name with
  | QualifiedName.mk lst_idents => lst_idents

  -- dbg_trace "== trying to get list of api names out of a func call Qual name =="
  let api_name : Identifier := lst_names[1]!

  let dest_struct_name : Identifier := lst_names[0]!
  let lst_exprs : List Pipeline.Expr := dsl_func_info.2

  -- Extract info, gen the murphi func code
  -- this is mostly setting up the Murphi Template
  let tail_search : Bool := lst_names.contains "tail_search"
  dbg_trace "&&&&& BEGIN is await function tail_search? &&&&&"
  dbg_trace tail_search
  dbg_trace "&&&&& END is await function tail_search? &&&&&"
  
  -- AZ TODO: This we know should have the one expr,
  -- we could techinically use Except and 'throw' here
  -- if the len isn't 1
  -- dbg_trace "== trying to get expr from func call arg list =="
  let curr_idx := Murϕ.Expr.designator (Murϕ.Designator.mk "curr_idx" [])

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
    entry_keyword_dest := Option.some dest_struct_name,
    trans_obj := term_trans_info.trans_obj,
    specific_murphi_dest_expr :=  curr_idx
  }

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

  let when_search_success_trans_info : stmt_translation_info := {
    stmt := when_search_success,
    lst_ctrlers := term_trans_info.lst_ctrlers,
    ctrler_name := term_trans_info.ctrler_name,
    src_ctrler := term_trans_info.src_ctrler,
    lst_src_args := term_trans_info.lst_src_args,
    func := term_trans_info.func,
    is_await := term_trans_info.is_await,
    entry_keyword_dest := Option.some dest_struct_name,
    trans_obj := term_trans_info.trans_obj,
    specific_murphi_dest_expr := curr_idx
  }
  let when_search_fail_trans_info : stmt_translation_info := {
    stmt := when_search_fail,
    lst_ctrlers := term_trans_info.lst_ctrlers,
    ctrler_name := term_trans_info.ctrler_name,
    src_ctrler := term_trans_info.src_ctrler,
    lst_src_args := term_trans_info.lst_src_args,
    func := term_trans_info.func,
    is_await := term_trans_info.is_await,
    entry_keyword_dest := Option.some dest_struct_name,
    trans_obj := term_trans_info.trans_obj,
    specific_murphi_dest_expr := curr_idx
  }

  let when_search_success_murphi_stmts : List Murϕ.Statement := ast_stmt_to_murphi_stmts when_search_success_trans_info
  let when_search_fail_murphi_stmts : List Murϕ.Statement := ast_stmt_to_murphi_stmts when_search_fail_trans_info
  dbg_trace "&&&&& BEGIN Murϕ when_success &&&&&"
  dbg_trace when_search_success_murphi_stmts
  dbg_trace "&&&&& END Murϕ when_success &&&&&"
  dbg_trace "&&&&& BEGIN Murϕ when_fail &&&&&"
  dbg_trace when_search_fail_murphi_stmts
  dbg_trace "&&&&& END Murϕ when_fail &&&&&"

  -- ex. SQ_NUM_ETNRIES_CONST
  let dest_num_entries_const_name := (String.join [dest_struct_name, "_NUM_ENTRIES_CONST"])

  -- AZ NOTE: Use this point to build a different template
  -- based on the specific API call...
  -- Or maybe earlier, but i'm not 100% sure
  -- what the "common code" segments are just yet...

  let overall_murphi_tail_search_template : List Murϕ.Statement :=
  [
    -- AZ TODO: introduce a type for the ACCESS_HASH
    -- or ACCESS_TAIL and just set it at the beginning

    -- [murϕ| next_state := Sta]
    -- [murϕ|  sq := Sta.core_[j].lsq_.sq_],
    -- [murϕ|  lq := Sta.core_[j].lsq_.lq_],
    [murϕ|  while_break := false],
    [murϕ|  found_entry := false],
    [murϕ|  if (next_state .core_[j] .£dest_struct_name .num_entries = 0) then
        while_break := true;
      endif],
      -- AZ TODO:
      -- no, can just map the condition check

    -- [murϕ|  if (£dest_ctrler_name .sq_msg_enum = £dest_ctrler_name_ACCESS_HASH) then
    --     st_idx := find_st_idx_of_seq_num(£dest_ctrler_name,
    --                                      £dest_ctrler_name .st_seq_num);
    --   elsif (£dest_ctrler_name .sq_msg_enum = SQ_ACCESS_TAIL) then
    --     st_idx := (£dest_ctrler_name .sq_tail + ( SQ_ENTRY_NUM + 1) - 1) % ( SQ_ENTRY_NUM + 1 );
    --   endif],
    [murϕ|  entry_idx := (next_state .core_[j] .£dest_struct_name .tail + £dest_num_entries_const_name - 1) % £dest_num_entries_const_name ],
    [murϕ|  difference := ( entry_idx + £dest_num_entries_const_name - next_state .core_[j] .£dest_struct_name .head ) % £dest_num_entries_const_name],
    [murϕ|  offset := 0],
    [murϕ|   while ( (offset <= difference) & (while_break = false) & ( found_entry = false ) ) do
        curr_idx := ( entry_idx + £dest_num_entries_const_name - offset ) % £dest_num_entries_const_name;
        if (
          -- AZ TODO:
          -- THIS IS WHERE TO TRANSLATE THE "API ARGS LIST"
          -- INTO CONDITIONS
          -- Keeping the code here as an example of
          -- what to expect kind of

          -- £dest_ctrler_name .entries[curr_idx] .phys_addr
          --   =
          --   £dest_ctrler_name .phys_addr
          £murphi_match_cond_expr

            ) then
  
          -- AZ TODO:
          -- This should be replaced with "this" ctrler's
          -- code
          -- Note that "entry" indicates access to the dest ctrler's
          -- fields
          -- So that we can even use entry in the when block...
          £when_search_success_murphi_stmts;

          -- value := £dest_ctrler_name .entries[curr_idx] .write_value;
  
          found_entry := true;
        endif;
  
        -- This is not really necessary
        if (offset != (difference + 1)) then
          offset := offset + 1;
        else
          while_break := true;
        endif;
      end],
    [murϕ|
      if (found_entry = false) then
        £when_search_fail_murphi_stmts
      endif]
  ]

-- AZ NOTE: Move this to the func call, instead of
-- in this await-when func call block
-- since the squash func here for the first LSQ Doesn't
-- check for the return when cases...
-- update this to work generically..?

  -- 0
  overall_murphi_tail_search_template
  -- default

-- AZ TODO: Implement these 2 functions!!!
partial def ast_stmt_to_murphi_stmts
(stmt_trans_info : stmt_translation_info)
:
(List Murϕ.Statement)
:=
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

    let stmt_lst : List Murϕ.Statement := ast_stmt_to_murphi_stmts new_translation_info

    stmt_lst
  | Statement.variable_declaration typed_ident =>
    -- AZ NOTE: must ignore declarations,
    -- since they go in the decl list,
    -- and not the stmt list
    []
  | Statement.value_declaration typed_ident expr =>
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
    let expr_trans_info := assn_stmt_to_expr_translation_info stmt_trans_info expr
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info
    
    let murphi_assignment_stmt :=
    Murϕ.Statement.assignment designator murphi_expr

    [murphi_assignment_stmt]

  | Statement.return_stmt expr =>
    let expr_trans_info :=
      assn_stmt_to_expr_translation_info stmt_trans_info expr
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info

    let murphi_return_stmt :=
    Murϕ.Statement.returnstmt murphi_expr

    [murphi_return_stmt]

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

    let murphi_stmts_lst :=
    List.join murphi_stmts_lst_lst

    murphi_stmts_lst

  /-
  The stray expr case is special..
  we need to make sure we correctly translate
  this "insert" operation

  This also applies to the memory interface call
  -/

  | Statement.stray_expr _ =>
  -- AZ NOTE!
  -- We don't want to call the
  -- ast_expr_to_murphi_expr here, since we
  -- want to disambiguate between the
  -- top level statement expr which is a func call
  -- and lower level exprs which is something else

  /-
  This is also a special one...
  This must set the current entry's state
  to the right given identifier state

  AZ TODO:
  Must also get the current transition's info,
  to know if this is an awaiting transition.

  If it is, the asynchronous action by the
  other structure should make this transition
  to this next state instead...

  AZ NOTE:
  [IMPORTANT]
  This would mean it would be helpful to
  keep a list of structures and transitions
  which rely on await+when responses from
  other structures in order to continue
  -/
  -- if this is not an await state
  -- translate into a state transition
  -- if it's an await state do nothing

  -- How to check if this is an await state?
  -- The top level func that calls this one
  -- can determine this,
  -- and simply pass it to this function as an input..
  -- or this func could figure it out with
  -- some deduction based on the inputs?
  -- | Statement.transition (String.mk _)

  /-
  "When" is also a special case...
  "When" is used when another structure
  executes it's action on another structure,
  and needs to execute the result as well

  this goes along with the await case,
  which will recursively call to this case

  Actually:
  If this transition is an await transition:
  An await state is moved forwards by another
  structure's transition in Murphi, so this will
  be implemented there?

  If this transition is an await transition with
  a structure function call argument, then
  we just translate everything together

  Revision, based on the await-API()-when case:
  We can still use this case recursively.
  and handle generating the corresponding code here.
  -/
  -- | Statement.when _ _ _

  /-
  "Await" with sending a request is a special case
  In this case, we execute an action based on the
  function,
  and the perform one of the results
  -- This will recursively call this func to get to
  -- the "when" case
  -/

  -- AZ NOTE: The function call part of this
  -- is similar to the stray_expr structure func call

  -- This should match the term similar to the
  -- stray_expr, and add code based on
  -- the API function the term contains

  -- The added code should then check the lst_stmts for
  -- given outcomes, if there are multiple "when" stmts
  -- basically

  -- If there are multiple whens, then the code
  -- has some kind of result that needs to be checked
  -- and the corresponding "when" block must be
  -- executed
  -- | Statement.await term lst_stmts =>

  /-
  "Await" without sending a request
  is also a special case...
  This means this state is an awaiting state
  if a state is an awaiting state
  then 

  -- This will NOT recursively call this func to get to
  -- the "when" case
  -/
  -- let expr_trans_info : expr_translation_info :=
  -- assn_stmt_to_expr_translation_info expr stmt_trans_info

-- TODO: Thurs, 12:36, was here!

  let lst_murphi_stmts : List Murϕ.Statement :=
  ast_stmt_stray_expr_to_murphi_expr stmt_trans_info

  lst_murphi_stmts

  | Statement.await none lst_stmts =>
    -- nothing to do here, we're awaiting on
    -- another structure to do something
    []

  /-
  Listen & Handle...
  Do we really need this at the moment?
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
    match conditional with
    | Conditional.if_else_statement expr stmt1 stmt2 =>
      -- map to Murphi
      -- This mapping is kind of simple
      -- Perhaps recursively checking the stmts
      -- would help map to a flatter Murphi structure
      let expr_trans_info := 
        assn_stmt_to_expr_translation_info stmt_trans_info expr
      let murphi_expr := ast_expr_to_murphi_expr expr_trans_info

      let stmt_trans_info1 := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt1
      let murphi_stmt1 :=
      ast_stmt_to_murphi_stmts stmt_trans_info1

      let stmt_trans_info2 := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt2
      let murphi_stmt2 :=
      ast_stmt_to_murphi_stmts stmt_trans_info2

      let murphi_if_stmt :=
      Murϕ.Statement.ifstmt murphi_expr murphi_stmt1 [] murphi_stmt2

  dbg_trace "!!!!! BEGIN generated if-else -> if stmt !!!!!"
  dbg_trace murphi_if_stmt
  dbg_trace "!!!!! END generated if-else -> if stmt !!!!!"

      [murphi_if_stmt]
    | Conditional.if_statement expr stmt =>
      let expr_trans_info := 
        assn_stmt_to_expr_translation_info stmt_trans_info expr
      let murphi_expr := ast_expr_to_murphi_expr expr_trans_info

      let stmt_trans_info' := 
        assn_stmt_to_stmt_translation_info stmt_trans_info stmt
      let murphi_stmt :=
      ast_stmt_to_murphi_stmts stmt_trans_info'

      let murphi_if_stmt :=
      Murϕ.Statement.ifstmt murphi_expr murphi_stmt [] []
  dbg_trace "!!!!! BEGIN generated if-stmt -> if stmt !!!!!"
  dbg_trace murphi_if_stmt
  dbg_trace "!!!!! END generated if-stmt -> if stmt !!!!!"

      [murphi_if_stmt]

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
    if unwrapped_func_name == "insert"
    then tail_or_entry.tail
    else tail_or_entry.entry

        let bool_thing : Bool :=
        if ctrler_name == "" then
        dbg_trace "===== BLANK STRING CTRLER NAME ====="
        false
        else
        true
    let murphi_var_name_designator :=
      match qual_name with
      | QualifiedName.mk lst_idents =>
        list_ident_to_murphi_designator_ctrler_var_check lst_idents ctrlers_lst ctrler_name tail_entry none

-- AZ TODO CHECKPOINT:
-- make this ast_expr_to_murphi_expr also
-- add the structure name stuff..
    let expr_trans_info := 
      assn_stmt_to_expr_translation_info stmt_trans_info expr
    let murphi_expr :=
      ast_expr_to_murphi_expr expr_trans_info

    let murphi_assn_expr :=
      Murϕ.Statement.assignment murphi_var_name_designator murphi_expr
    
    [murphi_assn_expr]

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
      []
    | await_or_not_state.not_await => 
      -- translate ident into updating
      -- this structure's entry state
      -- ident
      let this_ctrler : controller_info :=
    -- dbg_trace "===== dsl transition to murphi translation ====="
        get_ctrler_matching_name ctrler_name ctrlers_lst

      let ctrler_ordering :=
        get_ctrler_elem_ordering this_ctrler

      let is_fifo : Bool :=
        ctrler_ordering == "FIFO"

      if is_fifo
      then
      -- we access the specific entry i for the rule

      let ruleset_core_elem_idx := "j"
      let core_idx_designator :=
      Murϕ.Expr.designator (
        Designator.mk ruleset_core_elem_idx []
      )

      -- let ctrler_id := this_ctrler.name

      let entries := "entries"

      let ruleset_entry_elem_idx := "i"
      let entry_idx_designator :=
      Murϕ.Expr.designator (
        Designator.mk ruleset_entry_elem_idx []
      )

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
            Sum.inl ctrler_name,
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

      [murphi_state_assn]
      else
      -- AZ TODO NOTE: Consider the case of a unit
        -- If this isn't a FIFO / buffer structure
        -- Then do we just assign the unit's state?
        []
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

      let murphi_tail_search_template : List Murϕ.Statement :=
      api_term_func_to_murphi_func term_trans_info lst_stmts

      murphi_tail_search_template
    else
      []

  | Statement.when _ _ _ =>
  -- TODO : Implement this case
    []

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
    let msg_out_busy_designator :=
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
          Sum.inl dest_ctrler,
          -- core_[i].mem_interface_.out_busy
          Sum.inl out_busy
        ]
      )

    let murphi_false_expr :=
      Murϕ.Expr.negation msg_out_busy_designator
    -- let must_out_not_busy_murphi_expr :=
    --   Murϕ.Expr.binop
    --   "="
    --   msg_out_busy_designator
    --   ()
    return murphi_false_expr
  else
    throw s!"Input isn't insert or mem_access, how did it reach this?"

--========= Convert DSL Decl and Decl Assn Stmts to Decls =========
-- TODO: Check what I did to Decl Assn Stmts

def dsl_type_to_murphi_type
( dsl_type : Identifier )
: Murϕ.TypeExpr
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
    "inst_idx_t"
  else
    panic! "ERROR: ===== ENCOUNTERED UNEXPECTED DSL TYPE ====="

  let murphi_type_expr : Murϕ.TypeExpr :=
  Murϕ.TypeExpr.previouslyDefined murphi_type_name

  murphi_type_expr

-- structure decl_gen_info where
-- stmt : Pipeline.Statement
-- lst_ctrlers : List controller_info


structure decl_and_init_list where
decl_list : List Murϕ.Decl
init_list : List Murϕ.Statement
trans : Pipeline.Description
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
  let trans := (← get).trans
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
    let ast_decl_translation_monad := ast_decl_assn_decl_to_murphi_decl stmt
    let lst_decls := ast_decl_translation_monad.run
      { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
      |>.run.1
    return lst_decls
  | .conditional_stmt condition =>
    match condition with
    | Pipeline.Conditional.if_else_statement _ stmt1 stmt2 =>
      let ast_decl_translation_monad1 := ast_decl_assn_decl_to_murphi_decl stmt1
      let lst_decls1 := ast_decl_translation_monad1.run
        { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
      let ast_decl_translation_monad2 := ast_decl_assn_decl_to_murphi_decl stmt2
      let lst_decls2 := ast_decl_translation_monad2.run
        { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
      return (lst_decls1 ++ lst_decls2)
    | .if_statement _ stmt =>
      let ast_decl_translation_monad := ast_decl_assn_decl_to_murphi_decl stmt
      let lst_decls := ast_decl_translation_monad.run
        { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
      return lst_decls
  | .listen_handle stmt _ => ast_decl_assn_decl_to_murphi_decl stmt
  | .await none _ => return []
  | .await term stmt_list =>
    --  let await_stmts_decls :=
    --  List.join (stmt_list.map ast_decl_assn_decl_to_murphi_decl)
    let await_stmt_monads := stmt_list.map ast_decl_assn_decl_to_murphi_decl
    let lst_lst_decls : List (List Murϕ.Decl) := await_stmt_monads.map
      λ monad =>
        monad.run {trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
    let lst_decls : (List Murϕ.Decl) := List.join lst_lst_decls

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
    let matching_when : Pipeline.Statement := find_when_from_transition matching_ctrler.transition_list func_name curr_ctrler_name
    let when_stmts :=
    match matching_when with
    | Pipeline.Statement.when _ _ stmt => stmt
    | _ => dbg_trace "should be a when stmt!?"
      panic! "Should have just found a when stmt! why did we get something else?"

    let ast_decl_translation_monad := ast_decl_assn_decl_to_murphi_decl when_stmts
    let lst_decls_from_dest_struct := ast_decl_translation_monad.run
      { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
      |>.run.1

    -- await_stmts_decls
    return (lst_decls ++ lst_decls_from_dest_struct)
       
  | .when _ _ stmt =>
    let ast_decl_translation_monad := ast_decl_assn_decl_to_murphi_decl stmt
    let lst_decls := ast_decl_translation_monad.run
      { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
      |>.run.1
    return lst_decls
  -- ast_decl_assn_decl_to_murphi_decl stmt
  | .stray_expr expr =>
    let term_from_expr :=
    match expr with
    | Pipeline.Expr.some_term term => term
    | _ => dbg_trace "This shouldn't happen, malformed AST. TODO: Throw?"
      panic! "Shold Throw here..."

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
    let matching_when : Pipeline.Statement := find_when_from_transition matching_ctrler.transition_list func_name curr_ctrler_name
    let when_stmts :=
    match matching_when with
    | Pipeline.Statement.when _ _ stmt => stmt
    | _ => dbg_trace "should be a when stmt!?"
      panic! "Should have just found a when stmt! why did we get something else?"

    let ast_decl_translation_monad := ast_decl_assn_decl_to_murphi_decl when_stmts
    let lst_decls_from_dest_struct := ast_decl_translation_monad.run
      { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
      |>.run.1

    return lst_decls_from_dest_struct
  | .block stmt_lst =>
    let await_stmt_monads := stmt_lst.map ast_decl_assn_decl_to_murphi_decl
    let lst_lst_decls : List (List Murϕ.Decl) := await_stmt_monads.map
      λ monad =>
        monad.run {trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
    let lst_decls : (List Murϕ.Decl) := List.join lst_lst_decls

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
          Sum.inl desig_id
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
modify λ { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name } =>
  { trans := trans, init_list := init_list ++ decl_init_tuple.2, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name,
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
  let lst_ctrlers : List controller_info := (← get).lst_ctrlers

  -- The other stuff, but i don't use this here..
  -- { trans := trans, init_list := [], decl_list := simple_ast_murphi_decls, ctrler_names := lst_ctrler_names}
  let trans := (← get).trans
  let init_list := (← get).init_list
  let decl_list := (← get).decl_list
  let ctrler_names := (← get).ctrler_names
  let curr_ctrler_name := (← get).curr_ctrler_name

  -- foldl thru the stmts list, match stmt with | case
  -- translate line by line
  -- if blk, can recursively call
  --   if recursive, then still same; add decls & init to list..

  -- α : Unit, β : List Pipeline.Statement
  let _ := List.foldl (
    λ _ stmt =>
      let gen_decl_monad := gen_decl_from_stmt_and_append () stmt
      let lst_decls := gen_decl_monad.run
        { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name}
        |>.run.1
      -- modify λ
      --   { trans := trans, init_list := init_list, decl_list := decl_list, ctrler_names := ctrler_names, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name } =>
      --   { trans := trans, init_list := init_list ++ decl_init_tuple.2, lst_ctrlers := lst_ctrlers, curr_ctrler_name := curr_ctrler_name, decl_list := decl_list ++ decl_init_tuple.1, ctrler_names := ctrler_names }
      ()
  ) () stmts

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

--=========== DSL AST to Murphi AST =============
def dsl_trans_descript_to_murphi_rule
(trans_info : dsl_trans_info)
-- (ctrler_and_trans : (List controller_info) × Description)
-- (ctrler : controller_info)
-- (trans : Description) -- Description.transition

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
  let core_idx_designator :=
  Murϕ.Expr.designator (
    Designator.mk ruleset_core_elem_idx []
  )

  let ctrler_id := ctrler.name

  let entries := "entries"

  let ruleset_entry_elem_idx := "i"
  let entry_idx_designator :=
  Murϕ.Expr.designator (
    Designator.mk ruleset_entry_elem_idx []
  )

  let state := "state"

  let current_structure_entry_state :=
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
        Sum.inl ctrler_id,
        -- core_[i].LQ.entries
        Sum.inl entries,
        -- core_[i].LQ.entries[j]
        Sum.inr entry_idx_designator,
        -- core_[i].LQ.entries[j].state
        Sum.inl state
      ]
    )
  
  let current_state_expr :=
  Murϕ.Expr.designator (
    Designator.mk
    current_state_name
    []
  )

  let entry_is_at_state_expr :=
  Murϕ.Expr.binop (
    "="
  ) current_structure_entry_state current_state_expr

  /-
  2. Do a check on the transition, if we insert into
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

  -- for each of these func calls (list of idents)
  -- we want to get their guard,
  -- i.e. put this "insert" or "memory-access"
    -- specific code into a separate function,
  -- and finally use something like a foldl to 
  -- put together the guard condition

  let exception_murphi_guard_exprs := 
    calls_which_can_guard.map qualified_name_to_murphi_expr

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
  -- let guard_cond :=
  --   if insert_func_call.length != 0
  --   then
  --     Murϕ.Expr.binop (
  --       "&"
  --     ) entry_is_at_state_expr num_entries_of_dest_not_full
      
  --   else
  --     entry_is_at_state_expr

  -- dbg_trace "=== What did we find from the insert func? ===\n"
  -- dbg_trace insert_func_call
  -- dbg_trace "== END ==\n"

  /-
  3. Operational Code, DSL to Murphi
  
  Some statements are simple enough to translate
  -- state var access/assignment ==> Record access/assgn
  -- conditional if stmts ==> Murphi conditional
  etc...

  Add basic translations of note here if needed

  Some, not so much
  Our function calls, labelled statements, await/when
  will be more work to translate
  -- direct HW synch func call ==> (insert) just exec the
      when statement in the dest structure when exec'ing
      the transition we're on
  -- await/when ==> Same thing as the direct function call
      There's no await with insert, but this should be ok
      for now.
      This is primarily to handle multiple possible
      responses, in the event we want to react to different
      responses.
      This is helpful for API() calls.
  -- Labelled statements ==> same as whatever the stmt is

  Add difficult translations of note here if needed
  -/

  /-
  This step is probably better done recusively,
  since we interact with the AST (tree!)

  Write a function to translate either
  a list of stmts or stmts (don't know if this
  must be done in order or can be mapped in parallel)
  Probably better to execute in order for now..?

  Using match we will eventually cover all cases :)

  -- After this, we can use another function to
  -- check the Murphi code and generate any required
  -- decls
  -/

  -- Implementing this in-order sequentially
  -- makes sense if there's dependencies, and monad-like
  -- behaviour
  -- Map is ok if things are independent.
  -- I'll go with taking in a stmt and using map for
  -- sub-stmts.
  --
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
  }

  -- The murphi stmts for the transition body
  let lst_murphi_stmt : List Murϕ.Statement:=
  -- AZ TODO: Implement the AST Stmts => Murphi Stmts fn
    -- AZ TODO: Use the struct!
    ast_stmt_to_murphi_stmts stmt_trans_info
    
  -- List of ctrler names ( identifiers )
  let lst_ctrler_names : List Identifier := 
  ctrler_lst.map λ ctrler => ctrler.name

  let ast_decl_to_murphi_decl_monad : DeclInitM (List Murϕ.Decl) := ast_decl_assn_decl_to_murphi_decl trans_stmt_blk
  let simple_ast_murphi_decls : List Murϕ.Decl := ast_decl_to_murphi_decl_monad.run
    { trans := trans, init_list := [], decl_list := [], ctrler_names := lst_ctrler_names,
      lst_ctrlers := ctrler_lst, curr_ctrler_name := ctrler_name} |>.run.1

  let murphi_stmts_decls_monad := murphi_stmts_to_murphi_decls lst_murphi_stmt
  let murphi_stmts_decls := murphi_stmts_decls_monad.run
    { trans := trans, init_list := [], decl_list := simple_ast_murphi_decls, ctrler_names := lst_ctrler_names,
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

  let prepared_murphi_decls : List Murϕ.Decl :=
    -- add the next state, which is of type STATE...
    lst_murphi_decls ++ [Murϕ.Decl.var ["next_state"] (Murϕ.TypeExpr.previouslyDefined "STATE")]

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
  -- ======= After the analysis ======
  let murphi_core_ruleset :=
    Rule.ruleset -- List of quantifier, List of rule
    -- List of Quantifier (our TypeExpr of cores)
    [
      (
        Quantifier.simple
        -- ID
        (ruleset_core_elem_idx)
        -- TypeExpr
        (cores_t)
      )
    ]
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
