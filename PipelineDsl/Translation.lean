import PipelineDsl
import PipelineDsl.Murphi

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

-- private partial def typedIdentifierToString : TypedIdentifier → String
--   | .mk t id => (toString t) ++ " " ++ (toString id)

-- instance : ToString TypedIdentifier :=
-- ⟨
--   -- The (toString t) cannot synthesize!
--   λ t id => (toString t) ++ " " ++ (toString id)
-- ⟩

-- instance : ToString (List TypedIdentifier) :=
-- ⟨
--   λ lst => String.join ( (lst.map toString).intercalate [[","]] )  --.intercalate
-- ⟩

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
  | Statement.await none lst_stmt1 => List.join (lst_stmt1.map get_stmts_with_transitions)
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
          | Statement.await none await_lst =>
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
      | Statement.await none await_lst => await_lst
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
  | Statement.await none lst_stmt1 =>
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

partial def get_insert_function_calls
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

          if (lst_ident.contains "insert")
          then
            [lst_ident]
          else
            []
      | _ => -- ignore, term isn't a func call
        []
    | _ => -- isn't going to be a func call
      []
  | Statement.labelled_statement label stmt =>
    get_insert_function_calls stmt
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [get_insert_function_calls stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk qname iden_list stmt1 =>
            get_insert_function_calls stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_insert_function_calls)
    | Conditional.if_statement expr1 stmt1 => get_insert_function_calls stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_insert_function_calls)
  | Statement.await term lst_stmt1 => List.join (lst_stmt1.map get_insert_function_calls)
  | Statement.when qname list_idens stmt => get_insert_function_calls stmt
  -- | Statement.listen_handle  => 
  | _ => []

--=========== DSL AST to Murphi AST =============
def dsl_trans_descript_to_murphi_rule
(ctrler_and_trans : controller_info × Description)
-- (ctrler : controller_info)
-- (trans : Description) -- Description.transition

-- all other controllers, if we need to gen
-- something like "insert" code

-- (lst_ctrlers : List controller_info)
:=
  let ctrler := ctrler_and_trans.1
  let trans := ctrler_and_trans.2
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
  let ruleset_core_elem_idx := "i"

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

  let ruleset_entry_elem_idx := "j"
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
        "core_"
      )
      [
        -- Example in comments
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

  -- List order is backwards! (func, then object)
  let insert_func_call := List.join (
    get_insert_function_calls trans_stmt_blk
  )

  let insert_func := insert_func_call.take 1
  let dest_ctrler_lst := insert_func_call.take 1
  let dest_ctrler := match dest_ctrler_lst with
  | [dest_name] => dest_name
  | _ => dbg_trace "How are there other entries?"
    default

  let num_entries := "num_entries"

  -- now build up the conditional
  let dest_structure_entry_count :=
    Murϕ.Expr.designator (
      Designator.mk (
        -- Example in comments
        -- core_
        "core_"
      )
      [
        -- Example in comments
        -- core_[i]
        Sum.inr core_idx_designator,
        -- core_[i].LQ
        Sum.inl dest_ctrler,
        -- core_[i].LQ.entries
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

  --========== This is the guard condition =============
  let guard_cond :=
    if insert_func_call.length != 0
    then
      Murϕ.Expr.binop (
        "&"
      ) entry_is_at_state_expr num_entries_of_dest_not_full
      
    else
      entry_is_at_state_expr

  -- dbg_trace "=== What did we find from the insert func? ===\n"
  -- dbg_trace insert_func_call
  -- dbg_trace "== END ==\n"

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
        -- List Statement
      )
    ]
    -- List of Rule

  0

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
