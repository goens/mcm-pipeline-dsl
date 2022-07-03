import PipelineDsl

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
  | Term.var var => var
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

-- a trick: you can use the s! too (like printf in C or fstrings in python)
def TypedIdentifier.toString : TypedIdentifier → String
  | .mk type_iden iden => s!"{type_iden} {iden}"

instance : ToString TypedIdentifier where toString := TypedIdentifier.toString

instance : ToString controller_info := ⟨
  λ i =>
    "===controller===\n" ++
    "NAME: " ++ toString i.name ++ "\n" ++
    "CONTROLLER_DESCRIPTION: " ++ toString i.controller_descript ++ "\n" ++
    "ENTRY_DESCRIPT: " ++ toString i.entry_descript ++ "\n" ++
    "INIT_TRANS: " ++ toString i.init_trans ++ "\n" ++
    -- "STATE_VARS: " ++ toString i.state_vars ++ "\n" ++
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
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 => List.join ([stmt1,stmt2].map get_stmts_with_transitions)
    | Conditional.if_statement expr1 stmt1 => get_stmts_with_transitions stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map get_stmts_with_transitions)
  | Statement.await lst_stmt1 => List.join (lst_stmt1.map get_stmts_with_transitions)
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
          | Statement.await await_lst =>
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
          | _ => false
        )
      | Statement.await await_lst => await_lst
      | Statement.when qname ident_list stmt => [stmt]
      | Statement.transition iden2 => [stmt]
      | Statement.conditional_stmt cond => [stmt]
      -- | Statement.listen_handle  => 
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
  -- > also generate the variables to do the searchs in the controllers
  -- > like search younger than, etc.
  -- > Should be something that's expected?

--- ==== AST tests =====

def ex0000 : Identifier := "hullo"
def ex0002 : Term := Term.var ex0000
def ex0003 : Expr := Expr.some_term ex0002

-- === Statement
def ex0004 : Statement := Statement.stray_expr ex0003

-- === Conditional
def ex0005 : Conditional := Conditional.if_else_statement ex0003 ex0004 ex0004

-- === await
def ex0006 : Statement := Statement.await [ex0004]

-- === descriptions
def ex0007 : Description := Description.controller "example_structure" ex0006

-- === AST with 1 description!
def ex0008 : AST := AST.structure_descriptions [ ex0007 ]

def ex1000 : List Description := ast0002_get_controllers ex0008
#eval ex1000
-- Empty list because there are no assignment statements in there?
#eval ast0004 ex1000
