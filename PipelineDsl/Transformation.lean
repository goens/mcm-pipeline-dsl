
import PipelineDsl.AST
import PipelineDsl.Translation

-- import PipelineDsl.AST
import PipelineDsl.AnalysisHelpers

-- Basic in order load to load execution

/-
Idea: A load which is about to send a 
request to memory, will first check if
the load ahead (if there is one) has
already sent and received it's mem request.

If it has, then this load can send
a request to mem.
-/

/-
To implement an algorithm to transform the
DSL AST to enforce this,
(1)
the execution
points for a load is first identified
-/

-- Start with:
-- Controller Descriptions (from Translation.lean)

-- Identify:
-- (a) In what transition / state machine are
-- loads being executed. (lets us add the 
-- API call around here.)
-- (b) the controller details, so we know
-- where to "connect wires" or "send back a reponse"
-- to this load being executed.
-- probably something like, if it's a
-- buffer, then the current entry id or sth

open Pipeline

-- if there is a memory_interface
-- access, there will be a "true" in the
-- list of bools.
-- otherwise, the list should be empty!
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

partial def true_if_stmts_awaits_ld_mem_response
(stmt : Statement)
: List Bool
:=
  -- dbg_trace "==BEGIN GET-TRANSITIONS ==\n"
  -- dbg_trace stmt
  -- dbg_trace "==END GET-TRANSITIONS ==\n"
  match stmt with
  | Statement.transition _ => []
  | Statement.stray_expr _ => []
  | Statement.listen_handle stmt lst =>
    List.join
    (
      [true_if_stmts_awaits_ld_mem_response stmt]
      ++
      (
        lst.map
        (
          λ handl =>
          match handl with
          | HandleBlock.mk _ _ stmt1 =>
            true_if_stmts_awaits_ld_mem_response stmt1
        )
      )
    )
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement _ stmt1 stmt2 => List.join ([stmt1,stmt2].map true_if_stmts_awaits_ld_mem_response)
    | Conditional.if_statement _ stmt1 => true_if_stmts_awaits_ld_mem_response stmt1
  | Statement.block lst_stmt => List.join (lst_stmt.map true_if_stmts_awaits_ld_mem_response)
  | Statement.await none lst_stmt1 => List.join (lst_stmt1.map true_if_stmts_awaits_ld_mem_response)
  | Statement.when qual_name list_idents _ =>
    let list_names : List String :=
    match qual_name with
    | .mk strs => strs

    if ((list_names[0]! == "memory_interface") &&
      (list_names[1]! == "access_completed") &&
      (list_idents[0]! == "value") ) then
      [true]
    else
      []
    -- true_if_stmts_awaits_ld_mem_response stmt
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

/-
(2)
After identifying the execution point
of a load, we would then need to find
the state of the next oldest load
(if one exists)

For the basic LSQ, this amounts to
figuring out that the next oldest entry
in the LQ is the next oldest load...
-/

-- Start with:
-- transition of load that sends request,
-- and, the controller/entry details

-- Identify:
-- (a) where's the next load located?
-- (So we know where to send this request)
-- (b) the next load's state machine / controller
-- so we know where to make changes
-- and can analyze it and make changes
-- In Murphi we can kind of emulate reading a
-- wire of another structure by just reading
-- the structure's entry's field,
-- if we're just reading state.

/-
Thoughts after writing code for
item (1):

How do we really search for the controller
which contains the load ahead?

Can we examine where do we find 
load with seq_num < curr seq_num?

Or is it enough that we know this
LQ holds speculaitvley executing
loads and thus gives us enough
information?

This makes sense?
if the one structure holds loads
in FIFO, this is ok.

I suppose the main thing is still
to find the next load ahead.

i.e. Consider what happens for a
speculative load
-> Where is the load ahead

search for structures which
hold speculative in-flight loads!

determine what the set of sequence
numbers the loads can have are!

Given a FIFO, and that insertion is
in Program Order (PO), we know the next
load is basically the next entry
(how to show insertion order is in
PO???)

this is how we know to check the next entry!

=> And then handle the edge case,
of the head load, since the head load
is "correct"

-/

/-
SUMARY:
There are different cases.
one case is that all loads are speculatively executed 
in one structure,

But in general, any in-flight speculative load
that can begin to perform, would have a "guard" added
(or API call in our case)

And we check where the next speculatively executing load
is!
-> this can be somewhat involved
-> also consider the case where someone
defines a pipeline where we need to
add "scheduling" restrictions to the IQ,
so loads aren't scheduled out of order!

-> Main things to consider:
-> search for all speculatively executing load
controllers
-> check which one has the next speculatively executing load
--> is there some logic to follow to identify which
load will be the next oldest one?
--> considering the FIFO nature? If yes, then
check older entries
--> If no, must check by something that indicates
age, like seq_num or sth (better check the 4th LSQ for this)
--> Other question is, whether or not the LSQ contains
ONLY LOADS or other insts as well.
--> So should carry some flags or vars to help distinguish
between these cases
--> (or just read it out from the AST every time....)

--> If it only contains loads, then we do not need to
search this Structure
--> If it contains other insts, then we must "query"
  or search the structure.
  perhaps in constant time (with HW)

Key point:
This would work since we have some idea of the execution
flow.
Load insts will execute speculatively only in the queue.

Other cases to consider are ones where there's an exec
unit! rather than a queue.
The loads are scheduled to the exec unit, so
loads either need to be scheduled in-order for exec,
or use some other mechanism to enforce in order ld ld

i.e. in the algorithm we likely need to pass around
info about the structures,
Particularily if they speculatively execute all loads,
and how they're scheduled
Whether they're all scheduled in one buffer,
or if they're scheduled by another structure, (like an IQ)
and the executing structure has no control over the scheduling

* key consideration
i.e. whether or not schedule and "perform/execute"
  is coupled or not
-/

--======= Fns to get Controller Ordering info =====

-- def filter_lst_of_stmts_for_ordering_asn
-- (lst_stmts : List Statement)
-- :=
--   List.filter (
--     λ stmt => 
--       match stmt with
--       -- | Statement.variable_assignment qual_name expr =>
--       --   match qual_name with
--       --   | QualifiedName.mk lst_idents' =>
--       --     if (lst_idents'.contains "element_ordering")
--       --       then true
--       --       else false
--       | Statement.value_declaration typed_ident expr =>
--         match typed_ident with
--         | TypedIdentifier.mk tident ident =>
--           if (
--             or
--             (tident == "element_ordering")
--             (ident == "ordering")
--           )
--           then true
--           else false
--       | _ => false
        
--   )
--   lst_stmts

-- def get_val_decl_stmt_var
-- (stmt : Statement)
-- := 
--   match stmt with
--   | Statement.value_declaration typed_ident expr =>
--   -- | Statement.variable_assignment qual_name expr =>
--     match expr with
--     | Expr.some_term term =>
--       match term with
--       | Term.var ident =>
--         ident
--       | _ => dbg_trace "Error: unexpected Term"
--       default
--     | _ => dbg_trace "Error: unexpected Expr"
--       default
--   | _ => dbg_trace "Error: unexpected Stmt"
--     -- dbg_trace "BEGIN Stmt:\n"
--     -- dbg_trace stmt
--     -- dbg_trace "END Stmt:\n"
--     default

-- def get_ordering_from_ctrler_descript
-- (ctrler_descript : Description)
-- := 
--   match ctrler_descript with
--   | Description.controller ident stmt =>
--     match stmt with
--     | Statement.block lst_stmts =>
--       let ordering_stmt_lst := (
--         filter_lst_of_stmts_for_ordering_asn
--         lst_stmts
--       )
--       -- should only be 1 stmt for ordering
--       let ordering_stmt :=
--         match ordering_stmt_lst with
--         | [one_stmt] => one_stmt
--         | _ => dbg_trace "Error: unexpected List size?"
--           -- dbg_trace "List:\n"
--           -- dbg_trace ordering_stmt_lst
--           -- dbg_trace "List_stmts:\n"
--           -- dbg_trace lst_stmts
--           default
--       -- as an Identifier
--       let ordering_type :=
--         get_val_decl_stmt_var ordering_stmt

--       ordering_type
--     | _ => dbg_trace "Error: unexpected stmt in order search"
--       default
--   | _ => dbg_trace "Error: unexpected ctrler in order search"
--     default

-- def get_ctrler_elem_ordering
-- (ctrler : controller_info)
-- :=
--   let ctrler_description := ctrler.controller_descript
--   let ctrler_ordering :=
--     get_ordering_from_ctrler_descript (
--       ctrler_description
--     )
--   ctrler_ordering
      
--========= Disambiguating between Ctrler Types ======

def filter_lst_of_stmts_for_entry_type_or_types
(lst_stmts : List Statement)
:=
  List.filter (
    λ stmt => 
      match stmt with
      | Statement.variable_assignment qual_name expr =>
        match qual_name with
        | QualifiedName.mk lst_idents' =>
          if (lst_idents'.contains "entry_types")
            then true
            else false
      | _ => false
        
  )
  lst_stmts

-- gotta use partial :(
partial def match_expr_or_expr_lst_get_ident
(expr : Expr)
:=
  match expr with
  | Expr.some_term term =>
    match term with
    | Term.var ident =>
      [ident]
    | _ => dbg_trace "Error: unexpected Term in ident search"
    default
  | Expr.list lst_exprs =>
    let list_of_list_of_idents :=
      lst_exprs.map (
        match_expr_or_expr_lst_get_ident
      )
    let list_of_idents :=
      list_of_list_of_idents.join
    list_of_idents
  | _ => dbg_trace "Error: unexpected Expr in ident search"
    default

-- Get ident or ident list out of a
-- var assign statement
def get_expr_or_expr_list
(stmt : Statement)
:= 
  match stmt with
  | Statement.variable_assignment qual_name expr =>
    match_expr_or_expr_lst_get_ident expr
  | _ => dbg_trace "Error: unexpected stmt in expr search"
    default


def get_ctrler_entry_types
(ctrler : controller_info)
-- Should be fine without needing to
-- generalize and take fn args
-- (filter_lst_stmts_by : List Statement → List Statement)
-- (get_expr : Statement → Expr)
:= 
  let ctrler_entries :=
  match ctrler.controller_descript with
  | Description.controller ident stmt =>
    match stmt with
    | Statement.block lst_stmts =>
      let ordering_stmt_lst := (
        filter_lst_of_stmts_for_entry_type_or_types
        -- filter_lst_stmts_by
        lst_stmts
      )
      -- should only be 1 stmt for ordering
      let ordering_stmt :=
        match ordering_stmt_lst with
        | [one_stmt] => one_stmt
        | _ => dbg_trace "Error: unexpected stmt in ctrler entry types search"
          default
      -- as an Identifier
      let ordering_type :=
        get_expr_or_expr_list ordering_stmt
        -- get_expr ordering_stmt

      ordering_type
    | _ => dbg_trace "Error: unexpected stmt in ctrler entry type search"
      default
  | _ => dbg_trace "Error: unexpected descript in ctrler entry type search"
    default

  ctrler_entries

--======= helpers for analyze and transform =====
-- find mem access transition and update
-- with a stmt, otherwise return the
-- original transition
def is_stmt_mem_access_stmt
(stmt : Statement)
:=
  match stmt with
  | Statement.stray_expr expr' => 
    match expr' with
    | Expr.some_term term' =>
      match term' with
      | Term.function_call qual_name lst_expr =>
        match qual_name with
        | QualifiedName.mk lst_idents' =>
          if (lst_idents'.contains "memory_interface")
            then true
            else false
      | _ => false
    | _ => false
  | _ => false

def is_a_nested_stmt
(stmt : Statement)
:=
  match stmt with
  | Statement.listen_handle stmt lst =>
    true
  | Statement.conditional_stmt cond =>
    true
  | Statement.block lst_stmt =>
    true
  | Statement.await none lst_stmt1 =>
    true
  | Statement.when qname list_idens stmt =>
    true
  | _ => false


-- BEGIN A MUTUALLY RECURISVE SET OF FUNCTIONS
mutual

partial def find_stmts_lst_to_call_insert_recursively
(stmt_to_insert : Statement)
(stmt : Statement) : Statement
:=
  match stmt with
  | Statement.block lst_stmts =>
    Statement.block (insert_stmt_into_stmts_list stmt_to_insert lst_stmts)
  | Statement.conditional_stmt cond =>
    match cond with
    | Conditional.if_else_statement expr1 stmt1 stmt2 =>
      Statement.conditional_stmt
      (
        Conditional.if_else_statement
        expr1
        (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt1)
        (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt2)
      )
    | Conditional.if_statement expr1 stmt1 =>
      Statement.conditional_stmt
      (
        Conditional.if_statement
        expr1
        (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt1)
      )
  | Statement.await term lst_stmt1 =>
    Statement.await
    term
    (insert_stmt_into_stmts_list stmt_to_insert lst_stmt1)
  | Statement.when qname list_idens stmt =>
    Statement.when
    qname
    list_idens
    (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt)
  | Statement.listen_handle stmt lst =>
    Statement.listen_handle
    (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt)
    (
      lst.map
      (
        λ handl =>
        match handl with
        | HandleBlock.mk qname iden_list stmt1 =>
          HandleBlock.mk
          qname
          iden_list
          (find_stmts_lst_to_call_insert_recursively stmt_to_insert stmt1)
      )
    )
  | _ =>
    dbg_trace "SHOULDN'T GET HERE!"
    stmt

partial def insert_stmt_into_stmts_list
(stmt_to_insert : Statement)
(lst_stmts : List Statement)
: List Statement
:=
  dbg_trace "=== stmts list: =====\n"
  dbg_trace lst_stmts

  match lst_stmts with
  | [one_stmt] => 
    let is_mem_access :=
      is_stmt_mem_access_stmt one_stmt
    
    if is_mem_access
    then
      List.cons stmt_to_insert [one_stmt]
    else
      -- Else, check if this is a nested
      -- statement
      -- if it is, recursively call into it
      let is_nested :=
        is_a_nested_stmt one_stmt

      if is_nested
      then
        -- get to the lst of stmts
        -- and recurisvely call this fn
        -- and fill in the stmt's lst_of_stmts
        -- with the returned list
        let searched_and_replaced_nested_stmt :=
          (
            find_stmts_lst_to_call_insert_recursively
            stmt_to_insert
            one_stmt
          )
        [searched_and_replaced_nested_stmt]
      else
        [one_stmt]
  | h::t =>
    let is_head_mem_access
      := is_stmt_mem_access_stmt h
    
    if is_head_mem_access
    then
      List.cons stmt_to_insert (h::t)
    else
      -- Add a check here, similar to case above
      -- Check if head is a nested stmt to check
      let is_nested :=
        is_a_nested_stmt h

      if is_nested
      then
        -- get to the lst of stmts
        -- and recurisvely call this fn
        -- and fill in the stmt's lst_of_stmts
        -- with the returned list
        let searched_and_replaced_nested_stmt :=
          (
            find_stmts_lst_to_call_insert_recursively
            stmt_to_insert
            h
          )
        List.cons searched_and_replaced_nested_stmt (
          insert_stmt_into_stmts_list
          stmt_to_insert
          t
          )
      else
        List.cons h (
          insert_stmt_into_stmts_list
          stmt_to_insert
          t
          )

  | [] => []

-- END A MUTUALLY RECURSIVE SET OF FUNCTIONS
end

def return_transition_with_stmt_before_mem_access
-- a transition
-- (trans : Description)
-- (stmt_to_insert : Statement)
(trans_and_stmt_to_insert : Description × Statement)
:=
  let trans : Description := trans_and_stmt_to_insert.1
  let stmt_to_insert : Statement := trans_and_stmt_to_insert.2
  -- BEGIN setup.. get transition stmt block
  let ident_and_blk : Identifier × Statement
  :=
    match trans with
    | Description.state ident stmt =>
      (ident, stmt)
    | _ => dbg_trace "ERROR!"
      default

  let trans_ident := ident_and_blk.1
  let blk_stmt := ident_and_blk.2

  -- Also get the stmt block's lst of stmts
  let blk_stmt_lst : List Statement
  :=
    match blk_stmt with
    | Statement.block lst_stmts =>
      lst_stmts
    | _ => dbg_trace "ERROR!"
      default
  
  -- BEGIN code to search for the memory
  -- access and insert the stmt before it

  -- check if this transition has a mem access stmt
  let bool_lst :=
    true_if_stmts_have_mem_access blk_stmt
  
  let trans_has_mem_access
    := bool_lst.contains true

  if !trans_has_mem_access
  then 
    trans
  else
    -- start the replacement
    Description.state
    (trans_ident)
    (
      Statement.block
      (
        insert_stmt_into_stmts_list
        stmt_to_insert
        blk_stmt_lst
      )
    )

--======= Helper =======
def return_ctrler_with_updated_trans_list
(ctrler : controller_info)
(trans_lst : List Description) -- (Description.state)
: controller_info
:= {
  name                := ctrler.name,
  controller_descript := ctrler.controller_descript,
  entry_descript      := ctrler.entry_descript,
  init_trans          := ctrler.init_trans,
  state_vars          := ctrler.state_vars,
  transition_list     := trans_lst
}

--======== ANALYZE and TRANSFORM =======
-- Get info about the controller

def handle_load_perform_controller
(ctrler : controller_info)
:=
  -- handle load, disambiguate between
  -- different "LSQ" patterns

  let ctrler_ordering :=
    get_ctrler_elem_ordering ctrler


  -- is FIFO? (should be one identifier/string)
  -- (can't be multiple!? I think? well, technically yes)
  -- (but this describes insertion order!!!)
  let is_fifo :=
    ctrler_ordering == "FIFO"

  if is_fifo
  then
    let entry_types :=
      get_ctrler_entry_types ctrler

    let is_entry_only_load :=
      and
      (entry_types.contains "load")
      (entry_types.length == 1)
      
    -- this is kinda assumed...
    -- (assumed from litmus tests..)
    -- FUTURE TODO:
    -- but there's gotta be some way
    -- to check for this?
    -- perhaps by checking the transitions
    -- and what the awaits are on?
    let is_entry_load_speculation_unordered
      :=
      true

    -- does load performing ctrler
    -- only contain loads?
    if is_entry_only_load
    then
      -- case similar to henn/patt LSQ

      -- find the exec point in the
      -- ctrler transition, and
      -- add the API there to check
      -- the next elem

      -- Approach: use "map" with a
      -- fn on transitions
      -- fn that checks if a mem_access
      -- is there
      -- If it isn't, just return the transitions
      -- If it is, then add a stmt to do
      --    the check with the API()

      -- try to create the statement we'll add
      -- to the transition here?
      -- what should the statement(s) look like?
      -- We agree that this adds a stalling
      -- mechanism, like await/when
      -- uh oh.
      -- if it needs to wait until the next
      -- load reaches a received mem msg state,
      -- then we need that buffer to hold on to
      -- this msg some how
      -- This may conflict with squashes as well!
      -- So should we try to implement it as a
      -- 1. "send msg and wait for response"
      -- or 2. just a direct state check?
      -- For uarch a direct state check 2. would be
      -- more efficient and simpler
      -- This means there are some direct
      -- structure calls which can take multiple
      -- cycles while they're working or 'stalled'?
      -- But between 1. and 2. we already translate
      -- like 2. with await stmts

      -- store_queue.search
      -- (entry.phys_addr == phys_addr && key < seq_num)

      -- await entry.next().performed() { ... }

      -- well, the statement and the inserting the stmt
      -- are orthogonal things.
      -- perhaps the statement will come to me later

      -- for now (temporary):
      let stalling_stmt : Statement :=
        Statement.await 
          (
            -- Msg Fn this await uses
            Term.function_call (
              QualifiedName.mk
              ["entry", "next"]
            )
            []
          )
          (
            -- List of statements
            -- in this await block
            []
          )

      let transitions := ctrler.transition_list

      let num_transitions := transitions.length
      let replicated_stmt_lst :=
        List.replicate
        num_transitions
        stalling_stmt

      let trans_and_stmt :=
        List.zip
        transitions
        replicated_stmt_lst
      
      let guarded_trans
      :=
        trans_and_stmt.map
        return_transition_with_stmt_before_mem_access 

      let updated_ctrler :=
        return_ctrler_with_updated_trans_list
        ctrler
        guarded_trans

      updated_ctrler
    else
      -- case similar to the load-replay
      -- LSQ
      -- find the exec point in the
      -- ctrler transition, and
      -- add the API there to search
      -- for the next load in the queue!!
      --PLACEHOLDER!
      ctrler
      
  else
    -- let is_single_entry :=
    -- let is_exec_unit :=
    --PLACEHOLDER!
    ctrler

-- Can further disambiguate between
-- a "multiple load handler" (buffer/queue)
-- or just an "executing unit" (some load unit)
-- def handle_one_performing_load_ctrler
-- ()

-- TODO: Rename this to
-- "handle_one_executing_load_ctrler"
-- or just write a separate function?
def examine_load_perform_controllers
( lst_ctrlers : List controller_info)
:=
  -- there's only one load performing controller
  -- This conforms to the 3 LSQs we know of
  -- so far
  -- Technically after load-replay we add
  -- another load-perform site though... heh
  let only_one_load_performing_ctrler
    := lst_ctrlers.length == 1

  -- This is some buffer
  let ctrler_ordering
    :=
      lst_ctrlers.map get_ctrler_elem_ordering

  -- *** Thoughts
  -- Idea as of Monday July 25, 2022
  -- We should narrow down on the specific
  -- generic case.
  -- i.e. if the LSQ is either:
  -- (1) a FIFO ordered buffer,
  --   which lets loads perform speculatively
  --   (a) Either all entries are loads,
  --       thus we "know" where the next load is
  --   (b) Entries are mixed, must "search"
  --       for next oldest load
  -- (2) a single Load Execution Functional Unit
  --     This single unit 
  -- (3) NOT a FIFO ordered buffer structure
  --     This is a future case for the 4th LSQ

  -- So, should I separate these into cases in
  -- if statements?
  -- *** Thoughts
  
  -- Should also verify what is the
  -- "execution/perform order"?
  -- Not necessary? Thanks to the litmus tests?
  -- Maybe?

  -- But we know that the load entries
  -- are getting executed speculatively

  -- We know the entry state machines
  -- perform loads

  -- But we don't know if the entry state
  -- machines
  -- can exec in any order
  -- (But we can uh.. assume this for now..)
  -- (thanks to the litmus test.. (not really))
  -- (call this a "limitation")
  let ctrler_ordering_is_fifo :=
    ctrler_ordering.filter (
      λ ident =>
        if ident == "FIFO" 
          then true
          else false
    )
    
    -- match lst_ctrlers with
    --   | [a_ctrler] => (
    --       -- check if the stmt (block)
    --       -- has an num_entries of > 1 or
    --       -- buffer is a FIFO
    --       get_ctrler_elem_ordering a_ctrler
    --     )
    --   | [] => dbg_trace "Error: unexpected "
    --     default
    --   | h::t =>
    --   dbg_trace "TODO: Handle case of multiple Load Executors"
    --   default
    --   -- Future TODO: Get their ordering,
    --   -- AND which whether or not one is at commit!!
    --   -- It's okay if they're speculative or asych
    --   -- w.r.t eachother i assume?
    --   -- lst_ctrlers.map get_ctrler_elem_ordering

  -- Dummy return value so lean will typecheck!
  0
/-
(3)
Once we know where the "current" load and
next oldest load are,

we use our DSL API() to issue this
"access" and "await until next oldest load
reaches a received mem response state"
-/

-- Start with:
-- Entry info / controller info of both
-- a "current" preforming load and a load ahead
-- of it

-- What to do:
-- Add the right API
-- Something like
-- access_await(entry.seq_num == seq_num - 1 && state > completed_access)
