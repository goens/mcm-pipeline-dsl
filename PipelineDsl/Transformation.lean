import PipelineDsl.Translation

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
            | Description.transition ident stmt =>
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

def filter_lst_of_stmts_for_ordering_asn
(lst_stmts : List Statement)
:=
  List.filter (
    λ stmt => 
      match stmt with
      | Statement.variable_assignment qual_name expr =>
        match qual_name with
        | QualifiedName.mk lst_idents' =>
          if (lst_idents'.contains "element_ordering")
            then true
            else false
      | _ => false
        
  )
  lst_stmts

def get_assn_stmt_var
(stmt : Statement)
:= 
  match stmt with
  | Statement.variable_assignment qual_name expr =>
    match expr with
    | Expr.some_term term =>
      match term with
      | Term.var ident =>
        ident
      | _ => dbg_trace "error??"
      default
    | _ => dbg_trace "error??"
      default
  | _ => dbg_trace "error??"
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
        | _ => dbg_trace "error??"
          default
      -- as an Identifier
      let ordering_type :=
        get_assn_stmt_var ordering_stmt

      ordering_type
    | _ => dbg_trace "error??"
      default
  | _ => dbg_trace "error??"
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
    | _ => dbg_trace "error??"
    default
  | Expr.list lst_exprs =>
    let list_of_list_of_idents :=
      lst_exprs.map (
        match_expr_or_expr_lst_get_ident
      )
    let list_of_idents :=
      list_of_list_of_idents.join
    list_of_idents
  | _ => dbg_trace "error??"
    default

-- Get ident or ident list out of a
-- var assign statement
def get_expr_or_expr_list
(stmt : Statement)
:= 
  match stmt with
  | Statement.variable_assignment qual_name expr =>
    match_expr_or_expr_lst_get_ident expr
  | _ => dbg_trace "error??"
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
        | _ => dbg_trace "error??"
          default
      -- as an Identifier
      let ordering_type :=
        get_expr_or_expr_list ordering_stmt
        -- get_expr ordering_stmt

      ordering_type
    | _ => dbg_trace "error??"
      default
  | _ => dbg_trace "error??"
    default

  ctrler_entries


--========== Get info about the controller =======

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
      let is_entry_load_speculation_unordered
        :=
        true

      -- does load performing ctrler
      -- only contain loads?
      if is_entry_only load
        then
          -- case similar to henn/patt LSQ

          -- find the exec point in the
          -- ctrler transition, and
          -- add the API there to check
          -- the next elem
        else
          -- case similar to the load-replay
          -- LSQ
          -- find the exec point in the
          -- ctrler transition, and
          -- add the API there to search
          -- for the next load in the queue!!
      0
    else
      -- let is_single_entry :=
      -- let is_exec_unit :=
      0


  0

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
    --   | [] => dbg_trace "error??"
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