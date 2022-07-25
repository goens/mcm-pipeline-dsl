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

i.e. whether or not schedule and "perform/execute"
  is coupled or not
-/

def examine_load_perform_controllers
( lst_ctrlers : List controller_info)
:=
  -- there's only one load performing controller
  let only_one_load_performing_ctrler
  := lst_ctrlers.length == 1

  -- This is some buffer
  let ctrler_has_multiple_entries
  := match lst_ctrlers with
  | [a_ctrler] =>
    match a_ctrler.controller_descript with
    | Description.controller ident stmt =>
      -- check if the stmt (block)
      -- has an num_entries of > 1 or
      -- buffer is a FIFO

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