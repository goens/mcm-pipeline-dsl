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
              λ stmt' =>
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
            )
            -- [2] Check if there's a
            -- memory access in the
            -- transitions of this
            -- controller

            -- Now check if [1] and [2]
            -- are both true!
            -- If so, then we can
            -- return true for this filter
    )
  0

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