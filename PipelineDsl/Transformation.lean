import PipelineDsl

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