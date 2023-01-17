
import PipelineDsl.AST
import PipelineDsl.AnalysisHelpers
import PipelineDsl.CDFG

/-
1. Start by taking a DSL list of ctrlers.
Start from the "inst" source and "type" (load/store)
and see what the cdfg of all ctrler's state machines is.
-/

-- Open CDFG namespace, use the "nodes"
open CDFG

def DSLtoCDFG
(ctrlers : List controller_info)
: Except String (List CDFGNode)
:= do
/-
Take the DSL ctrler info objs, convert to Graph
1. For each ctrler
2. If the ctrler we're looking at is FIFO we add in FIFO info
3. For each state (except the init state) search it's stmts for
   -> transitions (also note the guards or API conditions used)
   -> message passing (also note the guards or API conditions used)
   and create edges for these

Do I need to iterate through states?
-- I don't think so, information is readily available from
-- the DSL ctrler and states
   
-/

  return []

-- TODO: Function to identify post-"receive" states
-- TODO: We can include a field in the CDFGNode type for
-- marking a state is a "receive state" or "send state"
