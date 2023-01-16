
import PipelineDsl.AnalysisHelpers


-- cdfg 'data structure'
-- use for IR?

-- Build the CDFG
-- describe the CDFG "nodes" : state
/-
1. describe the nodes' contents
- transitions / resets / completions
--> Maybe this isn't necessary; Can write a fn to extract from stmts
- message sends
- variables
- state
- Any queue (FIFO) info of this entry
- ctrler this belongs to
-/

abbrev StateName := String


-- NOTE: Simple thing for now.
-- Better to encode relevant positions..
inductive FIFOPosition
| Tail : FIFOPosition
| Head : FIFOPosition
| HeadOrTail : FIFOPosition -- probably some nicer way to write this

-- Queue info
inductive QueueInfo
| FIFOQueue : FIFOPosition → QueueInfo
| None : QueueInfo

inductive CDFGNode
| mk: QueueInfo → 
