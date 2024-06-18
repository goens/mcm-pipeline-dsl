
import PipelineDsl.ControllerHelpers
import PipelineDsl.InstructionHelpers

import PipelineDsl.CDFGInOrderTfsm
import PipelineDsl.CDFGInvalidationListener
import PipelineDsl.CDFGLoadReplay


inductive Transformation
| IO : Transformation
| LR : Transformation
| IT : Transformation

def Ctrlers.ApplyTransformation
( ctrlers : Ctrlers ) -- controllers
( tfsm : Transformation × MCMOrdering ) : Except String Ctrlers :=
  match tfsm with
  | ⟨.IO , mcm_ordering⟩ =>
    dbg_trace s!"++ In-Order: Adding ({mcm_ordering}) ordering."
    CDFG.InOrderTransform ctrlers mcm_ordering none
  | ⟨.LR , mcm_ordering⟩ =>
    dbg_trace s!"++ Load-Replay: Adding ({mcm_ordering}) ordering."
    Ctrlers.CDFGLoadReplayTfsm ctrlers mcm_ordering
  | ⟨.IT , mcm_ordering⟩ =>
    dbg_trace s!"++ Invalidation-Tracking: Adding ({mcm_ordering}) ordering."
    Ctrlers.AddInvalidationBasedLoadOrdering ctrlers

def Ctrlers.ApplyTransformations
( ctrlers : Ctrlers ) -- controllers
( tfsms : List (Transformation × MCMOrdering) ) : Except String Ctrlers :=
  -- Apply tfsms to ctrlers
  tfsms.foldlM
    Ctrlers.ApplyTransformation
    ctrlers
