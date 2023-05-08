
import PipelineDsl.LoadReplayHelpers
import PipelineDsl.InstructionHelpers
import PipelineDsl.AST

-- === BEGIN Legacy code for translation ===
-- NOTE: This ControllerType is now more "legacy", use the CtrlerType now..
structure ControllerType where
name : String
deriving Inhabited, BEq

def FIFO : ControllerType := {name := "FIFO"}
def Unordered : ControllerType := {name := "Unordered"}

def IndexableCtrlerTypes : List ControllerType := [FIFO, Unordered]
def IndexableCtrlerTypesStrings : List String := IndexableCtrlerTypes.map (fun ctrler_type => ctrler_type.name )
-- === END Legacy code for translation ===


open Pipeline in
structure controller_info where
  -- Name, like LQ, SQ, SB, etc.
  name : Identifier
  -- The controller description, probably some info here...
  controller_descript : Description
  -- The entry description, probably some info here...
  entry_descript : Option Description
  -- The init transition
  init_trans : Option Identifier
  -- Entry vars, like seq_num, ld_seq_num, inst, read_value
  -- NOTE: leave for now, figure out tomorrow
  -- Or translate from the entry_descript
  state_vars : Option (List TypedIdentifier)
  -- list of transitions this structure takes
  -- should be: Description.transition
  transition_list : Option (List Description)
  -- ======== CTRLER State Machine STUFF ========
  ctrler_init_trans : Option Identifier
  ctrler_trans_list : Option (List Description)
  ctrler_state_vars : Option (List TypedIdentifier)
deriving Inhabited

instance : ToString controller_info := ⟨
  λ i =>
    "===controller===\n" ++
    "NAME: " ++ toString i.name ++ "\n" ++
    "CONTROLLER_DESCRIPTION: " ++ toString i.controller_descript ++ "\n" ++
    "ENTRY_DESCRIPT: " ++ toString i.entry_descript ++ "\n" ++
    "INIT_TRANS: " ++ toString i.init_trans ++ "\n" ++
    "STATE_VARS: " ++ toString i.state_vars ++ "\n" ++
    "TRANSITION_LIST: " ++ toString i.transition_list ++ "\n" ++
    s!"CTRLER_init_trans: ({i.ctrler_init_trans})\n" ++
    s!"CTRLER_state_vars: ({i.ctrler_state_vars})\n" ++
    s!"CTRLER_trans_list: ({i.ctrler_trans_list})\n" ++
    "\n=== End Controller ===\n\n"
  ⟩ 

inductive CtrlerType
| FIFO : CtrlerType
| Unordered : CtrlerType
| BasicCtrler : CtrlerType
deriving Inhabited, BEq

-- abbrev CtrlerType := ControllerType

def CtrlerType.toString : CtrlerType → String
| .FIFO => "FIFO Queue"
| .Unordered => "Unordered Queue"
| .BasicCtrler => "BasicCtrler"
instance : ToString CtrlerType where toString := CtrlerType.toString

def CtrlerType.is_a_queue : CtrlerType → Bool
| .BasicCtrler => false
| _ => true

inductive entry_or_ctrler
| entry : entry_or_ctrler
| ctrler : entry_or_ctrler
deriving Inhabited
def entry_or_ctrler.toString : entry_or_ctrler → String
| .entry => "Currently translating for an entry-type structure"
| .ctrler => "Currently translating for a ctrler-type structure"
instance : ToString entry_or_ctrler where toString := entry_or_ctrler.toString

abbrev Ctrlers := List controller_info
abbrev Ctrler := controller_info

-- == Translation Helpers ==

def Ctrler.entry_or_ctrler_translation
(dest_ctrler : Ctrler)
: Except String entry_or_ctrler := do
  if dest_ctrler.init_trans.isSome then do
    pure entry_or_ctrler.entry
  else if dest_ctrler.ctrler_init_trans.isSome then do
    pure entry_or_ctrler.ctrler
  else do
    throw s!"ERROR, ctrler doesn't have entry or ctrler transition info? ({dest_ctrler})"

open Pipeline in
def Ctrler.states (ctrler : Ctrler) : Except String (List Description)
:= do
  if let some entry_states := ctrler.transition_list then do
    if ctrler.init_trans.isSome then do
      pure entry_states
    else
      throw s!"ERROR, (malformed entry-ctrler) doesn't have init transition info? ({ctrler})"
  else if let some basic_states := ctrler.ctrler_trans_list then do
    if ctrler.ctrler_init_trans.isSome then do
      pure basic_states
    else do
      throw s!"ERROR, (malformed basic-ctrler) doesn't have init transition info? ({ctrler})"
  else do
    throw s!"ERROR, ctrler doesn't have states? ({ctrler})"


