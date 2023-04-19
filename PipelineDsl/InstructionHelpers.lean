
import PipelineDsl.DSLHelpers

inductive MemoryAccess
| load : MemoryAccess
| store : MemoryAccess
deriving Inhabited, BEq

inductive MemoryOrdering
| mfence : MemoryOrdering
deriving Inhabited, BEq

inductive InstType
| memory_access : MemoryAccess → InstType
| memory_ordering : MemoryOrdering → InstType
deriving Inhabited, BEq

def memory_interface : CtrlerName := "memory_interface"
def load_completed : MsgName := "load_completed"
def load_perform : MsgName := "send_load_request"
def store_completed : MsgName := "store_completed"
def store_perform : MsgName := "send_store_request"

-- NOTE: the "name" of the load value from the load response api
def load_value : String := "load_value"

def InstType.completion_msg_name : InstType → Except String MsgName
| .memory_access access => do
  match access with
  | .load => do pure load_completed
  | .store => do pure store_completed
| .memory_ordering ordering => do
  match ordering with
  | .mfence => do throw "Error: mFence has no awaited completion message"

def InstType.perform_msg_name : InstType → Except String MsgName
| .memory_access access => do
  match access with
  | .load => do pure load_perform
  | .store => do pure store_perform
| .memory_ordering ordering => do
  match ordering with
  | .mfence => do throw "Error: mFence has no perform message"

def load : InstType := InstType.memory_access MemoryAccess.load
def store : InstType := InstType.memory_access MemoryAccess.load
def mfence : InstType := InstType.memory_ordering MemoryOrdering.mfence

-- NOTE: Should make another type just to list the Litmus Test ordering.
-- Use these for now.
def load' : MemoryAccess := MemoryAccess.load
def store' : MemoryAccess := MemoryAccess.load
def mfence' : MemoryOrdering := MemoryOrdering.mfence

def InstType.toString : InstType → String
| .memory_access access =>
  match access with
  | .load => "Load"
  | .store => "Store"  
| .memory_ordering ordering =>
  match ordering with
  | .mfence => "mfence"
instance : ToString InstType where toString := InstType.toString

def InstType.toMurphiString : InstType → String
| .memory_access access =>
  match access with
  | .load => "ld"
  | .store => "st"  
| .memory_ordering ordering =>
  match ordering with
  | .mfence => "mfence"

inductive Addresses where
| same : Addresses -- ex. ld[x] -> ld[x]
| different : Addresses -- ex. ld[x] -> ld[y]
| any : Addresses -- ex. ld[x] -> ld[y] and ld[x] -> ld[x]
deriving Inhabited, BEq

structure BinaryOrdering where
access₁ : MemoryAccess
access₂ : MemoryAccess
address : Addresses
deriving Inhabited, BEq

structure TernaryOrdering where
αccess₁ : MemoryAccess
ordering₁ : MemoryOrdering
access₂ : MemoryAccess
address : Addresses
deriving Inhabited, BEq

inductive MCMOrdering where
| binary_ordering : BinaryOrdering → MCMOrdering
| ternary_ordering : TernaryOrdering → MCMOrdering 

def binary_ordering (first_inst : MemoryAccess) (second_inst : MemoryAccess) (address : Addresses) : MCMOrdering :=
  MCMOrdering.binary_ordering ⟨first_inst, second_inst, address⟩

def ternary_ordering (first_inst : MemoryAccess) (second_inst : MemoryOrdering) (third_inst : MemoryAccess) (address : Addresses) : MCMOrdering :=
  MCMOrdering.ternary_ordering ⟨first_inst, second_inst, third_inst, address⟩