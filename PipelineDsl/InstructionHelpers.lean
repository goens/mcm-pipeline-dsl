
-- import PipelineDsl.DSLHelpers

inductive MemoryAccess
| load : MemoryAccess
| store : MemoryAccess
deriving Inhabited, BEq

def MemoryAccess.toString : MemoryAccess → String
| .load => "Load"
| .store => "Store"

instance : ToString MemoryAccess where toString := MemoryAccess.toString

inductive MemoryOrdering
| mfence : MemoryOrdering
deriving Inhabited, BEq

def MemoryOrdering.toString : MemoryOrdering → String
| .mfence => "mfence"

instance : ToString MemoryOrdering where toString := MemoryOrdering.toString

inductive InstType
| memory_access : MemoryAccess → InstType
| memory_ordering : MemoryOrdering → InstType
deriving Inhabited, BEq


def load : InstType := InstType.memory_access MemoryAccess.load
def store : InstType := InstType.memory_access MemoryAccess.store
def mfence : InstType := InstType.memory_ordering MemoryOrdering.mfence

-- NOTE: Should make another type just to list the Litmus Test ordering.
-- Use these for now.
def load' : MemoryAccess := MemoryAccess.load
def store' : MemoryAccess := MemoryAccess.store
def mfence' : MemoryOrdering := MemoryOrdering.mfence

def InstType.toString : InstType → String
| .memory_access access =>
  access.toString
| .memory_ordering ordering =>
  ordering.toString
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

def Addresses.toString : Addresses → String
| .same => "Same-Address"
| .different => "Different-Address"
| .any => "Any-Address"

instance : ToString Addresses where toString := Addresses.toString

structure BinaryOrdering where
accesses₁ : List InstType
accesses₂ : List InstType
address : Addresses
deriving Inhabited, BEq

def BinaryOrdering.toString : BinaryOrdering → String
| ⟨access₁, access₂, address⟩ =>
  s!"Binary Ordering ({access₁}) -> ({access₂}) | ({address})"

instance : ToString BinaryOrdering where toString := BinaryOrdering.toString

structure TernaryOrdering where
αccesses₁ : List MemoryAccess
ordering₁ : MemoryOrdering
accesses₂ : List MemoryAccess
address : Addresses
deriving Inhabited, BEq

def TernaryOrdering.toString : TernaryOrdering → String
| ⟨access₁, ordering₁, access₂, address⟩ =>
  s!"Ternary Ordering ({access₁}) -> ({ordering₁}) -> ({access₂}) | ({address})"

instance : ToString TernaryOrdering where toString := TernaryOrdering.toString

inductive MCMOrdering where
| binary_ordering : BinaryOrdering → MCMOrdering
| ternary_ordering : TernaryOrdering → MCMOrdering 
deriving Inhabited, BEq

def MCMOrdering.toString : MCMOrdering → String
| .binary_ordering ordering
| .ternary_ordering ordering =>
  ordering.toString

instance : ToString MCMOrdering where toString := MCMOrdering.toString

def binary_ordering (first_inst : List InstType) (second_inst : List InstType) (address : Addresses) : MCMOrdering :=
  MCMOrdering.binary_ordering ⟨first_inst, second_inst, address⟩

def ternary_ordering (first_inst : List MemoryAccess) (second_inst : MemoryOrdering) (third_inst : List MemoryAccess) (address : Addresses) : MCMOrdering :=
  MCMOrdering.ternary_ordering ⟨first_inst, second_inst, third_inst, address⟩

def List.to_inst_type_list : List MemoryAccess → List InstType
| [] => []
| h::t => (InstType.memory_access h)::(List.to_inst_type_list t)
