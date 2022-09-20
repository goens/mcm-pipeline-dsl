
import PipelineDsl.Murphi

open Murϕ in
structure MurphiFile where
filename : String
program : Murϕ.Program

-- Inst in Litmus Test
inductive InstType
| read : InstType
| write : InstType
deriving Inhabited

def load : InstType := InstType.read
def store : InstType := InstType.write

structure Inst where
inst_type : InstType
addr : Nat
write_val : Nat
dest_reg : Nat
deriving Inhabited

structure InstInCore where
inst : Inst
seq_num : Nat
queue_idx : Nat
deriving Inhabited

structure CoreInsts where
core_idx : Nat
insts : List InstInCore
deriving Inhabited

-- Litmus Test Info
inductive MCMOrdering where
| load_to_load : MCMOrdering
| load_to_store : MCMOrdering
| store_to_load : MCMOrdering
| store_to_store : MCMOrdering
-- There's probably also "(non) multi-copy atomic"
-- worry about it later when we reach it
deriving Inhabited

inductive Allowable where
| permitted : Allowable
| forbidden : Allowable
deriving Inhabited

-- structure AllowableOrderings where
-- ordering : MCMOrdering
-- allowable is determined by a given MCM, leaving it out.
-- allowable : Allowable

structure RegEntry where
reg_idx : Nat
reg_val : Nat
deriving Inhabited

structure CoreRegState where
core_idx : Nat
reg_entries : List RegEntry
deriving Inhabited

inductive ForbiddenOrRequired
| forbidden : ForbiddenOrRequired
| required : ForbiddenOrRequired
deriving Inhabited

structure ExpectedResult where
negate_or_not : ForbiddenOrRequired
per_core_reg_file : List CoreRegState
deriving Inhabited

-- A litmus test's structure
structure LitmusTest where
-- A name; a common litmus test or a custom one
test_name : String
-- list of instructions
-- inst: R or W? mem addr? write value?
-- what core is the inst on
insts_in_cores : List CoreInsts
-- The ordering it tests for
-- orderings : List AllowableOrderings
orderings : List MCMOrdering
-- Expected Result
expected : ExpectedResult
deriving Inhabited

-- ===== Helper Funcs =====
-- For EmitMurphi
open Murϕ in
def core_insts_to_emit_murphi_alias
(core_insts : CoreInsts)
: Murϕ.Statement
:=
  -- make rename_alias
  let core_idx := core_insts.core_idx
  let core_expr := Murϕ.Expr.integerConst core_idx

  let rename_alias_id := "rename_c" ++ (toString core_idx)

  let num_entries := toString core_insts.insts.length
  let list_inst_assignments : List Murϕ.Statement :=
  (List.join (core_insts.insts.map (
    fun inst =>
      let queue_idx : String := toString inst.queue_idx
      let op : String := match inst.inst.inst_type with
      | .read => "ld"
      | .write => "st"
      let seq_num : String := toString inst.seq_num
      let dest_reg : String := toString inst.inst.dest_reg
      let addr : String := toString inst.inst.addr
      let write_val : String := toString inst.inst.write_val

      [murϕ_statements|
        £rename_alias_id .test_insts[ £queue_idx ] .op := £op;
        £rename_alias_id .test_insts[ £queue_idx ] .seq_num := £seq_num;
        £rename_alias_id .test_insts[ £queue_idx ] .dest_reg := £dest_reg;
        £rename_alias_id .test_insts[ £queue_idx ] .imm := £addr; --# Addr
        £rename_alias_id .test_insts[ £queue_idx ] .write_value := £write_val;
      ]
  ))) ++ (
  [murϕ_statements|
  £rename_alias_id .rename_head := 0;
  £rename_alias_id .rename_tail := 0;
  £rename_alias_id .num_entries := £num_entries;
  ]
  )

  let rename_alias : Murϕ.Statement :=
  [murϕ_statement|
  alias £rename_alias_id : init_state .core_[ £core_expr ] .rename_ do
    £list_inst_assignments
  end
  ]

  rename_alias

open Murϕ in
def CoreRegState_to_emit_murphi_expr
(reg_state : CoreRegState)
: Murϕ.Expr
:=
  let core_idx : String := toString reg_state.core_idx

  let reg_val_check : List Murϕ.Expr :=
    reg_state.reg_entries.map ( fun reg_entry =>
      let reg_idx : String := toString reg_entry.reg_idx
      let reg_val : String := toString reg_entry.reg_val
      [murϕ_expr| Sta .core_[ £core_idx ] .rf_ .rf[ £reg_idx ] = £reg_val]
    )

  let and_exprs : Murϕ.Expr :=
  match reg_val_check with
  | [] => dbg_trace "There should be an expr?? THROW"
    panic! "Invariant has no tangible condition to check?"
  | [one] => one
  | h :: t =>
    t.foldl (fun expr1 expr2 => Murϕ.Expr.binop "&" expr1 expr2 ) h

  and_exprs

-- ===== Litmus Tests =====

-- ======  Basic 2 litmus tests, St [x] -> Ld [x], Ld -> Ld, St -> St ======
-- NOTE: Could also do a 1 core version of this as well
-- This also makes the trace shorter.
def iwp23b1 : LitmusTest := {
  test_name := "iwp23b1",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := ForbiddenOrRequired.required}
  orderings := [MCMOrdering.store_to_load]
}

def amd1 : LitmusTest := {
  test_name := "amd1",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := ForbiddenOrRequired.forbidden},
  orderings := [MCMOrdering.store_to_store, MCMOrdering.load_to_load]
}

-- ====== 
def amd2 : LitmusTest := {
  test_name := "amd2",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := ForbiddenOrRequired.forbidden},
  orderings := [MCMOrdering.store_to_store, MCMOrdering.load_to_load]
}

-- Do not use until I can generate tests which can check for an existing trace
-- But this test isn't that important..
-- The other amd tests are store-buffer type tests which allow for other cores
-- not to see stores, i.e. store - > load isn't enforced (i.e. TSO)
def amd3 : LitmusTest := {
  test_name := "amd3",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 0, write_val := 2, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 2, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 3, queue_idx := 2}
      ]}
  ],
  -- TODO NOTE: Should be a "permitted test". i.e. this result should be observable, but don't have a nice way
  -- to enforce this in Murphi
  -- Either there's a way to express "there exists an execution where this is true"
  -- or check with Nicolai if there's another way to express it in murphi..
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := ForbiddenOrRequired.forbidden},
  orderings := [MCMOrdering.store_to_store, MCMOrdering.load_to_load]
}

def ActiveLitmusTests : List LitmusTest := [
iwp23b1,
amd1
]
