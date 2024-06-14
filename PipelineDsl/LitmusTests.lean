import Murphi

import PipelineDsl.AnalysisHelpers
import PipelineDsl.InstructionHelpers

open Murϕ in
structure MurphiFile where
filename : String
program : Murϕ.Program

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
-- There's probably also "(non) multi-copy atomic"
-- worry about it later when we reach it

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

inductive TestResult where
| forbidden : TestResult
| required : TestResult
| permitted : TestResult
deriving Inhabited

structure ExpectedResult where
negate_or_not : TestResult
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
  let tail_idx := (toString (core_insts.insts.length)).append " % (CORE_INST_NUM + 1)"

  let list_inst_assignments : List Murϕ.Statement :=
  (List.join (core_insts.insts.map (
    fun inst =>
      let queue_idx : String := toString inst.queue_idx
      let op : String := inst.inst.inst_type.toMurphiString
      let seq_num : String := toString inst.seq_num
      let dest_reg : String := toString inst.inst.dest_reg
      let addr : String := toString inst.inst.addr
      let write_val : String := toString inst.inst.write_val

      [murϕ_statements|
        £rename_alias_id .entries[ £queue_idx ] .instruction .op := £op;
        £rename_alias_id .entries[ £queue_idx ] .instruction .seq_num := £seq_num;
        £rename_alias_id .entries[ £queue_idx ] .instruction .dest_reg := £dest_reg;
        £rename_alias_id .entries[ £queue_idx ] .instruction .imm := £addr; --# Addr
        £rename_alias_id .entries[ £queue_idx ] .instruction .write_value := £write_val;
      ]
  ))) ++ (
  [murϕ_statements|
  £rename_alias_id .head := 0;
  £rename_alias_id .tail := £tail_idx;
  £rename_alias_id .num_entries := £num_entries;
  ]
  )

  let rename_alias : Murϕ.Statement :=
  [murϕ_statement|
  alias £rename_alias_id : init_state .core_[ £core_expr ] .RENAME_ do
    £list_inst_assignments
  end
  ]

  rename_alias

open Murϕ in
def litmus_test_core_empty_murphi_expr
(litmus_test : LitmusTest)
(queue_ctrler_names : List Identifier)
: Murϕ.Expr
:=
  let core_idxs := List.range litmus_test.insts_in_cores.length

  let murphi_reset_cond_exprs := List.join <|
    core_idxs.map ( λ c_idx =>
      let c_idx_str := toString c_idx
      queue_ctrler_names.map ( λ q_cname =>
        let murphi_q_cname :=
          q_cname.append "_";
        [murϕ_expr|
          Sta .core_[ £c_idx_str ] . £murphi_q_cname .num_entries = 0
        ]
      )
    )
  let murphi_reset_cond_expr :=
    match murphi_reset_cond_exprs with
    | [lone_expr] => lone_expr
    | h :: t =>
      List.foldl (λ acc_m_expr m_expr =>
        Murϕ.Expr.binop
          "&" acc_m_expr m_expr
        )
        (h) (t)
    | [] =>
      -- Expecting at least one queue of instructions
      -- to be empty for this version of PipeGen/AQL.
      --throw s!"Error while generating Murphi reset condition, no queue structures?"
      dbg_trace s!"Error while generating Murphi reset condition, no queue structures?"
      default
  dbg_trace s!"@@01 murphi reset cond: ({murphi_reset_cond_expr})"
  murphi_reset_cond_expr

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
    negate_or_not := TestResult.required}
  orderings := [binary_ordering [ store ] [ load ] Addresses.same]
}

def amd1 : LitmusTest := {
  test_name := "amd1",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
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
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ load ] [ load ] Addresses.any), (binary_ordering [ store ] [ store ] Addresses.any)]
}

def amd1_dmb_sy : LitmusTest := {
  test_name := "amd1-dmb-sy",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_sy, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_sy, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [
    (ternary_ordering [ load' ] dmb_sy' [ load' ] Addresses.any),
    (ternary_ordering [ store' ] dmb_sy' [ store' ] Addresses.any)
  ]
}


def amd1_dmb_ld_st : LitmusTest := {
  test_name := "amd1-dmb-ld-st",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_ld, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_st, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [
    (ternary_ordering [ load' ] dmb_ld' [ load' ] Addresses.any),
    (ternary_ordering [ store' ] dmb_st' [ store' ] Addresses.any)
  ]
}

def amd1_one_ldar_stlr : LitmusTest := {
  test_name := "amd1-one-ldar-stlr",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := ldar, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := stlr, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ load ] [ load ] Addresses.any), (binary_ordering [ store ] [ store ] Addresses.any)]
}

def amd1_all_ldar_stlr : LitmusTest := {
  test_name := "amd1-all-ldar-stlr",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := ldar, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := ldar, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := stlr, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := stlr, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ load ] [ load ] Addresses.any), (binary_ordering [ store ] [ store ] Addresses.any)]
}

def amd1_relaxed_ldar_stlr : LitmusTest := {
  test_name := "amd1-relaxed-ldar-stlr",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := ldar, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := stlr, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    -- {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ load ] [ load ] Addresses.any), (binary_ordering [ store ] [ store ] Addresses.any)]
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
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [binary_ordering [ load ] [ store ] Addresses.any]
}

-- Do not use until I can generate tests which can check for an existing trace
-- But this test isn't that important..
-- The other amd tests are store-buffer type tests which allow for other cores
-- not to see stores, i.e. store - > load isn't enforced (i.e. TSO)
-- SO: I could just mark it as forbidden to see if it hits this case or not..
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
  -- Or we could just use an invariant to check if this is ever the case, and just say,
  -- 'this is observable!' if the invariant is triggered
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.permitted},
  orderings := [(binary_ordering [ store ] [ store ] Addresses.any), (binary_ordering [ store ] [ load ] Addresses.any)]
}

-- Definition n2
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "n2" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access W 1 1);
--     mkev 1 (mkiiid 0 1) (Access W 0 1);
--     mkev 2 (mkiiid 1 0) (Access W 0 2);
--     mkev 3 (mkiiid 1 1) (Access W 2 1);
--     mkev 4 (mkiiid 2 0) (Access R 0 1);
--     mkev 5 (mkiiid 2 1) (Access R 0 2);
--     mkev 6 (mkiiid 3 0) (Access R 2 1);
--     mkev 7 (mkiiid 3 1) (Access R 1 0)
--   ].

def n2 : LitmusTest := {
  test_name := "n2",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 2, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 2, write_val := 1, dest_reg := 0}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 2, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 3, insts := [
      {inst := {inst_type := load, addr := 2, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  -- TODO NOTE: Should be a "permitted test". i.e. this result should be observable, but don't have a nice way
  -- to enforce this in Murphi
  -- Either there's a way to express "there exists an execution where this is true"
  -- or check with Nicolai if there's another way to express it in murphi..
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 2, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 2}]},
    {core_idx := 3, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.permitted},
  orderings := [(binary_ordering [ load ] [ load ] Addresses.same), (binary_ordering [ load ] [ load ] Addresses.any), (binary_ordering [ store ] [ store ] Addresses.any)]
}

-- Definition n4
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "n4" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access R 0 2);
--     mkev 1 (mkiiid 0 1) (Access W 0 1);
--     mkev 2 (mkiiid 0 2) (Access R 0 1);
--     mkev 3 (mkiiid 1 0) (Access R 0 1);
--     mkev 4 (mkiiid 1 1) (Access W 0 2);
--     mkev 5 (mkiiid 1 2) (Access R 0 2)
--   ].

-- This shouldn't happen with a SB
-- This *might* happen if stores are speculative + they can just write to a speculative cache
-- but if this is the case then the read also should be able to read from the addr 0 immediately
def n4 : LitmusTest := {
  test_name := "n4",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 1}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 2}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 0, write_val := 2, dest_reg := 1}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 2}, seq_num := 3, queue_idx := 2}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 2}, {reg_idx := 1, reg_val := 0}, {reg_idx := 2, reg_val := 1}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}, {reg_idx := 2, reg_val := 2}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ store ] [ store ] Addresses.same), (binary_ordering [ load ] [ load ] Addresses.same)]
}

-- Definition n5
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "n5" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access W 0 1);
--     mkev 1 (mkiiid 0 1) (Access R 0 2);
--     mkev 2 (mkiiid 1 0) (Access W 0 2);
--     mkev 3 (mkiiid 1 1) (Access R 0 1)
--   ].

def n5 : LitmusTest := {
  test_name := "n5",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 2, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 2}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 1}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ store ] [ load ] Addresses.same)]
}

def n7 : LitmusTest := {
  test_name := "n7",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
    ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0}
    ]},
    {
      core_idx := 2,
      insts := [
        {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
        {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]
    }
  ],
  expected := {
    per_core_reg_file := [
      {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]},
      {core_idx := 2, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden}, -- Allow, Permitted in TSO
  orderings := [(binary_ordering [ store ] [ load ] Addresses.same)]
}

-- (** Ensure that po-loc is respected *)
-- Definition d1
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "d1" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access W 0 1);
--     mkev 1 (mkiiid 0 1) (Access W 0 2);
--     mkev 2 (mkiiid 0 2) (Access R 0 1)
--   ].

-- (** Same-address version of amd1/iwp2.1 *)
-- Definition d3
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "d3" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access W 0 1);
--     mkev 1 (mkiiid 0 1) (Access W 0 2);
--     mkev 2 (mkiiid 1 0) (Access R 0 2);
--     mkev 3 (mkiiid 1 1) (Access R 0 1)
--   ].

-- (** Ensure that po-loc is respected *)
-- Definition d4
--   (pipeline : Pipeline)
--   : list (string * string) :=
--   LitmusTest "d4" Forbidden pipeline [
--     mkev 0 (mkiiid 0 0) (Access W 0 1);
--     mkev 1 (mkiiid 0 1) (Access R 0 1);
--     mkev 2 (mkiiid 0 2) (Access R 0 0)
--   ].

def Dekker : LitmusTest := {
  test_name := "Dekker",
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 2, queue_idx := 1}
      ]}
  ],
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden},
  orderings := [(binary_ordering [ store ] [ load ] Addresses.any)]
}

-- =========================== Fence Litmus Tests ===============================

-- typical ld -> ld but with fence
def load_fence : LitmusTest := {
  test_name := "load-fence"
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := mfence, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 2, queue_idx := 1}
      ]}
  ]
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden}
  orderings := [/- (ternary_ordering load mfence load Addresses.any),-/
    (ternary_ordering [ load' ] mfence' [ load' ] Addresses.any),
    (binary_ordering [ store ] [ store ] Addresses.any)]
}

def load_fence_store_fence : LitmusTest := {
  test_name := "load-fence-store-fence"
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := mfence, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := mfence, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 3, queue_idx := 2}
      ]}
  ]
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 1}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden}
  orderings := [/- (ternary_ordering load mfence load Addresses.any),-/
    (ternary_ordering [ load' ] mfence' [ load' ] Addresses.any),
    (ternary_ordering [ store' ] mfence' [ store' ] Addresses.any)]
}

def dekker_fence : LitmusTest := {
  test_name := "dekker-fence"
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := mfence, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := mfence, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]}
  ]
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden}
  orderings := [/- (ternary_ordering load mfence load Addresses.any),-/
      (ternary_ordering [ store' ] mfence' [ load' ] Addresses.any)
    ]
}

def dekker_dmb_sy : LitmusTest := {
  test_name := "dekker-dmb-sy"
  insts_in_cores := [
    {core_idx := 0, insts := [
      {inst := {inst_type := store, addr := 0, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_sy, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 1, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]},
    {core_idx := 1, insts := [
      {inst := {inst_type := store, addr := 1, write_val := 1, dest_reg := 0}, seq_num := 1, queue_idx := 0},
      {inst := {inst_type := dmb_sy, addr := 0, write_val := 0, dest_reg := 0}, seq_num := 2, queue_idx := 1},
      {inst := {inst_type := load, addr := 0, write_val := 0, dest_reg := 1}, seq_num := 3, queue_idx := 2}
      ]}
  ]
  expected := {
    per_core_reg_file := [
    {core_idx := 0, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]},
    {core_idx := 1, reg_entries := [{reg_idx := 0, reg_val := 0}, {reg_idx := 1, reg_val := 0}]}
    ],
    negate_or_not := TestResult.forbidden}
  orderings := [/- (ternary_ordering load mfence load Addresses.any),-/
      (ternary_ordering [ store' ] dmb_sy' [ load' ] Addresses.any)
    ]
}

-- TODO: Add in the extra ones for fully testing
-- load/store ordering with memory instructions, for
-- artifact evaluation

def ActiveLitmusTests : List LitmusTest := [
iwp23b1, -- should pass, is for single core correctness
amd1,
amd1_dmb_sy,
amd1_dmb_ld_st,
amd1_one_ldar_stlr,
amd1_all_ldar_stlr,
amd1_relaxed_ldar_stlr,
amd2,
-- n2, -- permitted test, not yet implemented "permitted"
n4,
n5,
n7,
Dekker,
load_fence,
dekker_fence,
dekker_dmb_sy,
load_fence_store_fence
]
