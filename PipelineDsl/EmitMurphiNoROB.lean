import PipelineDsl.AnalysisHelpers

import PipelineDsl.Murphi

import PipelineDsl.AST

import PipelineDsl.LitmusTests

import PipelineDsl.Translation

import PipelineDsl.EmitMurphi

set_option maxHeartbeats 500000
open Pipeline
open Murϕ

-- ===== Overall func to build a murphi file =====
/-
Parts we need:
- 1. const
Define some default constants, and any generated ctrler constants

- 2. type
Define some default types/TypeExprs, and generated ctrler constants

- 3. var
Just Sta (the state variable)

4. functions, which doesn't have a section heading

5. rulesets, which also doesn't have a section heading

6. invariants, which also doesn't have a section heading
-/

-- comment this out for now, to make the
-- lean4 interpretation faster...
def compose_murphi_file_components_but_no_ROB
-- Consts, like num entries per buffer-type ctrler
( const_decls : List Murϕ.Decl)
-- Types, like ctrler defns
( type_decls : List Murϕ.Decl)
( func_decls : List Murϕ.ProcDecl)
( rules : List Murϕ.Rule)
( ctrler_list : List controller_info )
( litmus_test : LitmusTest )
: Murϕ.Program
:=
  let core_count : String := toString ( litmus_test.insts_in_cores.length - 1 ) --1
  let max_insts : String := toString ( ( litmus_test.insts_in_cores.map (fun core => core.insts.length) ).maximum?.get! - 1)
  let max_regs : String := max_insts;
  let max_val : String := toString (
    (List.join (
      ( litmus_test.insts_in_cores.map (fun core => core.insts.map (fun inst_ => inst_.inst.write_val)) )
    )).maximum?.get!
  )
  let max_addr : String := toString (
    (List.join (
      ( litmus_test.insts_in_cores.map (fun core => core.insts.map (fun inst_ => inst_.inst.addr)) )
    )).maximum?.get!
  )

  let list_const_decls := [murϕ_const_decls|
  const ---- Configuration parameters ----
  -- LQ_NUM_ENTRIES_ENUM_CONST : 1;
  -- SQ_NUM_ENTRIES_ENUM_CONST : 1;
  -- SB_NUM_ENTRIES_ENUM_CONST : 1;
  -- DATA_NUM : 2;
  CORE_INST_NUM : £max_insts;

  IC_ENTRY_NUM : 1; -- set to 1 for letting 2 in-flight mem ops in the overall system
  --# 2 cores..
  CORE_NUM : £core_count; -- Tests could be up to 4 cores generally?

  --# model a simple atomic memory
  ADDR_NUM : £max_addr; -- use 1, to represent 2 addrs (0 and 1)
  --# num of reg file entries
  REG_NUM : £max_regs; -- Just set to num insts...
  --# max value
  MAX_VALUE : £max_val; -- 2 also works..
  -- £const_decls;
  ] ++ const_decls

  -- Gen the decls for the ctrlers in the CORE type
  let ctrler_names : List String := ctrler_list.map (
    λ ctrler => ctrler.name
  )

  -- make the murphi decls
  let ctrler_type_decl : List Murϕ.Decl := ctrler_names.map (
    λ name =>
      Murϕ.Decl.type (name.append "_") (Murϕ.TypeExpr.previouslyDefined name)
  )

  -- AZ TODO: Build the CORE
  -- half-manually,
  -- i.e. synth the ctrler names..
  let core_and_state_type_decl :=
  [murϕ_type_decls|
  type

  CORE : record
  £ctrler_type_decl;
  -- lSQ_ : LSQ;
  -- SB_ : SB;
  rename_ : RENAME;
  iq_ : IQ;
  rf_ : REG_FILE;
  -- rob_ : ROB;
  mem_interface_ : MEM_INTERFACE;
  end;

  STATE : record
  core_ : array [cores_t] of CORE;
  mem_ : MEM_ARRAY;
  -- other things like message channels could go here
  ic_ : IC;
  --# Pretending this is
  --# a single port mem interface
  --# for each core..

  --#mem_interface_ : array [cores_t] of MEM_INTERFACE;
  end;
  ]

  let list_type_decls := [murϕ_type_decls|
type ---- Type declarations ----

  --# for value types
  val_t : 0 .. MAX_VALUE;

  --# GEN NOTE: have index & count types
  inst_idx_t : 0 .. CORE_INST_NUM;
  inst_count_t : 0 .. (CORE_INST_NUM + 1);
  --# NOTE: could define seq_num_t type later
  --# for more arbitrary seq_num lengths

  --# For memory array
  addr_idx_t : 0 .. ADDR_NUM;

  --# for reg file regs
  reg_idx_t : 0 .. REG_NUM;

  -- SQ_idx_t : 0 .. SQ_NUM_ENTRIES_ENUM_CONST;
  -- SQ_count_t : 0 .. (SQ_NUM_ENTRIES_CONST);

  -- SB_idx_t : 0 .. SB_NUM_ENTRIES_ENUM_CONST;
  -- SB_count_t : 0 .. (SB_NUM_ENTRIES_CONST);

  ic_idx_t : 0 .. IC_ENTRY_NUM;
  ic_count_t : 0 .. (IC_ENTRY_NUM + 1);

  cores_t : 0 .. CORE_NUM;

  MSG_DEST : enum {core, mem};

  IQ_MAX_INSTS : inst_idx_t;

  -- insts are either load or stores
  INST_TYPE : enum {ld, st, inval};
  ADDR_TYPE : enum {addr_reg, addr_imm};
  VAL_TYPE : enum {val_reg, val_imm};

  --# No polymorphism. just
  --# hack it in
  --# Might cause some potential "confusion"
  --# with fields being set and such,
  --# but should be ok for now...
  INST : record
  --# Inst Type Info
  --# Ld, st, uses immediate, etc.
  op : INST_TYPE;
  addr_type : ADDR_TYPE;

  --# Seq num. not strictly a part of
  --# an inst. TODO should move this out
  --# to the buffers..?
  --# Would make sense to use a separate
  --# Type as well...
  seq_num : inst_count_t; --#seq_num_t;

  --# A dest_reg. Only used by loads
  dest_reg : reg_idx_t;

  --# A src_reg. Src Addr
  src_reg1 : reg_idx_t;
  --# A src_reg. Src Val for St
  src_reg2 : reg_idx_t;

  --# Imm val. For now, only used
  --# as Address for ld / st
  --# Could be used with st
  --# as write value as well

  --# TODO figure out the imm stuff..?
  --# imm_addr : addr_idx_t;
  --# imm_val : val_t;
  imm : val_t;

  --# Hacked in for litmus tests
  --# Should move out
  write_value : val_t;
  end;

  £type_decls;
  -- just a 'dumb' copy of the state vars

  ---------------------- Rename ----------------------
  -- # Rename inserts insts into LSQ #(LQ for now)
  -- # So init it with a list of insts to feed into system
  -- # this will deadlock,
  -- # can it assert properties even with deadlock?
  -- # Yes, NOTE that if a rule is not violated it doesn't
  -- # print a message
  -- # Some rules may not fire as well
  -- # In any case it's not too important,
  -- # we can model an endless rename inst generator
  -- # if needed

  -- start with modeling the queue of "test" insts
  RENAME : record
  test_insts : array [inst_idx_t] of INST;
  rename_head : inst_idx_t;
  rename_tail : inst_idx_t;
  num_entries : inst_count_t;
  end;
  -- Then it's transitions
  -- move an inst into the LSQ (and IQ)
  -- Pre-condition:
  -- None (conditionless rule)
  -- Transition:
  -- Allocate inst info in IQ/LSQ

  -- may be worth using for rename and IQ
  ------- Generic Instruction Buffer -----
  --#IB : record
  --#ib_insts : array [inst_idx_t] of INST;
  --#ib_head : inst_idx_t;
  --#ib_tail : inst_idx_t;
  --#num_entries : inst_count_t;
  --#end;

  IQ_STATE : enum{ valid, invalid, ready };

  ---------------------- IQ --------------
  IQ : record
  -- # Instruction info
  iq_insts : array [inst_idx_t] of INST;
  -- # If entry in queue is valid
  iq_valid : array [inst_idx_t] of IQ_STATE;
  -- # iq_head : inst_idx_t;
  -- # iq_tail : inst_idx_t;
  -- # NOTE: just num entries, search for
  -- # an empty (invalid) entry to insert into
  -- # NOTE: this is to support scheduling
  -- # and removing arbitrary insts from the IQ
  num_entries : inst_count_t;
  end;

  ---------------------- ROB --------------
  -- ROB also has entry values
  -- include MSG slot per entry
  -- must identify where to send MSG
  -- (which entry)
  -- Also need listener to check when
  -- MSG is there & act accordingly
  -- (1)
  -- pre-cond:
  -- So additional transition rule
  -- pred on MSG slot available &
  -- current state is a listen state
  -- transition action:
  -- listen state gets MSG, does MSG action
  -- in handle block

  -- ROB_STATE : enum {commit_not_sent, commit_sig_sent};

  -- ROB: record
  -- rob_insts : array [inst_idx_t] of INST;
  -- rob_head : inst_idx_t;
  -- rob_tail : inst_idx_t;
  -- -- do we also include is_executed state?
  -- num_entries : inst_count_t;

  -- is_executed : array [inst_idx_t] of boolean;

  -- -- state : array [inst_idx_t] of ROB_STATE;
  -- end;

  ---------------------- mem interface --------------
  -- Memory interface
  -- send messages to mem interface
  -- know when MSG slot is occupied
  -- SO:
  -- (1) addtl transition rule:
  -- pre-cond:
  -- loads in a sending state
  -- & MSG slot empty
  -- transition action:
  -- a load will use the MSG slot

  -- # TODO: Continue to model LQ, make request to Memory
  -- # Also need to model memory
  -- # Make simple memory requests
  -- # So need a simple 2 or 3 addr memory
  -- # and load/store requests happen immediately
  -- record of data at addrs

  --# Read or Write? enum
  R_W : enum {read, write};

  --# Message to or from mem
  MEM_REQ : record
  addr : addr_idx_t;
  r_w : R_W;
  --# Either write val or read val
  --# depends on r_w & to/from mem
  value : val_t;
  valid : boolean;

  --# Destination of msg
  dest : MSG_DEST;
  dest_id : cores_t;

  --# seq_num in core
  seq_num : inst_count_t;
  end;

  MEM_ARRAY : record
  arr : array [addr_idx_t] of val_t;
  --# don't need to model this...
  --#msg : MEM_REQ;
  end;

  MEM_INTERFACE : record
  out_msg : MEM_REQ;
  in_msg : MEM_REQ;

  out_busy : boolean;
  in_busy : boolean;
  end;

  ------------------ Re-ordering Interconnect --------------
  IC : record
  --# IC will have a buffer...
  --# Idea: let the IC send packets
  --# in an arbitrary order.
  --# Emulate with a buffer of N msgs
  --# Any element that's valid can be
  --# forwarded
  buffer : array [ic_idx_t] of MEM_REQ;
  valid : array [ic_idx_t] of boolean;
  num_entries : ic_count_t;

  --# Just need 1 buffer
  --# core -> IC
  --# IC -> mem access
  --# change msg dest to core
  --# IC -> core

  --# alternate implementation:
  --# mem eats msg and sends back a msg
  --# Then we need a full duplex channel,
  --# i.e. 2 buffers, 1 to mem, 1 to core
  end;

  REG_FILE: record
  rf : array [reg_idx_t] of val_t;
  end;

  ---------------------- SQ ----------------------
  -- ---------------------- SB ----------------------
  ] ++ core_and_state_type_decl

  let list_var_decls : List Murϕ.Decl := [murϕ_var_decls|
  var -- state vars - explicit overall state --

  Sta :STATE
  ]

  let list_rename_init_insts : List Murϕ.Statement :=
  litmus_test.insts_in_cores.map core_insts_to_emit_murphi_alias

  let expected_core_reg_file_states : List Murϕ.Expr :=
  litmus_test.expected.per_core_reg_file.map CoreRegState_to_emit_murphi_expr
  let all_reg_file_states : Murϕ.Expr :=
  match expected_core_reg_file_states with
  | [] => dbg_trace "An invariant with no condition?? THROW"
    panic! "Invariant with no condition!"
  | [one] => one
  | h::t =>
    t.foldl (fun expr1 expr2 => Murϕ.Expr.binop "&" expr1 expr2) h

  let cond_to_check : Murϕ.Expr :=
  match litmus_test.expected.negate_or_not with
  | ForbiddenOrRequired.forbidden => Murϕ.Expr.negation all_reg_file_states
  | ForbiddenOrRequired.required => all_reg_file_states

  let empty_core_exprs : Murϕ.Expr := litmus_test_core_empty_murphi_expr litmus_test

  -- litmus_test
  let the_test_name := litmus_test.test_name
  let ordering_invariant_expr : Murϕ.Expr :=
  [murϕ_expr|
    (
      (£empty_core_exprs)
      ->
      (£cond_to_check)
    )
  ]
  let ordering_invariant : Murϕ.Rule :=
  Murϕ.Rule.invariant the_test_name ordering_invariant_expr

-- # ------------ HELPER FUNCTIONS --------------------
  let list_func_decls := List.join ([
  [murϕ_proc_decl|
  function search_rob_seq_num_idx(
    rob : ROB;
    seq_num : inst_count_t;
    ) : inst_idx_t;
    --#var rob_new : ROB;
    --#var rob_idx : inst_idx_t;
    begin
    
    for i : inst_idx_t do
      if (rob .entries[i] .instruction .seq_num = seq_num) then
        return i;
      end;
    end;
    error "ROB Search: didn't find it? how? bad seq_num idx?";
  end
  ],
  [murϕ_proc_decl|
  function rename_read_head( rename_q : RENAME) : INST;
    return rename_q .test_insts[rename_q .rename_head];
  end
  ],
  [murϕ_proc_decl|
  function rename_pop_head(
             -- head and tail
             -- head : inst_idx_t;
             -- tail : inst_idx_t;
             -- the array
                    rename_queue : RENAME
           ) : RENAME;
  var rename_q : RENAME;
begin
  rename_q := rename_queue;

  rename_q .rename_head := ( rename_queue .rename_head + 1 ) % (CORE_INST_NUM + 1);
  rename_q .num_entries := rename_queue .num_entries - 1;
  -- assert num_entries not less than 0?
  -- assert ( rename_q .num_entries > 0 ) "can't have neg entries";

  -- use this to overwrite the old one
  -- (immutable style)
  return rename_q;
end
  ],

[murϕ_proc_decl|
function lq_insert(
             lq : LQ;
             sq : SQ;
             inst : INST;
           ) : LQ;
  var lq_new : LQ;
  var lq_tail : LQ_idx_t;

  --# ADDED NOTE
  --#var sq : SQ;
  var curr_tail_entry : LQ_entry_values;
begin
  --
  lq_new := lq;
  curr_tail_entry := lq_new .entries[lq .tail];

  assert curr_tail_entry .state = await_creation "to insert, load should be awaiting creation";
  curr_tail_entry .state := await_scheduled;
  --# AZ TODO: do the store_queue check

  assert (curr_tail_entry .st_seq_num = 0) "should first be 0?";
  if (sq .num_entries != 0) then
    --#NOTE: REMEMBER TO CLEAR ST SEQ NUM
    --# at the end...
    curr_tail_entry .st_seq_num := sq .entries[sq .head] .instruction .seq_num;
  else
    --# Keep at none
    --# 0 is "none" here...
    curr_tail_entry .st_seq_num := 0;
  end;

  --# NOTE: Auto generate the standard "insert" part
  curr_tail_entry .instruction := inst;
  lq_new .tail := ( lq .tail + 1 ) % (LQ_NUM_ENTRIES_CONST);
  lq_new .num_entries := lq .num_entries + 1;
  --
  --# NOTE: assert, but not technically required, since
  --# if it's out of the range, Murphi throws an error
  assert (lq .num_entries < ( LQ_NUM_ENTRIES_CONST)) "can't add more!";

  lq_new .entries[lq .tail] := curr_tail_entry;

  return lq_new;
end
],
[murϕ_proc_decl|
function lq_schedule(
             lq : LQ;
             seq_num : inst_count_t; --#seq_num_t;--inst_idx_t;
           ) : LQ;
  var lq_new : LQ;
  var lq_iter : LQ_idx_t;
  var lq_count : LQ_count_t;
  var curr_entry : LQ_entry_values;
  var curr_entry_id : LQ_idx_t;
  begin
  --
  lq_new := lq;
  lq_iter := lq .head;
  lq_count := lq .num_entries;

  for i:0 .. LQ_NUM_ENTRIES_ENUM_CONST do
    -- error "trace load schedule?";
    -- Use i

    curr_entry_id := ( lq_iter + i ) % ( LQ_NUM_ENTRIES_CONST);
    curr_entry := lq_new .entries[curr_entry_id];
    if (curr_entry .instruction .seq_num = seq_num)
      then
      -- # put "\n ================ \n";
      -- # put "seq_num: ";
      -- # put seq_num;
      -- # put "\n";
      -- # put "\n ================ \n";
      assert ( curr_entry .state = await_scheduled ) "Should be in await scheduled?";
      curr_entry .state := await_translation;

      --# NOTE: For mem access stuff
      curr_entry .virt_addr := curr_entry .instruction .imm;

      lq_new .entries[curr_entry_id] := curr_entry;
      -- error "trace load schedule?";
      return lq_new;
    end;
  end;
  --
  error "didn't find the Load to Schedule?";
  return lq_new;
end
],
[murϕ_proc_decl|
function iq_insert(
             iq : IQ;
             inst : INST;
           ) : IQ;
  var iq_new : IQ;
  var iq_tail : inst_idx_t;
  var i : inst_idx_t;
  begin
  --
  iq_new := iq;
  --#iq_new .iq_insts[iq .iq_tail] := inst;
  --#iq_new .iq_tail := ( iq .iq_tail + 1 ) % CORE_INST_NUM;
  iq_new .num_entries := iq .num_entries + 1;
  --#for i:0 .. CORE_INST_NUM do
  i := CORE_INST_NUM;
  --#while i <= CORE_INST_NUM do
  while 0 <= i do
    -- find an entry to insert into
    if ( iq_new .iq_valid[i] = invalid )
      then
      iq_new .iq_insts[i] := inst;
      --# TODO NOTE: insert as valid
      --# TODO NOTE: and have scoreboard
      --# mark as ready!
      iq_new .iq_valid[i] := ready;
      --# Finish, leave fn
      --# assert (i = 0) "always 0 check?";
      return iq_new;
    endif;
    i := i - 1;
  endwhile;
  --
  -- error "should have inserted inst into IQ?";
  return iq_new;
end
],

[murϕ_proc_decl|
function rob_insert(
             rob : ROB;
             inst : INST;
           ) : ROB;
  var rob_new : ROB;
  var rob_tail : inst_idx_t;
  begin
  --
  rob_new := rob;
  rob_tail := rob .tail;

  rob_new .entries[rob .tail].instruction := inst;
  rob_new .tail := ( rob .tail + 1 ) % (CORE_INST_NUM + 1);
  rob_new .num_entries := rob .num_entries + 1;
  --
  -- # assert not needed...
  assert (rob .num_entries <= ( CORE_INST_NUM + 1)) "can't add more!";
  return rob_new;
end
],
[murϕ_proc_decl|

function rob_remove(
             rob : ROB;
           ) : ROB;
  var rob_new : ROB;
  var rob_head : inst_idx_t;
begin
  --
  rob_new := rob;
  rob_head := rob .head;

  rob_new .entries[rob .head] .op := inval;
  rob_new .head := ( rob .head + 1 ) % (CORE_INST_NUM + 1);
  rob_new .num_entries := rob .num_entries - 1;
  -- rob_new .state[rob .rob_head] := commit_not_sent;
  --
  -- # assert not needed...
  assert (rob .num_entries >= ( 0 )) "can't remove more!";
  return rob_new;
end
],
[murϕ_proc_decl|

function lq_clear_head(
             lq : LQ;
             --#lq_entry : LQ_idx_t;
           ) : LQ;
  var lq_new : LQ;
  var curr_head : LQ_idx_t;
begin

  lq_new := lq;
  curr_head := lq .head;

  lq_new .entries[curr_head] .instruction .seq_num := 0;
  lq_new .entries[curr_head] .instruction .op := inval;
  lq_new .entries[curr_head] .instruction .dest_reg := 0;
  lq_new .entries[curr_head] .instruction .imm := 0;
  -- AZ NOTE: CUSTOMIZATION PT?
  lq_new .entries[curr_head] .state := await_creation;
  -- lq_new .entries[curr_head] .commit := false;
  lq_new .entries[curr_head] .read_value := 0;
  lq_new .entries[curr_head] .virt_addr := 0;
  lq_new .entries[curr_head] .phys_addr := 0;
  lq_new .entries[curr_head] .st_seq_num := 0;
  lq_new .head := (curr_head + 1) % ( LQ_NUM_ENTRIES_CONST);
  lq_new .num_entries := (lq_new .num_entries - 1);

  return lq_new;
end
],
[murϕ_proc_decl|

function lq_commit_head(
             lq : LQ;
             --#inst : INST;
           ) : LQ;
  var lq_new : LQ;
  var lq_idx : LQ_idx_t;
  var lq_entry : LQ_entry_values;
begin

  lq_new := lq;
  --#lq_idx := search_lq_seq_num_idx(lq,
  --#                               inst .seq_num);

  -- check what state is the load in
  --#if (lq .entries[lq_idx] .state = await_committed)
  if (lq .entries[lq .head] .state = await_committed)
    then
    lq_new := lq_clear_head(lq_new);
  else
    -- simply set a flag in the ld entry
    error "Load should be on the await_committed state..";
    -- lq_new .entries[lq .head] .commit := true;
  end;
  -- # if state is not in await commit
  -- # then set a flag in the load entry state
  -- # else if it is in await commit
  -- # then set the state back to the await_creation
  -- # state after clearing entry info

  return lq_new;
end
],
[murϕ_proc_decl|

function search_lq_seq_num_idx(
             lq : LQ;
             seq_num : inst_count_t;
           ) : LQ_idx_t;
  var lq_new : LQ;
  var lq_idx : LQ_idx_t;
begin

  for i : LQ_idx_t do
    if (lq .entries[i] .instruction .seq_num = seq_num)
      then
      return i;
    end;
  end;
  error "LQ Search: didn't find it? how? bad seq_num idx?";
end
],
[murϕ_proc_decl|

function search_sq_seq_num_idx(
             sq : SQ;
             seq_num : inst_count_t;
           ) : SQ_idx_t;
  var sq_new : SQ;
  var sq_idx : SQ_idx_t;
  begin

  for i : SQ_idx_t do
    if (sq .entries[i] .instruction .seq_num = seq_num)
      then
      return i;
    end;
  end;
  error "SQ Search: didn't find it? how? bad seq_num idx?";
end
],
[murϕ_proc_decl|

function sq_insert(
             lq : LQ;
             sq : SQ;
             inst : INST;
           ) : SQ;
  var lq_new : LQ;
  var sq_new : SQ;
  var tail : SQ_idx_t;
begin
  --
  sq_new := sq;

  -- AZ NOTE: CUSTOMIZATION PT?
  assert sq_new .entries[sq .tail] .state = sq_await_creation "to insert, store should be awaiting creation";
  sq_new .entries[sq .tail] .state := sq_await_scheduled;

  lq_new := lq;
  if (lq .num_entries != 0) then
    --#NOTE: REMEMBER TO CLEAR ST SEQ NUM
    --# at the end...
    sq_new .entries[sq .tail] .ld_seq_num := lq .entries[lq .tail] .instruction .seq_num;
  else
    --# actually, if 0 entries, then the search should
    --# just start from the head element
    --# Treat 0 as a special symbol to start from head
    sq_new .entries[sq .tail] .ld_seq_num := 0;
  end;
  sq_new .entries[sq .tail] .instruction := inst;
  sq_new .tail := ( sq .tail + 1 ) % (SQ_NUM_ENTRIES_CONST);
  sq_new .num_entries := sq .num_entries + 1;
  --
  assert (sq .num_entries < ( SQ_NUM_ENTRIES_CONST)) "can't add more!";
  return sq_new;
end
],
[murϕ_proc_decl|

function sq_schedule(
             sq : SQ;
             seq_num : inst_count_t; --#seq_num_t;--inst_count_t;
           ) : SQ;
  var sq_new : SQ;
  var sq_iter : SQ_idx_t;
  var sq_count : SQ_count_t;
  var curr_entry : SQ_entry_values;
  var curr_entry_id : SQ_idx_t;
  begin
  --
  sq_new := sq;
  sq_iter := sq .head;
  sq_count := sq .num_entries;

  --#for i:0 .. lq_count do
  --# actually interesting,
  --# since if there's a collision
  --# it'll likely be because we
  --# didn't clear old LQ entries'
  --# seq_num var!
  --# or include condition on if
  --# so it must be in await state?
  --# Could use a while loop instead
  for i:0 .. SQ_NUM_ENTRIES_ENUM_CONST do
    -- error "trace load schedule?";
    -- Use i

    curr_entry_id := ( sq_iter + i ) % ( SQ_NUM_ENTRIES_CONST);
    curr_entry := sq_new .entries[curr_entry_id];
    if (curr_entry .instruction .seq_num = seq_num)
      then
      -- # put "\n ================ \n";
      -- # put "seq_num: ";
      -- # put seq_num;
      -- # put "\n";
      -- # put "\n ================ \n";
      -- AZ NOTE: CUSTOMIZATION PT?
      assert ( curr_entry .state = sq_await_scheduled ) "Should be in await scheduled?";
      curr_entry .state := sq_await_translation;

      --# NOTE: For mem access stuff
      curr_entry .virt_addr := curr_entry .instruction .imm;
      --# NOTE TODO: Hacked in for litmus test
      curr_entry .write_value := curr_entry .instruction .write_value;

      sq_new .entries[curr_entry_id] := curr_entry;
      -- error "trace load schedule?";
      return sq_new;
    end;
  end;
  --
  error "didn't find the Store to Schedule?";
  return sq_new;
end
],
[murϕ_proc_decl|

function sq_clear_head(
             sq : SQ;
             --#lq_entry : LQ_idx_t;
           ) : SQ;
  var sq_new : SQ;
  var curr_head : SQ_idx_t;
begin

  sq_new := sq;
  curr_head := sq .head;

  sq_new .entries[curr_head] .instruction .seq_num := 0;
  sq_new .entries[curr_head] .instruction .op := inval;
  sq_new .entries[curr_head] .instruction .dest_reg := 0;
  sq_new .entries[curr_head] .instruction .imm := 0;
  -- AZ NOTE: CUSTOMIZATION PT?
  sq_new .entries[curr_head] .state := sq_await_creation;
  -- sq_new .entries[curr_head] .commit := false;
  sq_new .entries[curr_head] .write_value := 0;
  sq_new .entries[curr_head] .virt_addr := 0;
  sq_new .entries[curr_head] .phys_addr := 0;
  sq_new .head := (curr_head + 1) % ( SQ_NUM_ENTRIES_CONST);
  sq_new .num_entries := (sq_new .num_entries - 1);

  return sq_new;
end
],
[murϕ_proc_decl|

function sq_commit_head(
             sq : SQ;
             --#inst : INST;
           ) : SQ;
  var sq_new : SQ;
  var sq_idx : SQ_idx_t;
  var sq_entry : SQ_entry_values;
begin

  sq_new := sq;
  --#sq_idx := search_sq_seq_num_idx(sq,
  --#                               inst .seq_num);

  -- check what state is the load in
  --#if (sq .entries[sq_idx] .sq_state = sq_await_committed)
  -- AZ NOTE: CUSTOMIZATION PT?
  if (sq .entries[sq .head] .state = sq_await_committed)
    then
    --# remove head
    sq_new := sq_clear_head(sq_new);
  else
    -- simply set a flag in the ld entry
    error "SQ entry should be on sq_await_committed";
    -- sq_new .entries[sq .head] .commit := true;
  end;
  -- # if state is not in await commit
  -- # then set a flag in the load entry state
  -- # else if it is in await commit
  -- # then set the state back to the await_creation
  -- # state after clearing entry info

  return sq_new;
end
],
[murϕ_proc_decl|

function sb_insert (sb : SB;
sq_entry : SQ_entry_values) : SB;
  var sb_new : SB;
  var sb_tail : SB_idx_t;

begin
  sb_new := sb;
  assert (sb.num_entries < SB_NUM_ENTRIES_CONST) "can't add more!";
  sb_new.num_entries := (sb.num_entries + 1);

  for i : SB_idx_t do
    if (sb_new.entries[ i ].state = sb_await_creation) then
      sb_new.entries[ i ].state := sb_await_send_mem_req;
      sb_new.entries[ i ].instruction := sq_entry.instruction;
      sb_new.entries[ i ].virt_addr := sq_entry.virt_addr;
      sb_new.entries[ i ].phys_addr := sq_entry.phys_addr;
      sb_new.entries[ i ].write_value := sq_entry.write_value;
      return sb_new;
    end;
  endfor;
end
],
[murϕ_proc_decl|
function sb_clear_entry (
  sb : SB;
  seq_num : inst_count_t
) : SB;
  var sb_new : SB;
  var curr_head : SB_idx_t;
begin
  sb_new := sb;
  -- curr_head := sb.head;
  for i : SB_idx_t do
    if (sb_new.entries[ i ].instruction.seq_num = seq_num) then
      sb_new.entries[ i ].instruction.seq_num := 0;
      sb_new.entries[ i ].instruction.op := inval;
      sb_new.entries[ i ].instruction.dest_reg := 0;
      sb_new.entries[ i ].instruction.imm := 0;
      sb_new.entries[ i ].state := sb_await_creation;
      sb_new.entries[ i ].write_value := 0;
      sb_new.entries[ i ].virt_addr := 0;
      sb_new.entries[ i ].phys_addr := 0;
      -- NOTE: Make sure to check if there's space before inserting
      sb_new.num_entries := (sb_new.num_entries - 1);
      return sb_new;
    end;
  endfor;
  error "Couldn't find the SB entry to clear!!!";
end
],
[murϕ_proc_decl|

function sb_clear_head(
             sb : SB;
             --#lq_entry : LQ_idx_t;
           ) : SB;
  var sb_new : SB;
  var curr_head : SB_idx_t;
  begin

  sb_new := sb;
  curr_head := sb .head;

  sb_new .entries[curr_head] .instruction .seq_num := 0;
  sb_new .entries[curr_head] .instruction .op := inval;
  sb_new .entries[curr_head] .instruction .dest_reg := 0;
  sb_new .entries[curr_head] .instruction .imm := 0;
  sb_new .entries[curr_head] .state := sb_await_creation;
  sb_new .entries[curr_head] .write_value := 0;
  sb_new .entries[curr_head] .virt_addr := 0;
  sb_new .entries[curr_head] .phys_addr := 0;
  sb_new .head := (curr_head + 1) % ( SB_NUM_ENTRIES_CONST);
  sb_new .num_entries := (sb_new .num_entries - 1);

  return sb_new;
end
],
[murϕ_proc_decl|

function insert_ld_in_mem_interface(
             --#mem_int : MEM_INTERFACE;
             ld_entry : LQ_entry_values;
             core : cores_t;
           ) : MEM_REQ;
  var msg : MEM_REQ;
begin

  msg .addr := ld_entry .phys_addr;
  msg .r_w := read;
  --#msg .value := 0;
  msg .valid := true;

  msg .dest := mem;
  msg .dest_id := core;
  msg .seq_num := ld_entry .instruction .seq_num;

  return msg;
end
],
[murϕ_proc_decl|

function insert_st_in_mem_interface(
             --#mem_int : MEM_INTERFACE;
             sb_entry : SB_entry_values;
             core : cores_t;
           ) : MEM_REQ;
  var msg : MEM_REQ;
begin

  msg .addr := sb_entry .phys_addr;
  msg .r_w := write;
  msg .value := sb_entry .write_value;
  msg .valid := true;

  msg .dest := mem;
  msg .dest_id := core;
  msg .seq_num := sb_entry .instruction .seq_num;

  return msg;
end
],
[murϕ_proc_decl|

function insert_msg_into_ic(
             ic : IC;
             msg : MEM_REQ;
           ) : IC;
  var ic_new : IC;
begin
  ic_new := ic;
  for i : ic_idx_t do
    if ic_new .valid[i] = false
      then
      ic_new .buffer[i] := msg;
      ic_new .valid[i] := true;
      ic_new .num_entries := ic .num_entries + 1;
      return ic_new;
    end;
  end;
end
],
[murϕ_proc_decl|

function associative_assign_lq(
             lq : LQ;
             msg : MEM_REQ; --#seq_num_t;--inst_count_t;
           ) : LQ;
  var lq_new : LQ;
  var lq_iter : LQ_idx_t;
  var lq_count : LQ_count_t;
  var curr_entry : LQ_entry_values;
  var curr_entry_id : LQ_idx_t;
  var seq_num : inst_count_t;
  begin
  --
  lq_new := lq;
  lq_iter := lq .head;
  lq_count := lq .num_entries;
  seq_num := msg .seq_num;

  for i:0 .. LQ_NUM_ENTRIES_ENUM_CONST do
    -- error "trace load schedule?";
    -- Use i

    curr_entry_id := ( lq_iter + i ) % ( LQ_NUM_ENTRIES_CONST);
    curr_entry := lq_new .entries[curr_entry_id];
    if (curr_entry .instruction .seq_num = seq_num)
      then
      --# NOTE: load can be in other states due to
      --# an asynch restart sig from the St [x] -> LQ search
      assert (( curr_entry .state = squashed_await_ld_mem_resp ) | ( curr_entry .state = await_mem_response ) ) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";

      --# This causes problems
      --# if load was reset, this will also set the state
      --# to an incorrect state with a stale value
      --# Checking if ld is in the await_mem_response state
      --# also is insufficient if it reaches this state
      --# after a reset.
      --# Therefore, there must be some way to ignore old
      --# requests?
      --# Since it'd be more intractable to hunt down
      --# a packet or request and stop it.
      --# It's simpler to ignore messages with a time
      --# stamp mis-match for example.
      if ( curr_entry .state = await_mem_response ) then
        curr_entry .state := write_result;
      elsif (curr_entry .state = squashed_await_ld_mem_resp) then
        --# Complete the squash action
        curr_entry .state := await_fwd_check;
      end;


      --# NOTE: For mem access stuff
      curr_entry .read_value := msg .value;

      lq_new .entries[curr_entry_id] := curr_entry;
      -- error "trace load schedule?";
      return lq_new;
    end;
  end;
  --
  --# NOTE: don't error, since if the LD was reset,
  --# then this request is effectively floating
  --# Maybe set a counter instead, to track
  --# number of "unnecessary" LD Messages?
  --#error "didn't find the Load to write the read val into?";
  return lq_new;
end
],
[murϕ_proc_decl|

function associative_ack_sb(
             sb : SB;
             msg : MEM_REQ; --#seq_num_t;--inst_count_t;
           ) : SB;
  var sb_new : SB;
  var sb_iter : SB_idx_t;
  var sb_count : SB_count_t;
  var curr_entry : SB_entry_values;
  var curr_entry_id : SB_idx_t;
  var seq_num : inst_count_t;
  begin
  --
  sb_new := sb;
  sb_iter := sb .head;
  sb_count := sb .num_entries;
  seq_num := msg .seq_num;

  for i:0 .. SB_NUM_ENTRIES_ENUM_CONST do
    curr_entry_id := ( sb_iter + i ) % ( SB_NUM_ENTRIES_CONST);
    curr_entry := sb_new .entries[curr_entry_id];
    if (curr_entry .instruction .seq_num = seq_num)
      then
      assert ( curr_entry .state = sb_await_mem_response ) "ACK SB: Should be in await mem resp?";
      --# curr_entry .state := sb_await_creation;
      -- NOTE: comment this out, since this checks for in-order stores!
      -- but we just use the test to 
      -- assert (curr_entry .instruction .seq_num = sb_new .entries[sb_new .head] .instruction .seq_num) "should be de-queuing the head!!";

      --#sb_new .entries[curr_entry_id] := curr_entry;
      --# Should implement a de-queue operation
      --# that's based on matching a field
      sb_new := sb_clear_entry(sb_new, seq_num);

      -- error "trace load schedule?";
      return sb_new;
    end;
  end;
  --
  error "didn't find the Load to write the read val into?";
  return sb_new;
end
],
[murϕ_proc_decl|

function store_queue_match_phys_addr_younger_than_seq_num_request
  (
    sq : SQ;
    st_seq_num : inst_count_t;
    phys_addr : addr_idx_t;
    ld_seq_num : inst_count_t;
    --#core : cores_t;
  ) : SQ;
  var sq_new : SQ;
begin
  sq_new := sq;

  --# seq sq flag, to perform the search in parallel
  -- sq_new .search_busy := true;
  -- sq_new .st_seq_num := st_seq_num;
  -- sq_new .phys_addr := phys_addr;
  -- sq_new .ld_seq_num := ld_seq_num;

  return sq_new;
end
],
[murϕ_proc_decl|

function store_buffer_match_phys_addr_younger_than_seq_num_request
  (
    sb : SB;
    st_seq_num : inst_count_t;
    phys_addr : addr_idx_t;
    ld_seq_num : inst_count_t;
    --#core : cores_t;
  ) : SB;
  var sb_new : SB;
begin
  sb_new := sb;

  --# seq sq flag, to perform the search in parallel
  -- sb_new .search_busy := true;
  -- sb_new .phys_addr := phys_addr;
  -- sb_new .ld_seq_num := ld_seq_num;

  return sb_new;
end
],
[murϕ_proc_decl|

function find_st_idx_of_seq_num
  (
    sq : SQ;
    seq_num : inst_count_t;
  ) : SQ_count_t;
  var sq_entry : SQ_entry_values;
begin
  for i : SQ_idx_t do
    sq_entry := sq .entries[i];
    if (sq_entry .instruction .seq_num = seq_num) then
      return i;
    end;
  end;
  --#return SQ_NUM_ENTRIES_CONST;
  return 0;
end
],
[murϕ_proc_decl|

function set_load_seq_num_entry_read
  (
    lq : LQ;
    --# sq : SQ;
    seq_num : inst_count_t;
    value : val_t;
  ) : LQ;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  --# write the val to the ld entry
  for i : LQ_idx_t do
    --# find matching st_num
    ld_entry := lq .entries[i];
    if (ld_entry .instruction .seq_num = seq_num) then
      lq_new .entries[i] .read_value := value;
      --#lq_new .entries[i] .state := write_result;
      --#put "Confirm found entry";
      --#put i;
      --#put value;
      --#put "\n";

      return lq_new;
    end;
  end;
  --#error "Set Load Read: load shouldn't have disappeared?";
  --# NOTE: Comment out for now, since the requests
  --# can "linger" even after the St and Ld have both
  --# exited their Queues
  --# leading to no load in the LQ
  return lq_new;
end
],
[murϕ_proc_decl|

function set_load_state_to_state
  (
    lq : LQ;
    seq_num : inst_count_t;
    state : LQ_state;
  ) : LQ;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  --# write the val to the ld entry
  for i : LQ_idx_t do
    ld_entry := lq .entries[i];
    if (ld_entry .instruction .seq_num = seq_num) then
      lq_new .entries[i] .state := state;

      return lq_new;
    end;
  end;
  --#error "Set Load State: load shouldn't have disappeared?";
  return lq_new;
end
],
[murϕ_proc_decl|

function assert_load_state_is_state
  (
    lq : LQ;
    seq_num : inst_count_t;
    state : LQ_state;
  ) : boolean;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  --# write the val to the ld entry
  for i : LQ_idx_t do
    --# find matching st_num
    ld_entry := lq .entries[i];
    if (ld_entry .instruction .seq_num = seq_num) then
      assert (lq_new .entries[i] .state = state) "bad state";

      --#return lq_new;
      return false;
    end;
  end;
  --#error "load shouldn't have disappeared?";
  return true;
end
],
[murϕ_proc_decl|

function init_state_fn () : STATE;
  var init_state : STATE;
begin

  undefine init_state;

  --# Memory & Interconnect
  alias mem:init_state .mem_ do
    for i : addr_idx_t do
      mem .arr[i] := 0;
    end;
    --#mem .msg 
  endalias;

  alias ic: init_state .ic_ do
    for i : ic_idx_t do
      --# init message entries
      ic .buffer[i] .addr := 0;
      ic .buffer[i] .r_w := read;
      ic .buffer[i] .value := 0;
      --# Key thing. invalid message
      ic .buffer[i] .valid := false;

      --# Destination info
      --# Core or Mem
      --# Core has seq_num
      ic .buffer[i] .dest := mem;
      ic .buffer[i] .dest_id := 0;
      ic .buffer[i] .seq_num := 0;

      --# also have invalid for IC
      --# could combine with msg
      --# valid flag
      ic .valid[i] := false;
    end;
    ic .num_entries := 0;
  end;

  for core : cores_t do
    --# Mem Interface
    alias mem_int:init_state .core_[core] .mem_interface_ do
      --#init the mem interfaces
      mem_int .out_msg .addr := 0;
      mem_int .out_msg .r_w := read;
      mem_int .out_msg .value := 0;
      --# Key thing. invalid message
      mem_int .out_msg .valid := false;

      --# Destination info
      --# Core or Mem
      --# Core has seq_num
      mem_int .out_msg .dest := mem;
      mem_int .out_msg .dest_id := 0;
      mem_int .out_msg .seq_num := 0;

      mem_int .in_msg .addr := 0;
      mem_int .in_msg .r_w := read;
      mem_int .in_msg .value := 0;
      mem_int .in_msg .valid := false;
      mem_int .in_msg .dest := mem;
      mem_int .in_msg .dest_id := 0;
      mem_int .in_msg .seq_num := 0;

      mem_int .out_busy := false;
      mem_int .in_busy := false;
    end;

    -- #Load Queue
    alias lq:init_state .core_[core] .LQ_ do
      for i : LQ_idx_t do
        -- assume imm insts for now in litmus tests
        lq .entries[i] .instruction .seq_num := 0;
        lq .entries[i] .instruction .op := inval;
        lq .entries[i] .instruction .imm := 0;
        lq .entries[i] .instruction .dest_reg := 0;

        lq .entries[i] .read_value := 0;
        lq .entries[i] .virt_addr := 0;
        lq .entries[i] .phys_addr := 0;
        -- lq .entries[i] .commit := false;
        lq .entries[i] .st_seq_num := 0;
        -- # Technically, this is init'd
        -- # by setting things to 0
        -- # so.. move on to next state
        lq .entries[i] .state := await_creation;
      end;
      lq .head := 0;
      lq .tail := 0;
      lq .num_entries := 0;

      -- lq .search_busy := false;
      -- lq .st_seq_num := 0;
      -- lq .phys_addr := 0;
      -- lq .ld_seq_num := 0;
    end;
    alias rename:init_state .core_[core] .rename_ do
      for i : 0 .. CORE_INST_NUM do
        rename .test_insts[i] .op := inval;
        rename .test_insts[i] .seq_num := 0;
      end;
      rename .rename_head := 0;
      rename .rename_tail := 0;
      rename .num_entries := 0;
    end;
    alias iq:init_state .core_[core] .iq_ do
      for i : 0 .. CORE_INST_NUM do
        iq .iq_insts[i] .op := inval;
        iq .iq_insts[i] .seq_num := 0;
        iq .iq_valid[i] := invalid;
      end;
      -- # iq .iq_head := 0;
      -- # iq .iq_tail := 0;
      iq .num_entries := 0;
      -- iq .iq_valid[CORE_INST_NUM] := ready;
      -- iq .iq_valid[CORE_INST_NUM-1] := ready;
    end;
    alias rf:init_state .core_[core] .rf_ do
      for i : reg_idx_t do
        rf .rf[i] := 0;
      end;
    end;
    alias rob:init_state .core_[core] .rob_ do
      for i : 0 .. CORE_INST_NUM do
        rob .entries[i] .op := inval;
        rob .entries[i] .seq_num := 0;
        -- rob .state[i] := commit_not_sent;
        rob .entries[i] .is_executed := false;
        rob .entries[i] .state := rob_await_creation;
      end;
      rob .head := 0;
      rob .tail := 0;
      rob .num_entries := 0;
    end;
    alias sq:init_state .core_[core] .SQ_ do
      for i : SQ_idx_t do
        --# assume imm insts for now in litmus tests
        sq .entries[i] .instruction .seq_num := 0;
        sq .entries[i] .instruction .op := inval;
        sq .entries[i] .instruction .imm := 0;
        sq .entries[i] .instruction .dest_reg := 0;

        sq .entries[i] .write_value := 0;
        sq .entries[i] .virt_addr := 0;
        sq .entries[i] .phys_addr := 0;
        -- sq .entries[i] .commit := false;
        -- # Technically, this is init'd
        -- # by setting things to 0
        -- # so.. move on to next state
        sq .entries[i] .state := sq_await_creation;
      end;
      sq .head := 0;
      sq .tail := 0;
      sq .num_entries := 0;

      --# stuff for searching for fwding
      -- sq .search_busy := false;
    end;
    alias sb:init_state .core_[core] .SB_ do
      for i : SB_idx_t do
        --# assume imm insts for now in litmus tests
        sb .entries[i] .instruction .seq_num := 0;
        sb .entries[i] .instruction .op := inval;
        sb .entries[i] .instruction .imm := 0;
        sb .entries[i] .instruction .dest_reg := 0;

        sb .entries[i] .write_value := 0;
        sb .entries[i] .virt_addr := 0;
        sb .entries[i] .phys_addr := 0;
        -- # Technically, this is init'd
        -- # by setting things to 0
        -- # so.. move on to next state
        sb .entries[i] .state := sb_await_creation;
      end;
      sb .head := 0;
      sb .tail := 0;
      sb .num_entries := 0;

      -- sb .search_busy := false;
      -- sb .phys_addr := 0;
      -- sb .ld_seq_num := 0;
    end;
  end;

  -- # set up litmus test
  £list_rename_init_insts;

  -- alias rename_c0:init_state .core_[0] .rename_ do
  --   --#for i : 0 .. CORE_INST_NUM do
  --   --#  rename .test_insts[i] .op := inval;
  --   --#  rename .test_insts[i] .seq_num := 0;
  --   --#end;

  --   --#--# amd1 test
  --   --#rename_c0.test_insts[0] .op := st;
  --   --#rename_c0.test_insts[0] .seq_num := 1;
  --   --#rename_c0.test_insts[0] .dest_reg := 0;
  --   --#rename_c0.test_insts[0] .imm := 0; --# Addr
  --   --#rename_c0.test_insts[0] .write_value := 1;

  --   --#rename_c0.test_insts[1] .op := st;
  --   --#rename_c0.test_insts[1] .seq_num := 2;
  --   --#rename_c0.test_insts[1] .dest_reg := 1;
  --   --#rename_c0.test_insts[1] .imm := 1; --# Addr
  --   --#rename_c0.test_insts[1] .write_value := 1;

  --   --# iwp23b1 test
  --   rename_c0.test_insts[0] .op := st;
  --   rename_c0.test_insts[0] .seq_num := 1;
  --   rename_c0.test_insts[0] .dest_reg := 0;
  --   rename_c0.test_insts[0] .imm := 0; --# Addr
  --   rename_c0.test_insts[0] .write_value := 1;

  --   rename_c0.test_insts[1] .op := ld;
  --   rename_c0.test_insts[1] .seq_num := 2;
  --   rename_c0.test_insts[1] .dest_reg := 1;
  --   rename_c0.test_insts[1] .imm := 0; --# Addr
  --   --#rename_c0.test_insts[1] .write_value := 0;

  --   rename_c0.rename_head := 0;
  --   rename_c0.rename_tail := 0;
  --   rename_c0.num_entries := 2;
  -- end;
  -- alias rename_c1:init_state .core_[1] .rename_ do
  --   --#for i : 0 .. CORE_INST_NUM do
  --   --#  rename .test_insts[i] .op := inval;
  --   --#  rename .test_insts[i] .seq_num := 0;
  --   --#end;

  --   --#--# amd1 test
  --   --#rename_c1.test_insts[0] .op := ld;
  --   --#rename_c1.test_insts[0] .seq_num := 1;
  --   --#rename_c1.test_insts[0] .dest_reg := 0;
  --   --#rename_c1.test_insts[0] .imm := 1;
  --   --#--#rename_c1.test_insts[0] .write_value := 0;

  --   --#rename_c1.test_insts[1] .op := ld;
  --   --#rename_c1.test_insts[1] .seq_num := 2;
  --   --#rename_c1.test_insts[1] .dest_reg := 1;
  --   --#rename_c1.test_insts[1] .imm := 0;
  --   --#--#rename_c1.test_insts[1] .write_value := 0;

  --   --# iwp23b1 test
  --   rename_c1.test_insts[0] .op := st;
  --   rename_c1.test_insts[0] .seq_num := 1;
  --   rename_c1.test_insts[0] .dest_reg := 0;
  --   rename_c1.test_insts[0] .imm := 1;
  --   rename_c1.test_insts[0] .write_value := 1;

  --   rename_c1.test_insts[1] .op := ld;
  --   rename_c1.test_insts[1] .seq_num := 2;
  --   rename_c1.test_insts[1] .dest_reg := 1;
  --   rename_c1.test_insts[1] .imm := 1;
  --   --#rename_c1.test_insts[1] .write_value := 0;

  --   rename_c1.rename_head := 0;
  --   rename_c1.rename_tail := 0;
  --   rename_c1.num_entries := 2;
  -- end;

  return init_state;
end
]
]) ++ func_decls

  let list_rules : List Murϕ.Rule := List.join (
    [
  [murϕ_rule|

startstate "init"
  undefine Sta;

  Sta := init_state_fn();
end
],
[murϕ_rule|
------------- MEMORY TRANSITIONS -----------------
--# Write the memory interface stuff

--# Insert from a mem interface into the IC
ruleset j : cores_t do
rule "move_msg_from_mem_interface_to_ic"
  ( Sta .core_[j] .mem_interface_.out_busy = true )
  &
  ( Sta .ic_.num_entries < ( IC_ENTRY_NUM + 1 ) )
==>
  -- decls
  var next_state : STATE;
  var ic : IC;
  var mem_int : MEM_INTERFACE;
begin
  next_state := Sta;
  --# move msg into ic from mem_int
  mem_int := Sta .core_[j] .mem_interface_;
  ic := Sta .ic_;

  --# copy msg into ic
  ic := insert_msg_into_ic(ic, mem_int .out_msg);

  mem_int .out_busy := false;

  next_state .core_[j] .mem_interface_ := mem_int;
  next_state .ic_ := ic;

  Sta := next_state;
end;
endruleset
],
[murϕ_rule|

--# Choose an IC msg to perform it's access operation
ruleset i : ic_idx_t do
rule "perform_ic_msg"
  --# have some ic entries
  ( Sta .ic_.num_entries > 0 )
  --# this particular entry is valid
  &
  ( Sta .ic_.valid[i] = true )
  --# this is going to memory
  &
  ( Sta .ic_.buffer[i] .dest = mem )
==>
-- decls
  var next_state : STATE;
  var ic : IC;
  var mem : MEM_ARRAY;
  var addr : addr_idx_t;
begin
  --# setup
  next_state := Sta;
  ic := Sta .ic_;
  mem := Sta .mem_;
  addr := ic .buffer[i] .addr;

  --# perform the access
  if ( ic .buffer[i] .r_w = read )
    then
    ic .buffer[i] .value := mem .arr[addr];
  elsif (ic .buffer[i] .r_w = write)
    then
    mem .arr[addr] := ic .buffer[i] .value;
  endif;

  --# Reverse direction for acknowledgement
  ic .buffer[i] .dest := core;

  next_state .ic_ := ic;
  next_state .mem_ := mem;

  Sta := next_state;
end;
endruleset
],
[murϕ_rule|

--# Choose an IC msg to return it's acknowledgement
ruleset j : cores_t do
ruleset i : ic_idx_t do
rule "acknowledge_ic_msg"
  --# have some ic entries
  ( Sta .ic_.num_entries > 0 )
  --# this particular entry is valid
  &
  ( Sta .ic_.valid[i] = true )
  --# this is going to a core
  &
  ( Sta .ic_.buffer[i] .dest = core )
  --# Current core j is the dest core
  &
  ( j = Sta .ic_.buffer[i] .dest_id )
  --# receiving mem interface is not busy
  &
  ( Sta .core_[j] .mem_interface_.in_busy = false )
==>
  -- decls
  var next_state : STATE;
  var ic : IC;
  var mem_interface : MEM_INTERFACE;
begin
  --# setup
  next_state := Sta;
  ic := Sta .ic_;
  mem_interface := Sta .core_[j] .mem_interface_;

  mem_interface .in_msg := ic .buffer[i];
  mem_interface .in_busy := true;

  ic .buffer[i] .addr := 0;
  ic .buffer[i] .r_w := read;
  ic .buffer[i] .value := 0;
  ic .buffer[i] .valid := false;
  ic .buffer[i] .dest := mem;
  ic .buffer[i] .dest_id := 0;
  ic .valid[i] := false;
  ic .num_entries := ic .num_entries - 1;

  next_state .ic_ := ic;
  next_state .core_[j] .mem_interface_ := mem_interface;

  Sta := next_state;
end; end;
endruleset
],
[murϕ_rule|

--# Core checks input msgs to notify dest structure
ruleset j : cores_t do
  rule "core_sends_in_msg_ack_to_structures"
  ( Sta .core_[j] .mem_interface_.in_busy = true )
==>
  --# Decls
  var next_state : STATE;
  var lq : LQ;
  var sb : SB;
  var mem_interface : MEM_INTERFACE;
begin
  next_state := Sta;
  lq := Sta .core_[j] .LQ_;
  sb := Sta .core_[j] .SB_;
  mem_interface := Sta .core_[j] .mem_interface_;

  if ( mem_interface .in_msg .r_w = read )
    then
    lq := associative_assign_lq(lq, mem_interface .in_msg);
  elsif ( mem_interface .in_msg .r_w = write )
    then
    --# advance SB state to ack'd
    --# basically clear'd
    sb := associative_ack_sb(sb, mem_interface .in_msg);
  endif;

  mem_interface .in_busy := false;

  next_state .core_[j] .LQ_ := lq;
  next_state .core_[j] .SB_ := sb;
  next_state .core_[j] .mem_interface_ := mem_interface;

  Sta := next_state;
end;
endruleset
],
[murϕ_rule|

ruleset j : cores_t do
rule "inject_inst_from_rename"
  (Sta .core_[j] .rename_.num_entries > 0)
==>
-- #decls
  var nxt_state : STATE;
  var rename_q : RENAME;
  var lq_q : LQ;
  var sq_q : SQ;
  var iq_q : IQ;
  var rob_q : ROB;
  -- #the inst being moved
  var inst : INST;
begin
  -- #init our vars
  nxt_state := Sta;
  rename_q := Sta .core_[j] .rename_;
  lq_q := Sta .core_[j] .LQ_;
  sq_q := Sta .core_[j] .SQ_;
  -- #sq_q := sta .SQ_;
  -- lsq_q := Sta .core_[j] .lSQ_;
  iq_q := Sta .core_[j] .iq_;
  rob_q := Sta .core_[j] .rob_;
--
  -- #NOTE! less than or equal
  if (rob_q .num_entries <= CORE_INST_NUM)
    then
    if (iq_q .num_entries <= CORE_INST_NUM)
      then

      inst := rename_read_head(rename_q);
      if (inst .op = ld) then
        -- #check if Full!
        -- #also note that this should check
        -- #the IQ as well, and must stop if
        -- # either is full

        if (lq_q .num_entries <= LQ_NUM_ENTRIES_ENUM_CONST)
          then
          -- #remove inst from rename,
          rename_q := rename_pop_head(rename_q);
          -- # Also transition to next state
          lq_q := lq_insert(lq_q, sq_q, inst);
          iq_q := iq_insert(iq_q, inst);
          rob_q := rob_insert(rob_q, inst);
        endif;
      elsif (inst .op = st) then
        -- #remove inst from rename,
        -- rename_q := rename_pop_head(rename_q);
        -- # insert into sq...
        if (sq_q .num_entries <= SQ_NUM_ENTRIES_ENUM_CONST)
          then
          -- #remove inst from rename,
          rename_q := rename_pop_head(rename_q);
          -- # Also transition to next state
          sq_q := sq_insert(lq_q, sq_q, inst);
          iq_q := iq_insert(iq_q, inst);
          rob_q := rob_insert(rob_q, inst);
        endif;
      elsif (inst .op = inval) then
        -- #remove inst from rename,
        -- rename_q := rename_pop_head(rename_q);
        error "shouldn't reach this??";
      endif;
    endif;
  endif;

  -- # also insert into IQ...

  -- #add it to IQ/LSQ

  -- # finish and update all state
  -- # set rename
  nxt_state .core_[j] .rename_ := rename_q;
  -- # set LSQ stuff
  -- lsq_q .LQ_ := lq_q;
  -- lsq_q .SQ_ := sq_q;
  nxt_state .core_[j] .LQ_ := lq_q;
  nxt_state .core_[j] .SQ_ := sq_q;
  -- # set IQ stuff
  nxt_state .core_[j] .iq_ := iq_q;
  -- # set ROB stuff
  nxt_state .core_[j] .rob_ := rob_q;

  -- # update state
  Sta := nxt_state;
  -- error "Trace?";
  -- assert !((iq_q .iq_valid[0] = ready)
  --          &
  --          (iq_q .iq_valid[1] = ready)
  --         ) "both iq entries won't be assigned";
end;
endruleset
],
[murϕ_rule|

-- # NOTE: Create rule for pop from IQ, tell LQ
-- # to start, advance 1 state
-- # Require helper fn for IQ pop
-- # Ruleset, any inst can be scheduled

ruleset j : cores_t do
ruleset i : IQ_MAX_INSTS do
rule "schedule_iq_inst"
-- # if IQ not empty, and entry i is valid
  (!( Sta .core_[j] .iq_.num_entries = 0 ))
  &
  (Sta .core_[j] .iq_.iq_valid[i] = ready)
==>
  -- overall state update
  var next_state : STATE;
  var inst : INST;
  var lq : LQ;
  var sq : SQ;
  var iq : IQ;
  var seq_num : inst_count_t; --#seq_num_t;--inst_count_t;
  var num_entries : inst_count_t;
begin
  -- assign our vars
  next_state := Sta;
  inst := Sta .core_[j] .iq_.iq_insts[i];
  lq := Sta .core_[j] .LQ_;
  sq := Sta .core_[j] .SQ_;
  iq := Sta .core_[j] .iq_;
  num_entries := Sta .core_[j] .iq_.num_entries;
  -- logic
  -- 1. read this instruction
  -- put it in var or alias For convenience
  -- #alias inst:Sta .core_[j] .iq_.iq_insts[i] do
  -- #End;
  -- 3. find it in LQ, and advance it's state
  if ( inst .op = ld ) then
    -- #put "\n ================ \n";
    -- #put "IQ i: ";
    -- #put i;
    -- #put "\n";
    -- #put "seq_num: ";
    -- #put iq .iq_insts[i] .seq_num;
    -- #put "\n";
    -- #put "\n ================ \n";
    seq_num := iq .iq_insts[i] .seq_num;
    lq := lq_schedule(lq, seq_num);
  elsif (inst .op = st) then
    seq_num := iq .iq_insts[i] .seq_num;
    sq := sq_schedule(sq, seq_num);
  elsif (inst .op = inval) then
    error "shouldn't have an inval inst in IQ?";
  endif;

  -- 2. de-alloc it
  --#next_state .core_[j] .iq_.iq_valid[i] := invalid;
  inst .op := inval;
  inst .seq_num := 0;
  -- # NOTE: Must adjust rename insert func
  -- # so it sets IQ insts state to ready
  -- # AND! it searches for a spot to insert an inst in?
  -- # order doesn't matter any more,
  -- # just need arbitrary orderings

  if Sta .core_[j] .iq_.iq_insts[i] .op = ld then
    next_state .core_[j] .LQ_ := lq;
  elsif Sta .core_[j] .iq_.iq_insts[i] .op = st then
    next_state .core_[j] .SQ_ := sq;
  endif;
  next_state .core_[j] .iq_.iq_insts[i] := inst;
  next_state .core_[j] .iq_.iq_valid[i] := invalid;
  next_state .core_[j] .iq_.num_entries := num_entries-1;
  Sta := next_state;
  -- error "trace";
end;
endruleset;
endruleset
],
[murϕ_rule|

-- # TODO: add simple transition rule to "remove"
-- # a load once it reaches some state like commit
-- # to avoid implementing it for now
-- # DONE, the ld remove rule.
-- # TODO, Replace with commit / ROB later

-- # TODO: Continue to model LQ, make request to Memory
-- # Also need to model memory
-- # Make simple memory requests
-- # So need a simple 2 or 3 addr memory
-- # and load/store requests happen immediately

-- # TODO then model commit stage to retire the load
-- # 
ruleset j : cores_t do
rule "rob_commit_head"
  -- pre cond
  -- have entries to commit
  (Sta .core_[j] .rob_.num_entries > 0)
  -- &
  -- -- and haven't tried commit this inst yet
  -- --# NOTE This is a trick, add state, to make sure
  -- --# this only runs once
  -- --# maybe different if we generate explicit
  -- --# substates (what andres was going to do?)
  -- (Sta .core_[j] .rob_.state[Sta .core_[j] .rob_.rob_head] = commit_not_sent)
  
  &
  --# head inst has executed
  (Sta .core_[j] .rob_ .is_executed[Sta .core_[j] .rob_ .rob_head] = true)
==>
  -- decls
  var next_state : STATE;
  var rob : ROB;
  var head_inst : INST;
  var lq_q : LQ;
  var sq_q : SQ;
  var sq_entry : SQ_entry_values;
  var sb_q : SB;
begin
  next_state := Sta;
  -- directly change state of lq or sq
  rob := Sta .core_[j] .rob_;

  --check head inst type
  head_inst := rob .rob_insts[rob .rob_head];

  lq_q := Sta .core_[j] .LQ_;

  sb_q := Sta .core_[j] .SB_;
  sq_q := Sta .core_[j] .SQ_;

  if (head_inst .op = ld) then
    -- #search for the load?
    --#lq_q := lq_commit(lq_q, head_inst);

    --# remove rob head if lq_entry was removed....
    --# This makes it all done atomically...
    if (lq_q .entries[lq_q .head] .state = await_committed)
      then
      rob .is_executed[Sta .core_[j] .rob_ .rob_head] := false;
      rob := rob_remove(rob);
    else
      --# If inst wasn't directly committed
      -- # set state to commit sig sent
      error "ld entry should be in await_comitted state..";
      -- rob .state[rob .rob_head] := commit_sig_sent;
    endif;

    -- # should be the head load...
    --# commit if in await commit, otherwise set
    --# saw commit sig flag to true
    lq_q := lq_commit_head(lq_q);


  elsif (head_inst .op = st) then
    --# SB Must have free space!
    if ( sb_q .num_entries < ( SB_NUM_ENTRIES_CONST ) )
      then

      --# Move inst into SB, remove from ROB
      if (sq_q .entries[sq_q .head] .state = sq_await_committed)
        then
        rob .is_executed[Sta .core_[j] .rob_ .rob_head] := false;
        rob := rob_remove(rob);

        sq_entry := sq_q .entries[sq_q .head];
        sb_q := sb_insert(sb_q, sq_entry);

      else
        --# If inst wasn't directly committed
        -- # set state to commit sig sent
        error "st entry should be in await_comitted state..";
        -- rob .state[rob .rob_head] := commit_sig_sent;
      endif;

      -- # should be the head load...
      -- # sq will insert head_inst into SB
      -- # SB does store later

      --# remove sq head if at await commit
      --# otherwise, set saw commit sig flag to true
      sq_q := sq_commit_head(sq_q);
    endif;

  elsif (head_inst .op = inval) then
    error "shouldn't have an inval head inst??";
  endif;

  next_state .core_[j] .rob_ := rob;

  if (head_inst .op = ld) then
    next_state .core_[j] .LQ_ := lq_q;
  elsif (head_inst .op = st) then
    next_state .core_[j] .SQ_ := sq_q;
    next_state .core_[j] .SB_ := sb_q;
  endif;

  Sta := next_state;
end;
endruleset
],

[murϕ_rule|

--#NOTE: Marking this transition as the mem access one!
--#ruleset i : SB_idx_t do
ruleset j : cores_t do
rule "sb_await_send_mem_req_to_await_mem_resp"
  --# when head SB entry is waiting to send out it's request
  ( Sta .core_[j] .SB_.entries[Sta .core_[j] .SB_.head] .state = sb_await_send_mem_req )
  &
  --# have sb entries
  ( Sta .core_[j] .SB_.num_entries > 0)
  --# this is a condition: mem's msg buffer is empty
  --# i .e. mem is free
  &
  (
    Sta .core_[j] .mem_interface_.out_busy = false
  )
==>
  -- decls
  var next_state : STATE;
  var sb : SB;
  var sb_entry : SB_entry_values;

  --# var mem : MEM_ARRAY;
  var phys_addr : addr_idx_t;

  var mem_inter : MEM_INTERFACE;
begin
  next_state := Sta;
  sb := Sta .core_[j] .SB_;
  sb_entry := sb .entries[sb .head];
  --#mem := Sta .mem_;
  mem_inter := Sta .core_[j] .mem_interface_;

  -- Update state
  sb_entry .state := sb_await_mem_response;

  -- Write to memory
  -- # Issue, wasn't using Imm before
  -- # could update it to write imm to virt/phys
  phys_addr := sb_entry .phys_addr;
  --#mem .arr[phys_addr] := sb_entry .write_value;

  --# NOTE: send to the mem interface
  mem_inter .out_msg := insert_st_in_mem_interface(
                                                  sb_entry,
                                                  j
                                                 );
  mem_inter .out_busy := true;

  next_state .core_[j] .SB_.entries[Sta .core_[j] .SB_.head] := sb_entry;
  next_state .core_[j] .mem_interface_ := mem_inter;

  --# AZ NOTE: This is a decent way to check if something went
  --# wrong, if an illegal inst (seq_num = 0) tries to
  --# perform any action while in any structure!!!
  assert ( sb .entries[sb .head] .instruction .seq_num != 0 ) "invalid st";

  Sta := next_state;
end;
endruleset
],
[murϕ_rule|


ruleset j : cores_t do
invariant "test_invariant"
  (Sta .core_[j] .LQ_.num_entries = 1)
  ->
  ( Sta .core_[j] .LQ_.tail
    =
    ( ( Sta .core_[j] .LQ_.head + 1 ) % (LQ_NUM_ENTRIES_CONST) )
  )
endruleset
],
[murϕ_rule|

rule "reset"
  (
    ( Sta .core_[0] .rename_.num_entries = 0 )
    &
    ( Sta .core_[0] .rob_.num_entries = 0 )
    &
    ( Sta .core_[0] .SB_.num_entries = 0 )
    &
    ( Sta .core_[0] .iq_.num_entries = 0 )
    &
    ( Sta .core_[0] .LQ_.num_entries = 0 )
    &
    ( Sta .core_[0] .SQ_.num_entries = 0 )
    &
    ( Sta .core_[1] .rename_.num_entries = 0 )
    &
    ( Sta .core_[1] .rob_.num_entries = 0 )
    &
    ( Sta .core_[1] .SB_.num_entries = 0 )
    &
    ( Sta .core_[1] .iq_.num_entries = 0 )
    &
    ( Sta .core_[1] .LQ_.num_entries = 0 )
    &
    ( Sta .core_[1] .SQ_.num_entries = 0 )
  )
==>
  -- decls
  var next_state : STATE;
begin
  next_state := Sta;

  --#Sta := next_state;
  Sta := init_state_fn();
end
]

]
) ++ rules ++ [ordering_invariant]

  let murphi_file : Murϕ.Program := {
    constdecls := list_const_decls,
    typedecls  := list_type_decls,
    vardecls   := list_var_decls,
    procdecls  := list_func_decls,
    rules      := list_rules
  }

  murphi_file

def gen_murphi_litmus_test_programs_no_ROB
-- Consts, like num entries per buffer-type ctrler
( const_decls : List Murϕ.Decl)
-- Types, like ctrler defns
( type_decls : List Murϕ.Decl)
( func_decls : List Murϕ.ProcDecl)
( rules : List Murϕ.Rule)
( ctrler_list : List controller_info )
-- ( litmus_tests : List LitmusTest )
: List MurphiFile
:=
  let murphi_files : List MurphiFile :=
  ActiveLitmusTests.map (
    λ litmus_test =>
      let name' := "generated-".append (litmus_test.test_name)
      let program' := compose_murphi_file_components_but_no_ROB const_decls type_decls func_decls rules ctrler_list litmus_test
      let murphi_file : MurphiFile := {
        filename := name',
        program := program'
      }
      murphi_file
  )

  murphi_files
