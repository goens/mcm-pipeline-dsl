import PipelineDsl.AnalysisHelpers

import Murphi

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
def compose_murphi_file_components_but_no_RENAME_no_IQ_no_ROB
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
  -- rename_ : RENAME;
  -- iq_ : IQ;
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

  -- IQ_MAX_INSTS : inst_idx_t;

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

  -- NOTE: Store State for Write Invalidations in the test harness
  STORE_STATE : enum {await_handling, await_invalidation_received, store_send_completion};
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

  -- NOTE: Store state in the Test Harness,
  -- whether it has sent out all invalidations to all cores yet
  store_state : STORE_STATE;
  store_inval_sent : array [cores_t] of boolean;
  store_inval_ackd : array [cores_t] of boolean;
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
  | TestResult.forbidden => Murϕ.Expr.negation all_reg_file_states
  | TestResult.required => all_reg_file_states
  | TestResult.permitted =>
    dbg_trace "Not handled Litmust Test case: Permitted"
    panic! "Not handled Litmust Test case: Permitted"

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
-- [murϕ_proc_decl|
-- function sb_clear_entry (
--   sb : SB;
--   seq_num : inst_count_t
-- ) : SB;
--   var sb_new : SB;
--   var curr_head : SB_idx_t;
-- begin
--   sb_new := sb;
--   -- curr_head := sb.head;
--   for i : SB_idx_t do
--     if (sb_new.entries[ i ].instruction.seq_num = seq_num) then
--       sb_new.entries[ i ].instruction.seq_num := 0;
--       sb_new.entries[ i ].instruction.op := inval;
--       sb_new.entries[ i ].instruction.dest_reg := 0;
--       sb_new.entries[ i ].instruction.imm := 0;
--       sb_new.entries[ i ].state := sb_await_creation;
--       sb_new.entries[ i ].write_value := 0;
--       sb_new.entries[ i ].virt_addr := 0;
--       sb_new.entries[ i ].phys_addr := 0;
--       -- NOTE: Make sure to check if there's space before inserting
--       sb_new.num_entries := (sb_new.num_entries - 1);
--       return sb_new;
--     end;
--   endfor;
--   error "Couldn't find the SB entry to clear!!!";
-- end
-- ],
[murϕ_proc_decl|

function insert_ld_in_mem_interface(
            -- NOTE: Swap when desired
            --  ld_entry : LQ_entry_values;
             ld_entry : LSQ_entry_values;
             core : cores_t;
           ) : MEM_REQ;
  var msg : MEM_REQ;
begin

  msg .addr := ld_entry .phys_addr;
  msg .r_w := read;
  msg .valid := true;

  msg .dest := mem;
  msg .dest_id := core;
  msg .seq_num := ld_entry .instruction .seq_num;

  return msg;
end
],
[murϕ_proc_decl|

function insert_st_in_mem_interface(
            --  sb_entry : SB_entry_values;
             sb_entry : ROB_entry_values;
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
-- Version for the LSQ
function associative_assign_ld(
            --  lq : LQ;
             lq : LSQ;
             msg : MEM_REQ; --#seq_num_t;--inst_count_t;
           ) : LSQ;
  var lq_new : LSQ;
  var lq_iter : LSQ_idx_t;
  var lq_count : LSQ_count_t;
  var curr_entry : LSQ_entry_values;
  -- var curr_entry_id : LSQ_idx_t;
  var seq_num : inst_count_t;

  var LQ_while_break : boolean;
  var LQ_found_entry : boolean;
  var LQ_entry_idx : LQ_idx_t;
  var LQ_difference : LQ_count_t;
  var LQ_offset : LQ_count_t;
  var LQ_curr_idx : LQ_idx_t;
  var LQ_squash_remove_count : LQ_count_t;

  begin
  --
  lq_new := lq;
  lq_iter := lq .head;
  lq_count := lq .num_entries;
  seq_num := msg .seq_num;
  LQ_while_break := false;
  LQ_found_entry := false;
  if (lq.num_entries = 0) then
    LQ_while_break := true;
  end;
  LQ_entry_idx := lq.head;
  LQ_difference := lq.num_entries;
  LQ_offset := 0;
  LQ_squash_remove_count := 0;
  -- for i : 0 .. LSQ_NUM_ENTRIES_ENUM_CONST do
  while ((LQ_offset < LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
    LQ_curr_idx := ((LQ_entry_idx + LQ_offset) % LQ_NUM_ENTRIES_CONST);

    curr_entry := lq_new.entries[ LQ_curr_idx ];
    if (curr_entry.instruction.seq_num = seq_num) then
      -- assert ((curr_entry.state = lsq_squashed_await_ld_mem_resp) | (curr_entry.state = lsq_await_load_mem_response)) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";
      assert (curr_entry.state = await_mem_response
      -- | curr_entry.state = replay_generated_await_mem_response
      ) "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = await_mem_response) then
        curr_entry.state := write_result;
        curr_entry.read_value := msg.value;
      -- elsif (curr_entry.state = replay_generated_await_mem_response) then
      --   curr_entry.state := replay_compare_and_check_state;
      --   curr_entry.replay_value := msg.value;
      end;
      lq_new.entries[ LQ_curr_idx ] := curr_entry;
      return lq_new;
    end;

    if (LQ_offset < LQ_difference) then
      LQ_offset := (LQ_offset + 1);
    end;
  -- endfor;
  end;

  return lq_new;
end
],
[murϕ_proc_decl|

function associative_ack_st(
--version for the LSQ
            --  sb : SB;
             sb : ROB;
             msg : MEM_REQ; --#seq_num_t;--inst_count_t;
           ) : ROB;
  var sb_new : ROB;
  var sb_iter : ROB_idx_t;
  var sb_count : ROB_count_t;
  var curr_entry : ROB_entry_values;
  var curr_entry_id : ROB_idx_t;
  var seq_num : inst_count_t;
  begin
  --
  sb_new := sb;
  sb_iter := 0;
  sb_count := sb .num_entries;
  seq_num := msg .seq_num;

  -- for i:0 .. SB_NUM_ENTRIES_ENUM_CONST do
  for i:0 .. ROB_NUM_ENTRIES_ENUM_CONST do
    -- curr_entry_id := ( sb_iter + i ) % ( SB_NUM_ENTRIES_CONST);
    curr_entry_id := ( sb_iter + i ) % ( ROB_NUM_ENTRIES_CONST);
    curr_entry := sb_new .entries[curr_entry_id];
    if (curr_entry .instruction .seq_num = seq_num)
      then
      -- assert ( curr_entry .state = sb_await_mem_response ) "ACK SB: Should be in await mem resp?";
      assert ( curr_entry .state = rob_commit_time_await_st_mem_resp ) "ACK ROB: Should be in await mem resp?";
      --# curr_entry .state := sb_await_creation;
      -- sb_new := sb_clear_entry(sb_new, seq_num);
      curr_entry.state := rob_clear_lsq_store_head;
      sb_new.entries[ curr_entry_id ] := curr_entry;

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

      -- Init IC buffer store invalidation state
      ic.buffer[i].store_state := await_handling; -- await_invalidation_received -- NOTE: remember to reset this state
      -- For each cores_t...
      for core_idx : cores_t do
        ic.buffer[i].store_inval_sent[core_idx] := false;
        ic.buffer[i].store_inval_ackd[core_idx] := false;
      endfor;
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

    --# LSQ
    alias lsq:init_state .core_[core] .LSQ_ do
      -- for i : LQ_idx_t do
      for i : LSQ_idx_t do
        -- assume imm insts for now in litmus tests
        lsq .entries[i] .instruction .seq_num := 0;
        lsq .entries[i] .instruction .op := inval;
        lsq .entries[i] .instruction .imm := 0;
        lsq .entries[i] .instruction .dest_reg := 0;

        lsq .entries[i] .read_value := 0;
        lsq .entries[i] .write_value := 0;
        lsq .entries[i] .virt_addr := 0;
        lsq .entries[i] .phys_addr := 0;
        -- # Technically, this is init'd
        -- # by setting things to 0
        -- # so.. move on to next state
        -- lsq .entries[i] .state := await_creation;
        lsq .entries[i] .state := lsq_await_creation;
      end;
      lsq .head := 0;
      lsq .tail := 0;
      lsq .num_entries := 0;
    end;
    -- #Load Queue
    -- alias lq:init_state .core_[core] .LQ_ do
    --   for i : LQ_idx_t do
    --     -- assume imm insts for now in litmus tests
    --     lq .entries[i] .instruction .seq_num := 0;
    --     lq .entries[i] .instruction .op := inval;
    --     lq .entries[i] .instruction .imm := 0;
    --     lq .entries[i] .instruction .dest_reg := 0;

    --     lq .entries[i] .read_value := 0;
    --     lq .entries[i] .virt_addr := 0;
    --     lq .entries[i] .phys_addr := 0;
    --     -- lq .entries[i] .commit := false;
    --     lq .entries[i] .st_seq_num := 0;
    --     -- # Technically, this is init'd
    --     -- # by setting things to 0
    --     -- # so.. move on to next state
    --     lq .entries[i] .state := await_creation;
    --   end;
    --   lq .head := 0;
    --   lq .tail := 0;
    --   lq .num_entries := 0;

    --   -- lq .search_busy := false;
    --   -- lq .st_seq_num := 0;
    --   -- lq .phys_addr := 0;
    --   -- lq .ld_seq_num := 0;
    -- end;
    alias rename:init_state .core_[core] .RENAME_ do
      for i : 0 .. CORE_INST_NUM do
        rename .entries[i] .instruction .op := inval;
        rename .entries[i] .instruction .seq_num := 0;
        -- rename .entries[i] .state := rename_await_creation;
        rename .entries[i] .state := issue_if_head;
      end;
      rename .head := 0;
      rename .tail := 0;
      rename .num_entries := 0;
    end;
    alias iq:init_state .core_[core] .IQ_ do
      -- AZ TODO NOTE: Generate these controller state init "functions"..
      for i : 0 .. IQ_NUM_ENTRIES_ENUM_CONST do
        iq .entries[i] .instruction .op := inval;
        iq .entries[i] .instruction .seq_num := 0;
        iq .entries[i] .state := iq_await_creation;
        iq .entries[i] .valid := false;
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
    alias rob:init_state .core_[core] .ROB_ do
      for i : 0 .. CORE_INST_NUM do
        rob .entries[i] .instruction .op := inval;
        rob .entries[i] .instruction .seq_num := 0;
        -- rob .state[i] := commit_not_sent;
        rob .entries[i] .is_executed := false;
        rob .entries[i] .state := rob_await_creation;
      end;
      rob .head := 0;
      rob .tail := 0;
      rob .num_entries := 0;
    end;
    alias seqnumreg : init_state.core_[core].SeqNumReg_ do
      seqnumreg.seq_num_counter := 1;
      seqnumreg.state := seq_num_interface;
    end;
    -- alias sq:init_state .core_[core] .SQ_ do
    --   for i : SQ_idx_t do
    --     --# assume imm insts for now in litmus tests
    --     sq .entries[i] .instruction .seq_num := 0;
    --     sq .entries[i] .instruction .op := inval;
    --     sq .entries[i] .instruction .imm := 0;
    --     sq .entries[i] .instruction .dest_reg := 0;

    --     sq .entries[i] .write_value := 0;
    --     sq .entries[i] .virt_addr := 0;
    --     sq .entries[i] .phys_addr := 0;
    --     sq .entries[i] .state := sq_await_creation;
    --   end;
    --   sq .head := 0;
    --   sq .tail := 0;
    --   sq .num_entries := 0;

    --   --# stuff for searching for fwding
    --   -- sq .search_busy := false;
    -- end;
    -- alias sb:init_state .core_[core] .SB_ do
    --   for i : SB_idx_t do
    --     --# assume imm insts for now in litmus tests
    --     sb .entries[i] .instruction .seq_num := 0;
    --     sb .entries[i] .instruction .op := inval;
    --     sb .entries[i] .instruction .imm := 0;
    --     sb .entries[i] .instruction .dest_reg := 0;

    --     sb .entries[i] .write_value := 0;
    --     sb .entries[i] .virt_addr := 0;
    --     sb .entries[i] .phys_addr := 0;
    --     sb .entries[i] .state := sb_await_creation;
    --   end;
    --   -- sb .head := 0;
    --   -- sb .tail := 0;
    --   sb .num_entries := 0;
    -- end;
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
  -- AZ NOTE: for Store invalidation sent to cores
  var mem_interface : MEM_INTERFACE;
  var store_invals_sent : boolean;
  var store_invals_ackd : boolean;
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
    -- Set destination to core, when done with request..
    ic.buffer[ i ].dest := core;
  elsif (ic .buffer[i] .r_w = write)
    then
    if (ic.buffer[i].store_state = await_handling) then
      --put "Store awaiting handling\n";
      --put Sta.ic_.buffer[i].store_state;

      -- TODO AZ: maybe convert into a guard...
      -- If there exists a core that's not the dest of this msg, send an invalidation
      -- continue until all cores have been sent an "invalidation" msg
      -- then progress to the next state...

      -- i.e. For all cores that are not the dest of this core AND
      -- haven't been sent to already AND
      -- aren't busy with a message, send an invalidation
      for core_idx : cores_t do
        if (core_idx != ic.buffer[i].dest_id) & (ic.buffer[i].store_inval_sent[core_idx] = false) & (Sta.core_[core_idx].mem_interface_.in_busy = false) then
          -- Do the write to memory...
          mem.arr[ addr ] := ic.buffer[ i ].value;

          -- mem_interface := Sta.core_[ j ].mem_interface_;
          next_state.core_[core_idx].mem_interface_.in_msg := ic.buffer[ i ];
          next_state.core_[core_idx].mem_interface_.in_msg.dest := core;
          next_state.core_[core_idx].mem_interface_.in_busy := true;

          ic.buffer[i].store_inval_sent[core_idx] := true;
          --put Sta.ic_.buffer[i].store_inval_sent[core_idx];
          --put ic.buffer[i].store_inval_sent[core_idx];
        end;
      endfor;

      store_invals_sent := true;
      for core_idx : cores_t do
        if (core_idx != ic.buffer[i].dest_id) then
          -- TODO: check if all core - dests have an invalidation sent already....
          store_invals_sent := store_invals_sent & ic.buffer[i].store_inval_sent[core_idx];
        end;
      endfor;

      if store_invals_sent then
        ic.buffer[i].store_state := await_invalidation_received;
      end;

    elsif (ic.buffer[i].store_state = await_invalidation_received) then
      --put "Store awaiting invalidations ack'd\n";
      --put Sta.ic_.buffer[i].store_state;

      store_invals_ackd := true;
      for core_idx : cores_t do
        if (core_idx != ic.buffer[i].dest_id) then
          --put ic.buffer[i].dest_id;
          -- TODO: check if all core - dests have an invalidation sent already....
          store_invals_ackd := store_invals_ackd & ic.buffer[i].store_inval_ackd[core_idx];
        end;
      endfor;

      if store_invals_ackd then
        ic.buffer[i].store_state := store_send_completion;
      end;
      --put store_invals_ackd;

    elsif (ic.buffer[i].store_state = store_send_completion) then
      --put "Store: send completion.. Store got invalidations ack'd\n";
      --put Sta.ic_.buffer[i].store_state;
      -- Set destination to core, when done with request..

      -- NOTE: Don't have to do this below, since there's a rule to move msgs to cores
      -- when dest is set to core..
      -- next_state.core_[ ic.buffer[i].dest_id ].mem_interface_.in_msg := ic.buffer[ i ];

      ic.buffer[i].store_state := store_send_completion;
      ic.buffer[ i ].dest := core;

      for core_idx : cores_t do
        if (core_idx != ic.buffer[i].dest_id) then
          -- Reset this state...
          ic.buffer[i].store_inval_sent[core_idx] := false;
          ic.buffer[i].store_inval_ackd[core_idx] := false;
        end;
      endfor;
    -- elsif (ic.buffer[i].store_state = store_send_completion) then
    --   put "Store completed\n";
    --   put Sta.ic_.buffer[i].store_state;
    --   -- NOTE: do nothing. another controler/rule will take the message and send it?
    --   -- i could put the code here as well....
    else
      error "Unreachable or unhandled case of store state in the InterConnect.";
    end;
  endif;

  --# Reverse direction for acknowledgement
  -- AZ NOTE: Don't need to set dest if this is a store sending invalidations
  -- ic .buffer[i] .dest := core;

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
  -- var lq : LQ;
  var lq : LSQ;
  -- var sb : SB;
  var sb : ROB;
  var mem_interface : MEM_INTERFACE;
  var found_msg_in_ic : boolean;
begin
  next_state := Sta;
  lq := Sta .core_[j] .LSQ_;
  sb := Sta .core_[j] .ROB_;
  mem_interface := Sta .core_[j] .mem_interface_;

  if ( mem_interface .in_msg .r_w = read )
    then
    -- lq := associative_assign_lq(lq, mem_interface .in_msg);
    lq := associative_assign_ld(lq, mem_interface .in_msg);
    mem_interface.in_busy := false;
  elsif ( mem_interface .in_msg .r_w = write )
    then
    if (mem_interface.in_msg.store_state = await_handling) then
      -- Overview: Send back an ack for the invalidation sent..
      -- match this message with it's copy in the IC, set it's ack bool to true.

      -- next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;

      found_msg_in_ic := false;
      for ic_idx : ic_idx_t do
        -- if msg entry is valid & seq num matches, use this...
        -- ..and ack the IC entry
        if (Sta.ic_.valid[ic_idx] = true) & (Sta.ic_.buffer[ic_idx].seq_num = Sta.core_[j].mem_interface_.in_msg.seq_num)
          & (Sta.ic_.buffer[ic_idx].dest_id = Sta.core_[j].mem_interface_.in_msg.dest_id) then

          -- (1) send ack
          next_state.ic_.buffer[ic_idx].store_inval_ackd[j] := true;

          -- put next_state.ic_.buffer[ic_idx].store_inval_ackd[j];

          if found_msg_in_ic = true then
            error "we found 2 matching ic entries? shouldn't happen...";
          elsif found_msg_in_ic = false then
            found_msg_in_ic := true;
          endif;
        endif;
      endfor;

      assert (found_msg_in_ic = true) "Should have found a msg in the IC? Otherwise we wouldn't be performing this invalidation's squash.";
      assert (Sta.core_[j].mem_interface_.in_busy = true) "The memory interface of this core should be busy, this is the msg we're processing.";
      next_state.core_[j].mem_interface_.in_busy := false;

      mem_interface.in_busy := false;

    elsif (mem_interface.in_msg.store_state = store_send_completion) then
      --# advance SB state to ack'd
      --# basically clear'd
      -- sb := associative_ack_sb(sb, mem_interface .in_msg);
      sb := associative_ack_st(sb, mem_interface.in_msg);
      mem_interface.in_busy := false;
    end;
  endif;


  -- next_state .core_[j] .LQ_ := lq;
  -- next_state .core_[j] .SB_ := sb;
  next_state .core_[j] .LSQ_ := lq;
  next_state .core_[j] .ROB_ := sb;
  next_state .core_[j] .mem_interface_ := mem_interface;

  Sta := next_state;
end;
endruleset
],

-- [murϕ_rule|
-- ruleset j : cores_t do
-- invariant "test_invariant"
--   (Sta .core_[j] .LSQ_.num_entries = 1)
--   ->
--   ( Sta .core_[j] .LSQ_.tail
--     =
--     ( ( Sta .core_[j] .LSQ_.head + 1 ) % (LSQ_NUM_ENTRIES_CONST) )
--   )
-- endruleset
-- ],
[murϕ_rule|

rule "reset"
  (
    ( Sta .core_[0] .RENAME_.num_entries = 0 )
    &
    ( Sta .core_[0] .ROB_.num_entries = 0 )
    &
    -- ( Sta .core_[0] .SB_.num_entries = 0 )
    -- &
    ( Sta .core_[0] .IQ_.num_entries = 0 )
    &
    ( Sta .core_[0] .LSQ_.num_entries = 0 )
    &
    -- ( Sta .core_[0] .LQ_.num_entries = 0 )
    -- &
    -- ( Sta .core_[0] .SQ_.num_entries = 0 )
    -- &
    ( Sta .core_[1] .RENAME_.num_entries = 0 )
    &
    ( Sta .core_[1] .ROB_.num_entries = 0 )
    &
    -- ( Sta .core_[1] .SB_.num_entries = 0 )
    -- &
    ( Sta .core_[1] .IQ_.num_entries = 0 )
    &
    ( Sta .core_[1] .LSQ_.num_entries = 0 )
    -- ( Sta .core_[1] .LQ_.num_entries = 0 )
    -- &
    -- ( Sta .core_[1] .SQ_.num_entries = 0 )
  )
==>
  -- decls
  var next_state : STATE;
begin
  next_state := Sta;

  -- put "  === BEGIN Reached End, Reg File: ===\n";
  -- put Sta.core_[ 0 ].rf_.rf[ 0 ];
  -- put Sta.core_[ 0 ].rf_.rf[ 1 ];
  -- put "  === END Reached End, Reg File: ===\n";

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

def gen_murphi_litmus_test_programs_no_RENAME_no_IQ_no_ROB
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
      let program' := compose_murphi_file_components_but_no_RENAME_no_IQ_no_ROB const_decls type_decls func_decls rules ctrler_list litmus_test
      let murphi_file : MurphiFile := {
        filename := name',
        program := program'
      }
      murphi_file
  )

  murphi_files
