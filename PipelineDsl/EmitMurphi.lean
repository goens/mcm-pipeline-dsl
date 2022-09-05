
import PipelineDsl.Murphi

import PipelineDsl.AST

import PipelineDsl.Translation

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

-- I have function
-- ast0048_generate_controller_murphi_record
-- in Translation.lean, so this is probably not necessary?
-- just adjust the names where necessary

def ctrler_to_record
(ctrler : controller_info)
: Murϕ.TypeExpr -- record
:=
  -- get the list of state vars,
  let state_vars : List TypedIdentifier := ctrler.state_vars
  -- convert them from ast to murphi
  let murphi_decls : List Murϕ.Decl :=
  state_vars.map (
    λ typedident =>
      let (tident, ident) :=
      match typedident with
      | TypedIdentifier.mk tident ident => (tident, ident)

      let type_expr : Murϕ.TypeExpr :=
      Murϕ.TypeExpr.previouslyDefined tident

      let murphi_decl : Murϕ.Decl :=
      Murϕ.Decl.var [ident] type_expr

      murphi_decl
  )
  Murϕ.TypeExpr.record murphi_decls

-- Probably want a gen func that gens the
-- "search for queue idx based on seq_num"

def gen_search_buffer_seq_num_idx
(ctrler_name : Identifier)
: Murϕ.ProcDecl
:=

  -- TODO: Rename var instances of ctrler_name to ctrler_name_
  -- in the Translation.lean file...

  -- Specific var/type names we generate/use
  -- ex. SQ_
  let ctrler_var_name := String.join [ctrler_name, "_"]
  -- ex. search_SQ_seq_num_idx
  let search_func_name := String.join ["search_", ctrler_name, "_seq_num_idx"]
  -- ex. SQ_idx_t
  let ctrler_idx_name := String.join [ctrler_name, "_idx_t"]

  let murphi_func_prog : Murϕ.ProcDecl :=
  [murϕ_proc_decl|
function £search_func_name(
             £ctrler_var_name : £ctrler_name;
             -- assume inst_count_t is our seq_num type..
             seq_num : inst_count_t;
             -- assume £ctrler_name _idx_t is the
             -- <ctrler>. entry idx type name

           ) : £ctrler_idx_name;
  begin

  for i : £ctrler_idx_name do
    if (£ctrler_var_name .entries[i] .instruction .seq_num = seq_num)
      then
      return i;
    end;
  end;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end
  ]

  murphi_func_prog

def gen_buffer_ctrler_seq_num_search_func
(lst_ctrler_names : List Identifier)
: List Murϕ.ProcDecl
:=
  lst_ctrler_names.map gen_search_buffer_seq_num_idx

def construct_murphi_output_file
(lst_ctrlers : List controller_info)
-- the dsl to murphi rules/transitions
( translated_rules : List Description )
: List Murϕ.Decl × List Murϕ.Decl
:=
-- Take controller info
-- and gen the parts!

-- 1. Generate the constants...
-- Check each ctrler's state vars, and generate the
-- "Record" for each ctrler, consisting of it's state vars,
-- translated into Mur \varphi decls
  -- ast0048_generate_controller_murphi_record
  let ctrler_records : List ctrler_decl_entry_decl_const_decl
  := lst_ctrlers.map ast0048_generate_controller_murphi_record
-- structure ctrler_decl_entry_decl_const_decl where
-- -- Decl.type ctrl.name (TypeExpr.record, ID/String TypeExpr.record)
-- ctrler_decl : Decl
-- entry_decl : Decl -- Decl.type, ID/String TypeExpr.record
-- const_decl_lst : List Decl -- Decl.const, ID/String Expr.integerConst
-- range_enum_decl : List Decl -- 
-- entry_state_decl : Decl

  -- Separate the "const decls"
  -- and the "type decls"
  let const_decls : List Murϕ.Decl := List.join (ctrler_records.map (λ records => records.const_decl_lst))
  let ctrler_type_decl : List Murϕ.Decl := List.join (ctrler_records.map (
    λ records => ((records.range_enum_decl.concat records.entry_decl).concat records.entry_state_decl).concat records.ctrler_decl
    ))

  -- let entries : List Murϕ.Decl := ctrler_records.map ( λ records => records.entry_decl )
  -- let ctrlers : List Murϕ.Decl := ctrler_records.map ( λ records => records.ctrler_decl)

  (const_decls, ctrler_type_decl)

-- -- comment this out for now, to make the
-- -- lean4 interpretation faster...
-- def compose_murphi_file_components
-- -- Consts, like num entries per buffer-type ctrler
-- ( const_decls : List Murϕ.Decl)
-- -- Types, like ctrler defns
-- ( type_decls : List Murϕ.Decl)
-- ( func_decls : List Murϕ.ProcDecl)
-- ( rules : List Murϕ.Rule)
-- : Murϕ.Program
-- :=
--   let murphi_file := [murϕ_program|
-- const ---- Configuration parameters ----

--   -- LD_ENTRY_NUM : 1;
--   -- SQ_ENTRY_NUM : 1;
--   -- SB_ENTRY_NUM : 1;
--   -- DATA_NUM : 2;
--   CORE_INST_NUM : 1;

--   IC_ENTRY_NUM : 1;
--   --# 2 cores..
--   CORE_NUM : 1;

--   --# model a simple atomic memory
--   ADDR_NUM : 1; --# 2 addrs
--   --# num of reg file entries
--   REG_NUM : 1;
--   --# max value
--   MAX_VALUE : 1;
--   £const_decls

-- type ---- Type declarations ----

--   --# for value types
--   val_t : 0 .. MAX_VALUE;

--   --# GEN NOTE: have index & count types
--   inst_idx_t : 0 .. CORE_INST_NUM;
--   inst_count_t : 0 .. (CORE_INST_NUM + 1);
--   --# NOTE: could define seq_num_t type later
--   --# for more arbitrary seq_num lengths

--   -- ld_idx_t : 0 .. LD_ENTRY_NUM;
--   -- ld_count_t : 0 .. (LD_ENTRY_NUM + 1);

--   --# For memory array
--   addr_idx_t : 0 .. ADDR_NUM;

--   --# for reg file regs
--   reg_idx_t : 0 .. REG_NUM;

--   -- sq_idx_t : 0 .. SQ_ENTRY_NUM;
--   -- sq_count_t : 0 .. (SQ_ENTRY_NUM + 1);

--   -- sb_idx_t : 0 .. SB_ENTRY_NUM;
--   -- sb_count_t : 0 .. (SB_ENTRY_NUM + 1);

--   ic_idx_t : 0 .. IC_ENTRY_NUM;
--   ic_count_t : 0 .. (IC_ENTRY_NUM + 1);

--   cores_t : 0 .. CORE_NUM;

--   MSG_DEST : enum {core, mem};

--   --# So far haven't found use case for
--   --# symmetry stuff
--   -- LD_ENTRY : scalarset(LD_ENTRY_NUM);
--   £type_decls

--   -- LD_ENTRY : ld_idx_t;
--   IQ_MAX_INSTS : inst_idx_t;

--   -- Is there a point using the enum?
--   -- can I simply not just model
--   -- it similar to the DSL as well?
--   -- instead of as a state machine?
--   LD_ENTRY_STATE : enum {await_creation,
--                       await_scheduled,
--                       await_fwd_check,
--                       await_sb_fwd_check,
--                       await_translation,
--                       await_check_forwarding_or_load_response,
--                       await_sb_fwd_check_response,
--                       build_packet,
--                       send_memory_request,
--                       await_mem_response,
--                       squashed_await_mem_response,
--                       write_result,
--                       await_committed
--                      };

--   -- insts are either load or stores
--   INST_TYPE : enum {ld, st, inval};
--   ADDR_TYPE : enum {addr_reg, addr_imm};
--   VAL_TYPE : enum {val_reg, val_imm};

--   --# No polymorphism. just
--   --# hack it in
--   --# Might cause some potential "confusion"
--   --# with fields being set and such,
--   --# but should be ok for now...
--   INST : record
--   --# Inst Type Info
--   --# Ld, st, uses immediate, etc.
--   op : INST_TYPE;
--   addr_type : ADDR_TYPE;

--   --# Seq num. not strictly a part of
--   --# an inst. TODO should move this out
--   --# to the buffers..?
--   --# Would make sense to use a separate
--   --# Type as well...
--   seq_num : inst_count_t; --#seq_num_t;

--   --# A dest_reg. Only used by loads
--   dest_reg : reg_idx_t;

--   --# A src_reg. Src Addr
--   src_reg1 : reg_idx_t;
--   --# A src_reg. Src Val for St
--   src_reg2 : reg_idx_t;

--   --# Imm val. For now, only used
--   --# as Address for ld / st
--   --# Could be used with st
--   --# as write value as well

--   --# TODO figure out the imm stuff..?
--   --# imm_addr : addr_idx_t;
--   --# imm_val : val_t;
--   imm : val_t;

--   --# Hacked in for litmus tests
--   --# Should move out
--   write_value : val_t;
--   end;

--   -- just a 'dumb' copy of the state vars
--   LD_ENTRY_VALUES : record
--   ld_state : LD_ENTRY_STATE;
--   --#seq_num : inst_count_t; --#val_t;
--   instruction : INST;
--   virt_addr : addr_idx_t;
--   phys_addr : addr_idx_t;
--   read_value : val_t;
--   commit : boolean;
--   -- latest_store_seq_num : val_t;
--   st_seq_num : inst_count_t;
--   end;

--   ---------------------- SQ_ENTRY ----------------------

--   --# Murphi doesn't like the same enum names?
--   ST_ENTRY_STATE : enum {st_init,
--                          st_await_creation,
--                          st_await_scheduled,
--                          st_await_translation,
--                          st_await_lq_squash,
--                          st_build_packet,
--                          -- #send_memory_request,
--                          -- #await_mem_response,
--                          -- #write_result,
--                          st_await_committed
--                         };

--   SQ_ENTRY_VALUES : record
--   st_state : ST_ENTRY_STATE;
--   --#seq_num : inst_count_t; --#val_t;
--   instruction : INST;
--   virt_addr : addr_idx_t;
--   phys_addr : addr_idx_t;
--   write_value : val_t;
--   commit : boolean;
--   -- latest_load_seq_num : val_t;
--   ld_seq_num : inst_count_t;
--   end;

--   ---------------------- SB_ENTRY ----------------------

--   --# Murphi doesn't like the same enum names?
--   SB_ENTRY_STATE : enum {
--                          sb_await_creation,
--                          sb_await_send_mem_req,
--                          sb_await_mem_response
--                         };

--   SB_ENTRY_VALUES : record
--   sb_state : SB_ENTRY_STATE;
--   --#seq_num : inst_count_t; --#val_t;
--   instruction : INST;
--   --#TODO: ideally, replace phys_addr
--   --# with the packet.
--   --# more "accurate" to generated code
--   virt_addr : addr_idx_t;
--   phys_addr : addr_idx_t;
--   write_value : val_t;
--   end;

--   ---------------------- Rename ----------------------
--   -- # Rename inserts insts into LSQ #(LQ for now)
--   -- # So init it with a list of insts to feed into system
--   -- # this will deadlock,
--   -- # can it assert properties even with deadlock?
--   -- # Yes, NOTE that if a rule is not violated it doesn't
--   -- # print a message
--   -- # Some rules may not fire as well
--   -- # In any case it's not too important,
--   -- # we can model an endless rename inst generator
--   -- # if needed

--   -- start with modeling the queue of "test" insts
--   RENAME : record
--   test_insts : array [inst_idx_t] of INST;
--   rename_head : inst_idx_t;
--   rename_tail : inst_idx_t;
--   num_entries : inst_count_t;
--   end;
--   -- Then it's transitions
--   -- move an inst into the LSQ (and IQ)
--   -- Pre-condition:
--   -- None (conditionless rule)
--   -- Transition:
--   -- Allocate inst info in IQ/LSQ

--   -- may be worth using for rename and IQ
--   ------- Generic Instruction Buffer -----
--   --#IB : record
--   --#ib_insts : array [inst_idx_t] of INST;
--   --#ib_head : inst_idx_t;
--   --#ib_tail : inst_idx_t;
--   --#num_entries : inst_count_t;
--   --#end;

--   IQ_STATE : enum{ valid, invalid, ready };

--   ---------------------- IQ --------------
--   IQ : record
--   -- # Instruction info
--   iq_insts : array [inst_idx_t] of INST;
--   -- # If entry in queue is valid
--   iq_valid : array [inst_idx_t] of IQ_STATE;
--   -- # iq_head : inst_idx_t;
--   -- # iq_tail : inst_idx_t;
--   -- # NOTE: just num entries, search for
--   -- # an empty (invalid) entry to insert into
--   -- # NOTE: this is to support scheduling
--   -- # and removing arbitrary insts from the IQ
--   num_entries : inst_count_t;
--   end;

--   ---------------------- ROB --------------
--   -- ROB also has entry values
--   -- include MSG slot per entry
--   -- must identify where to send MSG
--   -- (which entry)
--   -- Also need listener to check when
--   -- MSG is there & act accordingly
--   -- (1)
--   -- pre-cond:
--   -- So additional transition rule
--   -- pred on MSG slot available &
--   -- current state is a listen state
--   -- transition action:
--   -- listen state gets MSG, does MSG action
--   -- in handle block

--   ROB_STATE : enum {commit_not_sent, commit_sig_sent};

--   ROB: record
--   rob_insts : array [inst_idx_t] of INST;
--   rob_head : inst_idx_t;
--   rob_tail : inst_idx_t;
--   -- do we also include is_executed state?
--   num_entries : inst_count_t;
--   state : array [inst_idx_t] of ROB_STATE;
--   end;

--   ---------------------- mem interface --------------
--   -- Memory interface
--   -- send messages to mem interface
--   -- know when MSG slot is occupied
--   -- SO:
--   -- (1) addtl transition rule:
--   -- pre-cond:
--   -- loads in a sending state
--   -- & MSG slot empty
--   -- transition action:
--   -- a load will use the MSG slot

--   -- # TODO: Continue to model LQ, make request to Memory
--   -- # Also need to model memory
--   -- # Make simple memory requests
--   -- # So need a simple 2 or 3 addr memory
--   -- # and load/store requests happen immediately
--   -- record of data at addrs

--   --# Read or Write? enum
--   R_W : enum {read, write};

--   --# Message to or from mem
--   MEM_REQ : record
--   addr : addr_idx_t;
--   r_w : R_W;
--   --# Either write val or read val
--   --# depends on r_w & to/from mem
--   value : val_t;
--   valid : boolean;

--   --# Destination of msg
--   dest : MSG_DEST;
--   dest_id : cores_t;

--   --# seq_num in core
--   seq_num : inst_count_t;
--   end;

--   MEM_ARRAY : record
--   arr : array [addr_idx_t] of val_t;
--   --# don't need to model this...
--   --#msg : MEM_REQ;
--   end;

--   MEM_INTERFACE : record
--   out_msg : MEM_REQ;
--   in_msg : MEM_REQ;

--   out_busy : boolean;
--   in_busy : boolean;
--   end;

--   ------------------ Re-ordering Interconnect --------------
--   IC : record
--   --# IC will have a buffer...
--   --# Idea: let the IC send packets
--   --# in an arbitrary order.
--   --# Emulate with a buffer of N msgs
--   --# Any element that's valid can be
--   --# forwarded
--   buffer : array [ic_idx_t] of MEM_REQ;
--   valid : array [ic_idx_t] of boolean;
--   num_entries : ic_count_t;

--   --# Just need 1 buffer
--   --# core -> IC
--   --# IC -> mem access
--   --# change msg dest to core
--   --# IC -> core

--   --# alternate implementation:
--   --# mem eats msg and sends back a msg
--   --# Then we need a full duplex channel,
--   --# i.e. 2 buffers, 1 to mem, 1 to core
--   end;

--   REG_FILE: record
--   rf : array [reg_idx_t] of val_t;
--   end;

--   -- Other # TODO
--   -- Try to understand what theyre using MSG and MSG_CMD for
--   -- I dont think I need it? maybe. might be a decent way to
--   -- send messages


--   -- organize this slightly better
--   -- arrange core stuff into a "core"
--   -- compose "multi-core-system" from "core"
--   -- instantiate "multi-core-system" as state

--   LQ : record
--   ld_entries : array [LD_ENTRY] of LD_ENTRY_VALUES;
--   ld_head : ld_idx_t;
--   ld_tail : ld_idx_t;
--   num_entries : ld_count_t;
--   --# Search related things
--   --# consider creating a record
--   search_busy : boolean;
--   --# LQ doesn't need to know which store sent a req
--   --# No wait it does, to send the completion
--   --# and let the store know it can move states or sth
--   st_seq_num : inst_count_t;
--   phys_addr : addr_idx_t;
--   ld_seq_num : inst_count_t;
--   end;

--   ---------------------- SQ ----------------------
--   SQ : record
--   sq_entries : array [sq_idx_t] of SQ_ENTRY_VALUES;
--   sq_head : sq_idx_t;
--   sq_tail : sq_idx_t;
--   num_entries : sq_count_t;
--   --# Search related things
--   --# consider creating a record
--   search_busy : boolean;
--   st_seq_num : inst_count_t;
--   phys_addr : addr_idx_t;
--   ld_seq_num : inst_count_t;
--   end;

--   ---------------------- SB ----------------------
--   SB : record
--   sb_entries : array [sb_idx_t] of SB_ENTRY_VALUES;
--   sb_head : sb_idx_t;
--   sb_tail : sb_idx_t;
--   num_entries : sb_count_t;

--   --# Search related things
--   --# consider creating a record
--   search_busy : boolean;
--   phys_addr : addr_idx_t;
--   ld_seq_num : inst_count_t;
--   end;

--   LSQ : record
--   lq_ : LQ;
--   sq_ : SQ;
--   end;

--   CORE : record
--   lsq_ : LSQ;
--   rename_ : RENAME;
--   iq_ : IQ;
--   rf_ : REG_FILE;
--   rob_ : ROB;
--   sb_ : SB;
--   mem_interface_ : MEM_INTERFACE;
--   end;

--   STATE : record
--   core_ : array [cores_t] of CORE;
--   mem_ : MEM_ARRAY;
--   -- other things like message channels could go here
--   ic_ : IC;
--   --# Pretending this is
--   --# a single port mem interface
--   --# for each core..

--   --#mem_interface_ : array [cores_t] of MEM_INTERFACE;
--   end;

-- var -- state vars - explicit overall state --

--   Sta :STATE;

-- -- # ------------ HELPER FUNCTIONS --------------------

-- £func_decls

-- function rename_read_head( rename_q : RENAME) : INST;
--   return rename_q.test_insts[rename_q.rename_head];
-- end;

-- function rename_pop_head(
--              -- head and tail
--              -- head : inst_idx_t;
--              -- tail : inst_idx_t;
--              -- the array
--                     rename_queue : RENAME
--            ) : RENAME;
--   var rename_q : RENAME;
-- begin
--   rename_q := rename_queue;

--   rename_q.rename_head := ( rename_queue.rename_head + 1 ) % (CORE_INST_NUM + 1);
--   rename_q.num_entries := rename_queue.num_entries - 1;
--   -- assert num_entries not less than 0?
--   -- assert ( rename_q.num_entries > 0 ) "can't have neg entries";

--   -- use this to overwrite the old one
--   -- (immutable style)
--   return rename_q;
-- end;

-- function lq_insert(
--              lq : LQ;
--              sq : SQ;
--              inst : INST;
--            ) : LQ;
--   var lq_new : LQ;
--   var lq_tail : ld_idx_t;

--   --# ADDED NOTE
--   --#var sq : SQ;
--   var curr_tail_entry : LD_ENTRY_VALUES;
-- begin
--   --
--   lq_new := lq;
--   curr_tail_entry := lq_new.ld_entries[lq.ld_tail];

--   assert curr_tail_entry.ld_state = await_creation "to insert, load should be awaiting creation";
--   curr_tail_entry.ld_state := await_scheduled;
--   --# AZ TODO: do the store_queue check

--   --# Consider placing the Check Store_queue latest
--   --# Entry here!
--   --# Though if I do, this means this action is atomic,
--   --# and no other message passing operations 
--   --# NOTE should be add the assert in automatically?
--   --# or allow the user to specify asserts as well?
--   --# Or both?
--   --# Generated asserts shouldn't cause problems for the user
--   assert (curr_tail_entry.st_seq_num = 0) "should first be 0?";
--   if (sq.num_entries != 0) then
--     --#NOTE: REMEMBER TO CLEAR ST SEQ NUM
--     --# at the end...
--     curr_tail_entry.st_seq_num := sq.sq_entries[sq.sq_head].instruction.seq_num;
--   else
--     --# Keep at none
--     --# 0 is "none" here...
--     curr_tail_entry.st_seq_num := 0;
--   end;

--   --# NOTE: Auto generate the standard "insert" part
--   curr_tail_entry.instruction := inst;
--   lq_new.ld_tail := ( lq.ld_tail + 1 ) % (LD_ENTRY_NUM + 1);
--   lq_new.num_entries := lq.num_entries + 1;
--   --
--   --# NOTE: assert, but not technically required, since
--   --# if it's out of the range, Murphi throws an error
--   assert (lq.num_entries < ( LD_ENTRY_NUM + 1)) "can't add more!";

--   lq_new.ld_entries[lq.ld_tail] := curr_tail_entry;

--   return lq_new;
-- end;

-- function lq_schedule(
--              lq : LQ;
--              seq_num : inst_count_t; --#seq_num_t;--inst_idx_t;
--            ) : LQ;
--   var lq_new : LQ;
--   var lq_iter : ld_idx_t;
--   var lq_count : ld_count_t;
--   var curr_entry : LD_ENTRY_VALUES;
--   var curr_entry_id : ld_idx_t;
--   begin
--   --
--   lq_new := lq;
--   lq_iter := lq.ld_head;
--   lq_count := lq.num_entries;

--   --#for i:0 .. lq_count do
--   --# actually interesting,
--   --# since if there's a collision
--   --# it'll likely be because we
--   --# didn't clear old LQ entries'
--   --# seq_num var!
--   --# or include condition on if
--   --# so it must be in await state?
--   --# Could use a while loop instead
--   for i:0 .. LD_ENTRY_NUM do
--     -- error "trace load schedule?";
--     -- Use i

--     curr_entry_id := ( lq_iter + i ) % ( LD_ENTRY_NUM + 1);
--     curr_entry := lq_new.ld_entries[curr_entry_id];
--     if (curr_entry.instruction.seq_num = seq_num)
--       then
--       -- # put "\n ================ \n";
--       -- # put "seq_num: ";
--       -- # put seq_num;
--       -- # put "\n";
--       -- # put "\n ================ \n";
--       assert ( curr_entry.ld_state = await_scheduled ) "Should be in await scheduled?";
--       curr_entry.ld_state := await_translation;

--       --# NOTE: For mem access stuff
--       curr_entry.virt_addr := curr_entry.instruction.imm;

--       lq_new.ld_entries[curr_entry_id] := curr_entry;
--       -- error "trace load schedule?";
--       return lq_new;
--     end;
--   end;
--   --
--   error "didn't find the Load to Schedule?";
--   return lq_new;
-- end;

-- function iq_insert(
--              iq : IQ;
--              inst : INST;
--            ) : IQ;
--   var iq_new : IQ;
--   var iq_tail : inst_idx_t;
--   var i : inst_idx_t;
--   begin
--   --
--   iq_new := iq;
--   --#iq_new.iq_insts[iq.iq_tail] := inst;
--   --#iq_new.iq_tail := ( iq.iq_tail + 1 ) % CORE_INST_NUM;
--   iq_new.num_entries := iq.num_entries + 1;
--   --#for i:0 .. CORE_INST_NUM do
--   i := CORE_INST_NUM;
--   --#while i <= CORE_INST_NUM do
--   while 0 <= i do
--     -- find an entry to insert into
--     if ( iq_new.iq_valid[i] = invalid )
--       then
--       iq_new.iq_insts[i] := inst;
--       --# TODO NOTE: insert as valid
--       --# TODO NOTE: and have scoreboard
--       --# mark as ready!
--       iq_new.iq_valid[i] := ready;
--       --# Finish, leave fn
--       --# assert (i = 0) "always 0 check?";
--       return iq_new;
--     endif;
--     i := i - 1;
--   endwhile;
--   --
--   -- error "should have inserted inst into IQ?";
--   return iq_new;
-- end;

-- function rob_insert(
--              rob : ROB;
--              inst : INST;
--            ) : ROB;
--   var rob_new : ROB;
--   var rob_tail : ld_idx_t;
--   begin
--   --
--   rob_new := rob;
--   rob_tail := rob.rob_tail;

--   rob_new.rob_insts[rob.rob_tail] := inst;
--   rob_new.rob_tail := ( rob.rob_tail + 1 ) % (CORE_INST_NUM + 1);
--   rob_new.num_entries := rob.num_entries + 1;
--   --
--   -- # assert not needed...
--   assert (rob.num_entries <= ( CORE_INST_NUM + 1)) "can't add more!";
--   return rob_new;
-- end;

-- function rob_remove(
--              rob : ROB;
--            ) : ROB;
--   var rob_new : ROB;
--   var rob_head : ld_idx_t;
-- begin
--   --
--   rob_new := rob;
--   rob_head := rob.rob_head;

--   rob_new.rob_insts[rob.rob_head].op := inval;
--   rob_new.rob_head := ( rob.rob_head + 1 ) % (CORE_INST_NUM + 1);
--   rob_new.num_entries := rob.num_entries - 1;
--   rob_new.state[rob.rob_head] := commit_not_sent;
--   --
--   -- # assert not needed...
--   assert (rob.num_entries >= ( 0 )) "can't remove more!";
--   return rob_new;
-- end;

-- function lq_clear_head(
--              lq : LQ;
--              --#lq_entry : ld_idx_t;
--            ) : LQ;
--   var lq_new : LQ;
--   var curr_head : ld_idx_t;
-- begin

--   lq_new := lq;
--   curr_head := lq.ld_head;

--   lq_new.ld_entries[curr_head].instruction.seq_num := 0;
--   lq_new.ld_entries[curr_head].instruction.op := inval;
--   lq_new.ld_entries[curr_head].instruction.dest_reg := 0;
--   lq_new.ld_entries[curr_head].instruction.imm := 0;
--   lq_new.ld_entries[curr_head].ld_state := await_creation;
--   lq_new.ld_entries[curr_head].commit := false;
--   lq_new.ld_entries[curr_head].read_value := 0;
--   lq_new.ld_entries[curr_head].virt_addr := 0;
--   lq_new.ld_entries[curr_head].phys_addr := 0;
--   lq_new.ld_entries[curr_head].st_seq_num := 0;
--   lq_new.ld_head := (curr_head + 1) % ( LD_ENTRY_NUM + 1);
--   lq_new.num_entries := (lq_new.num_entries - 1);

--   return lq_new;
-- end;

-- function lq_commit_head(
--              lq : LQ;
--              --#inst : INST;
--            ) : LQ;
--   var lq_new : LQ;
--   var lq_idx : ld_idx_t;
--   var lq_entry : LD_ENTRY_VALUES;
-- begin

--   lq_new := lq;
--   --#lq_idx := search_lq_seq_num_idx(lq,
--   --#                               inst.seq_num);

--   -- check what state is the load in
--   --#if (lq.ld_entries[lq_idx].ld_state = await_committed)
--   if (lq.ld_entries[lq.ld_head].ld_state = await_committed)
--     then
--     lq_new := lq_clear_head(lq_new);
--   else
--     -- simply set a flag in the ld entry
--     lq_new.ld_entries[lq.ld_head].commit := true;
--   end;
--   -- # if state is not in await commit
--   -- # then set a flag in the load entry state
--   -- # else if it is in await commit
--   -- # then set the state back to the await_creation
--   -- # state after clearing entry info

--   return lq_new;
-- end;

-- function search_lq_seq_num_idx(
--              lq : LQ;
--              seq_num : inst_count_t;
--            ) : ld_idx_t;
--   var lq_new : LQ;
--   var lq_idx : ld_idx_t;
-- begin

--   for i : ld_idx_t do
--     if (lq.ld_entries[i].instruction.seq_num = seq_num)
--       then
--       return i;
--     end;
--   end;
--   error "LQ Search: didn't find it? how? bad seq_num idx?";
-- end;

-- function search_sq_seq_num_idx(
--              sq : SQ;
--              seq_num : inst_count_t;
--            ) : sq_idx_t;
--   var sq_new : SQ;
--   var sq_idx : sq_idx_t;
--   begin

--   for i : sq_idx_t do
--     if (sq.sq_entries[i].instruction.seq_num = seq_num)
--       then
--       return i;
--     end;
--   end;
--   error "SQ Search: didn't find it? how? bad seq_num idx?";
-- end;

-- function sq_insert(
--              lq : LQ;
--              sq : SQ;
--              inst : INST;
--            ) : SQ;
--   var lq_new : LQ;
--   var sq_new : SQ;
--   var sq_tail : sq_idx_t;
-- begin
--   --
--   sq_new := sq;

--   assert sq_new.sq_entries[sq.sq_tail].st_state = st_await_creation "to insert, store should be awaiting creation";
--   sq_new.sq_entries[sq.sq_tail].st_state := st_await_scheduled;

--   lq_new := lq;
--   if (lq.num_entries != 0) then
--     --#NOTE: REMEMBER TO CLEAR ST SEQ NUM
--     --# at the end...
--     sq_new.sq_entries[sq.sq_tail].ld_seq_num := lq.ld_entries[lq.ld_tail].instruction.seq_num;
--   else
--     --# actually, if 0 entries, then the search should
--     --# just start from the head element
--     --# Treat 0 as a special symbol to start from head
--     sq_new.sq_entries[sq.sq_tail].ld_seq_num := 0;
--   end;
--   sq_new.sq_entries[sq.sq_tail].instruction := inst;
--   sq_new.sq_tail := ( sq.sq_tail + 1 ) % (SQ_ENTRY_NUM + 1);
--   sq_new.num_entries := sq.num_entries + 1;
--   --
--   assert (sq.num_entries < ( SQ_ENTRY_NUM + 1)) "can't add more!";
--   return sq_new;
-- end;

-- function sq_schedule(
--              sq : SQ;
--              seq_num : inst_count_t; --#seq_num_t;--inst_count_t;
--            ) : SQ;
--   var sq_new : SQ;
--   var sq_iter : sq_idx_t;
--   var sq_count : sq_count_t;
--   var curr_entry : SQ_ENTRY_VALUES;
--   var curr_entry_id : sq_idx_t;
--   begin
--   --
--   sq_new := sq;
--   sq_iter := sq.sq_head;
--   sq_count := sq.num_entries;

--   --#for i:0 .. lq_count do
--   --# actually interesting,
--   --# since if there's a collision
--   --# it'll likely be because we
--   --# didn't clear old LQ entries'
--   --# seq_num var!
--   --# or include condition on if
--   --# so it must be in await state?
--   --# Could use a while loop instead
--   for i:0 .. SQ_ENTRY_NUM do
--     -- error "trace load schedule?";
--     -- Use i

--     curr_entry_id := ( sq_iter + i ) % ( SQ_ENTRY_NUM + 1);
--     curr_entry := sq_new.sq_entries[curr_entry_id];
--     if (curr_entry.instruction.seq_num = seq_num)
--       then
--       -- # put "\n ================ \n";
--       -- # put "seq_num: ";
--       -- # put seq_num;
--       -- # put "\n";
--       -- # put "\n ================ \n";
--       assert ( curr_entry.st_state = st_await_scheduled ) "Should be in await scheduled?";
--       curr_entry.st_state := st_await_translation;

--       --# NOTE: For mem access stuff
--       curr_entry.virt_addr := curr_entry.instruction.imm;
--       --# NOTE TODO: Hacked in for litmus test
--       curr_entry.write_value := curr_entry.instruction.write_value;

--       sq_new.sq_entries[curr_entry_id] := curr_entry;
--       -- error "trace load schedule?";
--       return sq_new;
--     end;
--   end;
--   --
--   error "didn't find the Store to Schedule?";
--   return sq_new;
-- end;

-- function sq_clear_head(
--              sq : SQ;
--              --#lq_entry : ld_idx_t;
--            ) : SQ;
--   var sq_new : SQ;
--   var curr_head : sq_idx_t;
-- begin

--   sq_new := sq;
--   curr_head := sq.sq_head;

--   sq_new.sq_entries[curr_head].instruction.seq_num := 0;
--   sq_new.sq_entries[curr_head].instruction.op := inval;
--   sq_new.sq_entries[curr_head].instruction.dest_reg := 0;
--   sq_new.sq_entries[curr_head].instruction.imm := 0;
--   sq_new.sq_entries[curr_head].st_state := st_await_creation;
--   sq_new.sq_entries[curr_head].commit := false;
--   sq_new.sq_entries[curr_head].write_value := 0;
--   sq_new.sq_entries[curr_head].virt_addr := 0;
--   sq_new.sq_entries[curr_head].phys_addr := 0;
--   sq_new.sq_head := (curr_head + 1) % ( SQ_ENTRY_NUM + 1);
--   sq_new.num_entries := (sq_new.num_entries - 1);

--   return sq_new;
-- end;

-- function sq_commit_head(
--              sq : SQ;
--              --#inst : INST;
--            ) : SQ;
--   var sq_new : SQ;
--   var sq_idx : sq_idx_t;
--   var sq_entry : SQ_ENTRY_VALUES;
-- begin

--   sq_new := sq;
--   --#sq_idx := search_sq_seq_num_idx(sq,
--   --#                               inst.seq_num);

--   -- check what state is the load in
--   --#if (sq.sq_entries[sq_idx].sq_state = st_await_committed)
--   if (sq.sq_entries[sq.sq_head].st_state = st_await_committed)
--     then
--     --# remove head
--     sq_new := sq_clear_head(sq_new);
--   else
--     -- simply set a flag in the ld entry
--     sq_new.sq_entries[sq.sq_head].commit := true;
--   end;
--   -- # if state is not in await commit
--   -- # then set a flag in the load entry state
--   -- # else if it is in await commit
--   -- # then set the state back to the await_creation
--   -- # state after clearing entry info

--   return sq_new;
-- end;

-- function sb_insert(
--              sb : SB;
--              sq_entry : SQ_ENTRY_VALUES;
--            ) : SB;
--   var sb_new : SB;
--   var sb_tail : sb_idx_t;
--   begin
--   --
--   sb_new := sb;
--   sb_tail := sb.sb_tail;

--   assert sb_new.sb_entries[sb.sb_tail].sb_state = sb_await_creation "to insert into SB, store should be awaiting creation";
--   sb_new.sb_entries[sb.sb_tail].sb_state := sb_await_send_mem_req;

--   sb_new.sb_entries[sb.sb_tail].instruction := sq_entry.instruction;
--   sb_new.sb_entries[sb.sb_tail].virt_addr := sq_entry.virt_addr;
--   sb_new.sb_entries[sb.sb_tail].phys_addr := sq_entry.phys_addr;
--   sb_new.sb_entries[sb.sb_tail].write_value := sq_entry.write_value;
--   sb_new.sb_tail := ( sb.sb_tail + 1 ) % (SB_ENTRY_NUM + 1);
--   sb_new.num_entries := sb.num_entries + 1;
--   --
--   assert (sb.num_entries < ( SB_ENTRY_NUM + 1)) "can't add more!";
--   return sb_new;
-- end;

-- function sb_clear_head(
--              sb : SB;
--              --#lq_entry : ld_idx_t;
--            ) : SB;
--   var sb_new : SB;
--   var curr_head : sb_idx_t;
--   begin

--   sb_new := sb;
--   curr_head := sb.sb_head;

--   sb_new.sb_entries[curr_head].instruction.seq_num := 0;
--   sb_new.sb_entries[curr_head].instruction.op := inval;
--   sb_new.sb_entries[curr_head].instruction.dest_reg := 0;
--   sb_new.sb_entries[curr_head].instruction.imm := 0;
--   sb_new.sb_entries[curr_head].sb_state := sb_await_creation;
--   sb_new.sb_entries[curr_head].write_value := 0;
--   sb_new.sb_entries[curr_head].virt_addr := 0;
--   sb_new.sb_entries[curr_head].phys_addr := 0;
--   sb_new.sb_head := (curr_head + 1) % ( SB_ENTRY_NUM + 1);
--   sb_new.num_entries := (sb_new.num_entries - 1);

--   return sb_new;
-- end;

-- function insert_ld_in_mem_interface(
--              --#mem_int : MEM_INTERFACE;
--              ld_entry : LD_ENTRY_VALUES;
--              core : cores_t;
--            ) : MEM_REQ;
--   var msg : MEM_REQ;
-- begin

--   msg.addr := ld_entry.phys_addr;
--   msg.r_w := read;
--   --#msg.value := 0;
--   msg.valid := true;

--   msg.dest := mem;
--   msg.dest_id := core;
--   msg.seq_num := ld_entry.instruction.seq_num;

--   return msg;
-- end;

-- function insert_st_in_mem_interface(
--              --#mem_int : MEM_INTERFACE;
--              sb_entry : SB_ENTRY_VALUES;
--              core : cores_t;
--            ) : MEM_REQ;
--   var msg : MEM_REQ;
-- begin

--   msg.addr := sb_entry.phys_addr;
--   msg.r_w := write;
--   msg.value := sb_entry.write_value;
--   msg.valid := true;

--   msg.dest := mem;
--   msg.dest_id := core;
--   msg.seq_num := sb_entry.instruction.seq_num;

--   return msg;
-- end;

-- function insert_msg_into_ic(
--              ic : IC;
--              msg : MEM_REQ;
--            ) : IC;
--   var ic_new : IC;
-- begin
--   ic_new := ic;
--   for i : ic_idx_t do
--     if ic_new.valid[i] = false
--       then
--       ic_new.buffer[i] := msg;
--       ic_new.valid[i] := true;
--       ic_new.num_entries := ic.num_entries + 1;
--       return ic_new;
--     end;
--   end;
-- end;

-- function associative_assign_lq(
--              lq : LQ;
--              msg : MEM_REQ; --#seq_num_t;--inst_count_t;
--            ) : LQ;
--   var lq_new : LQ;
--   var lq_iter : ld_idx_t;
--   var lq_count : ld_count_t;
--   var curr_entry : LD_ENTRY_VALUES;
--   var curr_entry_id : ld_idx_t;
--   var seq_num : inst_count_t;
--   begin
--   --
--   lq_new := lq;
--   lq_iter := lq.ld_head;
--   lq_count := lq.num_entries;
--   seq_num := msg.seq_num;

--   for i:0 .. LD_ENTRY_NUM do
--     -- error "trace load schedule?";
--     -- Use i

--     curr_entry_id := ( lq_iter + i ) % ( LD_ENTRY_NUM + 1);
--     curr_entry := lq_new.ld_entries[curr_entry_id];
--     if (curr_entry.instruction.seq_num = seq_num)
--       then
--       --# NOTE: load can be in other states due to
--       --# an asynch restart sig from the St [x] -> LQ search
--       assert (( curr_entry.ld_state = squashed_await_mem_response ) | ( curr_entry.ld_state = await_mem_response ) ) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";

--       --# This causes problems
--       --# if load was reset, this will also set the state
--       --# to an incorrect state with a stale value
--       --# Checking if ld is in the await_mem_response state
--       --# also is insufficient if it reaches this state
--       --# after a reset.
--       --# Therefore, there must be some way to ignore old
--       --# requests?
--       --# Since it'd be more intractable to hunt down
--       --# a packet or request and stop it.
--       --# It's simpler to ignore messages with a time
--       --# stamp mis-match for example.
--       if ( curr_entry.ld_state = await_mem_response ) then
--         curr_entry.ld_state := write_result;
--       elsif (curr_entry.ld_state = squashed_await_mem_response) then
--         --# Complete the squash action
--         curr_entry.ld_state := await_fwd_check;
--       end;


--       --# NOTE: For mem access stuff
--       curr_entry.read_value := msg.value;

--       lq_new.ld_entries[curr_entry_id] := curr_entry;
--       -- error "trace load schedule?";
--       return lq_new;
--     end;
--   end;
--   --
--   --# NOTE: don't error, since if the LD was reset,
--   --# then this request is effectively floating
--   --# Maybe set a counter instead, to track
--   --# number of "unnecessary" LD Messages?
--   --#error "didn't find the Load to write the read val into?";
--   return lq_new;
-- end;

-- function associative_ack_sb(
--              sb : SB;
--              msg : MEM_REQ; --#seq_num_t;--inst_count_t;
--            ) : SB;
--   var sb_new : SB;
--   var sb_iter : sb_idx_t;
--   var sb_count : sb_count_t;
--   var curr_entry : SB_ENTRY_VALUES;
--   var curr_entry_id : sb_idx_t;
--   var seq_num : inst_count_t;
--   begin
--   --
--   sb_new := sb;
--   sb_iter := sb.sb_head;
--   sb_count := sb.num_entries;
--   seq_num := msg.seq_num;

--   for i:0 .. SB_ENTRY_NUM do
--     curr_entry_id := ( sb_iter + i ) % ( SB_ENTRY_NUM + 1);
--     curr_entry := sb_new.sb_entries[curr_entry_id];
--     if (curr_entry.instruction.seq_num = seq_num)
--       then
--       assert ( curr_entry.sb_state = sb_await_mem_response ) "ACK SB: Should be in await mem resp?";
--       --# curr_entry.sb_state := sb_await_creation;
--       assert (curr_entry.instruction.seq_num = sb_new.sb_entries[sb_new.sb_head].instruction.seq_num) "should be de-queuing the head!!";

--       --#sb_new.sb_entries[curr_entry_id] := curr_entry;
--       --# Should implement a de-queue operation
--       --# that's based on matching a field
--       sb_new := sb_clear_head(sb_new);

--       -- error "trace load schedule?";
--       return sb_new;
--     end;
--   end;
--   --
--   error "didn't find the Load to write the read val into?";
--   return sb_new;
-- end;


-- function store_queue_match_phys_addr_younger_than_seq_num_request
--   (
--     sq : SQ;
--     st_seq_num : inst_count_t;
--     phys_addr : addr_idx_t;
--     ld_seq_num : inst_count_t;
--     --#core : cores_t;
--   ) : SQ;
--   var sq_new : SQ;
-- begin
--   sq_new := sq;

--   --# seq sq flag, to perform the search in parallel
--   sq_new.search_busy := true;
--   sq_new.st_seq_num := st_seq_num;
--   sq_new.phys_addr := phys_addr;
--   sq_new.ld_seq_num := ld_seq_num;

--   return sq_new;
-- end;

-- function store_buffer_match_phys_addr_younger_than_seq_num_request
--   (
--     sb : SB;
--     st_seq_num : inst_count_t;
--     phys_addr : addr_idx_t;
--     ld_seq_num : inst_count_t;
--     --#core : cores_t;
--   ) : SB;
--   var sb_new : SB;
-- begin
--   sb_new := sb;

--   --# seq sq flag, to perform the search in parallel
--   sb_new.search_busy := true;
--   sb_new.phys_addr := phys_addr;
--   sb_new.ld_seq_num := ld_seq_num;

--   return sb_new;
-- end;

-- function find_st_idx_of_seq_num
--   (
--     sq : SQ;
--     seq_num : inst_count_t;
--   ) : sq_count_t;
--   var sq_entry : SQ_ENTRY_VALUES;
-- begin
--   for i : sq_idx_t do
--     sq_entry := sq.sq_entries[i];
--     if (sq_entry.instruction.seq_num = seq_num) then
--       return i;
--     end;
--   end;
--   --#return SQ_ENTRY_NUM + 1;
--   return 0;
-- end;

-- function set_load_seq_num_entry_read
--   (
--     lq : LQ;
--     --# sq : SQ;
--     seq_num : inst_count_t;
--     value : val_t;
--   ) : LQ;
--   var lq_new : LQ;
--   var ld_entry : LD_ENTRY_VALUES;
-- begin
--   lq_new := lq;
--   --# write the val to the ld entry
--   for i : ld_idx_t do
--     --# find matching st_num
--     ld_entry := lq.ld_entries[i];
--     if (ld_entry.instruction.seq_num = seq_num) then
--       lq_new.ld_entries[i].read_value := value;
--       --#lq_new.ld_entries[i].ld_state := write_result;
--       --#put "Confirm found entry";
--       --#put i;
--       --#put value;
--       --#put "\n";

--       return lq_new;
--     end;
--   end;
--   --#error "Set Load Read: load shouldn't have disappeared?";
--   --# NOTE: Comment out for now, since the requests
--   --# can "linger" even after the St and Ld have both
--   --# exited their Queues
--   --# leading to no load in the LQ
--   return lq_new;
-- end;

-- function set_load_state_to_state
--   (
--     lq : LQ;
--     seq_num : inst_count_t;
--     state : LD_ENTRY_STATE;
--   ) : LQ;
--   var lq_new : LQ;
--   var ld_entry : LD_ENTRY_VALUES;
-- begin
--   lq_new := lq;
--   --# write the val to the ld entry
--   for i : ld_idx_t do
--     ld_entry := lq.ld_entries[i];
--     if (ld_entry.instruction.seq_num = seq_num) then
--       lq_new.ld_entries[i].ld_state := state;

--       return lq_new;
--     end;
--   end;
--   --#error "Set Load State: load shouldn't have disappeared?";
--   return lq_new;
-- end;

-- function assert_load_state_is_state
--   (
--     lq : LQ;
--     seq_num : inst_count_t;
--     state : LD_ENTRY_STATE;
--   ) : boolean;
--   var lq_new : LQ;
--   var ld_entry : LD_ENTRY_VALUES;
-- begin
--   lq_new := lq;
--   --# write the val to the ld entry
--   for i : ld_idx_t do
--     --# find matching st_num
--     ld_entry := lq.ld_entries[i];
--     if (ld_entry.instruction.seq_num = seq_num) then
--       assert (lq_new.ld_entries[i].ld_state = state) "bad state";

--       --#return lq_new;
--       return false;
--     end;
--   end;
--   --#error "load shouldn't have disappeared?";
--   return true;
-- end;

-- function init_state_fn () : STATE;
--   var init_state : STATE;
-- begin

--   undefine init_state;

--   --# Memory & Interconnect
--   alias mem:init_state.mem_ do
--     for i : addr_idx_t do
--       mem.arr[i] := 0;
--     end;
--     --#mem.msg 
--   endalias;

--   alias ic: init_state.ic_ do
--     for i : ic_idx_t do
--       --# init message entries
--       ic.buffer[i].addr := 0;
--       ic.buffer[i].r_w := read;
--       ic.buffer[i].value := 0;
--       --# Key thing. invalid message
--       ic.buffer[i].valid := false;

--       --# Destination info
--       --# Core or Mem
--       --# Core has seq_num
--       ic.buffer[i].dest := mem;
--       ic.buffer[i].dest_id := 0;
--       ic.buffer[i].seq_num := 0;

--       --# also have invalid for IC
--       --# could combine with msg
--       --# valid flag
--       ic.valid[i] := false;
--     end;
--     ic.num_entries := 0;
--   end;

--   for core : cores_t do
--     --# Mem Interface
--     alias mem_int:init_state.core_[core].mem_interface_ do
--       --#init the mem interfaces
--       mem_int.out_msg.addr := 0;
--       mem_int.out_msg.r_w := read;
--       mem_int.out_msg.value := 0;
--       --# Key thing. invalid message
--       mem_int.out_msg.valid := false;

--       --# Destination info
--       --# Core or Mem
--       --# Core has seq_num
--       mem_int.out_msg.dest := mem;
--       mem_int.out_msg.dest_id := 0;
--       mem_int.out_msg.seq_num := 0;

--       mem_int.in_msg.addr := 0;
--       mem_int.in_msg.r_w := read;
--       mem_int.in_msg.value := 0;
--       mem_int.in_msg.valid := false;
--       mem_int.in_msg.dest := mem;
--       mem_int.in_msg.dest_id := 0;
--       mem_int.in_msg.seq_num := 0;

--       mem_int.out_busy := false;
--       mem_int.in_busy := false;
--     end;

--     -- #Load Queue
--     alias lq:init_state.core_[core].lsq_.lq_ do
--       for i : LD_ENTRY do
--         -- assume imm insts for now in litmus tests
--         lq.ld_entries[i].instruction.seq_num := 0;
--         lq.ld_entries[i].instruction.op := inval;
--         lq.ld_entries[i].instruction.imm := 0;
--         lq.ld_entries[i].instruction.dest_reg := 0;

--         lq.ld_entries[i].read_value := 0;
--         lq.ld_entries[i].virt_addr := 0;
--         lq.ld_entries[i].phys_addr := 0;
--         lq.ld_entries[i].commit := false;
--         lq.ld_entries[i].st_seq_num := 0;
--         -- # Technically, this is init'd
--         -- # by setting things to 0
--         -- # so.. move on to next state
--         lq.ld_entries[i].ld_state := await_creation;
--       end;
--       lq.ld_head := 0;
--       lq.ld_tail := 0;
--       lq.num_entries := 0;

--       lq.search_busy := false;
--       lq.st_seq_num := 0;
--       lq.phys_addr := 0;
--       lq.ld_seq_num := 0;
--     end;
--     alias rename:init_state.core_[core].rename_ do
--       for i : 0 .. CORE_INST_NUM do
--         rename.test_insts[i].op := inval;
--         rename.test_insts[i].seq_num := 0;
--       end;
--       rename.rename_head := 0;
--       rename.rename_tail := 0;
--       rename.num_entries := 0;
--     end;
--     alias iq:init_state.core_[core].iq_ do
--       for i : 0 .. CORE_INST_NUM do
--         iq.iq_insts[i].op := inval;
--         iq.iq_insts[i].seq_num := 0;
--         iq.iq_valid[i] := invalid;
--       end;
--       -- # iq.iq_head := 0;
--       -- # iq.iq_tail := 0;
--       iq.num_entries := 0;
--       -- iq.iq_valid[CORE_INST_NUM] := ready;
--       -- iq.iq_valid[CORE_INST_NUM-1] := ready;
--     end;
--     alias rf:init_state.core_[core].rf_ do
--       for i : reg_idx_t do
--         rf.rf[i] := 0;
--       end;
--     end;
--     alias rob:init_state.core_[core].rob_ do
--       for i : 0 .. CORE_INST_NUM do
--         rob.rob_insts[i].op := inval;
--         rob.rob_insts[i].seq_num := 0;
--         rob.state[i] := commit_not_sent;
--       end;
--       rob.rob_head := 0;
--       rob.rob_tail := 0;
--       rob.num_entries := 0;
--     end;
--     alias sq:init_state.core_[core].lsq_.sq_ do
--       for i : sq_idx_t do
--         --# assume imm insts for now in litmus tests
--         sq.sq_entries[i].instruction.seq_num := 0;
--         sq.sq_entries[i].instruction.op := inval;
--         sq.sq_entries[i].instruction.imm := 0;
--         sq.sq_entries[i].instruction.dest_reg := 0;

--         sq.sq_entries[i].write_value := 0;
--         sq.sq_entries[i].virt_addr := 0;
--         sq.sq_entries[i].phys_addr := 0;
--         sq.sq_entries[i].commit := false;
--         -- # Technically, this is init'd
--         -- # by setting things to 0
--         -- # so.. move on to next state
--         sq.sq_entries[i].st_state := st_await_creation;
--       end;
--       sq.sq_head := 0;
--       sq.sq_tail := 0;
--       sq.num_entries := 0;

--       --# stuff for searching for fwding
--       sq.search_busy := false;
--     end;
--     alias sb:init_state.core_[core].sb_ do
--       for i : sb_idx_t do
--         --# assume imm insts for now in litmus tests
--         sb.sb_entries[i].instruction.seq_num := 0;
--         sb.sb_entries[i].instruction.op := inval;
--         sb.sb_entries[i].instruction.imm := 0;
--         sb.sb_entries[i].instruction.dest_reg := 0;

--         sb.sb_entries[i].write_value := 0;
--         sb.sb_entries[i].virt_addr := 0;
--         sb.sb_entries[i].phys_addr := 0;
--         -- # Technically, this is init'd
--         -- # by setting things to 0
--         -- # so.. move on to next state
--         sb.sb_entries[i].sb_state := sb_await_creation;
--       end;
--       sb.sb_head := 0;
--       sb.sb_tail := 0;
--       sb.num_entries := 0;

--       sb.search_busy := false;
--       sb.phys_addr := 0;
--       sb.ld_seq_num := 0;
--     end;
--   end;

--   -- # set up litmus test
--   alias rename_c0:init_state.core_[0].rename_ do
--     --#for i : 0 .. CORE_INST_NUM do
--     --#  rename.test_insts[i].op := inval;
--     --#  rename.test_insts[i].seq_num := 0;
--     --#end;

--     --#--# amd1 test
--     --#rename_c0.test_insts[0].op := st;
--     --#rename_c0.test_insts[0].seq_num := 1;
--     --#rename_c0.test_insts[0].dest_reg := 0;
--     --#rename_c0.test_insts[0].imm := 0; --# Addr
--     --#rename_c0.test_insts[0].write_value := 1;

--     --#rename_c0.test_insts[1].op := st;
--     --#rename_c0.test_insts[1].seq_num := 2;
--     --#rename_c0.test_insts[1].dest_reg := 1;
--     --#rename_c0.test_insts[1].imm := 1; --# Addr
--     --#rename_c0.test_insts[1].write_value := 1;

--     --# iwp23b1 test
--     rename_c0.test_insts[0].op := st;
--     rename_c0.test_insts[0].seq_num := 1;
--     rename_c0.test_insts[0].dest_reg := 0;
--     rename_c0.test_insts[0].imm := 0; --# Addr
--     rename_c0.test_insts[0].write_value := 1;

--     rename_c0.test_insts[1].op := ld;
--     rename_c0.test_insts[1].seq_num := 2;
--     rename_c0.test_insts[1].dest_reg := 1;
--     rename_c0.test_insts[1].imm := 0; --# Addr
--     --#rename_c0.test_insts[1].write_value := 0;

--     rename_c0.rename_head := 0;
--     rename_c0.rename_tail := 0;
--     rename_c0.num_entries := 2;
--   end;
--   alias rename_c1:init_state.core_[1].rename_ do
--     --#for i : 0 .. CORE_INST_NUM do
--     --#  rename.test_insts[i].op := inval;
--     --#  rename.test_insts[i].seq_num := 0;
--     --#end;

--     --#--# amd1 test
--     --#rename_c1.test_insts[0].op := ld;
--     --#rename_c1.test_insts[0].seq_num := 1;
--     --#rename_c1.test_insts[0].dest_reg := 0;
--     --#rename_c1.test_insts[0].imm := 1;
--     --#--#rename_c1.test_insts[0].write_value := 0;

--     --#rename_c1.test_insts[1].op := ld;
--     --#rename_c1.test_insts[1].seq_num := 2;
--     --#rename_c1.test_insts[1].dest_reg := 1;
--     --#rename_c1.test_insts[1].imm := 0;
--     --#--#rename_c1.test_insts[1].write_value := 0;

--     --# iwp23b1 test
--     rename_c1.test_insts[0].op := st;
--     rename_c1.test_insts[0].seq_num := 1;
--     rename_c1.test_insts[0].dest_reg := 0;
--     rename_c1.test_insts[0].imm := 1;
--     rename_c1.test_insts[0].write_value := 1;

--     rename_c1.test_insts[1].op := ld;
--     rename_c1.test_insts[1].seq_num := 2;
--     rename_c1.test_insts[1].dest_reg := 1;
--     rename_c1.test_insts[1].imm := 1;
--     --#rename_c1.test_insts[1].write_value := 0;

--     rename_c1.rename_head := 0;
--     rename_c1.rename_tail := 0;
--     rename_c1.num_entries := 2;
--   end;

--   return init_state;
-- end;

-- -- # func for iq remove an elem..
-- -- # but an arbitrary elem?
-- -- # So maybe we don't need "head"
-- -- # and "tail" per se?
-- -- # just a way to add elems & remove them?
-- -- # and know when it's full?
-- -- # so a counter?

-- -- # So IQ should:
-- -- # randomly choose an elem to "schedule"
-- -- # and then remove it from the buffer

-- -- function iq_

-- -- # ------------ RULES --------------------
-- £rules
-- -- init state --

-- -- #symmetry for scalar set checks
-- -- #just use normal for loop
-- -- ruleset i : LD_ENTRY do
-- -- rule
-- startstate "init"
--   undefine Sta;

--   Sta := init_state_fn();
-- end;

-- -- # alias ld_sm:Sta.core_[j].lsq_.lq_[i] do
-- -- #   -- move from curr state to next state
-- -- #   -- might be useful to use a table here
-- -- #   if ld_sm.ld_state = init then
-- -- #     ld_sm.ld_state := await_creation;
-- -- #   elsif ld_sm.ld_state = build_packet then
-- -- #     ld_sm.ld_state := send_memory_request;
-- -- #   elsif ld_sm.ld_state = send_memory_request then
-- -- #     ld_sm.ld_state := await_mem_response;
-- -- #   elsif ld_sm.ld_state = write_result then
-- -- #     ld_sm.ld_state := await_comitted;
-- -- #   end;
-- -- # end;

-- -- # ====== Actual States ========

-- --# ruleset i : LD_ENTRY do
-- --#   -- # progress thru ld states
-- --#   -- # for states that are
-- --#   -- # "await" on sth,
-- --#   -- # another rule needs to be
-- --#   -- # written to change the state
-- --# rule "init_to_await_insert"
-- --#   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = init
-- --# ==>
-- --#   -- decls
-- --#   var next_state : STATE;
-- --#   var ld_entry : LD_ENTRY_VALUES;
-- --# begin
-- --#   next_state := Sta;
-- --#   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
-- --#   ld_entry.ld_state := await_creation;
-- --#   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
-- --# 
-- --#   Sta := next_state;
-- --# end; end;

-- --------------------- LQ TRANSITIONS -----------------
-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
-- rule "await_fwd_check_to_await_store_queue_response"
--   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_fwd_check
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;
--   var sq : SQ;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
--   sq := Sta.core_[j].lsq_.sq_;

--   --# if no valid seq num, go to translate & build_packet
--   --# NOTE: This means to skip to the check SB instead!
--   if (ld_entry.st_seq_num != 0) then
--     --# But I want to split the transitions (& behaviour),
--     --# this can happen in interleaved order
--     --# So send a message to SQ first, and there'll be
--     --# another transition after to message back
--     --# TODO NOTE: Add a check on number of elements.
--     if (sq.search_busy = false) then
--       sq := store_queue_match_phys_addr_younger_than_seq_num_request(
--           sq,
--           ld_entry.st_seq_num,
--           ld_entry.phys_addr,
--           ld_entry.instruction.seq_num
--         );
--       next_state.core_[j].lsq_.sq_ := sq;

--       --# TODO: Implement transition for the SQ to check & return
--       --# SQ transition moves this transition to next state,
--       --# setting the relevant LQ entry's regs.. in then when
--       --# statement
--       --# The send it to LQ entry can be "atomic" due to
--       --# the associativity,
--       --# otherwise we can also create an interface as well
--       --# "busy" or "not_busy".
--       ld_entry.ld_state := await_check_forwarding_or_load_response;
--     else
--       --# NOTE: This is more auto-generated
--       --# contention stuff...
--       --# Try again when resource free.
--       ld_entry.ld_state := await_fwd_check;
--     endif;
--   else
--     --# go to build_packet!
--     --#ld_entry.ld_state := build_packet;
--     --# NOTE: No! go to check SB!
--     ld_entry.ld_state := await_sb_fwd_check;
--   endif;

--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- ------# NOTE: SQ TRANSITION, MOVE to SQ SECTION LATER ------
-- ruleset j : cores_t do
-- --#ruleset i : LD_ENTRY do
-- rule "sq_process_search_request"
-- --# Condition if there's a search request made
-- Sta.core_[j].lsq_.sq_.search_busy = true
-- ==>
-- -- decls
--   var next_state : STATE;
--   var sq : SQ;
--   var lq : LQ;
--   var st_idx : sq_count_t;
--   var offset : sq_idx_t;
--   var difference : sq_idx_t;
--   var curr_idx : sq_idx_t;
--   var value : val_t;
--   var while_break : boolean;
--   var assert_check : boolean;
-- begin
--   next_state := Sta;
--   sq := Sta.core_[j].lsq_.sq_;
--   lq := Sta.core_[j].lsq_.lq_;
--   while_break := false;

--   --# TODO:
--   --# do the search in the SQ
--   --# and return the value to the load
--   --# Move the load into the next state

--   --# (1) find the st index w/ st_seq_num
--   --# If it exists, do step (2)
--   --# put "=====\n";
--   --# put sq.num_entries;
--   if (sq.num_entries = 0) then
--     while_break := true;
--     lq := set_load_state_to_state(lq,
--                                   Sta.core_[j].lsq_.sq_.ld_seq_num,
--                                   await_sb_fwd_check);
--   endif;
--   st_idx := find_st_idx_of_seq_num(sq,
--                                    sq.st_seq_num);
--   --# put st_idx;
--   --# Treat ( SQ_ENTRY_NUM + 1 ) as not found
--   --#put "============\n";
--   --#put "st_idx: ";
--   --#put st_idx;
--   --#put "\n";

--   --#if ( st_idx != ( SQ_ENTRY_NUM + 1 ) ) then
--   difference := ( st_idx + ( SQ_ENTRY_NUM + 1) - sq.sq_head ) % ( SQ_ENTRY_NUM + 1);
--   offset := 0;
--   --#put "difference: ";
--   --#put difference;
--   --#put "\n";
--   while ( (offset <= difference) & (while_break = false) ) do
--     --#put "offset: ";
--     --#put offset;
--     --#put "\n";
--     curr_idx := ( st_idx + ( SQ_ENTRY_NUM + 1) - offset ) % ( SQ_ENTRY_NUM + 1);
--     if (sq.sq_entries[curr_idx].phys_addr
--         =
--         sq.phys_addr) then
--       --#st_idx := i;
--       --# (2) return val to the load
--       value := sq.sq_entries[curr_idx].write_value;
--       --# func to write to load's read val
--       --# This is the when block
--       --# NOTE: This function also advances the load's state
--       lq := set_load_seq_num_entry_read(lq,
--                                         sq.ld_seq_num,
--                                         value);
--       --#put "value: ";
--       --#put value;
--       --#put "\n";

--       --# NOTE: Commented out the assert check
--       --# Since the reset from the St [x] -> LQ check
--       --# resets the state

--       --#assert_check := assert_load_state_is_state(lq,
--       --#                           Sta.core_[j].lsq_.sq_.ld_seq_num,
--       --#                           await_check_forwarding_or_load_response
--       --#                          );
--       lq := set_load_state_to_state(lq,
--                                     Sta.core_[j].lsq_.sq_.ld_seq_num,
--                                     write_result);

--       --# Break out of the while loop
--       --#offset := difference + 1;
--       while_break := true;
--     endif;

--     --#if (while_break = true) then
--     --#  offset := difference + 1;
--     --#else
--     if (offset != SQ_ENTRY_NUM) then
--       offset := offset + 1;
--     else
--       while_break := true;
--     endif;
--   end;
--   --#end;

--   --# NOTE:
--   --# If SQ does not contain any entries, check SB
--   --# Set this in motion by moving the load to the next
--   --# stage..
--   --# Interestingly, the SQ could forward the request
--   --# to the SB as an optimization.
--   --# But it's better to keep to the DSL code for now
--   --# Change this check to offset = SQ_ENTRY_NUM
--   if (while_break = false) then
--     lq := set_load_state_to_state(lq,
--                                   Sta.core_[j].lsq_.sq_.ld_seq_num,
--                                   await_sb_fwd_check);
--   endif;

--   --# whether it was found or not, the request has been
--   --# completed
--   sq.search_busy := false;
--   sq.ld_seq_num := 0;
--   sq.st_seq_num := 0;
--   sq.phys_addr := 0;

--   --# (3) otherwise "return" fail
--   --# TODO NOTE: Make this search the store buffer as well!!!
--   --# all entries that is..
--   --#lq := set_load_state_to_state(lq,
--   --#                              Sta.core_[j].lsq_.sq_.ld_seq_num,
--   --#                              build_packet);

--   next_state.core_[j].lsq_.sq_ := sq;
--   next_state.core_[j].lsq_.lq_ := lq;

--   Sta := next_state;
-- end; --#end;
-- endruleset;

-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
-- rule "await_sb_fwd_check_to_await_sb_response"
--   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_sb_fwd_check
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;
--   var sb : SB;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
--   sb := Sta.core_[j].sb_;

--   --# TODO NOTE don't send request if buffer is empty!
--   --# Since the transition changes this load's state
--   --# This can mess with the load later..
--   if (sb.search_busy = false) then
--     sb := store_buffer_match_phys_addr_younger_than_seq_num_request(
--         sb,
--         ld_entry.st_seq_num,
--         ld_entry.phys_addr,
--         ld_entry.instruction.seq_num
--       );
--     next_state.core_[j].sb_ := sb;

--     ld_entry.ld_state := await_sb_fwd_check_response;
--   else
--     --# NOTE: This is more auto-generated
--     --# contention stuff...
--     --# Try again when resource free.
--     --# NOTE: doesn't need to be here,
--     --# Transition could have a guard
--     --# condition on the core # j's SB
--     --# search busy boolean flag
--     --# which is more efficient...
--     ld_entry.ld_state := await_sb_fwd_check;
--   endif;

--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- ------# NOTE: SB TRANSITION, MOVE to SB SECTION LATER ------
-- ruleset j : cores_t do
-- --#ruleset i : LD_ENTRY do
-- rule "sb_process_search_request"
-- --# Condition if there's a search request made
-- Sta.core_[j].sb_.search_busy = true
-- ==>
-- -- decls
--   var next_state : STATE;
--   var sb : SB;
--   var lq : LQ;
--   var st_idx : sb_idx_t;
--   var offset : sb_idx_t;
--   var difference : sb_idx_t;
--   var curr_idx : sb_idx_t;
--   var value : val_t;
--   var while_break : boolean;
--   var assert_check : boolean;
-- begin
--   next_state := Sta;
--   sb := Sta.core_[j].sb_;
--   lq := Sta.core_[j].lsq_.lq_;

--   --# TODO:
--   --# do the search in the SB
--   --# and return the value to the load
--   --# Move the load into the next state

--   --# (1) find the st index w/ st_seq_num
--   --# If it exists, do step (2)

--   --#st_idx := find_st_idx_of_seq_num(sb,
--   --#                                 sb.st_seq_num);
--   --# Instead we just try to find the youngest store
--   --# to fwd from
--   while_break := false;

--   st_idx := ( sb.sb_tail + ( SB_ENTRY_NUM + 1) - 1 ) % ( SB_ENTRY_NUM + 1);
--   if (sb.num_entries = 0) then
--     while_break := true;
--     lq := set_load_state_to_state(lq,
--                                   Sta.core_[j].sb_.ld_seq_num,
--                                   build_packet);
--   endif;

--   difference := ( st_idx + ( SB_ENTRY_NUM + 1) - sb.sb_head ) % ( SB_ENTRY_NUM + 1);
--   offset := 0;
--   while ( (offset <= difference) & (while_break = false) )  do
--     curr_idx := ( st_idx + ( SB_ENTRY_NUM + 1 ) - offset ) % ( SB_ENTRY_NUM + 1);
--     if (sb.sb_entries[curr_idx].phys_addr
--         =
--         sb.phys_addr) then
--       --#st_idx := i;
--       --# (2) return val to the load
--       value := sb.sb_entries[curr_idx].write_value;
--       --#put "1=======\n";
--       --#put value;
--       --# func to write to load's read val
--       --# This is the when block
--       --# NOTE: This function also advances the load's state

--       lq := set_load_seq_num_entry_read(lq,
--                                         sb.ld_seq_num,
--                                         value);
--       --#put "2=======\n";
--       --#put lq;
--       --# Consider adjust the fn into a "procedure"
--       --#NOTE: Assuming this is similar to the SQ case
--       --# And the St [x] -> LQ Search
--       --# request can reset the Load's state

--       --# actually, after this resets the load's state
--       --# and it will retry, is there any problem?
--       --# there shouldn't be?
--       --# The transitions should give up if no elems in
--       --# SB or SQ?

--       --#assert_check := assert_load_state_is_state(lq,
--       --#                           Sta.core_[j].sb_.ld_seq_num,
--       --#                           await_sb_fwd_check_response
--       --#                          );
--       lq := set_load_state_to_state(lq,
--                                     Sta.core_[j].sb_.ld_seq_num,
--                                     write_result);

--       --# Break out of the while loop
--       --#offset := difference + 1;
--       while_break := true;
--     endif;
--     if ( offset != SB_ENTRY_NUM ) then
--       offset := offset + 1;
--     else
--       while_break := true;
--     endif;
--   end;

--   --# Change this check to if offset = SB_ENTRY_NUM
--   if (while_break = false) then
--     lq := set_load_state_to_state(lq,
--                                   Sta.core_[j].sb_.ld_seq_num,
--                                   build_packet);
--   endif;

--   --# whether sth was found or not, the request has
--   --# been completed
--   sb.search_busy := false;
--   sb.ld_seq_num := 0;
--   sb.phys_addr := 0;


--   next_state.core_[j].sb_ := sb;
--   next_state.core_[j].lsq_.lq_ := lq;

--   Sta := next_state;
-- end; --#end;
-- endruleset;


-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
-- rule "await_translation_to_build_packet"
--   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_translation
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];

--   --# "translate" the address
--   --#NOTE TODO: access tlb? not quite necessary for litmus tests
--   ld_entry.phys_addr := ld_entry.virt_addr;

--   ld_entry.ld_state := await_fwd_check;

--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

--   Sta := next_state;
-- end; end;
-- endruleset;


-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
-- rule "build_packet_to_send_mem_req"
-- (
--   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = build_packet
-- )

-- ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
--   ld_entry.ld_state := send_memory_request;
--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- --#NOTE: Marking this transition as the mem access one!
-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
--   rule "send_mem_req_to_await_mem_resp"
--   ( Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = send_memory_request )
--   --# this is a condition: act only if ld ahead has completed
--   &
--   (
--     --# if this isn't the head inst
--     ( i != Sta.core_[j].lsq_.lq_.ld_head)
--     --# then ensure the next inst is in a state
--     --# where it's already finished reading from mem
--     ->
--     (
--       --# NOTE: Removed this condition, since it's not ready yet
--       --# ( Sta.core_[j].lsq_.lq_.ld_entries[( i + LD_ENTRY_NUM - 1 ) % LD_ENTRY_NUM].ld_state
--       --# =
--       --# await_mem_response
--       --# )
--       --# |
--       ( Sta.core_[j].lsq_.lq_.ld_entries[( i + LD_ENTRY_NUM - 1 ) % LD_ENTRY_NUM].ld_state
--       =
--       write_result
--       )
--       |
--       ( Sta.core_[j].lsq_.lq_.ld_entries[( i + LD_ENTRY_NUM - 1 ) % LD_ENTRY_NUM].ld_state
--       =
--       await_committed
--       )
--     )
--   )
-- --# --# Add condition that there is a
-- --# --# free slot in the Inter-conn to mem
-- --#   &
-- --#   (
-- --#     Sta.ic_.num_entries < (IC_ENTRY_NUM+1)
-- --#   )

--   --# this is a condition: mem's msg buffer is empty
--   --# i.e. mem is free
--   &
--   (
--     Sta.core_[j].mem_interface_.out_busy = false
--   )
--   ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;

--   --# var mem : MEM_ARRAY;
--   var phys_addr : addr_idx_t;

--   --# Hacked hacky
--   --# var rf : REG_FILE;

--   var mem_inter : MEM_INTERFACE;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
--   --# mem := Sta.mem_;
--   --# rf := Sta.core_[j].rf_;
--   mem_inter := Sta.core_[j].mem_interface_;

--   -- Update state
--   ld_entry.ld_state := await_mem_response;

--   -- Read from memory
--   phys_addr := ld_entry.phys_addr;
--   --# ld_entry.read_value := mem.arr[phys_addr];
--   --# hacked hacky
--   --# rf.rf[ld_entry.instruction.dest_reg] := ld_entry.read_value;

--   --# NOTE: send to the mem interface
--   mem_inter.out_msg := insert_ld_in_mem_interface(
--                                                   ld_entry,
--                                                   j
--                                                  );
--   mem_inter.out_busy := true;

--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
--   next_state.core_[j].mem_interface_ := mem_inter;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- -- # ====== Shortcut/Testing States =====
-- --# NOTE: Receive mem response?
-- --# ruleset j : cores_t do
-- --# ruleset i : LD_ENTRY do
-- --# rule "await_mem_resp_to_writeback"
-- --#   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_mem_response
-- --# ==>
-- --# -- decls
-- --#   var next_state : STATE;
-- --#   var ld_entry : LD_ENTRY_VALUES;
-- --#   begin
-- --#     next_state := Sta;
-- --#   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
-- --#   ld_entry.ld_state := write_result;
-- --#   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
-- --# 
-- --#   Sta := next_state;
-- --# end; end;
-- --# endruleset;

-- ruleset j : cores_t do
-- ruleset i : LD_ENTRY do
-- rule "writeback_to_await_committed"
--   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = write_result
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ld_entry : LD_ENTRY_VALUES;
--   var rf : REG_FILE;
--   var dest_reg : reg_idx_t;
-- begin
--   next_state := Sta;
--   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
--   rf := Sta.core_[j].rf_;
--   dest_reg := ld_entry.instruction.dest_reg;

--   rf.rf[dest_reg] := ld_entry.read_value;
--   ld_entry.ld_state := await_committed;

--   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
--   next_state.core_[j].rf_ := rf;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- -- change the state of await states


-- --# ruleset i : LD_ENTRY do
-- --#   rule "await_comitted_to_un_alloc"
-- --#   Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_committed
-- --#   ==>
-- --#   -- decls
-- --#   var next_state : STATE;
-- --#   var ld_entry : LD_ENTRY_VALUES;
-- --# begin
-- --#   next_state := Sta;
-- --#   ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];
-- --#   ld_entry.ld_state := init;
-- --#   next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;
-- --#
-- --#   Sta := next_state;
-- --# end; end;
-- -- # TODO NOTE: also unalloc the load
-- -- #for now at least..

-- --------------------- END LQ TRANSITIONS -----------------

-- ------------- MEMORY TRANSITIONS -----------------
-- --# Write the memory interface stuff

-- --# Insert from a mem interface into the IC
-- ruleset j : cores_t do
-- rule "move_msg_from_mem_interface_to_ic"
--   ( Sta.core_[j].mem_interface_.out_busy = true )
--   &
--   ( Sta.ic_.num_entries < ( IC_ENTRY_NUM + 1 ) )
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ic : IC;
--   var mem_int : MEM_INTERFACE;
-- begin
--   next_state := Sta;
--   --# move msg into ic from mem_int
--   mem_int := Sta.core_[j].mem_interface_;
--   ic := Sta.ic_;

--   --# copy msg into ic
--   ic := insert_msg_into_ic(ic, mem_int.out_msg);

--   mem_int.out_busy := false;

--   next_state.core_[j].mem_interface_ := mem_int;
--   next_state.ic_ := ic;

--   Sta := next_state;
-- end;
-- endruleset;

-- --# Choose an IC msg to perform it's access operation
-- ruleset i : ic_idx_t do
-- rule "perform_ic_msg"
--   --# have some ic entries
--   ( Sta.ic_.num_entries > 0 )
--   --# this particular entry is valid
--   &
--   ( Sta.ic_.valid[i] = true )
--   --# this is going to memory
--   &
--   ( Sta.ic_.buffer[i].dest = mem )
-- ==>
-- -- decls
--   var next_state : STATE;
--   var ic : IC;
--   var mem : MEM_ARRAY;
--   var addr : addr_idx_t;
-- begin
--   --# setup
--   next_state := Sta;
--   ic := Sta.ic_;
--   mem := Sta.mem_;
--   addr := ic.buffer[i].addr;

--   --# perform the access
--   if ( ic.buffer[i].r_w = read )
--     then
--     ic.buffer[i].value := mem.arr[addr];
--   elsif (ic.buffer[i].r_w = write)
--     then
--     mem.arr[addr] := ic.buffer[i].value;
--   endif;

--   --# Reverse direction for acknowledgement
--   ic.buffer[i].dest := core;

--   next_state.ic_ := ic;
--   next_state.mem_ := mem;

--   Sta := next_state;
-- end;
-- endruleset;

-- --# Choose an IC msg to return it's acknowledgement
-- ruleset j : cores_t do
-- ruleset i : ic_idx_t do
-- rule "acknowledge_ic_msg"
--   --# have some ic entries
--   ( Sta.ic_.num_entries > 0 )
--   --# this particular entry is valid
--   &
--   ( Sta.ic_.valid[i] = true )
--   --# this is going to a core
--   &
--   ( Sta.ic_.buffer[i].dest = core )
--   --# Current core j is the dest core
--   &
--   ( j = Sta.ic_.buffer[i].dest_id )
--   --# receiving mem interface is not busy
--   &
--   ( Sta.core_[j].mem_interface_.in_busy = false )
-- ==>
--   -- decls
--   var next_state : STATE;
--   var ic : IC;
--   var mem_interface : MEM_INTERFACE;
-- begin
--   --# setup
--   next_state := Sta;
--   ic := Sta.ic_;
--   mem_interface := Sta.core_[j].mem_interface_;

--   mem_interface.in_msg := ic.buffer[i];
--   mem_interface.in_busy := true;

--   ic.buffer[i].addr := 0;
--   ic.buffer[i].r_w := read;
--   ic.buffer[i].value := 0;
--   ic.buffer[i].valid := false;
--   ic.buffer[i].dest := mem;
--   ic.buffer[i].dest_id := 0;
--   ic.valid[i] := false;
--   ic.num_entries := ic.num_entries - 1;

--   next_state.ic_ := ic;
--   next_state.core_[j].mem_interface_ := mem_interface;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- --# Core checks input msgs to notify dest structure
-- ruleset j : cores_t do
--   rule "core_sends_in_msg_ack_to_structures"
--   ( Sta.core_[j].mem_interface_.in_busy = true )
-- ==>
--   --# Decls
--   var next_state : STATE;
--   var lq : LQ;
--   var sb : SB;
--   var mem_interface : MEM_INTERFACE;
-- begin
--   next_state := Sta;
--   lq := Sta.core_[j].lsq_.lq_;
--   sb := Sta.core_[j].sb_;
--   mem_interface := Sta.core_[j].mem_interface_;

--   if ( mem_interface.in_msg.r_w = read )
--     then
--     lq := associative_assign_lq(lq, mem_interface.in_msg);
--   elsif ( mem_interface.in_msg.r_w = write )
--     then
--     --# advance SB state to ack'd
--     --# basically clear'd
--     sb := associative_ack_sb(sb, mem_interface.in_msg);
--   endif;

--   mem_interface.in_busy := false;

--   next_state.core_[j].lsq_.lq_ := lq;
--   next_state.core_[j].sb_ := sb;
--   next_state.core_[j].mem_interface_ := mem_interface;

--   Sta := next_state;
-- end;
-- endruleset;

-- ------------- END MEMORY TRANSITIONS -----------------

-- ------------------ SQ STATE TRANSITIONS -----------------
-- ruleset j : cores_t do
-- ruleset i : sq_idx_t do
-- rule "st_await_translation_to_await_squash"
--   ( Sta.core_[j].lsq_.sq_.sq_entries[i].st_state = st_await_translation )
--   &
--   (Sta.core_[j].lsq_.lq_.search_busy = false)
-- ==>
--   -- decls
--   var next_state : STATE;
--   var sq_entry : SQ_ENTRY_VALUES;
--   var lq : LQ;
-- begin
--   next_state := Sta;
--   sq_entry := Sta.core_[j].lsq_.sq_.sq_entries[i];
--   lq := Sta.core_[j].lsq_.lq_;

--   --#NOTE TODO: access tlb? not quite necessary for litmus tests
--   sq_entry.phys_addr := sq_entry.virt_addr;

--   --# perform LQ search and squash
--   --# Technically don't need to await "completion"
--   --# from the LQ

--   --# NOTE
--   --# This could be an asynchronous event
--   --# However, we also want this to be served in due time
--   --# The consequences of not doing so mean that:
--   --# speculative loads that go before a fwding
--   --# store has it's phys addr ready will not be squashed
--   --# on time
--   --# (1)Thus we must wait until this request has completed
--   --# in order to stall the pipeline,
--   --# (2)Alternatively, we must specify a clock system
--   --# to simulate these things happening concurrently,
--   --# but this requires low level details that aren't
--   --# strictly available or required at a higher level
--   --# (3)Or mark another point in this store's SM
--   --# as the latest point which it can let this
--   --# LQ search be loosely scheduled/interleaved
--   --# (4) But this does bring up a good point
--   --# perhaps some events can be executed lazily
--   --# and just need to complete by some time
--   --# -> meaning there's some flexible scheduling
--   --# or some scheduling we can do here to "optimize"
--   --# the implementation or exact point where it does
--   --# start or must synch by
--   --# TODO: put in the LQ search here.
--   --# NOTE: in the LQ search processing transition
--   --# DURING A "SQUASH": remember to
--   --# Remove in-flight SQ and SB requests,
--   --# As these change the load's state
--   --# and the current implementation does not
--   --# stop if the load is in a reset'd state.
--   lq.st_seq_num := sq_entry.instruction.seq_num;
--   lq.ld_seq_num := sq_entry.ld_seq_num;
--   lq.phys_addr := sq_entry.phys_addr;
--   lq.search_busy := true;

--   --#sq_entry.st_state := st_build_packet;
--   sq_entry.st_state := st_await_lq_squash;

--   --# Update any changed state
--   next_state.core_[j].lsq_.sq_.sq_entries[i] := sq_entry;
--   next_state.core_[j].lsq_.lq_ := lq;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- --#============ LQ Transition =================
-- ruleset j : cores_t do
-- --#ruleset i : sq_idx_t do
-- rule "lq_process_search_request"
-- --# Rule sq_process_search_request, j:0 fired.
--   Sta.core_[j].lsq_.lq_.search_busy = true
-- ==>
--   -- decls
--   var next_state : STATE;
--   var sq_entry : SQ_ENTRY_VALUES;
--   var st_idx : sq_idx_t;
--   var lq : LQ;
--   var sq : SQ;
--   var sb : SB;
--   var ld_idx : ld_idx_t;
--   var offset : ld_idx_t;
--   var curr_idx : ld_idx_t;
--   var difference : ld_idx_t;
--   var already_read_mem : boolean;
--   var phys_addr_match : boolean;
--   var ld_entry : LD_ENTRY_VALUES;
--   var loop_break : boolean;
-- begin
--   next_state := Sta;
--   lq := Sta.core_[j].lsq_.lq_;
--   sq := Sta.core_[j].lsq_.sq_;
--   sb := Sta.core_[j].sb_;
--   st_idx := search_sq_seq_num_idx(sq,
--                                   lq.st_seq_num);

--   --# Find all executed loads after the ld_seq_num
--   --# with a matching phys_addr
--   --# if match, then "squash" them
--   --# How do i want to implement squash here
--   --# maybe a simpler version for now,
--   --# where they're just reset, and will re-exec,
--   --# But this should also catch any dependent insts?
--   --# But the only dependent insts are loads or stores
--   --# But my litmus tests are simple and don't
--   --# check for dependent loads / stores
--   --# since there's no scoreboard modeled for this
--   --# simple first approach
--   --# So a simple reset is ok

--   loop_break := false;
--   if lq.num_entries = 0 then
--     loop_break := true;
--   endif;

--   --# (1) get matching LQ index num
--   if (sq.sq_entries[st_idx].ld_seq_num = 0) then
--     ld_idx := lq.ld_head;
--   else
--     --# Plus 1 to start from the elem after
--     --# but only if there are loads after this...
--     --# otherwise we run into problems...
--     ld_idx := search_lq_seq_num_idx(lq,
--                                     lq.ld_seq_num);
--     if (ld_idx != lq.ld_tail) then
--       ld_idx := ld_idx + 1;
--     else
--       loop_break := true;
--     endif;
--   endif;
--   --# (2) loop to tail searching for:
--   --# if plus 1 is outside this range, this should be caught
--   --# by difference check
--   difference := ( lq.ld_tail + (LD_ENTRY_NUM + 1) - ld_idx ) % ( LD_ENTRY_NUM + 1);
--   offset := 0;
--   --#if (difference != 0) then
--   while ( (offset <= difference) & (loop_break = false) ) do
--     --# do the search
--     curr_idx := ( ld_idx + offset ) % LD_ENTRY_NUM;
--     --# (3) a load entry that's in a state where it's
--     --# already done a read AND has a matching phys addr
--     ld_entry := lq.ld_entries[curr_idx];
--     already_read_mem := !(
--                           ( ld_entry.ld_state = await_fwd_check)
--                           |
--                           ( ld_entry.ld_state = await_scheduled)
--                         );
--     phys_addr_match := ld_entry.phys_addr = lq.phys_addr;
--     --# (4) if match, then reset it's state to before
--     --# it actually tried to check for a fwding st
--     --# (don't have any inst/scoreboard squashing to do)
--     if ( already_read_mem & phys_addr_match ) then
--       --# NOTE: fix:
--       --# remove in-flight SQ and SB reqs if
--       --# Load is in a given state
--       if ( ld_entry.ld_state = await_check_forwarding_or_load_response ) then
--         if (sq.ld_seq_num = ld_entry.instruction.seq_num) then
--           sq.search_busy := false;
--           --# I should probably clear the other fields...
--         endif;
--       elsif ( ld_entry.ld_state = await_sb_fwd_check_response ) then
--         if (sb.ld_seq_num = ld_entry.instruction.seq_num) then
--           sb.search_busy := false;
--           --# I should probably clear the other fields...
--         endif;
--       endif;

--       --#put "=== BEGIN ===\n";
--       --#put ld_entry.ld_state;
--       if ( ld_entry.ld_state = await_mem_response ) then
--         --# set to squashed state
--         ld_entry.ld_state := squashed_await_mem_response;
--         --#put "squashing\n";
--       else
--         ld_entry.ld_state := await_fwd_check;
--         --#put "just reset\n";
--       endif;
--       --#put "==== END ====\n";
--       --# don't bother doing any sophisticated rollback
--       --# or squashing for now
--       --# UPDATE STATE
--       lq.ld_entries[curr_idx] := ld_entry;

--       --# NOTE IMPORTANT! exit from loop!
--       loop_break := true;
--     endif;

--     if (offset != LD_ENTRY_NUM) then
--       offset := offset + 1;
--     else
--       loop_break := true;
--     endif;
--   end;
--   --#end;

--   --# set it's state fwd to st_build_packet
--   sq_entry := Sta.core_[j].lsq_.sq_.sq_entries[st_idx];
--   sq_entry.st_state := st_build_packet;
--   lq.search_busy := false;

--   next_state.core_[j].lsq_.sq_.sq_entries[st_idx] := sq_entry;
--   next_state.core_[j].lsq_.lq_ := lq;
--   next_state.core_[j].sb_ := sb;

--   Sta := next_state;
-- end;
-- endruleset;
-- --#============ LQ Transition =================

-- ruleset j : cores_t do
-- ruleset i : sq_idx_t do
-- rule "st_build_packet_to_send_mem_req"
--   Sta.core_[j].lsq_.sq_.sq_entries[i].st_state = st_build_packet
-- ==>
--   -- decls
--   var next_state : STATE;
--   var sq_entry : SQ_ENTRY_VALUES;
-- begin
--   next_state := Sta;
--   sq_entry := Sta.core_[j].lsq_.sq_.sq_entries[i];
--   sq_entry.st_state := st_await_committed;
--   next_state.core_[j].lsq_.sq_.sq_entries[i] := sq_entry;

--   Sta := next_state;
-- end; end;
-- endruleset;

-- --# TODO: Comment this out for the Load
-- --# Might be useful for debugging later?? dunno
-- -- # ====== Shortcut/Testing States =====
--   --#rule "await_mem_resp_to_writeback"
-- ------------------ END SQ STATE TRANSITIONS -------------

-- ------------------ SB STATE TRANSITIONS -------------
-- ------------------ END SB STATE TRANSITIONS -------------


-- -- invariants --


-- -- #rules for transitions --

-- -- #need to implement an in order insertion
-- -- #do we also implement an array for the queue?
-- -- maybe not

-- -- # operation: insert
-- -- # so, set the item in the tail ptr
-- -- # increment the tail pointer, modulo by list len
-- -- # How to check if full? before inserting?
-- -- # either maintain a "full" bit reg
-- -- # or check thru computing size
-- -- # or maintain the curr size (simplest)

-- -- # rule "insert"
-- -- #   !(
-- -- #     (Sta.core_[j].lsq_.lq_.ld_head = Sta.core_[j].lsq_.lq_.ld_tail)
-- -- #     &
-- -- #     (Sta.core_[j].lsq_.lq_.num_entries = LD_ENTRY_NUM)
-- -- #   )
-- -- # ==>
-- -- #   var NxtSta : STATE;
-- -- #   var curr_head : ld_idx_t;
-- -- #   var curr_tail : ld_idx_t;
-- -- # begin
-- -- #   NxtSta := Sta;
-- -- #   curr_head := Sta.core_[j].lsq_.lq_.ld_head;
-- -- #   curr_tail := Sta.core_[j].lsq_.lq_.ld_tail;
-- -- # --
-- -- #   NxtSta.core_[j].lsq_.lq_.ld_entries[curr_tail].seq_num := 3;
-- -- #   NxtSta.core_[j].lsq_.lq_.ld_entries[curr_tail].instruction.op := ld;
-- -- #   NxtSta.core_[j].lsq_.lq_.ld_tail := (curr_tail + 1) % LD_ENTRY_NUM;
-- -- #   NxtSta.core_[j].lsq_.lq_.num_entries := (Sta.core_[j].lsq_.lq_.num_entries + 1);
-- -- # --
-- -- #   Sta := NxtSta;
-- -- # end;

-- -- # rule "remove"
-- -- #   !( Sta.num_entries = 0 )
-- -- # ==>
-- -- #   var NxtSta : STATE;
-- -- #   var curr_head : ld_idx_t;
-- -- #   var curr_tail : ld_idx_t;
-- -- # begin
-- -- #   NxtSta := Sta;
-- -- #   curr_head := Sta.ld_head;
-- -- #   curr_tail := Sta.ld_tail;
-- -- # --
-- -- #   NxtSta.ld_entries[curr_tail].seq_num := 2;
-- -- #   NxtSta.ld_entries[curr_tail].instruction.op := inval;
-- -- #   NxtSta.ld_head := (curr_head + 1) % LD_ENTRY_NUM;
-- -- #   NxtSta.num_entries := (Sta.num_entries - 1);
-- -- # --
-- -- #   Sta := NxtSta;
-- -- # end;

-- --#NOTE: This was for an "endless"
-- --# Test with instructions added infinitely
-- --# NOTE: It's not configured for litmus tests
-- -- i.e. at the time of writing, there is no
-- -- write_value in INST to initiate with for litmus
-- -- testing, or "random" litmus testing

-- --#ruleset j : cores_t do
-- --#rule "insert_ld_into_rename"
-- --#  -- # If not full, add another instruction!
-- --#  -- # Use less than, since if equal, it's full
-- --#  (Sta.core_[j].rename_.num_entries <= CORE_INST_NUM)
-- --#  --#!(
-- --#  --#  (Sta.core_[j].rename_.rename_head = Sta.core_[j].rename_.rename_tail)
-- --#  --#   &
-- --#  --#   (Sta.core_[j].rename_.num_entries = CORE_INST_NUM)
-- --#  --# )
-- --#==>
-- --#  var NxtSta : STATE;
-- --#  var curr_head : inst_idx_t;
-- --#  var curr_tail : inst_idx_t;
-- --#begin
-- --#  NxtSta := Sta;
-- --#  curr_head := Sta.core_[j].rename_.rename_head;
-- --#  curr_tail := Sta.core_[j].rename_.rename_tail;
-- --#--
-- --#  -- #NxtSta.core_[j].rename_.test_insts[curr_tail].seq_num := 3;
-- --#  alias test_inst:NxtSta.core_[j].rename_.test_insts[curr_tail] do
-- --#    test_inst.op := ld;
-- --#    test_inst.dest_reg := 0;
-- --#    test_inst.imm := 0;
-- --#    test_inst.seq_num := curr_tail+1;
-- --#  end;
-- --#  NxtSta.core_[j].rename_.rename_tail := (curr_tail + 1) % ( CORE_INST_NUM + 1);
-- --#  NxtSta.core_[j].rename_.num_entries := (Sta.core_[j].rename_.num_entries + 1);
-- --#--
-- --#assert (Sta.core_[j].rename_.num_entries <= CORE_INST_NUM) "full? can't insert?";
-- --#  Sta := NxtSta;
-- --#end;
-- --#endruleset;
-- -- End of the rule...

-- -- invariant "rename_non_neg_entry_count"
-- --   if !( Sta.core_[j].rename_.num_entries > 0 )
-- --   then
-- --     error "ERROR: === negative number of entries in rename queue? ==="
-- --   end;

-- --#NOTE: This was for an "endless"
-- --# Test with instructions added infinitely
-- --# NOTE: It's not configured for litmus tests
-- -- i.e. at the time of writing, there is no
-- -- write_value in INST to initiate with for litmus
-- -- testing, or "random" litmus testing

-- --#ruleset j : cores_t do
-- --#rule "insert_st_into_rename"
-- --#  -- # If not full, add another instruction!
-- --#  -- # Use less than, since if equal, it's full
-- --#  (Sta.core_[j].rename_.num_entries <= CORE_INST_NUM)
-- --#  --#!(
-- --#  --#  (Sta.core_[j].rename_.rename_head = Sta.core_[j].rename_.rename_tail)
-- --#  --#   &
-- --#  --#   (Sta.core_[j].rename_.num_entries = CORE_INST_NUM)
-- --#  --# )
-- --#==>
-- --#  var NxtSta : STATE;
-- --#  var curr_head : inst_idx_t;
-- --#  var curr_tail : inst_idx_t;
-- --#begin
-- --#  NxtSta := Sta;
-- --#  curr_head := Sta.core_[j].rename_.rename_head;
-- --#  curr_tail := Sta.core_[j].rename_.rename_tail;
-- --#--
-- --#  -- #NxtSta.core_[j].rename_.test_insts[curr_tail].seq_num := 3;
-- --#  alias test_inst:NxtSta.core_[j].rename_.test_insts[curr_tail] do
-- --#    test_inst.op := st;
-- --#    test_inst.dest_reg := 0;
-- --#    test_inst.imm := 0;
-- --#    test_inst.seq_num := curr_tail+1;
-- --#    test_inst.write_value := 1;
-- --#  end;
-- --#  NxtSta.core_[j].rename_.rename_tail := (curr_tail + 1) % ( CORE_INST_NUM + 1);
-- --#  NxtSta.core_[j].rename_.num_entries := (Sta.core_[j].rename_.num_entries + 1);
-- --#--
-- --#assert (Sta.core_[j].rename_.num_entries <= CORE_INST_NUM) "full? can't insert?";
-- --#  Sta := NxtSta;
-- --#end;
-- --#endruleset;
-- -- End of the rule...

-- -- invariant "rename_non_neg_entry_count"
-- --   if !( Sta.core_[j].rename_.num_entries > 0 )
-- --   then
-- --     error "ERROR: === negative number of entries in rename queue? ==="
-- --   end;


-- ruleset j : cores_t do
-- rule "inject_inst_from_rename"
--   (Sta.core_[j].rename_.num_entries > 0)
-- ==>
-- -- #decls
--   var nxt_state : STATE;
--   var rename_q : RENAME;
--   var lq_q : LQ;
--   var sq_q : SQ;
--   var lsq_q : LSQ;
--   var iq_q : IQ;
--   var rob_q : ROB;
--   -- #the inst being moved
--   var inst : INST;
-- begin
--   -- #init our vars
--   nxt_state := Sta;
--   rename_q := Sta.core_[j].rename_;
--   lq_q := Sta.core_[j].lsq_.lq_;
--   sq_q := Sta.core_[j].lsq_.sq_;
--   -- #sq_q := sta.sq_;
--   lsq_q := Sta.core_[j].lsq_;
--   iq_q := Sta.core_[j].iq_;
--   rob_q := Sta.core_[j].rob_;
-- --
--   -- #NOTE! less than or equal
--   if (rob_q.num_entries <= CORE_INST_NUM)
--     then
--     if (iq_q.num_entries <= CORE_INST_NUM)
--       then

--       inst := rename_read_head(rename_q);
--       if (inst.op = ld) then
--         -- #check if Full!
--         -- #also note that this should check
--         -- #the IQ as well, and must stop if
--         -- # either is full

--         if (lq_q.num_entries <= LD_ENTRY_NUM)
--           then
--           -- #remove inst from rename,
--           rename_q := rename_pop_head(rename_q);
--           -- # Also transition to next state
--           lq_q := lq_insert(lq_q, sq_q, inst);
--           iq_q := iq_insert(iq_q, inst);
--           rob_q := rob_insert(rob_q, inst);
--         endif;
--       elsif (inst.op = st) then
--         -- #remove inst from rename,
--         -- rename_q := rename_pop_head(rename_q);
--         -- # insert into sq...
--         if (sq_q.num_entries <= SQ_ENTRY_NUM)
--           then
--           -- #remove inst from rename,
--           rename_q := rename_pop_head(rename_q);
--           -- # Also transition to next state
--           sq_q := sq_insert(lq_q, sq_q, inst);
--           iq_q := iq_insert(iq_q, inst);
--           rob_q := rob_insert(rob_q, inst);
--         endif;
--       elsif (inst.op = inval) then
--         -- #remove inst from rename,
--         -- rename_q := rename_pop_head(rename_q);
--         error "shouldn't reach this??";
--       endif;
--     endif;
--   endif;

--   -- # also insert into IQ...

--   -- #add it to IQ/LSQ

--   -- # finish and update all state
--   -- # set rename
--   nxt_state.core_[j].rename_ := rename_q;
--   -- # set LSQ stuff
--   lsq_q.lq_ := lq_q;
--   lsq_q.sq_ := sq_q;
--   nxt_state.core_[j].lsq_ := lsq_q;
--   -- # set IQ stuff
--   nxt_state.core_[j].iq_ := iq_q;
--   -- # set ROB stuff
--   nxt_state.core_[j].rob_ := rob_q;

--   -- # update state
--   Sta := nxt_state;
--   -- error "Trace?";
--   -- assert !((iq_q.iq_valid[0] = ready)
--   --          &
--   --          (iq_q.iq_valid[1] = ready)
--   --         ) "both iq entries won't be assigned";
-- end;
-- endruleset;

-- -- # NOTE: Create rule for pop from IQ, tell LQ
-- -- # to start, advance 1 state
-- -- # Require helper fn for IQ pop
-- -- # Ruleset, any inst can be scheduled

-- ruleset j : cores_t do
-- ruleset i : IQ_MAX_INSTS do
-- rule "schedule_iq_inst"
-- -- # if IQ not empty, and entry i is valid
--   !( Sta.core_[j].iq_.num_entries = 0 )
--   &
--   Sta.core_[j].iq_.iq_valid[i] = ready
-- ==>
--   -- overall state update
--   var next_state : STATE;
--   var inst : INST;
--   var lq : LQ;
--   var sq : SQ;
--   var iq : IQ;
--   var seq_num : inst_count_t; --#seq_num_t;--inst_count_t;
--   var num_entries : inst_count_t;
-- begin
--   -- assign our vars
--   next_state := Sta;
--   inst := Sta.core_[j].iq_.iq_insts[i];
--   lq := Sta.core_[j].lsq_.lq_;
--   sq := Sta.core_[j].lsq_.sq_;
--   iq := Sta.core_[j].iq_;
--   num_entries := Sta.core_[j].iq_.num_entries;
--   -- logic
--   -- 1. read this instruction
--   -- put it in var or alias For convenience
--   -- #alias inst:Sta.core_[j].iq_.iq_insts[i] do
--   -- #End;
--   -- 3. find it in LQ, and advance it's state
--   if ( inst.op = ld ) then
--     -- #put "\n ================ \n";
--     -- #put "IQ i: ";
--     -- #put i;
--     -- #put "\n";
--     -- #put "seq_num: ";
--     -- #put iq.iq_insts[i].seq_num;
--     -- #put "\n";
--     -- #put "\n ================ \n";
--     seq_num := iq.iq_insts[i].seq_num;
--     lq := lq_schedule(lq, seq_num);
--   elsif (inst.op = st) then
--     seq_num := iq.iq_insts[i].seq_num;
--     sq := sq_schedule(sq, seq_num);
--   elsif (inst.op = inval) then
--     error "shouldn't have an inval inst in IQ?";
--   endif;

--   -- 2. de-alloc it
--   --#next_state.core_[j].iq_.iq_valid[i] := invalid;
--   inst.op := inval;
--   inst.seq_num := 0;
--   -- # NOTE: Must adjust rename insert func
--   -- # so it sets IQ insts state to ready
--   -- # AND! it searches for a spot to insert an inst in?
--   -- # order doesn't matter any more,
--   -- # just need arbitrary orderings

--   if Sta.core_[j].iq_.iq_insts[i].op = ld then
--     next_state.core_[j].lsq_.lq_ := lq;
--   elsif Sta.core_[j].iq_.iq_insts[i].op = st then
--     next_state.core_[j].lsq_.sq_ := sq;
--   endif;
--   next_state.core_[j].iq_.iq_insts[i] := inst;
--   next_state.core_[j].iq_.iq_valid[i] := invalid;
--   next_state.core_[j].iq_.num_entries := num_entries-1;
--   Sta := next_state;
--   -- error "trace";
-- end;
-- endruleset;
-- endruleset;

-- -- # TODO: add simple transition rule to "remove"
-- -- # a load once it reaches some state like commit
-- -- # to avoid implementing it for now
-- -- # DONE, the ld remove rule.
-- -- # TODO, Replace with commit / ROB later

-- -- # TODO: Continue to model LQ, make request to Memory
-- -- # Also need to model memory
-- -- # Make simple memory requests
-- -- # So need a simple 2 or 3 addr memory
-- -- # and load/store requests happen immediately

-- -- # TODO then model commit stage to retire the load
-- -- # 
-- ruleset j : cores_t do
-- rule "rob_commit_head"
--   -- pre cond
--   -- have entries to commit
--   (Sta.core_[j].rob_.num_entries > 0)
--   &
--   -- and haven't tried commit this inst yet
--   --# NOTE This is a trick, add state, to make sure
--   --# this only runs once
--   --# maybe different if we generate explicit
--   --# substates (what andres was going to do?)
--   (Sta.core_[j].rob_.state[Sta.core_[j].rob_.rob_head] = commit_not_sent)
-- ==>
--   -- decls
--   var next_state : STATE;
--   var rob : ROB;
--   var head_inst : INST;
--   var lq_q : LQ;
--   var sq_q : SQ;
--   var sq_entry : SQ_ENTRY_VALUES;
--   var sb_q : SB;
-- begin
--   next_state := Sta;
--   -- directly change state of lq or sq
--   rob := Sta.core_[j].rob_;

--   --check head inst type
--   head_inst := rob.rob_insts[rob.rob_head];

--   lq_q := Sta.core_[j].lsq_.lq_;

--   sb_q := Sta.core_[j].sb_;
--   sq_q := Sta.core_[j].lsq_.sq_;

--   if (head_inst.op = ld) then
--     -- #search for the load?
--     --#lq_q := lq_commit(lq_q, head_inst);

--     --# remove rob head if lq_entry was removed....
--     --# This makes it all done atomically...
--     if (lq_q.ld_entries[lq_q.ld_head].ld_state = await_committed)
--       then
--       rob := rob_remove(rob);
--     else
--       --# If inst wasn't directly committed
--       -- # set state to commit sig sent
--       rob.state[rob.rob_head] := commit_sig_sent;
--     endif;

--     -- # should be the head load...
--     --# commit if in await commit, otherwise set
--     --# saw commit sig flag to true
--     lq_q := lq_commit_head(lq_q);


--   elsif (head_inst.op = st) then
--     --# SB Must have free space!
--     if ( sb_q.num_entries < ( SB_ENTRY_NUM + 1 ) )
--       then

--       --# Move inst into SB, remove from ROB
--       if (sq_q.sq_entries[sq_q.sq_head].st_state = st_await_committed)
--         then
--         rob := rob_remove(rob);

--         sq_entry := sq_q.sq_entries[sq_q.sq_head];
--         sb_q := sb_insert(sb_q, sq_entry);

--       else
--         --# If inst wasn't directly committed
--         -- # set state to commit sig sent
--         rob.state[rob.rob_head] := commit_sig_sent;
--       endif;

--       -- # should be the head load...
--       -- # sq will insert head_inst into SB
--       -- # SB does store later

--       --# remove sq head if at await commit
--       --# otherwise, set saw commit sig flag to true
--       sq_q := sq_commit_head(sq_q);
--     endif;

--   elsif (head_inst.op = inval) then
--     error "shouldn't have an inval head inst??";
--   endif;

--   next_state.core_[j].rob_ := rob;

--   if (head_inst.op = ld) then
--     next_state.core_[j].lsq_.lq_ := lq_q;
--   elsif (head_inst.op = st) then
--     next_state.core_[j].lsq_.sq_ := sq_q;
--     next_state.core_[j].sb_ := sb_q;
--   endif;

--   Sta := next_state;
-- end;
-- endruleset;

-- -- # TODO then model the store queue?
-- -- # TODO then add code to transition to check store queue
-- -- # for St [x] -> Ld [x]
-- -- # TODO also the post commit store-buffer?

-- -- # TODO Model litmus test checking at the end,
-- -- # invariants, check if register values are not default 0,
-- -- # then implies should be some expected value...

-- --# NOTE TODO implement trnsition to write out from SB

-- --# Reuse this as
-- --# "remove from lq if commit
-- --# signal given in advance"
-- ruleset j : cores_t do
-- rule "remove_ld_entry_already_got_commit_signal"
--   -- # there are lq entries
--   ( Sta.core_[j].lsq_.lq_.num_entries > 0 )
--   &
--   -- # head saw commit signal already
--   (
--     Sta.core_[j].lsq_.lq_.ld_entries[Sta.core_[j].lsq_.lq_.ld_head].commit
--     =
--     true
--   )
--   &
--   -- # awaiting commit signal
--   (
--     Sta.core_[j].lsq_.lq_.ld_entries[Sta.core_[j].lsq_.lq_.ld_head].ld_state
--     =
--     await_committed
--   )
-- ==>
--   var NxtSta : STATE;
--   var rob : ROB;
--   var lq_q : LQ;
--   var curr_head : ld_idx_t;
--   var curr_tail : ld_idx_t;
-- begin
--   NxtSta := Sta;
--   rob := Sta.core_[j].rob_;
--   lq_q := Sta.core_[j].lsq_.lq_;
--   --#curr_head := lq_q.ld_head;
--   --#curr_tail := lq_q.ld_tail;
-- --
--   --#lq_q.ld_entries[curr_head].instruction.seq_num := 0;
--   --#lq_q.ld_entries[curr_head].instruction.op := inval;
--   --#lq_q.ld_entries[curr_head].ld_state := await_creation;
--   --#lq_q.ld_head := (curr_head + 1) % ( LD_ENTRY_NUM + 1);
--   --#lq_q.num_entries := (lq_q.num_entries - 1);
--   lq_q := lq_clear_head(lq_q);
--   assert (lq_q.num_entries >= 0) "num entries should be non-negative";

--   rob := rob_remove(rob);
--   NxtSta.core_[j].rob_ := rob;
--   NxtSta.core_[j].lsq_.lq_ := lq_q;
-- --
--   Sta := NxtSta;
-- end;
-- endruleset;

-- --# TODO make copy for STORE
-- ruleset j : cores_t do
-- rule "remove_st_entry_already_got_commit_signal"
--   -- # there are lq entries
--   ( Sta.core_[j].lsq_.sq_.num_entries > 0 )
--   &
--   -- # head saw commit signal already
--   (
--     Sta.core_[j].lsq_.sq_.sq_entries[Sta.core_[j].lsq_.sq_.sq_head].commit
--     =
--     true
--   )
--   &
--   -- # awaiting commit signal
--   (
--     Sta.core_[j].lsq_.sq_.sq_entries[Sta.core_[j].lsq_.sq_.sq_head].st_state
--     =
--     st_await_committed
--   )
-- ==>
--   var NxtSta : STATE;
--   var rob : ROB;
--   var sq_q : SQ;
--   var sb_q : SB;
--   var curr_head : ld_idx_t;
--   var curr_tail : ld_idx_t;
--   var sq_entry : SQ_ENTRY_VALUES;
-- begin
--   NxtSta := Sta;
--   rob := Sta.core_[j].rob_;
--   sq_q := Sta.core_[j].lsq_.sq_;
--   sb_q := Sta.core_[j].sb_;

--   --# insert sq entry into sb
--   sq_entry := sq_q.sq_entries[sq_q.sq_head];
--   if (sb_q.num_entries < ( SB_ENTRY_NUM + 1))
--     then
--     sb_q := sb_insert(sb_q, sq_entry);

--     --# remove sq entry
--     sq_q := sq_clear_head(sq_q);
--     assert (sq_q.num_entries >= 0) "num entries should be non-negative";

--     --# "signal back" to rob & remove the head
--     rob := rob_remove(rob);
--   endif;

--   NxtSta.core_[j].rob_ := rob;
--   NxtSta.core_[j].lsq_.sq_ := sq_q;
--   NxtSta.core_[j].sb_ := sb_q;
-- --
--   Sta := NxtSta;
-- end;
-- endruleset;

-- --#NOTE: Marking this transition as the mem access one!
-- --#ruleset i : sb_idx_t do
-- ruleset j : cores_t do
-- rule "sb_await_send_mem_req_to_await_mem_resp"
--   --# when head SB entry is waiting to send out it's request
--   ( Sta.core_[j].sb_.sb_entries[Sta.core_[j].sb_.sb_head].sb_state = sb_await_send_mem_req )
--   &
--   --# have sb entries
--   ( Sta.core_[j].sb_.num_entries > 0)
--   --# this is a condition: mem's msg buffer is empty
--   --# i.e. mem is free
--   &
--   (
--     Sta.core_[j].mem_interface_.out_busy = false
--   )
-- ==>
--   -- decls
--   var next_state : STATE;
--   var sb : SB;
--   var sb_entry : SB_ENTRY_VALUES;

--   --# var mem : MEM_ARRAY;
--   var phys_addr : addr_idx_t;

--   var mem_inter : MEM_INTERFACE;
-- begin
--   next_state := Sta;
--   sb := Sta.core_[j].sb_;
--   sb_entry := sb.sb_entries[sb.sb_head];
--   --#mem := Sta.mem_;
--   mem_inter := Sta.core_[j].mem_interface_;

--   -- Update state
--   sb_entry.sb_state := sb_await_mem_response;

--   -- Write to memory
--   -- # Issue, wasn't using Imm before
--   -- # could update it to write imm to virt/phys
--   phys_addr := sb_entry.phys_addr;
--   --#mem.arr[phys_addr] := sb_entry.write_value;

--   --# NOTE: send to the mem interface
--   mem_inter.out_msg := insert_st_in_mem_interface(
--                                                   sb_entry,
--                                                   j
--                                                  );
--   mem_inter.out_busy := true;

--   next_state.core_[j].sb_.sb_entries[Sta.core_[j].sb_.sb_head] := sb_entry;
--   next_state.core_[j].mem_interface_ := mem_inter;

--   --# AZ NOTE: This is a decent way to check if something went
--   --# wrong, if an illegal inst (seq_num = 0) tries to
--   --# perform any action while in any structure!!!
--   assert ( sb.sb_entries[sb.sb_head].instruction.seq_num != 0 ) "invalid st";

--   Sta := next_state;
-- end;
-- endruleset;

-- --# NOTE: don't need to do this, the associative_ack
-- --# will clear the head in the SB

-- --#--# NOTE: lazily implemented, don't actually
-- --#--# have another transition from mem to reply
-- --#--# to request, just got wrote the mem value immediately
-- --#ruleset j : cores_t do
-- --#rule "sb_await_mem_resp_to_await_creation"
-- --#--# when head SB entry is waiting to send out it's request
-- --#  ( Sta.core_[j].sb_.sb_entries[Sta.core_[j].sb_.sb_head].sb_state = sb_await_mem_response )
-- --#  &
-- --#  --# have sb entries
-- --#  ( Sta.core_[j].sb_.num_entries > 0)
-- --#==>
-- --#  -- decls
-- --#  var next_state : STATE;
-- --#  var sb : SB;
-- --#  var sb_entry : SB_ENTRY_VALUES;
-- --#
-- --#  --# var mem : MEM_ARRAY;
-- --#  --# var phys_addr : addr_idx_t;
-- --#begin
-- --#  next_state := Sta;
-- --#  sb := Sta.core_[j].sb_;
-- --#  sb_entry := Sta.core_[j].sb_.sb_entries[Sta.core_[j].sb_.sb_head];
-- --#  --#mem := Sta.core_[j].mem_;
-- --#
-- --#  --#-- Update state
-- --#  --#sb_entry.sb_state := sb_await_mem_response;
-- --#
-- --#  --#-- Write to memory
-- --#  --#phys_addr := sb_entry.phys_addr;
-- --#  --#mem.arr[phys_addr] := sb_entry.write_value;
-- --#
-- --#  sb := sb_clear_head(sb);
-- --#
-- --#  --#next_state.core_[j].sb_.sb_entries[i] := sb_entry;
-- --#  --#--#next_state.core_[j].mem_ := mem;
-- --#
-- --#  next_state.core_[j].sb_ := sb;
-- --#
-- --#  Sta := next_state;
-- --#end;
-- --#endruleset;

-- ruleset j : cores_t do
-- invariant "test_invariant"
--   Sta.core_[j].lsq_.lq_.num_entries = 1
--   ->
--   ( Sta.core_[j].lsq_.lq_.ld_tail
--     =
--     ( ( Sta.core_[j].lsq_.lq_.ld_head + 1 ) % (LD_ENTRY_NUM + 1) )
--   )
-- endruleset;

-- rule "reset"
--   (
--     ( Sta.core_[0].rename_.num_entries = 0 )
--     &
--     ( Sta.core_[0].rob_.num_entries = 0 )
--     &
--     ( Sta.core_[0].sb_.num_entries = 0 )
--     &
--     ( Sta.core_[0].iq_.num_entries = 0 )
--     &
--     ( Sta.core_[0].lsq_.lq_.num_entries = 0 )
--     &
--     ( Sta.core_[0].lsq_.sq_.num_entries = 0 )
--     &
--     ( Sta.core_[1].rename_.num_entries = 0 )
--     &
--     ( Sta.core_[1].rob_.num_entries = 0 )
--     &
--     ( Sta.core_[1].sb_.num_entries = 0 )
--     &
--     ( Sta.core_[1].iq_.num_entries = 0 )
--     &
--     ( Sta.core_[1].lsq_.lq_.num_entries = 0 )
--     &
--     ( Sta.core_[1].lsq_.sq_.num_entries = 0 )
--   )
-- ==>
--   -- decls
--   var next_state : STATE;
-- begin
--   next_state := Sta;

--   --#Sta := next_state;
--   Sta := init_state_fn();
-- end;


-- --#invariant "amd1_verif"
-- --#  (
-- --#    ( Sta.core_[0].rename_.num_entries = 0 )
-- --#    &
-- --#    ( Sta.core_[0].rob_.num_entries = 0 )
-- --#    &
-- --#    ( Sta.core_[0].sb_.num_entries = 0 )
-- --#    &
-- --#    ( Sta.core_[1].rename_.num_entries = 0 )
-- --#    &
-- --#    ( Sta.core_[1].rob_.num_entries = 0 )
-- --#    &
-- --#    ( Sta.core_[1].sb_.num_entries = 0 )
-- --#  )
-- --#  ->
-- --#--# AMD1 test from pipecheck
-- --#    !(
-- --#      --#( Sta.core_[0].rf_.rf[0] = 0 )
-- --#      --#&
-- --#      --#( Sta.core_[0].rf_.rf[1] = 0 )
-- --#      --#&
-- --#      ( Sta.core_[1].rf_.rf[0] = 1 )
-- --#      &
-- --#      ( Sta.core_[1].rf_.rf[1] = 0 )
-- --#    )

-- invariant "iwp23b1"
-- (
--   ( Sta.core_[0].rename_.num_entries = 0 )
--   &
--   ( Sta.core_[0].rob_.num_entries = 0 )
--   &
--   ( Sta.core_[0].sb_.num_entries = 0 )
--   &
--   ( Sta.core_[1].rename_.num_entries = 0 )
--   &
--   ( Sta.core_[1].rob_.num_entries = 0 )
--   &
--   ( Sta.core_[1].sb_.num_entries = 0 )
-- )
-- ->
--  --#iwp23b1 REQUIRED OUTPUT test from pipecheck
--  --#Note it's reg 1 since I just follow the inst #
--  --#Should have this result, always!
-- (
--   ( Sta.core_[0].rf_.rf[0] = 0 )
--   &
--   ( Sta.core_[0].rf_.rf[1] = 1 )
--   &
--   ( Sta.core_[1].rf_.rf[0] = 0 )
--   &
--   ( Sta.core_[1].rf_.rf[1] = 1 )
-- )
--   ]
--   murphi_file