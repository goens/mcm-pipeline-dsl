const
CORE_INST_NUM : 1;
IC_ENTRY_NUM : 1;
CORE_NUM : 1;
ADDR_NUM : 1;
REG_NUM : 1;
MAX_VALUE : 1;
LSQ_NUM_ENTRIES_ENUM_CONST : 1;
LSQ_NUM_ENTRIES_CONST : 2;
ROB_NUM_ENTRIES_ENUM_CONST : 1;
ROB_NUM_ENTRIES_CONST : 2;
IQ_NUM_ENTRIES_ENUM_CONST : 1;
IQ_NUM_ENTRIES_CONST : 2;
RENAME_NUM_ENTRIES_ENUM_CONST : 1;
RENAME_NUM_ENTRIES_CONST : 2;

type
val_t : 0 .. MAX_VALUE;
inst_idx_t : 0 .. CORE_INST_NUM;
inst_count_t : 0 .. (CORE_INST_NUM + 6); -- make it 2, allow for reseting once in a core
addr_idx_t : 0 .. ADDR_NUM;
reg_idx_t : 0 .. REG_NUM;
ic_idx_t : 0 .. IC_ENTRY_NUM;
ic_count_t : 0 .. (IC_ENTRY_NUM + 1);
cores_t : 0 .. CORE_NUM;
MSG_DEST : enum {core, mem};
INST_TYPE : enum {ld, st, inval, mfence, dmb_sy, dmb_ld, dmb_st, ldar, stlr};
ADDR_TYPE : enum {addr_reg, addr_imm};
VAL_TYPE : enum {val_reg, val_imm};
INST : record
  op : INST_TYPE;
  addr_type : ADDR_TYPE;
  seq_num : inst_count_t;
  dest_reg : reg_idx_t;
  src_reg1 : reg_idx_t;
  src_reg2 : reg_idx_t;
  imm : val_t;
  write_value : val_t;
end;
LSQ_state : enum {lsq_st_await_committed, lsq_ld_await_committed, lsq_ld_write_result, lsq_await_load_mem_response, lsq_build_packet_send_mem_request, lsq_ld_st_fwd_branch, lsq_ld_st_await_translation, lsq_await_scheduled, lsq_await_creation, init_default_state};
LSQ_idx_t : 0 .. LSQ_NUM_ENTRIES_ENUM_CONST;
LSQ_count_t : 0 .. LSQ_NUM_ENTRIES_CONST;
LSQ_entry_values : record
  virt_addr : addr_idx_t;
  phys_addr : addr_idx_t;
  read_value : val_t;
  write_value : val_t;
  seq_num : inst_count_t;
  instruction : INST;
  state : LSQ_state;
end;
LSQ : record
  entries : array [LSQ_idx_t] of LSQ_entry_values;
  num_entries : LSQ_count_t;
  head : LSQ_idx_t;
  tail : LSQ_idx_t;
end;
ROB_state : enum {rob_await_executed, rob_clear_lsq_store_head, rob_commit_time_await_st_mem_resp, rob_commit_po, rob_commit_if_head, rob_await_creation, init_rob_entry};
ROB_idx_t : 0 .. ROB_NUM_ENTRIES_ENUM_CONST;
ROB_count_t : 0 .. ROB_NUM_ENTRIES_CONST;
ROB_entry_values : record
  instruction : INST;
  seq_num : inst_count_t;
  is_executed : boolean;
  write_value : val_t;
  phys_addr : val_t;
  state : ROB_state;
end;
ROB : record
  entries : array [ROB_idx_t] of ROB_entry_values;
  num_entries : ROB_count_t;
  head : ROB_idx_t;
  tail : ROB_idx_t;
end;
IQ_state : enum {iq_schedule_inst, iq_await_creation, init_iq_entry};
IQ_idx_t : 0 .. IQ_NUM_ENTRIES_ENUM_CONST;
IQ_count_t : 0 .. IQ_NUM_ENTRIES_CONST;
IQ_entry_values : record
  instruction : INST;
  seq_num : inst_count_t;
  state : IQ_state;
  valid : boolean;
end;
IQ : record
  entries : array [IQ_idx_t] of IQ_entry_values;
  num_entries : IQ_count_t;
end;
RENAME_state : enum {issue_if_head, rename_await_creation, init_rename_entry};
RENAME_idx_t : 0 .. RENAME_NUM_ENTRIES_ENUM_CONST;
RENAME_count_t : 0 .. RENAME_NUM_ENTRIES_CONST;
RENAME_entry_values : record
  instruction : INST;
  state : RENAME_state;
end;
RENAME : record
  entries : array [RENAME_idx_t] of RENAME_entry_values;
  num_entries : RENAME_count_t;
  head : RENAME_idx_t;
  tail : RENAME_idx_t;
end;
SeqNumReg_state : enum {seq_num_interface, init_seq_num_counter};
SeqNumReg : record
  seq_num_counter : inst_count_t;
  state : SeqNumReg_state;
end;
STORE_STATE : enum {await_handling, await_invalidation_received, store_send_completion};
R_W : enum {read, write};
MEM_REQ : record
  addr : addr_idx_t;
  r_w : R_W;
  value : val_t;
  valid : boolean;
  dest : MSG_DEST;
  dest_id : cores_t;
  seq_num : inst_count_t;
  store_state : STORE_STATE;
  store_inval_sent : array [cores_t] of boolean;
  store_inval_ackd : array [cores_t] of boolean;
end;
MEM_ARRAY : record
  arr : array [addr_idx_t] of val_t;
end;
MEM_INTERFACE : record
  out_msg : MEM_REQ;
  in_msg : MEM_REQ;
  out_busy : boolean;
  in_busy : boolean;
end;
IC : record
  buffer : array [ic_idx_t] of MEM_REQ;
  valid : array [ic_idx_t] of boolean;
  num_entries : ic_count_t;
end;
REG_FILE : record
  rf : array [reg_idx_t] of val_t;
end;
CORE : record
  LSQ_ : LSQ;
  ROB_ : ROB;
  IQ_ : IQ;
  RENAME_ : RENAME;
  SeqNumReg_ : SeqNumReg;
  rf_ : REG_FILE;
  mem_interface_ : MEM_INTERFACE;
end;
STATE : record
  core_ : array [cores_t] of CORE;
  mem_ : MEM_ARRAY;
  ic_ : IC;
end;

var
Sta : STATE;


function search_rob_seq_num_idx (
  rob : ROB;
  seq_num : inst_count_t
) : inst_idx_t;
begin
  for i : inst_idx_t do
    if (rob.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "ROB Search: didn't find it? how? bad seq_num idx?";
end;

function insert_ld_in_mem_interface (
  ld_entry : LSQ_entry_values;
  core : cores_t
) : MEM_REQ;
  var msg : MEM_REQ;
begin
  msg.addr := ld_entry.phys_addr;
  msg.r_w := read;
  msg.valid := true;
  msg.dest := mem;
  msg.dest_id := core;
  msg.seq_num := ld_entry.instruction.seq_num;
  return msg;
end;

function insert_st_in_mem_interface (
  sb_entry : ROB_entry_values;
  core : cores_t
) : MEM_REQ;
  var msg : MEM_REQ;
begin
  msg.addr := sb_entry.phys_addr;
  msg.r_w := write;
  msg.value := sb_entry.write_value;
  msg.valid := true;
  msg.dest := mem;
  msg.dest_id := core;
  msg.seq_num := sb_entry.instruction.seq_num;
  return msg;
end;

function insert_msg_into_ic (
  ic : IC;
  msg : MEM_REQ
) : IC;
  var ic_new : IC;
begin
  ic_new := ic;
  for i : ic_idx_t do
    if (ic_new.valid[ i ] = false) then
      ic_new.buffer[ i ] := msg;
      ic_new.valid[ i ] := true;
      ic_new.num_entries := (ic.num_entries + 1);
      return ic_new;
    end;
  endfor;
end;

function associative_assign_ld (
  lq : LSQ;
  msg : MEM_REQ
) : LSQ;
  var lq_new : LSQ;
  var lq_iter : LSQ_idx_t;
  var lq_count : LSQ_count_t;
  var curr_entry : LSQ_entry_values;
  var seq_num : inst_count_t;

  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var LSQ_squash_remove_count : LSQ_count_t;
begin
  lq_new := lq;
  lq_iter := lq.head;
  lq_count := lq.num_entries;
  seq_num := msg.seq_num;

  LSQ_while_break := false;
  LSQ_found_entry := false;
  if (lq.num_entries = 0) then
    LSQ_while_break := true;
  end;
  LSQ_entry_idx := lq.head;
  LSQ_difference := lq.num_entries;
  LSQ_offset := 0;
  LSQ_squash_remove_count := 0;
  -- for i : 0 .. LSQ_NUM_ENTRIES_ENUM_CONST do
  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
    LSQ_curr_idx := ((LSQ_entry_idx + LSQ_offset) % LSQ_NUM_ENTRIES_CONST);

    curr_entry := lq_new.entries[ LSQ_curr_idx ];
    if (curr_entry.instruction.seq_num = seq_num) then
      -- assert ((curr_entry.state = lsq_squashed_await_ld_mem_resp) | (curr_entry.state = lsq_await_load_mem_response)) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";
      assert (curr_entry.state = lsq_await_load_mem_response
        -- | curr_entry.state = replay_generated_lsq_await_load_mem_response
        )
        "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = lsq_await_load_mem_response) then
        curr_entry.state := lsq_ld_write_result;
        curr_entry.read_value := msg.value;
      -- elsif (curr_entry.state = replay_generated_lsq_await_load_mem_response) then
      --   curr_entry.state := replay_compare_and_check_state;
      --   curr_entry.replay_value := msg.value;
      end;
      lq_new.entries[ LSQ_curr_idx ] := curr_entry;
      return lq_new;
    end;

    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  return lq_new;
end;

function associative_ack_st (
  sb : ROB;
  msg : MEM_REQ
) : ROB;
  var sb_new : ROB;
  var sb_iter : ROB_idx_t;
  var sb_count : ROB_count_t;
  var curr_entry : ROB_entry_values;
  var curr_entry_id : ROB_idx_t;
  var seq_num : inst_count_t;
begin
  sb_new := sb;
  sb_iter := 0;
  sb_count := sb.num_entries;
  seq_num := msg.seq_num;
  for i : 0 .. ROB_NUM_ENTRIES_ENUM_CONST do
    curr_entry_id := ((sb_iter + i) % ROB_NUM_ENTRIES_CONST);
    curr_entry := sb_new.entries[ curr_entry_id ];
    if (curr_entry.instruction.seq_num = seq_num) then
      assert (curr_entry.state = rob_commit_time_await_st_mem_resp) "ACK ROB: Should be in await mem resp?";
      curr_entry.state := rob_clear_lsq_store_head;
      sb_new.entries[ curr_entry_id ] := curr_entry;
      return sb_new;
    end;
  endfor;
  error "didn't find the Load to write the read val into?";
  return sb_new;
end;

function init_state_fn (
  
) : STATE;
  var init_state : STATE;
begin
  undefine init_state;
  alias mem : init_state.mem_ do
    for i : addr_idx_t do
      mem.arr[ i ] := 0;
    endfor;
  end;
  alias ic : init_state.ic_ do
    for i : ic_idx_t do
      ic.buffer[ i ].addr := 0;
      ic.buffer[ i ].r_w := read;
      ic.buffer[ i ].value := 0;
      ic.buffer[ i ].valid := false;
      ic.buffer[ i ].dest := mem;
      ic.buffer[ i ].dest_id := 0;
      ic.buffer[ i ].seq_num := 0;
      ic.buffer[ i ].store_state := await_handling;
      for core_idx : cores_t do
        ic.buffer[ i ].store_inval_sent[ core_idx ] := false;
        ic.buffer[ i ].store_inval_ackd[ core_idx ] := false;
      endfor;
      ic.valid[ i ] := false;
    endfor;
    ic.num_entries := 0;
  end;
  for core : cores_t do
    alias mem_int : init_state.core_[ core ].mem_interface_ do
      mem_int.out_msg.addr := 0;
      mem_int.out_msg.r_w := read;
      mem_int.out_msg.value := 0;
      mem_int.out_msg.valid := false;
      mem_int.out_msg.dest := mem;
      mem_int.out_msg.dest_id := 0;
      mem_int.out_msg.seq_num := 0;
      mem_int.in_msg.addr := 0;
      mem_int.in_msg.r_w := read;
      mem_int.in_msg.value := 0;
      mem_int.in_msg.valid := false;
      mem_int.in_msg.dest := mem;
      mem_int.in_msg.dest_id := 0;
      mem_int.in_msg.seq_num := 0;
      mem_int.out_busy := false;
      mem_int.in_busy := false;
    end;
    alias lsq : init_state.core_[ core ].LSQ_ do
      for i : LSQ_idx_t do
        lsq.entries[ i ].instruction.seq_num := 0;
        lsq.entries[ i ].instruction.op := inval;
        lsq.entries[ i ].instruction.imm := 0;
        lsq.entries[ i ].instruction.dest_reg := 0;
        lsq.entries[ i ].read_value := 0;
        lsq.entries[ i ].write_value := 0;
        lsq.entries[ i ].virt_addr := 0;
        lsq.entries[ i ].phys_addr := 0;
        lsq.entries[ i ].state := lsq_await_creation;
      endfor;
      lsq.head := 0;
      lsq.tail := 0;
      lsq.num_entries := 0;
    end;
    alias rename : init_state.core_[ core ].RENAME_ do
      for i : 0 .. CORE_INST_NUM do
        rename.entries[ i ].instruction.op := inval;
        rename.entries[ i ].instruction.seq_num := 0;
        rename.entries[ i ].state := issue_if_head;
      endfor;
      rename.head := 0;
      rename.tail := 0;
      rename.num_entries := 0;
    end;
    alias iq : init_state.core_[ core ].IQ_ do
      for i : 0 .. IQ_NUM_ENTRIES_ENUM_CONST do
        iq.entries[ i ].instruction.op := inval;
        iq.entries[ i ].instruction.seq_num := 0;
        iq.entries[ i ].state := iq_await_creation;
        iq.entries[ i ].valid := false;
      endfor;
      iq.num_entries := 0;
    end;
    alias rf : init_state.core_[ core ].rf_ do
      for i : reg_idx_t do
        rf.rf[ i ] := 0;
      endfor;
    end;
    alias rob : init_state.core_[ core ].ROB_ do
      for i : 0 .. CORE_INST_NUM do
        rob.entries[ i ].instruction.op := inval;
        rob.entries[ i ].instruction.seq_num := 0;
        rob.entries[ i ].is_executed := false;
        rob.entries[ i ].state := rob_await_creation;
      endfor;
      rob.head := 0;
      rob.tail := 0;
      rob.num_entries := 0;
    end;
    alias seqnumreg : init_state.core_[ core ].SeqNumReg_ do
      seqnumreg.seq_num_counter := 1;
      seqnumreg.state := seq_num_interface;
    end;
  endfor;
  alias rename_c0 : init_state.core_[ 0 ].RENAME_ do
    rename_c0.entries[ 0 ].instruction.op := st;
    rename_c0.entries[ 0 ].instruction.seq_num := 1;
    rename_c0.entries[ 0 ].instruction.dest_reg := 0;
    rename_c0.entries[ 0 ].instruction.imm := 0;
    rename_c0.entries[ 0 ].instruction.write_value := 1;
    rename_c0.entries[ 1 ].instruction.op := ld;
    rename_c0.entries[ 1 ].instruction.seq_num := 2;
    rename_c0.entries[ 1 ].instruction.dest_reg := 1;
    rename_c0.entries[ 1 ].instruction.imm := 1;
    rename_c0.entries[ 1 ].instruction.write_value := 0;
    rename_c0.head := 0;
    rename_c0.tail := 2 % (CORE_INST_NUM + 1);
    rename_c0.num_entries := 2;
  end;
  alias rename_c1 : init_state.core_[ 1 ].RENAME_ do
    rename_c1.entries[ 0 ].instruction.op := st;
    rename_c1.entries[ 0 ].instruction.seq_num := 1;
    rename_c1.entries[ 0 ].instruction.dest_reg := 0;
    rename_c1.entries[ 0 ].instruction.imm := 1;
    rename_c1.entries[ 0 ].instruction.write_value := 1;
    rename_c1.entries[ 1 ].instruction.op := ld;
    rename_c1.entries[ 1 ].instruction.seq_num := 2;
    rename_c1.entries[ 1 ].instruction.dest_reg := 1;
    rename_c1.entries[ 1 ].instruction.imm := 0;
    rename_c1.entries[ 1 ].instruction.write_value := 0;
    rename_c1.head := 0;
    rename_c1.tail := 2 % (CORE_INST_NUM + 1);
    rename_c1.num_entries := 2;
  end;
  return init_state;
end;

function search_LSQ_seq_num_idx (
  LSQ_ : LSQ;
  seq_num : inst_count_t
) : LSQ_idx_t;
begin
  for i : LSQ_idx_t do
    if (LSQ_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;

function search_ROB_seq_num_idx (
  ROB_ : ROB;
  seq_num : inst_count_t
) : ROB_idx_t;
begin
  for i : ROB_idx_t do
    if (ROB_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;

function search_IQ_seq_num_idx (
  IQ_ : IQ;
  seq_num : inst_count_t
) : IQ_idx_t;
begin
  for i : IQ_idx_t do
    if (IQ_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;

function search_RENAME_seq_num_idx (
  RENAME_ : RENAME;
  seq_num : inst_count_t
) : RENAME_idx_t;
begin
  for i : RENAME_idx_t do
    if (RENAME_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;


startstate "init"
begin
  undefine Sta;
  Sta := init_state_fn();
 end;


ruleset j : cores_t do 
  rule "move_msg_from_mem_interface_to_ic" 
((Sta.core_[ j ].mem_interface_.out_busy = true) & (Sta.ic_.num_entries < (IC_ENTRY_NUM + 1)))
==>
 
  var next_state : STATE;
  var ic : IC;
  var mem_int : MEM_INTERFACE;

begin
  next_state := Sta;
  mem_int := Sta.core_[ j ].mem_interface_;
  ic := Sta.ic_;
  ic := insert_msg_into_ic(ic, mem_int.out_msg);
  mem_int.out_busy := false;
  next_state.core_[ j ].mem_interface_ := mem_int;
  next_state.ic_ := ic;
  Sta := next_state;

end;
end;


ruleset i : ic_idx_t do 
  rule "perform_ic_msg" 
((Sta.ic_.num_entries > 0) & ((Sta.ic_.valid[ i ] = true) & (Sta.ic_.buffer[ i ].dest = mem)))
==>
 
  var next_state : STATE;
  var ic : IC;
  var mem : MEM_ARRAY;
  var addr : addr_idx_t;
  var mem_interface : MEM_INTERFACE;
  var store_invals_sent : boolean;
  var store_invals_ackd : boolean;

begin
  next_state := Sta;
  ic := Sta.ic_;
  mem := Sta.mem_;
  addr := ic.buffer[ i ].addr;
  if (ic.buffer[ i ].r_w = read) then
    ic.buffer[ i ].value := mem.arr[ addr ];
    ic.buffer[ i ].dest := core;
  elsif (ic.buffer[ i ].r_w = write) then
    if (ic.buffer[ i ].store_state = await_handling) then
      for core_idx : cores_t do
        if ((core_idx != ic.buffer[ i ].dest_id) & ((ic.buffer[ i ].store_inval_sent[ core_idx ] = false) & (Sta.core_[ core_idx ].mem_interface_.in_busy = false))) then
          mem.arr[ addr ] := ic.buffer[ i ].value;
          next_state.core_[ core_idx ].mem_interface_.in_msg := ic.buffer[ i ];
          next_state.core_[ core_idx ].mem_interface_.in_msg.dest := core;
          next_state.core_[ core_idx ].mem_interface_.in_busy := true;
          ic.buffer[ i ].store_inval_sent[ core_idx ] := true;
        end;
      endfor;
      store_invals_sent := true;
      for core_idx : cores_t do
        if (core_idx != ic.buffer[ i ].dest_id) then
          store_invals_sent := (store_invals_sent & ic.buffer[ i ].store_inval_sent[ core_idx ]);
        end;
      endfor;
      if store_invals_sent then
        ic.buffer[ i ].store_state := await_invalidation_received;
      end;
    elsif (ic.buffer[ i ].store_state = await_invalidation_received) then
      store_invals_ackd := true;
      for core_idx : cores_t do
        if (core_idx != ic.buffer[ i ].dest_id) then
          store_invals_ackd := (store_invals_ackd & ic.buffer[ i ].store_inval_ackd[ core_idx ]);
        end;
      endfor;
      if store_invals_ackd then
        ic.buffer[ i ].store_state := store_send_completion;
      end;
     elsif (ic.buffer[ i ].store_state = store_send_completion) then
      ic.buffer[ i ].store_state := store_send_completion;
      ic.buffer[ i ].dest := core;
      for core_idx : cores_t do
        if (core_idx != ic.buffer[ i ].dest_id) then
          ic.buffer[ i ].store_inval_sent[ core_idx ] := false;
          ic.buffer[ i ].store_inval_ackd[ core_idx ] := false;
        end;
      endfor;
    else
      error "Unreachable or unhandled case of store state in the InterConnect.";
    end;
  end;
  next_state.ic_ := ic;
  next_state.mem_ := mem;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  
ruleset i : ic_idx_t do 
  rule "acknowledge_ic_msg" 
((Sta.ic_.num_entries > 0) & ((Sta.ic_.valid[ i ] = true) & ((Sta.ic_.buffer[ i ].dest = core) & ((j = Sta.ic_.buffer[ i ].dest_id) & (Sta.core_[ j ].mem_interface_.in_busy = false)))))
==>
 
  var next_state : STATE;
  var ic : IC;
  var mem_interface : MEM_INTERFACE;

begin
  next_state := Sta;
  ic := Sta.ic_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  mem_interface.in_msg := ic.buffer[ i ];
  mem_interface.in_busy := true;
  ic.buffer[ i ].addr := 0;
  ic.buffer[ i ].r_w := read;
  ic.buffer[ i ].value := 0;
  ic.buffer[ i ].valid := false;
  ic.buffer[ i ].dest := mem;
  ic.buffer[ i ].dest_id := 0;
  ic.valid[ i ] := false;
  ic.num_entries := (ic.num_entries - 1);
  next_state.ic_ := ic;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end;
end;


ruleset j : cores_t do 
  rule "core_sends_in_msg_ack_to_structures" 
(Sta.core_[ j ].mem_interface_.in_busy = true)
==>
 
  var next_state : STATE;
  var lq : LSQ;
  var sb : ROB;
  var mem_interface : MEM_INTERFACE;
  var found_msg_in_ic : boolean;

begin
  next_state := Sta;
  lq := Sta.core_[ j ].LSQ_;
  sb := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  if (mem_interface.in_msg.r_w = read) then
    lq := associative_assign_ld(lq, mem_interface.in_msg);
    mem_interface.in_busy := false;
  elsif (mem_interface.in_msg.r_w = write) then
    if (mem_interface.in_msg.store_state = await_handling) then
      found_msg_in_ic := false;
      for ic_idx : ic_idx_t do
        if ((Sta.ic_.valid[ ic_idx ] = true) & ((Sta.ic_.buffer[ ic_idx ].seq_num = Sta.core_[ j ].mem_interface_.in_msg.seq_num) & (Sta.ic_.buffer[ ic_idx ].dest_id = Sta.core_[ j ].mem_interface_.in_msg.dest_id))) then
          next_state.ic_.buffer[ ic_idx ].store_inval_ackd[ j ] := true;
          if (found_msg_in_ic = true) then
            error "we found 2 matching ic entries? shouldn't happen...";
          elsif (found_msg_in_ic = false) then
            found_msg_in_ic := true;
          end;
        end;
      endfor;
      assert (found_msg_in_ic = true) "Should have found a msg in the IC? Otherwise we wouldn't be performing this invalidation's squash.";
      assert (Sta.core_[ j ].mem_interface_.in_busy = true) "The memory interface of this core should be busy, this is the msg we're processing.";
      next_state.core_[ j ].mem_interface_.in_busy := false;
      mem_interface.in_busy := false;
    elsif (mem_interface.in_msg.store_state = store_send_completion) then
      sb := associative_ack_st(sb, mem_interface.in_msg);
      mem_interface.in_busy := false;
    end;
  end;
  next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := sb;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end;

rule "reset" 
((Sta.core_[ 0 ].RENAME_.num_entries = 0) & ((Sta.core_[ 0 ].ROB_.num_entries = 0) & ((Sta.core_[ 0 ].IQ_.num_entries = 0) & ((Sta.core_[ 0 ].LSQ_.num_entries = 0) & ((Sta.core_[ 1 ].RENAME_.num_entries = 0) & ((Sta.core_[ 1 ].ROB_.num_entries = 0) & ((Sta.core_[ 1 ].IQ_.num_entries = 0) & (Sta.core_[ 1 ].LSQ_.num_entries = 0))))))))
==>
 
  var next_state : STATE;

begin
  put "  === BEGIN Reached End, Reg File: ===\n";
  put Sta.core_[ 0 ].rf_.rf[ 0 ];
  put Sta.core_[ 0 ].rf_.rf[ 1 ];
  put "  === END Reached End, Reg File: ===\n";
  next_state := Sta;
  Sta := init_state_fn();

end;


ruleset j : cores_t; i : LSQ_idx_t do 
  rule "LSQ lsq_ld_write_result ===> lsq_ld_await_committed || lsq_await_creation" 
(Sta.core_[ j ].LSQ_.entries[ i ].state = lsq_ld_write_result)
==>
 
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].rf_.rf[ next_state.core_[ j ].LSQ_.entries[ i ].instruction.dest_reg ] := next_state.core_[ j ].LSQ_.entries[ i ].read_value;
  ROB_while_break := false;
  ROB_found_entry := false;
  if (next_state.core_[ j ].ROB_.num_entries = 0) then
    ROB_while_break := true;
  end;
  ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
  ROB_difference := next_state.core_[ j ].ROB_.num_entries;
  ROB_offset := 0;
  while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
    ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) then
      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) then
        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
      else
        error "Controller is not on an expected state for a msg: (ld_executed) from: (LSQ) to: (ROB)";
      end;
      ROB_found_entry := true;
    end;
    if (ROB_offset < ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_ld_await_committed;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LSQ_idx_t do 
  rule "LSQ lsq_build_packet_send_mem_request ===> lsq_build_packet_send_mem_request || lsq_await_load_mem_response || lsq_await_creation" 
(((((((((((((((((Sta.core_[ j ].LSQ_.entries[ i ].state = lsq_build_packet_send_mem_request) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var RENAME_dmb_sy_is_in_state_set : boolean;
  var ROB_dmb_sy_is_in_state_set : boolean;
  var is_instruction_on_any_state : boolean;
  var RENAME_dmb_ld_is_in_state_set : boolean;
  var ROB_dmb_ld_is_in_state_set : boolean;
  var RENAME_LDAR_is_in_state_set : boolean;
  var IQ_LDAR_is_in_state_set : boolean;
  var LSQ_LDAR_is_in_state_set : boolean;
  var ROB_LDAR_is_in_state_set : boolean;
  var RENAME_while_break : boolean;
  var RENAME_found_entry : boolean;
  var RENAME_entry_idx : RENAME_idx_t;
  var RENAME_difference : RENAME_count_t;
  var RENAME_offset : RENAME_count_t;
  var RENAME_curr_idx : RENAME_idx_t;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : IQ_idx_t;
  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  RENAME_dmb_sy_is_in_state_set := false;
  RENAME_while_break := false;
  RENAME_found_entry := false;
  if (next_state.core_[ j ].RENAME_.num_entries = 0) then
    RENAME_while_break := true;
  end;
  RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
  RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
  RENAME_offset := 0;
  while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
    RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
    if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_sy)) then
      if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
        RENAME_dmb_sy_is_in_state_set := true;
      else
        RENAME_dmb_sy_is_in_state_set := false;
      end;
      RENAME_found_entry := true;
    end;
    if (RENAME_offset < RENAME_difference) then
      RENAME_offset := (RENAME_offset + 1);
    end;
  end;
  if (RENAME_found_entry = false) then
  end;
  ROB_dmb_sy_is_in_state_set := false;
  ROB_while_break := false;
  ROB_found_entry := false;
  if (next_state.core_[ j ].ROB_.num_entries = 0) then
    ROB_while_break := true;
  end;
  ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
  ROB_difference := next_state.core_[ j ].ROB_.num_entries;
  ROB_offset := 0;
  while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
    ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
    if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_sy)) then
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po))))) then
        ROB_dmb_sy_is_in_state_set := true;
      else
        ROB_dmb_sy_is_in_state_set := false;
      end;
      ROB_found_entry := true;
    end;
    if (ROB_offset < ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  is_instruction_on_any_state := (RENAME_dmb_sy_is_in_state_set | ROB_dmb_sy_is_in_state_set);
  if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
    if is_instruction_on_any_state then
      next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
    else
      RENAME_dmb_ld_is_in_state_set := false;
      RENAME_while_break := false;
      RENAME_found_entry := false;
      if (next_state.core_[ j ].RENAME_.num_entries = 0) then
        RENAME_while_break := true;
      end;
      RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
      RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
      RENAME_offset := 0;
      while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
        RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_ld)) then
          if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
            RENAME_dmb_ld_is_in_state_set := true;
          else
            RENAME_dmb_ld_is_in_state_set := false;
          end;
          RENAME_found_entry := true;
        end;
        if (RENAME_offset < RENAME_difference) then
          RENAME_offset := (RENAME_offset + 1);
        end;
      end;
      if (RENAME_found_entry = false) then
      end;
      ROB_dmb_ld_is_in_state_set := false;
      ROB_while_break := false;
      ROB_found_entry := false;
      if (next_state.core_[ j ].ROB_.num_entries = 0) then
        ROB_while_break := true;
      end;
      ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
      ROB_difference := next_state.core_[ j ].ROB_.num_entries;
      ROB_offset := 0;
      while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
        ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_ld)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))) then
            ROB_dmb_ld_is_in_state_set := true;
          else
            ROB_dmb_ld_is_in_state_set := false;
          end;
          ROB_found_entry := true;
        end;
        if (ROB_offset < ROB_difference) then
          ROB_offset := (ROB_offset + 1);
        end;
      end;
      if (ROB_found_entry = false) then
      end;
      is_instruction_on_any_state := (RENAME_dmb_ld_is_in_state_set | ROB_dmb_ld_is_in_state_set);
      if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
        else
          RENAME_LDAR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_LDAR_is_in_state_set := true;
              else
                RENAME_LDAR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_LDAR_is_in_state_set := true;
            else
              IQ_LDAR_is_in_state_set := false;
            end;
          end;
          LSQ_LDAR_is_in_state_set := false;
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                LSQ_LDAR_is_in_state_set := true;
              else
                LSQ_LDAR_is_in_state_set := false;
              end;
              LSQ_found_entry := true;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          if (LSQ_found_entry = false) then
          end;
          ROB_LDAR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                ROB_LDAR_is_in_state_set := true;
              else
                ROB_LDAR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
          if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
            else
              RENAME_LDAR_is_in_state_set := false;
              RENAME_while_break := false;
              RENAME_found_entry := false;
              if (next_state.core_[ j ].RENAME_.num_entries = 0) then
                RENAME_while_break := true;
              end;
              RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
              RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
              RENAME_offset := 0;
              while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
                RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                    RENAME_LDAR_is_in_state_set := true;
                  else
                    RENAME_LDAR_is_in_state_set := false;
                  end;
                  RENAME_found_entry := true;
                end;
                if (RENAME_offset < RENAME_difference) then
                  RENAME_offset := (RENAME_offset + 1);
                end;
              end;
              if (RENAME_found_entry = false) then
              end;
              IQ_LDAR_is_in_state_set := false;
              found_entry := false;
              for IQ_iter : IQ_idx_t do
                if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                  if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                    if (found_entry = false) then
                      found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    else
                      if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                        found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                        found_idx := IQ_iter;
                      end;
                    end;
                    found_entry := true;
                  end;
                end;
              endfor;
              if (found_entry = false) then
              elsif (found_entry = true) then
                if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                  IQ_LDAR_is_in_state_set := true;
                else
                  IQ_LDAR_is_in_state_set := false;
                end;
              end;
              LSQ_LDAR_is_in_state_set := false;
              LSQ_while_break := false;
              LSQ_found_entry := false;
              if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                LSQ_while_break := true;
              end;
              LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
              LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
              LSQ_offset := 0;
              while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
                  if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                    LSQ_LDAR_is_in_state_set := true;
                  else
                    LSQ_LDAR_is_in_state_set := false;
                  end;
                  LSQ_found_entry := true;
                end;
                if (LSQ_offset < LSQ_difference) then
                  LSQ_offset := (LSQ_offset + 1);
                end;
              end;
              if (LSQ_found_entry = false) then
              end;
              ROB_LDAR_is_in_state_set := false;
              ROB_while_break := false;
              ROB_found_entry := false;
              if (next_state.core_[ j ].ROB_.num_entries = 0) then
                ROB_while_break := true;
              end;
              ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
              ROB_difference := next_state.core_[ j ].ROB_.num_entries;
              ROB_offset := 0;
              while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
                ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                    ROB_LDAR_is_in_state_set := true;
                  else
                    ROB_LDAR_is_in_state_set := false;
                  end;
                  ROB_found_entry := true;
                end;
                if (ROB_offset < ROB_difference) then
                  ROB_offset := (ROB_offset + 1);
                end;
              end;
              if (ROB_found_entry = false) then
              end;
              is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
              if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
                if is_instruction_on_any_state then
                  next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
                else
                  next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
                  next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
                  next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                  next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                  next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                  next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
                  next_state.core_[ j ].mem_interface_.out_busy := true;
                  next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
                end;
              else
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
              end;
            end;
          else
            RENAME_LDAR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_LDAR_is_in_state_set := true;
                else
                  RENAME_LDAR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_LDAR_is_in_state_set := true;
              else
                IQ_LDAR_is_in_state_set := false;
              end;
            end;
            LSQ_LDAR_is_in_state_set := false;
            LSQ_while_break := false;
            LSQ_found_entry := false;
            if (next_state.core_[ j ].LSQ_.num_entries = 0) then
              LSQ_while_break := true;
            end;
            LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
            LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
            LSQ_offset := 0;
            while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
              LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                  LSQ_LDAR_is_in_state_set := true;
                else
                  LSQ_LDAR_is_in_state_set := false;
                end;
                LSQ_found_entry := true;
              end;
              if (LSQ_offset < LSQ_difference) then
                LSQ_offset := (LSQ_offset + 1);
              end;
            end;
            if (LSQ_found_entry = false) then
            end;
            ROB_LDAR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                  ROB_LDAR_is_in_state_set := true;
                else
                  ROB_LDAR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
            if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
              else
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
              end;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          end;
        end;
      else
        RENAME_LDAR_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_LDAR_is_in_state_set := true;
            else
              RENAME_LDAR_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_LDAR_is_in_state_set := true;
          else
            IQ_LDAR_is_in_state_set := false;
          end;
        end;
        LSQ_LDAR_is_in_state_set := false;
        LSQ_while_break := false;
        LSQ_found_entry := false;
        if (next_state.core_[ j ].LSQ_.num_entries = 0) then
          LSQ_while_break := true;
        end;
        LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
        LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
        LSQ_offset := 0;
        while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
          LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
              LSQ_LDAR_is_in_state_set := true;
            else
              LSQ_LDAR_is_in_state_set := false;
            end;
            LSQ_found_entry := true;
          end;
          if (LSQ_offset < LSQ_difference) then
            LSQ_offset := (LSQ_offset + 1);
          end;
        end;
        if (LSQ_found_entry = false) then
        end;
        ROB_LDAR_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
              ROB_LDAR_is_in_state_set := true;
            else
              ROB_LDAR_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
        if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
          else
            RENAME_LDAR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_LDAR_is_in_state_set := true;
                else
                  RENAME_LDAR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_LDAR_is_in_state_set := true;
              else
                IQ_LDAR_is_in_state_set := false;
              end;
            end;
            LSQ_LDAR_is_in_state_set := false;
            LSQ_while_break := false;
            LSQ_found_entry := false;
            if (next_state.core_[ j ].LSQ_.num_entries = 0) then
              LSQ_while_break := true;
            end;
            LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
            LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
            LSQ_offset := 0;
            while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
              LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                  LSQ_LDAR_is_in_state_set := true;
                else
                  LSQ_LDAR_is_in_state_set := false;
                end;
                LSQ_found_entry := true;
              end;
              if (LSQ_offset < LSQ_difference) then
                LSQ_offset := (LSQ_offset + 1);
              end;
            end;
            if (LSQ_found_entry = false) then
            end;
            ROB_LDAR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                  ROB_LDAR_is_in_state_set := true;
                else
                  ROB_LDAR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
            if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
              else
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
              end;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          end;
        else
          RENAME_LDAR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_LDAR_is_in_state_set := true;
              else
                RENAME_LDAR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_LDAR_is_in_state_set := true;
            else
              IQ_LDAR_is_in_state_set := false;
            end;
          end;
          LSQ_LDAR_is_in_state_set := false;
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                LSQ_LDAR_is_in_state_set := true;
              else
                LSQ_LDAR_is_in_state_set := false;
              end;
              LSQ_found_entry := true;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          if (LSQ_found_entry = false) then
          end;
          ROB_LDAR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                ROB_LDAR_is_in_state_set := true;
              else
                ROB_LDAR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
          if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          else
            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
            next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
            next_state.core_[ j ].mem_interface_.out_busy := true;
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
          end;
        end;
      end;
    end;
  else
    RENAME_dmb_ld_is_in_state_set := false;
    RENAME_while_break := false;
    RENAME_found_entry := false;
    if (next_state.core_[ j ].RENAME_.num_entries = 0) then
      RENAME_while_break := true;
    end;
    RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
    RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
    RENAME_offset := 0;
    while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
      RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
      if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_ld)) then
        if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
          RENAME_dmb_ld_is_in_state_set := true;
        else
          RENAME_dmb_ld_is_in_state_set := false;
        end;
        RENAME_found_entry := true;
      end;
      if (RENAME_offset < RENAME_difference) then
        RENAME_offset := (RENAME_offset + 1);
      end;
    end;
    if (RENAME_found_entry = false) then
    end;
    ROB_dmb_ld_is_in_state_set := false;
    ROB_while_break := false;
    ROB_found_entry := false;
    if (next_state.core_[ j ].ROB_.num_entries = 0) then
      ROB_while_break := true;
    end;
    ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
    ROB_difference := next_state.core_[ j ].ROB_.num_entries;
    ROB_offset := 0;
    while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
      ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_ld)) then
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))) then
          ROB_dmb_ld_is_in_state_set := true;
        else
          ROB_dmb_ld_is_in_state_set := false;
        end;
        ROB_found_entry := true;
      end;
      if (ROB_offset < ROB_difference) then
        ROB_offset := (ROB_offset + 1);
      end;
    end;
    if (ROB_found_entry = false) then
    end;
    is_instruction_on_any_state := (RENAME_dmb_ld_is_in_state_set | ROB_dmb_ld_is_in_state_set);
    if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
      if is_instruction_on_any_state then
        next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
      else
        RENAME_LDAR_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_LDAR_is_in_state_set := true;
            else
              RENAME_LDAR_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_LDAR_is_in_state_set := true;
          else
            IQ_LDAR_is_in_state_set := false;
          end;
        end;
        LSQ_LDAR_is_in_state_set := false;
        LSQ_while_break := false;
        LSQ_found_entry := false;
        if (next_state.core_[ j ].LSQ_.num_entries = 0) then
          LSQ_while_break := true;
        end;
        LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
        LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
        LSQ_offset := 0;
        while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
          LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
              LSQ_LDAR_is_in_state_set := true;
            else
              LSQ_LDAR_is_in_state_set := false;
            end;
            LSQ_found_entry := true;
          end;
          if (LSQ_offset < LSQ_difference) then
            LSQ_offset := (LSQ_offset + 1);
          end;
        end;
        if (LSQ_found_entry = false) then
        end;
        ROB_LDAR_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
              ROB_LDAR_is_in_state_set := true;
            else
              ROB_LDAR_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
        if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
          else
            RENAME_LDAR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_LDAR_is_in_state_set := true;
                else
                  RENAME_LDAR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_LDAR_is_in_state_set := true;
              else
                IQ_LDAR_is_in_state_set := false;
              end;
            end;
            LSQ_LDAR_is_in_state_set := false;
            LSQ_while_break := false;
            LSQ_found_entry := false;
            if (next_state.core_[ j ].LSQ_.num_entries = 0) then
              LSQ_while_break := true;
            end;
            LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
            LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
            LSQ_offset := 0;
            while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
              LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                  LSQ_LDAR_is_in_state_set := true;
                else
                  LSQ_LDAR_is_in_state_set := false;
                end;
                LSQ_found_entry := true;
              end;
              if (LSQ_offset < LSQ_difference) then
                LSQ_offset := (LSQ_offset + 1);
              end;
            end;
            if (LSQ_found_entry = false) then
            end;
            ROB_LDAR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                  ROB_LDAR_is_in_state_set := true;
                else
                  ROB_LDAR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
            if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
              else
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
              end;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          end;
        else
          RENAME_LDAR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_LDAR_is_in_state_set := true;
              else
                RENAME_LDAR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_LDAR_is_in_state_set := true;
            else
              IQ_LDAR_is_in_state_set := false;
            end;
          end;
          LSQ_LDAR_is_in_state_set := false;
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                LSQ_LDAR_is_in_state_set := true;
              else
                LSQ_LDAR_is_in_state_set := false;
              end;
              LSQ_found_entry := true;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          if (LSQ_found_entry = false) then
          end;
          ROB_LDAR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                ROB_LDAR_is_in_state_set := true;
              else
                ROB_LDAR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
          if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          else
            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
            next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
            next_state.core_[ j ].mem_interface_.out_busy := true;
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
          end;
        end;
      end;
    else
      RENAME_LDAR_is_in_state_set := false;
      RENAME_while_break := false;
      RENAME_found_entry := false;
      if (next_state.core_[ j ].RENAME_.num_entries = 0) then
        RENAME_while_break := true;
      end;
      RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
      RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
      RENAME_offset := 0;
      while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
        RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
          if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
            RENAME_LDAR_is_in_state_set := true;
          else
            RENAME_LDAR_is_in_state_set := false;
          end;
          RENAME_found_entry := true;
        end;
        if (RENAME_offset < RENAME_difference) then
          RENAME_offset := (RENAME_offset + 1);
        end;
      end;
      if (RENAME_found_entry = false) then
      end;
      IQ_LDAR_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              end;
            end;
            found_entry := true;
          end;
        end;
      endfor;
      if (found_entry = false) then
      elsif (found_entry = true) then
        if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
          IQ_LDAR_is_in_state_set := true;
        else
          IQ_LDAR_is_in_state_set := false;
        end;
      end;
      LSQ_LDAR_is_in_state_set := false;
      LSQ_while_break := false;
      LSQ_found_entry := false;
      if (next_state.core_[ j ].LSQ_.num_entries = 0) then
        LSQ_while_break := true;
      end;
      LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
      LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
      LSQ_offset := 0;
      while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
        LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
            LSQ_LDAR_is_in_state_set := true;
          else
            LSQ_LDAR_is_in_state_set := false;
          end;
          LSQ_found_entry := true;
        end;
        if (LSQ_offset < LSQ_difference) then
          LSQ_offset := (LSQ_offset + 1);
        end;
      end;
      if (LSQ_found_entry = false) then
      end;
      ROB_LDAR_is_in_state_set := false;
      ROB_while_break := false;
      ROB_found_entry := false;
      if (next_state.core_[ j ].ROB_.num_entries = 0) then
        ROB_while_break := true;
      end;
      ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
      ROB_difference := next_state.core_[ j ].ROB_.num_entries;
      ROB_offset := 0;
      while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
        ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
            ROB_LDAR_is_in_state_set := true;
          else
            ROB_LDAR_is_in_state_set := false;
          end;
          ROB_found_entry := true;
        end;
        if (ROB_offset < ROB_difference) then
          ROB_offset := (ROB_offset + 1);
        end;
      end;
      if (ROB_found_entry = false) then
      end;
      is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
      if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
        else
          RENAME_LDAR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_LDAR_is_in_state_set := true;
              else
                RENAME_LDAR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_LDAR_is_in_state_set := true;
            else
              IQ_LDAR_is_in_state_set := false;
            end;
          end;
          LSQ_LDAR_is_in_state_set := false;
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
                LSQ_LDAR_is_in_state_set := true;
              else
                LSQ_LDAR_is_in_state_set := false;
              end;
              LSQ_found_entry := true;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          if (LSQ_found_entry = false) then
          end;
          ROB_LDAR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
                ROB_LDAR_is_in_state_set := true;
              else
                ROB_LDAR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
          if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
            else
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
            end;
          else
            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
            next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
            next_state.core_[ j ].mem_interface_.out_busy := true;
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
          end;
        end;
      else
        RENAME_LDAR_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_LDAR_is_in_state_set := true;
            else
              RENAME_LDAR_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_LDAR_is_in_state_set := true;
          else
            IQ_LDAR_is_in_state_set := false;
          end;
        end;
        LSQ_LDAR_is_in_state_set := false;
        LSQ_while_break := false;
        LSQ_found_entry := false;
        if (next_state.core_[ j ].LSQ_.num_entries = 0) then
          LSQ_while_break := true;
        end;
        LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
        LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
        LSQ_offset := 0;
        while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
          LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_creation) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled))))))) then
              LSQ_LDAR_is_in_state_set := true;
            else
              LSQ_LDAR_is_in_state_set := false;
            end;
            LSQ_found_entry := true;
          end;
          if (LSQ_offset < LSQ_difference) then
            LSQ_offset := (LSQ_offset + 1);
          end;
        end;
        if (LSQ_found_entry = false) then
        end;
        ROB_LDAR_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed))))) then
              ROB_LDAR_is_in_state_set := true;
            else
              ROB_LDAR_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | (LSQ_LDAR_is_in_state_set | ROB_LDAR_is_in_state_set)));
        if (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
          else
            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
            next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
            next_state.core_[ j ].mem_interface_.out_busy := true;
            next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
          end;
        else
          next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ i ].phys_addr;
          next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
          next_state.core_[ j ].mem_interface_.out_msg.valid := true;
          next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
          next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
          next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num;
          next_state.core_[ j ].mem_interface_.out_busy := true;
          next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_load_mem_response;
        end;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LSQ_idx_t do 
  rule "LSQ lsq_ld_st_fwd_branch ===> lsq_build_packet_send_mem_request || lsq_ld_write_result || lsq_st_await_committed || lsq_await_creation" 
(Sta.core_[ j ].LSQ_.entries[ i ].state = lsq_ld_st_fwd_branch)
==>
 
  var violating_seq_num : inst_count_t;
  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var inclusive_squash : boolean;
  var squash_inclusive_if_from_sq : boolean;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_squash_curr_idx : ROB_idx_t;
  var ROB_squash_remove_count : ROB_count_t;
  var LSQ_squash_curr_idx : LSQ_idx_t;
  var LSQ_squash_remove_count : LSQ_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = ldar)) then
    LSQ_while_break := false;
    LSQ_found_entry := false;
    if (next_state.core_[ j ].LSQ_.num_entries = 0) then
      LSQ_while_break := true;
    end;
    LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
    LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
    LSQ_offset := 0;
    while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
      LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
      if (((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = st) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = stlr)) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr = next_state.core_[ j ].LSQ_.entries[ i ].phys_addr) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state != lsq_await_creation) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state != lsq_await_scheduled) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state != lsq_ld_st_await_translation)))))) then
        next_state.core_[ j ].LSQ_.entries[ i ].read_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
        next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_ld_write_result;
        LSQ_found_entry := true;
      end;
      if (LSQ_offset < LSQ_difference) then
        LSQ_offset := (LSQ_offset + 1);
      end;
    end;
    if (LSQ_found_entry = false) then
      next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_build_packet_send_mem_request;
    end;
  else
    if ((next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].LSQ_.entries[ i ].instruction.op = stlr)) then
      LSQ_while_break := false;
      LSQ_found_entry := false;
      if (next_state.core_[ j ].LSQ_.num_entries = 0) then
        LSQ_while_break := true;
      end;
      LSQ_entry_idx := next_state.core_[ j ].LSQ_.head;
      LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
      LSQ_offset := 0;
      while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
        LSQ_curr_idx := ((LSQ_entry_idx + LSQ_offset) % LSQ_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr = next_state.core_[ j ].LSQ_.entries[ i ].phys_addr) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num > next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) & ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ldar)))) then
          violating_seq_num := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := next_state.core_[ j ].ROB_.head;
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          ROB_squash_remove_count := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_squash_curr_idx := ((ROB_entry_idx + ROB_offset) % ROB_NUM_ENTRIES_CONST);
            if true then
              if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_await_executed) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ldar)) then
                  end;
                  inclusive_squash := true;
                  for IQ_squash_idx : IQ_idx_t do
                    if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                      if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                        violating_seq_num := violating_seq_num;
                        inclusive_squash := inclusive_squash;
                        if inclusive_squash then
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        else
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        end;
                        if squash_inclusive_if_from_sq then
                          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                        end;
                      elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                      else
                        error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                      end;
                    end;
                  endfor;
                  ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].state := issue_if_head;
                  next_state.core_[ j ].RENAME_.tail := ((next_state.core_[ j ].RENAME_.tail + 1) % RENAME_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries + 1);
                  next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                end;
              elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_clear_lsq_store_head) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ldar)) then
                  end;
                  inclusive_squash := true;
                  for IQ_squash_idx : IQ_idx_t do
                    if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                      if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                        violating_seq_num := violating_seq_num;
                        inclusive_squash := inclusive_squash;
                        if inclusive_squash then
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        else
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        end;
                        if squash_inclusive_if_from_sq then
                          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                        end;
                      elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                      else
                        error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                      end;
                    end;
                  endfor;
                  ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].state := issue_if_head;
                  next_state.core_[ j ].RENAME_.tail := ((next_state.core_[ j ].RENAME_.tail + 1) % RENAME_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries + 1);
                  next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                end;
               elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_commit_time_await_st_mem_resp) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ldar)) then
                  end;
                  inclusive_squash := true;
                  for IQ_squash_idx : IQ_idx_t do
                    if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                      if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                        violating_seq_num := violating_seq_num;
                        inclusive_squash := inclusive_squash;
                        if inclusive_squash then
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        else
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        end;
                        if squash_inclusive_if_from_sq then
                          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                        end;
                      elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                      else
                        error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                      end;
                    end;
                  endfor;
                  ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].state := issue_if_head;
                  next_state.core_[ j ].RENAME_.tail := ((next_state.core_[ j ].RENAME_.tail + 1) % RENAME_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries + 1);
                  next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                end;
               elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_commit_po) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ldar)) then
                  end;
                  inclusive_squash := true;
                  for IQ_squash_idx : IQ_idx_t do
                    if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                      if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                        violating_seq_num := violating_seq_num;
                        inclusive_squash := inclusive_squash;
                        if inclusive_squash then
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        else
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        end;
                        if squash_inclusive_if_from_sq then
                          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                        end;
                      elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                      else
                        error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                      end;
                    end;
                  endfor;
                  ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].state := issue_if_head;
                  next_state.core_[ j ].RENAME_.tail := ((next_state.core_[ j ].RENAME_.tail + 1) % RENAME_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries + 1);
                  next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                end;
               elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_commit_if_head) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.op = ldar)) then
                  end;
                  inclusive_squash := true;
                  for IQ_squash_idx : IQ_idx_t do
                    if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                      if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                        violating_seq_num := violating_seq_num;
                        inclusive_squash := inclusive_squash;
                        if inclusive_squash then
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        else
                          squash_inclusive_if_from_sq := (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num);
                        end;
                        if squash_inclusive_if_from_sq then
                          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                          next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                        end;
                      elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                      else
                        error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                      end;
                    end;
                  endfor;
                  ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                  next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.tail ].state := issue_if_head;
                  next_state.core_[ j ].RENAME_.tail := ((next_state.core_[ j ].RENAME_.tail + 1) % RENAME_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries + 1);
                  next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                end;
               elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_await_creation) then
              else
                error "Controller is not on an expected state for a msg: (squash) from: (LSQ) to: (ROB)";
              end;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - ROB_squash_remove_count)) % ROB_NUM_ENTRIES_CONST);
          next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - ROB_squash_remove_count);
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := next_state.core_[ j ].LSQ_.head;
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          LSQ_squash_remove_count := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_squash_curr_idx := ((LSQ_entry_idx + LSQ_offset) % LSQ_NUM_ENTRIES_CONST);
            if true then
              if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_st_await_committed) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
              elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_ld_await_committed) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_ld_write_result) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_await_load_mem_response) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_build_packet_send_mem_request) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_ld_st_fwd_branch) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_ld_st_await_translation) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
               elsif (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state = lsq_await_scheduled) then
                violating_seq_num := violating_seq_num;
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                  LSQ_squash_remove_count := (LSQ_squash_remove_count + 1);
                  next_state.core_[ j ].LSQ_.entries[ LSQ_squash_curr_idx ].state := lsq_await_creation;
                end;
              else
                error "Controller is not on an expected state for a msg: (squash) from: (LSQ) to: (LSQ)";
              end;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          next_state.core_[ j ].LSQ_.tail := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - LSQ_squash_remove_count)) % LSQ_NUM_ENTRIES_CONST);
          next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - LSQ_squash_remove_count);
          LSQ_found_entry := true;
        end;
        if (LSQ_offset != LSQ_difference) then
          LSQ_offset := (LSQ_offset + 1);
        end;
      end;
      if (LSQ_found_entry = false) then
      end;
      ROB_while_break := false;
      ROB_found_entry := false;
      if (next_state.core_[ j ].ROB_.num_entries = 0) then
        ROB_while_break := true;
      end;
      ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
      ROB_difference := next_state.core_[ j ].ROB_.num_entries;
      ROB_offset := 0;
      while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
        ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num) then
          if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) then
            next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
            next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
          else
            error "Controller is not on an expected state for a msg: (st_executed) from: (LSQ) to: (ROB)";
          end;
          ROB_found_entry := true;
        end;
        if (ROB_offset < ROB_difference) then
          ROB_offset := (ROB_offset + 1);
        end;
      end;
      if (ROB_found_entry = false) then
      end;
      next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_st_await_committed;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LSQ_idx_t do 
  rule "LSQ lsq_ld_st_await_translation ===> lsq_ld_st_fwd_branch || lsq_await_creation" 
(Sta.core_[ j ].LSQ_.entries[ i ].state = lsq_ld_st_await_translation)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].LSQ_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ i ].virt_addr;
  next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_ld_st_fwd_branch;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LSQ_idx_t do 
  rule "LSQ init_default_state ===> lsq_await_creation" 
(Sta.core_[ j ].LSQ_.entries[ i ].state = init_default_state)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].LSQ_.entries[ i ].virt_addr := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].phys_addr := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].read_value := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].write_value := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].seq_num := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].LSQ_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].LSQ_.entries[ i ].state := lsq_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_clear_lsq_store_head ===> rob_await_creation" 
(Sta.core_[ j ].ROB_.entries[ i ].state = rob_clear_lsq_store_head)
==>
 
  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  LSQ_while_break := false;
  LSQ_found_entry := false;
  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
    LSQ_while_break := true;
  end;
  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
  LSQ_offset := 0;
  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) then
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
      else
        error "Controller is not on an expected state for a msg: (store_committed) from: (ROB) to: (LSQ)";
      end;
      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
      LSQ_found_entry := true;
    end;
    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  if (LSQ_found_entry = false) then
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_commit_po ===> rob_commit_po || rob_await_creation || rob_commit_time_await_st_mem_resp" 
(((((((((((((((((Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_po) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var RENAME_dmb_sy_is_in_state_set : boolean;
  var ROB_dmb_sy_is_in_state_set : boolean;
  var is_instruction_on_any_state : boolean;
  var RENAME_dmb_st_is_in_state_set : boolean;
  var ROB_dmb_st_is_in_state_set : boolean;
  var RENAME_Load_is_in_state_set : boolean;
  var IQ_Load_is_in_state_set : boolean;
  var LSQ_Load_is_in_state_set : boolean;
  var ROB_Load_is_in_state_set : boolean;
  var RENAME_Store_is_in_state_set : boolean;
  var IQ_Store_is_in_state_set : boolean;
  var ROB_Store_is_in_state_set : boolean;
  var RENAME_STLR_is_in_state_set : boolean;
  var IQ_STLR_is_in_state_set : boolean;
  var ROB_STLR_is_in_state_set : boolean;
  var RENAME_while_break : boolean;
  var RENAME_found_entry : boolean;
  var RENAME_entry_idx : RENAME_idx_t;
  var RENAME_difference : RENAME_count_t;
  var RENAME_offset : RENAME_count_t;
  var RENAME_curr_idx : RENAME_idx_t;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : IQ_idx_t;
  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  RENAME_dmb_sy_is_in_state_set := false;
  RENAME_while_break := false;
  RENAME_found_entry := false;
  if (next_state.core_[ j ].RENAME_.num_entries = 0) then
    RENAME_while_break := true;
  end;
  RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
  RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
  RENAME_offset := 0;
  while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
    RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
    if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_sy)) then
      if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
        RENAME_dmb_sy_is_in_state_set := true;
      else
        RENAME_dmb_sy_is_in_state_set := false;
      end;
      RENAME_found_entry := true;
    end;
    if (RENAME_offset < RENAME_difference) then
      RENAME_offset := (RENAME_offset + 1);
    end;
  end;
  if (RENAME_found_entry = false) then
  end;
  ROB_dmb_sy_is_in_state_set := false;
  ROB_while_break := false;
  ROB_found_entry := false;
  if (next_state.core_[ j ].ROB_.num_entries = 0) then
    ROB_while_break := true;
  end;
  ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
  ROB_difference := next_state.core_[ j ].ROB_.num_entries;
  ROB_offset := 0;
  while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
    ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
    if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_sy)) then
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po))))) then
        ROB_dmb_sy_is_in_state_set := true;
      else
        ROB_dmb_sy_is_in_state_set := false;
      end;
      ROB_found_entry := true;
    end;
    if (ROB_offset < ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  is_instruction_on_any_state := (RENAME_dmb_sy_is_in_state_set | ROB_dmb_sy_is_in_state_set);
  if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) then
    if is_instruction_on_any_state then
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
    else
      RENAME_dmb_st_is_in_state_set := false;
      RENAME_while_break := false;
      RENAME_found_entry := false;
      if (next_state.core_[ j ].RENAME_.num_entries = 0) then
        RENAME_while_break := true;
      end;
      RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
      RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
      RENAME_offset := 0;
      while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
        RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_st)) then
          if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
            RENAME_dmb_st_is_in_state_set := true;
          else
            RENAME_dmb_st_is_in_state_set := false;
          end;
          RENAME_found_entry := true;
        end;
        if (RENAME_offset < RENAME_difference) then
          RENAME_offset := (RENAME_offset + 1);
        end;
      end;
      if (RENAME_found_entry = false) then
      end;
      ROB_dmb_st_is_in_state_set := false;
      ROB_while_break := false;
      ROB_found_entry := false;
      if (next_state.core_[ j ].ROB_.num_entries = 0) then
        ROB_while_break := true;
      end;
      ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
      ROB_difference := next_state.core_[ j ].ROB_.num_entries;
      ROB_offset := 0;
      while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
        ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_st)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po))))) then
            ROB_dmb_st_is_in_state_set := true;
          else
            ROB_dmb_st_is_in_state_set := false;
          end;
          ROB_found_entry := true;
        end;
        if (ROB_offset < ROB_difference) then
          ROB_offset := (ROB_offset + 1);
        end;
      end;
      if (ROB_found_entry = false) then
      end;
      is_instruction_on_any_state := (RENAME_dmb_st_is_in_state_set | ROB_dmb_st_is_in_state_set);
      if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
        else
          RENAME_Load_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ld)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_Load_is_in_state_set := true;
              else
                RENAME_Load_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_Load_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ld)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_Load_is_in_state_set := true;
            else
              IQ_Load_is_in_state_set := false;
            end;
          end;
          LSQ_Load_is_in_state_set := false;
          LSQ_while_break := false;
          LSQ_found_entry := false;
          if (next_state.core_[ j ].LSQ_.num_entries = 0) then
            LSQ_while_break := true;
          end;
          LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
          LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
          LSQ_offset := 0;
          while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
            LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ld)) then
              if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request))))))) then
                LSQ_Load_is_in_state_set := true;
              else
                LSQ_Load_is_in_state_set := false;
              end;
              LSQ_found_entry := true;
            end;
            if (LSQ_offset < LSQ_difference) then
              LSQ_offset := (LSQ_offset + 1);
            end;
          end;
          if (LSQ_found_entry = false) then
          end;
          ROB_Load_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ld)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))))) then
                ROB_Load_is_in_state_set := true;
              else
                ROB_Load_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_Load_is_in_state_set | (IQ_Load_is_in_state_set | (LSQ_Load_is_in_state_set | ROB_Load_is_in_state_set)));
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
            else
              RENAME_Store_is_in_state_set := false;
              RENAME_while_break := false;
              RENAME_found_entry := false;
              if (next_state.core_[ j ].RENAME_.num_entries = 0) then
                RENAME_while_break := true;
              end;
              RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
              RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
              RENAME_offset := 0;
              while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
                RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
                  if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                    RENAME_Store_is_in_state_set := true;
                  else
                    RENAME_Store_is_in_state_set := false;
                  end;
                  RENAME_found_entry := true;
                end;
                if (RENAME_offset < RENAME_difference) then
                  RENAME_offset := (RENAME_offset + 1);
                end;
              end;
              if (RENAME_found_entry = false) then
              end;
              IQ_Store_is_in_state_set := false;
              found_entry := false;
              for IQ_iter : IQ_idx_t do
                if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                  if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                    if (found_entry = false) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                        found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                        found_idx := IQ_iter;
                      end;
                    end;
                    found_entry := true;
                  end;
                end;
              endfor;
              if (found_entry = false) then
              elsif (found_entry = true) then
                if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                  IQ_Store_is_in_state_set := true;
                else
                  IQ_Store_is_in_state_set := false;
                end;
              end;
              ROB_Store_is_in_state_set := false;
              ROB_while_break := false;
              ROB_found_entry := false;
              if (next_state.core_[ j ].ROB_.num_entries = 0) then
                ROB_while_break := true;
              end;
              ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
              ROB_difference := next_state.core_[ j ].ROB_.num_entries;
              ROB_offset := 0;
              while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
                ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                    ROB_Store_is_in_state_set := true;
                  else
                    ROB_Store_is_in_state_set := false;
                  end;
                  ROB_found_entry := true;
                end;
                if (ROB_offset < ROB_difference) then
                  ROB_offset := (ROB_offset + 1);
                end;
              end;
              if (ROB_found_entry = false) then
              end;
              RENAME_STLR_is_in_state_set := false;
              RENAME_while_break := false;
              RENAME_found_entry := false;
              if (next_state.core_[ j ].RENAME_.num_entries = 0) then
                RENAME_while_break := true;
              end;
              RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
              RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
              RENAME_offset := 0;
              while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
                RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
                  if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                    RENAME_STLR_is_in_state_set := true;
                  else
                    RENAME_STLR_is_in_state_set := false;
                  end;
                  RENAME_found_entry := true;
                end;
                if (RENAME_offset < RENAME_difference) then
                  RENAME_offset := (RENAME_offset + 1);
                end;
              end;
              if (RENAME_found_entry = false) then
              end;
              IQ_STLR_is_in_state_set := false;
              found_entry := false;
              for IQ_iter : IQ_idx_t do
                if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                  if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                    if (found_entry = false) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                        found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                        found_idx := IQ_iter;
                      end;
                    end;
                    found_entry := true;
                  end;
                end;
              endfor;
              if (found_entry = false) then
              elsif (found_entry = true) then
                if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                  IQ_STLR_is_in_state_set := true;
                else
                  IQ_STLR_is_in_state_set := false;
                end;
              end;
              ROB_STLR_is_in_state_set := false;
              ROB_while_break := false;
              ROB_found_entry := false;
              if (next_state.core_[ j ].ROB_.num_entries = 0) then
                ROB_while_break := true;
              end;
              ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
              ROB_difference := next_state.core_[ j ].ROB_.num_entries;
              ROB_offset := 0;
              while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
                ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
                  if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                    ROB_STLR_is_in_state_set := true;
                  else
                    ROB_STLR_is_in_state_set := false;
                  end;
                  ROB_found_entry := true;
                end;
                if (ROB_offset < ROB_difference) then
                  ROB_offset := (ROB_offset + 1);
                end;
              end;
              if (ROB_found_entry = false) then
              end;
              is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
              if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
                if is_instruction_on_any_state then
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                    next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                  else
                    LSQ_while_break := false;
                    LSQ_found_entry := false;
                    if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                      LSQ_while_break := true;
                    end;
                    LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                    LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                    LSQ_offset := 0;
                    while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                      LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                          if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                            next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                            next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                            next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                            next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                          else
                            error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                          end;
                          next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                          next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                          next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                          next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                        else
                          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                            next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                            next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                            next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                            next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                            next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                            for core_idx : cores_t do
                              next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                              next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                            endfor;
                            next_state.core_[ j ].mem_interface_.out_busy := true;
                            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                          end;
                        end;
                        LSQ_found_entry := true;
                      end;
                      if (LSQ_offset < LSQ_difference) then
                        LSQ_offset := (LSQ_offset + 1);
                      end;
                    end;
                    if (LSQ_found_entry = false) then
                    end;
                  end;
                end;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  LSQ_while_break := false;
                  LSQ_found_entry := false;
                  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                    LSQ_while_break := true;
                  end;
                  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                  LSQ_offset := 0;
                  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                        if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                          next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                          next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                          next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                        else
                          error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                        end;
                        next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                        next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                      else
                        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                          next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                          next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                          next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                          next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                          next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                          next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                          for core_idx : cores_t do
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                          endfor;
                          next_state.core_[ j ].mem_interface_.out_busy := true;
                          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                        end;
                      end;
                      LSQ_found_entry := true;
                    end;
                    if (LSQ_offset < LSQ_difference) then
                      LSQ_offset := (LSQ_offset + 1);
                    end;
                  end;
                  if (LSQ_found_entry = false) then
                  end;
                end;
              end;
            end;
          else
            RENAME_Store_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_Store_is_in_state_set := true;
                else
                  RENAME_Store_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_Store_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_Store_is_in_state_set := true;
              else
                IQ_Store_is_in_state_set := false;
              end;
            end;
            ROB_Store_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_Store_is_in_state_set := true;
                else
                  ROB_Store_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            RENAME_STLR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_STLR_is_in_state_set := true;
                else
                  RENAME_STLR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_STLR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_STLR_is_in_state_set := true;
              else
                IQ_STLR_is_in_state_set := false;
              end;
            end;
            ROB_STLR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_STLR_is_in_state_set := true;
                else
                  ROB_STLR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
            if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  LSQ_while_break := false;
                  LSQ_found_entry := false;
                  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                    LSQ_while_break := true;
                  end;
                  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                  LSQ_offset := 0;
                  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                        if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                          next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                          next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                          next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                        else
                          error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                        end;
                        next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                        next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                      else
                        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                          next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                          next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                          next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                          next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                          next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                          next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                          for core_idx : cores_t do
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                          endfor;
                          next_state.core_[ j ].mem_interface_.out_busy := true;
                          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                        end;
                      end;
                      LSQ_found_entry := true;
                    end;
                    if (LSQ_offset < LSQ_difference) then
                      LSQ_offset := (LSQ_offset + 1);
                    end;
                  end;
                  if (LSQ_found_entry = false) then
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          end;
        end;
      else
        RENAME_Load_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ld)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_Load_is_in_state_set := true;
            else
              RENAME_Load_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_Load_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ld)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_Load_is_in_state_set := true;
          else
            IQ_Load_is_in_state_set := false;
          end;
        end;
        LSQ_Load_is_in_state_set := false;
        LSQ_while_break := false;
        LSQ_found_entry := false;
        if (next_state.core_[ j ].LSQ_.num_entries = 0) then
          LSQ_while_break := true;
        end;
        LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
        LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
        LSQ_offset := 0;
        while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
          LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ld)) then
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request))))))) then
              LSQ_Load_is_in_state_set := true;
            else
              LSQ_Load_is_in_state_set := false;
            end;
            LSQ_found_entry := true;
          end;
          if (LSQ_offset < LSQ_difference) then
            LSQ_offset := (LSQ_offset + 1);
          end;
        end;
        if (LSQ_found_entry = false) then
        end;
        ROB_Load_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ld)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))))) then
              ROB_Load_is_in_state_set := true;
            else
              ROB_Load_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_Load_is_in_state_set | (IQ_Load_is_in_state_set | (LSQ_Load_is_in_state_set | ROB_Load_is_in_state_set)));
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
          else
            RENAME_Store_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_Store_is_in_state_set := true;
                else
                  RENAME_Store_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_Store_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_Store_is_in_state_set := true;
              else
                IQ_Store_is_in_state_set := false;
              end;
            end;
            ROB_Store_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_Store_is_in_state_set := true;
                else
                  ROB_Store_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            RENAME_STLR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_STLR_is_in_state_set := true;
                else
                  RENAME_STLR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_STLR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_STLR_is_in_state_set := true;
              else
                IQ_STLR_is_in_state_set := false;
              end;
            end;
            ROB_STLR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_STLR_is_in_state_set := true;
                else
                  ROB_STLR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
            if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  LSQ_while_break := false;
                  LSQ_found_entry := false;
                  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                    LSQ_while_break := true;
                  end;
                  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                  LSQ_offset := 0;
                  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                        if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                          next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                          next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                          next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                        else
                          error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                        end;
                        next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                        next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                      else
                        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                          next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                          next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                          next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                          next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                          next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                          next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                          for core_idx : cores_t do
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                          endfor;
                          next_state.core_[ j ].mem_interface_.out_busy := true;
                          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                        end;
                      end;
                      LSQ_found_entry := true;
                    end;
                    if (LSQ_offset < LSQ_difference) then
                      LSQ_offset := (LSQ_offset + 1);
                    end;
                  end;
                  if (LSQ_found_entry = false) then
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          end;
        else
          RENAME_Store_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_Store_is_in_state_set := true;
              else
                RENAME_Store_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_Store_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_Store_is_in_state_set := true;
            else
              IQ_Store_is_in_state_set := false;
            end;
          end;
          ROB_Store_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_Store_is_in_state_set := true;
              else
                ROB_Store_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          RENAME_STLR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_STLR_is_in_state_set := true;
              else
                RENAME_STLR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_STLR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_STLR_is_in_state_set := true;
            else
              IQ_STLR_is_in_state_set := false;
            end;
          end;
          ROB_STLR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_STLR_is_in_state_set := true;
              else
                ROB_STLR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              LSQ_while_break := false;
              LSQ_found_entry := false;
              if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                LSQ_while_break := true;
              end;
              LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
              LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
              LSQ_offset := 0;
              while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                      next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                      next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                    else
                      error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                    end;
                    next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                    next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                      next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                      next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                      next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                      next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                      for core_idx : cores_t do
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                      endfor;
                      next_state.core_[ j ].mem_interface_.out_busy := true;
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                    end;
                  end;
                  LSQ_found_entry := true;
                end;
                if (LSQ_offset < LSQ_difference) then
                  LSQ_offset := (LSQ_offset + 1);
                end;
              end;
              if (LSQ_found_entry = false) then
              end;
            end;
          end;
        end;
      end;
    end;
  else
    RENAME_dmb_st_is_in_state_set := false;
    RENAME_while_break := false;
    RENAME_found_entry := false;
    if (next_state.core_[ j ].RENAME_.num_entries = 0) then
      RENAME_while_break := true;
    end;
    RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
    RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
    RENAME_offset := 0;
    while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
      RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
      if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = dmb_st)) then
        if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
          RENAME_dmb_st_is_in_state_set := true;
        else
          RENAME_dmb_st_is_in_state_set := false;
        end;
        RENAME_found_entry := true;
      end;
      if (RENAME_offset < RENAME_difference) then
        RENAME_offset := (RENAME_offset + 1);
      end;
    end;
    if (RENAME_found_entry = false) then
    end;
    ROB_dmb_st_is_in_state_set := false;
    ROB_while_break := false;
    ROB_found_entry := false;
    if (next_state.core_[ j ].ROB_.num_entries = 0) then
      ROB_while_break := true;
    end;
    ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
    ROB_difference := next_state.core_[ j ].ROB_.num_entries;
    ROB_offset := 0;
    while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
      ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_st)) then
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po))))) then
          ROB_dmb_st_is_in_state_set := true;
        else
          ROB_dmb_st_is_in_state_set := false;
        end;
        ROB_found_entry := true;
      end;
      if (ROB_offset < ROB_difference) then
        ROB_offset := (ROB_offset + 1);
      end;
    end;
    if (ROB_found_entry = false) then
    end;
    is_instruction_on_any_state := (RENAME_dmb_st_is_in_state_set | ROB_dmb_st_is_in_state_set);
    if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) then
      if is_instruction_on_any_state then
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
      else
        RENAME_Load_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ld)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_Load_is_in_state_set := true;
            else
              RENAME_Load_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_Load_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ld)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_Load_is_in_state_set := true;
          else
            IQ_Load_is_in_state_set := false;
          end;
        end;
        LSQ_Load_is_in_state_set := false;
        LSQ_while_break := false;
        LSQ_found_entry := false;
        if (next_state.core_[ j ].LSQ_.num_entries = 0) then
          LSQ_while_break := true;
        end;
        LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
        LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
        LSQ_offset := 0;
        while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
          LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ld)) then
            if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request))))))) then
              LSQ_Load_is_in_state_set := true;
            else
              LSQ_Load_is_in_state_set := false;
            end;
            LSQ_found_entry := true;
          end;
          if (LSQ_offset < LSQ_difference) then
            LSQ_offset := (LSQ_offset + 1);
          end;
        end;
        if (LSQ_found_entry = false) then
        end;
        ROB_Load_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ld)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))))) then
              ROB_Load_is_in_state_set := true;
            else
              ROB_Load_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_Load_is_in_state_set | (IQ_Load_is_in_state_set | (LSQ_Load_is_in_state_set | ROB_Load_is_in_state_set)));
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
          else
            RENAME_Store_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_Store_is_in_state_set := true;
                else
                  RENAME_Store_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_Store_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_Store_is_in_state_set := true;
              else
                IQ_Store_is_in_state_set := false;
              end;
            end;
            ROB_Store_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_Store_is_in_state_set := true;
                else
                  ROB_Store_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            RENAME_STLR_is_in_state_set := false;
            RENAME_while_break := false;
            RENAME_found_entry := false;
            if (next_state.core_[ j ].RENAME_.num_entries = 0) then
              RENAME_while_break := true;
            end;
            RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
            RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
            RENAME_offset := 0;
            while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
              RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
                if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                  RENAME_STLR_is_in_state_set := true;
                else
                  RENAME_STLR_is_in_state_set := false;
                end;
                RENAME_found_entry := true;
              end;
              if (RENAME_offset < RENAME_difference) then
                RENAME_offset := (RENAME_offset + 1);
              end;
            end;
            if (RENAME_found_entry = false) then
            end;
            IQ_STLR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    end;
                  end;
                  found_entry := true;
                end;
              end;
            endfor;
            if (found_entry = false) then
            elsif (found_entry = true) then
              if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
                IQ_STLR_is_in_state_set := true;
              else
                IQ_STLR_is_in_state_set := false;
              end;
            end;
            ROB_STLR_is_in_state_set := false;
            ROB_while_break := false;
            ROB_found_entry := false;
            if (next_state.core_[ j ].ROB_.num_entries = 0) then
              ROB_while_break := true;
            end;
            ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
            ROB_difference := next_state.core_[ j ].ROB_.num_entries;
            ROB_offset := 0;
            while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
              ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                  ROB_STLR_is_in_state_set := true;
                else
                  ROB_STLR_is_in_state_set := false;
                end;
                ROB_found_entry := true;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            if (ROB_found_entry = false) then
            end;
            is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
            if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  LSQ_while_break := false;
                  LSQ_found_entry := false;
                  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                    LSQ_while_break := true;
                  end;
                  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                  LSQ_offset := 0;
                  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                        if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                          next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                          next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                          next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                          next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                        else
                          error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                        end;
                        next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                        next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                      else
                        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                          next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                          next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                          next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                          next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                          next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                          next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                          next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                          next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                          for core_idx : cores_t do
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                            next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                          endfor;
                          next_state.core_[ j ].mem_interface_.out_busy := true;
                          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                        end;
                      end;
                      LSQ_found_entry := true;
                    end;
                    if (LSQ_offset < LSQ_difference) then
                      LSQ_offset := (LSQ_offset + 1);
                    end;
                  end;
                  if (LSQ_found_entry = false) then
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          end;
        else
          RENAME_Store_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_Store_is_in_state_set := true;
              else
                RENAME_Store_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_Store_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_Store_is_in_state_set := true;
            else
              IQ_Store_is_in_state_set := false;
            end;
          end;
          ROB_Store_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_Store_is_in_state_set := true;
              else
                ROB_Store_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          RENAME_STLR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_STLR_is_in_state_set := true;
              else
                RENAME_STLR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_STLR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_STLR_is_in_state_set := true;
            else
              IQ_STLR_is_in_state_set := false;
            end;
          end;
          ROB_STLR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_STLR_is_in_state_set := true;
              else
                ROB_STLR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              LSQ_while_break := false;
              LSQ_found_entry := false;
              if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                LSQ_while_break := true;
              end;
              LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
              LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
              LSQ_offset := 0;
              while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                      next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                      next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                    else
                      error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                    end;
                    next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                    next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                      next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                      next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                      next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                      next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                      for core_idx : cores_t do
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                      endfor;
                      next_state.core_[ j ].mem_interface_.out_busy := true;
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                    end;
                  end;
                  LSQ_found_entry := true;
                end;
                if (LSQ_offset < LSQ_difference) then
                  LSQ_offset := (LSQ_offset + 1);
                end;
              end;
              if (LSQ_found_entry = false) then
              end;
            end;
          end;
        end;
      end;
    else
      RENAME_Load_is_in_state_set := false;
      RENAME_while_break := false;
      RENAME_found_entry := false;
      if (next_state.core_[ j ].RENAME_.num_entries = 0) then
        RENAME_while_break := true;
      end;
      RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
      RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
      RENAME_offset := 0;
      while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
        RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = ld)) then
          if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
            RENAME_Load_is_in_state_set := true;
          else
            RENAME_Load_is_in_state_set := false;
          end;
          RENAME_found_entry := true;
        end;
        if (RENAME_offset < RENAME_difference) then
          RENAME_offset := (RENAME_offset + 1);
        end;
      end;
      if (RENAME_found_entry = false) then
      end;
      IQ_Load_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ld)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              end;
            end;
            found_entry := true;
          end;
        end;
      endfor;
      if (found_entry = false) then
      elsif (found_entry = true) then
        if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
          IQ_Load_is_in_state_set := true;
        else
          IQ_Load_is_in_state_set := false;
        end;
      end;
      LSQ_Load_is_in_state_set := false;
      LSQ_while_break := false;
      LSQ_found_entry := false;
      if (next_state.core_[ j ].LSQ_.num_entries = 0) then
        LSQ_while_break := true;
      end;
      LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
      LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
      LSQ_offset := 0;
      while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
        LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.op = ld)) then
          if ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_await_translation) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_st_fwd_branch) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_load_mem_response) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_st_await_committed) | ((next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled) | (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_build_packet_send_mem_request))))))) then
            LSQ_Load_is_in_state_set := true;
          else
            LSQ_Load_is_in_state_set := false;
          end;
          LSQ_found_entry := true;
        end;
        if (LSQ_offset < LSQ_difference) then
          LSQ_offset := (LSQ_offset + 1);
        end;
      end;
      if (LSQ_found_entry = false) then
      end;
      ROB_Load_is_in_state_set := false;
      ROB_while_break := false;
      ROB_found_entry := false;
      if (next_state.core_[ j ].ROB_.num_entries = 0) then
        ROB_while_break := true;
      end;
      ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
      ROB_difference := next_state.core_[ j ].ROB_.num_entries;
      ROB_offset := 0;
      while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
        ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ld)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_clear_lsq_store_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po)))))) then
            ROB_Load_is_in_state_set := true;
          else
            ROB_Load_is_in_state_set := false;
          end;
          ROB_found_entry := true;
        end;
        if (ROB_offset < ROB_difference) then
          ROB_offset := (ROB_offset + 1);
        end;
      end;
      if (ROB_found_entry = false) then
      end;
      is_instruction_on_any_state := (RENAME_Load_is_in_state_set | (IQ_Load_is_in_state_set | (LSQ_Load_is_in_state_set | ROB_Load_is_in_state_set)));
      if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
        else
          RENAME_Store_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_Store_is_in_state_set := true;
              else
                RENAME_Store_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_Store_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_Store_is_in_state_set := true;
            else
              IQ_Store_is_in_state_set := false;
            end;
          end;
          ROB_Store_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_Store_is_in_state_set := true;
              else
                ROB_Store_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          RENAME_STLR_is_in_state_set := false;
          RENAME_while_break := false;
          RENAME_found_entry := false;
          if (next_state.core_[ j ].RENAME_.num_entries = 0) then
            RENAME_while_break := true;
          end;
          RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
          RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
          RENAME_offset := 0;
          while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
            RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
              if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
                RENAME_STLR_is_in_state_set := true;
              else
                RENAME_STLR_is_in_state_set := false;
              end;
              RENAME_found_entry := true;
            end;
            if (RENAME_offset < RENAME_difference) then
              RENAME_offset := (RENAME_offset + 1);
            end;
          end;
          if (RENAME_found_entry = false) then
          end;
          IQ_STLR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  end;
                end;
                found_entry := true;
              end;
            end;
          endfor;
          if (found_entry = false) then
          elsif (found_entry = true) then
            if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
              IQ_STLR_is_in_state_set := true;
            else
              IQ_STLR_is_in_state_set := false;
            end;
          end;
          ROB_STLR_is_in_state_set := false;
          ROB_while_break := false;
          ROB_found_entry := false;
          if (next_state.core_[ j ].ROB_.num_entries = 0) then
            ROB_while_break := true;
          end;
          ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
          ROB_difference := next_state.core_[ j ].ROB_.num_entries;
          ROB_offset := 0;
          while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
            ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
                ROB_STLR_is_in_state_set := true;
              else
                ROB_STLR_is_in_state_set := false;
              end;
              ROB_found_entry := true;
            end;
            if (ROB_offset < ROB_difference) then
              ROB_offset := (ROB_offset + 1);
            end;
          end;
          if (ROB_found_entry = false) then
          end;
          is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                LSQ_while_break := false;
                LSQ_found_entry := false;
                if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                  LSQ_while_break := true;
                end;
                LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
                LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
                LSQ_offset := 0;
                while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                  LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                        next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                        next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                        next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                      else
                        error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                      end;
                      next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                      next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                    else
                      if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                        next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                        next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                        next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                        next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                        for core_idx : cores_t do
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                          next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                        endfor;
                        next_state.core_[ j ].mem_interface_.out_busy := true;
                        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                      end;
                    end;
                    LSQ_found_entry := true;
                  end;
                  if (LSQ_offset < LSQ_difference) then
                    LSQ_offset := (LSQ_offset + 1);
                  end;
                end;
                if (LSQ_found_entry = false) then
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              LSQ_while_break := false;
              LSQ_found_entry := false;
              if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                LSQ_while_break := true;
              end;
              LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
              LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
              LSQ_offset := 0;
              while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                      next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                      next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                    else
                      error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                    end;
                    next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                    next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                      next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                      next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                      next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                      next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                      for core_idx : cores_t do
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                      endfor;
                      next_state.core_[ j ].mem_interface_.out_busy := true;
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                    end;
                  end;
                  LSQ_found_entry := true;
                end;
                if (LSQ_offset < LSQ_difference) then
                  LSQ_offset := (LSQ_offset + 1);
                end;
              end;
              if (LSQ_found_entry = false) then
              end;
            end;
          end;
        end;
      else
        RENAME_Store_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = st)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_Store_is_in_state_set := true;
            else
              RENAME_Store_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_Store_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_Store_is_in_state_set := true;
          else
            IQ_Store_is_in_state_set := false;
          end;
        end;
        ROB_Store_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
              ROB_Store_is_in_state_set := true;
            else
              ROB_Store_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        RENAME_STLR_is_in_state_set := false;
        RENAME_while_break := false;
        RENAME_found_entry := false;
        if (next_state.core_[ j ].RENAME_.num_entries = 0) then
          RENAME_while_break := true;
        end;
        RENAME_entry_idx := ((next_state.core_[ j ].RENAME_.tail + (RENAME_NUM_ENTRIES_CONST - 1)) % RENAME_NUM_ENTRIES_CONST);
        RENAME_difference := next_state.core_[ j ].RENAME_.num_entries;
        RENAME_offset := 0;
        while ((RENAME_offset < RENAME_difference) & ((RENAME_while_break = false) & (RENAME_found_entry = false))) do
          RENAME_curr_idx := ((RENAME_entry_idx + (RENAME_NUM_ENTRIES_CONST - RENAME_offset)) % RENAME_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].instruction.op = stlr)) then
            if (next_state.core_[ j ].RENAME_.entries[ RENAME_curr_idx ].state = issue_if_head) then
              RENAME_STLR_is_in_state_set := true;
            else
              RENAME_STLR_is_in_state_set := false;
            end;
            RENAME_found_entry := true;
          end;
          if (RENAME_offset < RENAME_difference) then
            RENAME_offset := (RENAME_offset + 1);
          end;
        end;
        if (RENAME_found_entry = false) then
        end;
        IQ_STLR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                end;
              end;
              found_entry := true;
            end;
          end;
        endfor;
        if (found_entry = false) then
        elsif (found_entry = true) then
          if (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) then
            IQ_STLR_is_in_state_set := true;
          else
            IQ_STLR_is_in_state_set := false;
          end;
        end;
        ROB_STLR_is_in_state_set := false;
        ROB_while_break := false;
        ROB_found_entry := false;
        if (next_state.core_[ j ].ROB_.num_entries = 0) then
          ROB_while_break := true;
        end;
        ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
        ROB_difference := next_state.core_[ j ].ROB_.num_entries;
        ROB_offset := 0;
        while ((ROB_offset < ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
          ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_po) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_time_await_st_mem_resp) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_executed)))) then
              ROB_STLR_is_in_state_set := true;
            else
              ROB_STLR_is_in_state_set := false;
            end;
            ROB_found_entry := true;
          end;
          if (ROB_offset < ROB_difference) then
            ROB_offset := (ROB_offset + 1);
          end;
        end;
        if (ROB_found_entry = false) then
        end;
        is_instruction_on_any_state := (RENAME_Store_is_in_state_set | (IQ_Store_is_in_state_set | (ROB_Store_is_in_state_set | (RENAME_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | ROB_STLR_is_in_state_set)))));
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              LSQ_while_break := false;
              LSQ_found_entry := false;
              if (next_state.core_[ j ].LSQ_.num_entries = 0) then
                LSQ_while_break := true;
              end;
              LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
              LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
              LSQ_offset := 0;
              while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
                LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
                if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                      next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                      next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                      next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                      next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                    else
                      error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                    end;
                    next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                    next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                  else
                    if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                      next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                      next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                      next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                      next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                      next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                      for core_idx : cores_t do
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                        next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                      endfor;
                      next_state.core_[ j ].mem_interface_.out_busy := true;
                      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                    end;
                  end;
                  LSQ_found_entry := true;
                end;
                if (LSQ_offset < LSQ_difference) then
                  LSQ_offset := (LSQ_offset + 1);
                end;
              end;
              if (LSQ_found_entry = false) then
              end;
            end;
          end;
        else
          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld)))) then
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
          else
            LSQ_while_break := false;
            LSQ_found_entry := false;
            if (next_state.core_[ j ].LSQ_.num_entries = 0) then
              LSQ_while_break := true;
            end;
            LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
            LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
            LSQ_offset := 0;
            while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
              LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
              if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_ld_await_committed) then
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].virt_addr := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].phys_addr := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].read_value := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].write_value := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].seq_num := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.seq_num := 0;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].instruction.op := inval;
                    next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.head ].state := lsq_await_creation;
                    next_state.core_[ j ].LSQ_.head := ((next_state.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries - 1);
                    next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_await_creation;
                  else
                    error "Controller is not on an expected state for a msg: (load_committed) from: (ROB) to: (LSQ)";
                  end;
                  next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].write_value := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].phys_addr := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) then
                    next_state.core_[ j ].ROB_.entries[ i ].phys_addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                    next_state.core_[ j ].ROB_.entries[ i ].write_value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                    next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].phys_addr;
                    next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                    next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value;
                    next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                    next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                    next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                    next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num;
                    next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                    for core_idx : cores_t do
                      next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                      next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                    endfor;
                    next_state.core_[ j ].mem_interface_.out_busy := true;
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_time_await_st_mem_resp;
                  end;
                end;
                LSQ_found_entry := true;
              end;
              if (LSQ_offset < LSQ_difference) then
                LSQ_offset := (LSQ_offset + 1);
              end;
            end;
            if (LSQ_found_entry = false) then
            end;
          end;
        end;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_commit_if_head ===> rob_commit_po || rob_commit_if_head || rob_await_creation" 
(Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_if_head)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].ROB_.head = i) then
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_po;
  else
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB init_rob_entry ===> rob_await_creation" 
(Sta.core_[ j ].ROB_.entries[ i ].state = init_rob_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].ROB_.entries[ i ].seq_num := 0;
  next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
  next_state.core_[ j ].ROB_.entries[ i ].write_value := 0;
  next_state.core_[ j ].ROB_.entries[ i ].phys_addr := 0;
  next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : IQ_idx_t do 
  rule "IQ iq_schedule_inst ===> iq_await_creation" 
(Sta.core_[ j ].IQ_.entries[ i ].state = iq_schedule_inst)
==>
 
  var LSQ_while_break : boolean;
  var LSQ_found_entry : boolean;
  var LSQ_entry_idx : LSQ_idx_t;
  var LSQ_difference : LSQ_count_t;
  var LSQ_offset : LSQ_count_t;
  var LSQ_curr_idx : LSQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  LSQ_while_break := false;
  LSQ_found_entry := false;
  if (next_state.core_[ j ].LSQ_.num_entries = 0) then
    LSQ_while_break := true;
  end;
  LSQ_entry_idx := ((next_state.core_[ j ].LSQ_.tail + (LSQ_NUM_ENTRIES_CONST - 1)) % LSQ_NUM_ENTRIES_CONST);
  LSQ_difference := next_state.core_[ j ].LSQ_.num_entries;
  LSQ_offset := 0;
  while ((LSQ_offset < LSQ_difference) & ((LSQ_while_break = false) & (LSQ_found_entry = false))) do
    LSQ_curr_idx := ((LSQ_entry_idx + (LSQ_NUM_ENTRIES_CONST - LSQ_offset)) % LSQ_NUM_ENTRIES_CONST);
    if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
      if (next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state = lsq_await_scheduled) then
        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].virt_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
        if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
          next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].write_value := next_state.core_[ j ].IQ_.entries[ i ].instruction.write_value;
        end;
        next_state.core_[ j ].LSQ_.entries[ LSQ_curr_idx ].state := lsq_ld_st_await_translation;
      else
        error "Controller is not on an expected state for a msg: (lsq_schedule_imm) from: (IQ) to: (LSQ)";
      end;
      next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
      next_state.core_[ j ].IQ_.entries[ i ].valid := false;
      next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
      LSQ_found_entry := true;
    end;
    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  if (LSQ_found_entry = false) then
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : IQ_idx_t do 
  rule "IQ init_iq_entry ===> iq_await_creation" 
(Sta.core_[ j ].IQ_.entries[ i ].state = init_iq_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].IQ_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].IQ_.entries[ i ].seq_num := 0;
  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : RENAME_idx_t do 
  rule "RENAME issue_if_head ===> rename_await_creation || issue_if_head" 
((Sta.core_[ j ].RENAME_.entries[ i ].state = issue_if_head) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST))
==>
 
  var IQ_loop_break : boolean;
  var IQ_entry_idx : IQ_idx_t;
  var IQ_found_entry : boolean;
  var IQ_offset : IQ_count_t;
  var IQ_curr_idx : IQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if ((next_state.core_[ j ].RENAME_.head = i) & (!((next_state.core_[ j ].RENAME_.num_entries = 0)) & !((next_state.core_[ j ].ROB_.num_entries = ROB_NUM_ENTRIES_CONST)))) then
    if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) then
      next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
      if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
        next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
        next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
      else
        error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
      end;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
      if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld)))) then
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
      else
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_executed;
      end;
      next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
      next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.op := inval;
      next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
      next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].state := rename_await_creation;
      next_state.core_[ j ].RENAME_.head := ((next_state.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
      next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
      next_state.core_[ j ].RENAME_.entries[ i ].state := rename_await_creation;
    else
      if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) then
        next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
        if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
          next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
          next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
        else
          error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
        end;
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
        if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld)))) then
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
        else
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_executed;
        end;
        next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
        next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.op := inval;
        next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
        next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].state := rename_await_creation;
        next_state.core_[ j ].RENAME_.head := ((next_state.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
        next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
        next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
      else
        if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld) then
          next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
          if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
            next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
            next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
          else
            error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
          end;
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
          if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld)))) then
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
          else
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_executed;
          end;
          next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
          next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
          next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.op := inval;
          next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
          next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].state := rename_await_creation;
          next_state.core_[ j ].RENAME_.head := ((next_state.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
          next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
          next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
        else
          if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) then
            next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
            if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
              next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
              next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
            else
              error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
            end;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
            if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld)))) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
            else
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_executed;
            end;
            next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
            next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.op := inval;
            next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].state := rename_await_creation;
            next_state.core_[ j ].RENAME_.head := ((next_state.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
            next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
            next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
          else
            if (!((next_state.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST)) & !((next_state.core_[ j ].LSQ_.num_entries = LSQ_NUM_ENTRIES_CONST))) then
              next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
              if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
                next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
                next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
              else
                error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
              end;
              IQ_loop_break := false;
              if (next_state.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST) then
                IQ_loop_break := true;
              end;
              IQ_entry_idx := 0;
              IQ_found_entry := false;
              IQ_offset := 0;
              while ((IQ_offset < IQ_NUM_ENTRIES_CONST) & ((IQ_loop_break = false) & (IQ_found_entry = false))) do
                IQ_curr_idx := ((IQ_entry_idx + IQ_offset) % IQ_NUM_ENTRIES_CONST);
                if (next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].state = iq_await_creation) then
                  next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
                  next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].state := iq_schedule_inst;
                  next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].valid := true;
                  IQ_found_entry := true;
                end;
                if (IQ_offset < IQ_NUM_ENTRIES_CONST) then
                  IQ_offset := (IQ_offset + 1);
                end;
              end;
              next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries + 1);
              if (IQ_found_entry = false) then
                error "Couldn't find an empty entry to insert into";
              end;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
              if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld)))) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
              else
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_executed;
              end;
              next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
              next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
              next_state.core_[ j ].LSQ_.entries[ next_state.core_[ j ].LSQ_.tail ].state := lsq_await_scheduled;
              next_state.core_[ j ].LSQ_.tail := ((next_state.core_[ j ].LSQ_.tail + 1) % LSQ_NUM_ENTRIES_CONST);
              next_state.core_[ j ].LSQ_.num_entries := (next_state.core_[ j ].LSQ_.num_entries + 1);
              next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.op := inval;
              next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].RENAME_.entries[ next_state.core_[ j ].RENAME_.head ].state := rename_await_creation;
              next_state.core_[ j ].RENAME_.head := ((next_state.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
              next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
              next_state.core_[ j ].RENAME_.entries[ i ].state := rename_await_creation;
            end;
          end;
        end;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : RENAME_idx_t do 
  rule "RENAME init_rename_entry ===> rename_await_creation" 
(Sta.core_[ j ].RENAME_.entries[ i ].state = init_rename_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].RENAME_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].RENAME_.entries[ i ].state := rename_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "SeqNumReg init_seq_num_counter ===> seq_num_interface" 
(Sta.core_[ j ].SeqNumReg_.state = init_seq_num_counter)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].SeqNumReg_.seq_num_counter := 0;
  next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
  Sta := next_state;

end;
end;

invariant "Dekker"
((((Sta.core_[ 0 ].RENAME_.num_entries = 0) & (Sta.core_[ 0 ].ROB_.num_entries = 0)) & ((Sta.core_[ 1 ].RENAME_.num_entries = 0) & (Sta.core_[ 1 ].ROB_.num_entries = 0))) -> !((((Sta.core_[ 0 ].rf_.rf[ 0 ] = 0) & (Sta.core_[ 0 ].rf_.rf[ 1 ] = 0)) & ((Sta.core_[ 1 ].rf_.rf[ 0 ] = 0) & (Sta.core_[ 1 ].rf_.rf[ 1 ] = 0)))));

