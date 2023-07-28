const
CORE_INST_NUM : 1;
IC_ENTRY_NUM : 1;
CORE_NUM : 1;
ADDR_NUM : 1;
REG_NUM : 1;
MAX_VALUE : 1;
ROB_NUM_ENTRIES_ENUM_CONST : 1;
ROB_NUM_ENTRIES_CONST : 2;
IQ_NUM_ENTRIES_ENUM_CONST : 1;
IQ_NUM_ENTRIES_CONST : 2;
RENAME_NUM_ENTRIES_ENUM_CONST : 1;
RENAME_NUM_ENTRIES_CONST : 2;
skid_buffer_NUM_ENTRIES_ENUM_CONST : 1;
skid_buffer_NUM_ENTRIES_CONST : 2;
load_address_table_NUM_ENTRIES_ENUM_CONST : 1;
load_address_table_NUM_ENTRIES_CONST : 2;

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
invalidation_listener_state : enum {init_inval_listener, squash_speculative_loads, await_invalidation};
invalidation_listener : record
  invalidation_seq_num : inst_count_t;
  invalidation_address : addr_idx_t;
  state : invalidation_listener_state;
end;
ROB_state : enum {rob_await_is_executed, rob_complete_store, rob_wait_store_completed, rob_commit_based_on_inst, rob_commit_if_head, rob_await_creation, init_rob_entry};
ROB_idx_t : 0 .. ROB_NUM_ENTRIES_ENUM_CONST;
ROB_count_t : 0 .. ROB_NUM_ENTRIES_CONST;
ROB_entry_values : record
  instruction : INST;
  seq_num : inst_count_t;
  is_executed : boolean;
  write_value : val_t;
  phys_addr : val_t;
  replay_value : val_t;
  state : ROB_state;
end;
ROB : record
  entries : array [ROB_idx_t] of ROB_entry_values;
  num_entries : ROB_count_t;
  head : ROB_idx_t;
  tail : ROB_idx_t;
end;
second_memory_stage_state : enum {second_mem_unit_receive, second_mem_unit_send, second_mem_unit_get_inputs, secnd_mem_init};
second_memory_stage : record
  instruction : INST;
  write_value : val_t;
  phys_addr : addr_idx_t;
  state : second_memory_stage_state;
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
memory_unit_sender_state : enum {memory_unit_load_result_write, memory_unit_receiver, memory_unit_stage_send, mem_unit_send_get_input, memory_unit_send_init};
memory_unit_sender : record
  instruction : INST;
  phys_addr : addr_idx_t;
  read_value : val_t;
  state : memory_unit_sender_state;
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
skid_buffer_state : enum {skid_issue_if_head, skid_buffer_await_creation, init_skid_buffer};
skid_buffer_idx_t : 0 .. skid_buffer_NUM_ENTRIES_ENUM_CONST;
skid_buffer_count_t : 0 .. skid_buffer_NUM_ENTRIES_CONST;
skid_buffer_entry_values : record
  instruction : INST;
  state : skid_buffer_state;
end;
skid_buffer : record
  entries : array [skid_buffer_idx_t] of skid_buffer_entry_values;
  num_entries : skid_buffer_count_t;
  head : skid_buffer_idx_t;
  tail : skid_buffer_idx_t;
end;
load_address_table_state : enum {init_table_load_address_table, load_address_table_await_insert_remove};
load_address_table_idx_t : 0 .. load_address_table_NUM_ENTRIES_ENUM_CONST;
load_address_table_count_t : 0 .. load_address_table_NUM_ENTRIES_CONST;
load_address_table_entry_values : record
  lat_seq_num : inst_count_t;
  lat_address : addr_idx_t;
  state : load_address_table_state;
  valid : boolean;
end;
load_address_table : record
  entries : array [load_address_table_idx_t] of load_address_table_entry_values;
  num_entries : load_address_table_count_t;
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
  invalidation_listener_ : invalidation_listener;
  ROB_ : ROB;
  second_memory_stage_ : second_memory_stage;
  IQ_ : IQ;
  memory_unit_sender_ : memory_unit_sender;
  RENAME_ : RENAME;
  SeqNumReg_ : SeqNumReg;
  skid_buffer_ : skid_buffer;
  load_address_table_ : load_address_table;
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

-- function associative_ack_st (
--   sb : ROB;
--   msg : MEM_REQ
-- ) : ROB;
--   var sb_new : ROB;
--   var sb_iter : ROB_idx_t;
--   var sb_count : ROB_count_t;
--   var curr_entry : ROB_entry_values;
--   var curr_entry_id : ROB_idx_t;
--   var seq_num : inst_count_t;
-- begin
--   sb_new := sb;
--   sb_iter := 0;
--   sb_count := sb.num_entries;
--   seq_num := msg.seq_num;
--   for i : 0 .. ROB_NUM_ENTRIES_ENUM_CONST do
--     curr_entry_id := ((sb_iter + i) % ROB_NUM_ENTRIES_CONST);
--     curr_entry := sb_new.entries[ curr_entry_id ];
--     if (curr_entry.instruction.seq_num = seq_num) then
--       -- assert (curr_entry.state = rob_commit_time_await_st_mem_resp) "ACK ROB: Should be in await mem resp?";
--       curr_entry.state := rob_clear_lsq_store_head;
--       sb_new.entries[ curr_entry_id ] := curr_entry;
--       return sb_new;
--     end;
--   endfor;
--   error "didn't find the Load to write the read val into?";
--   return sb_new;
-- end;

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
    alias skid : init_state.core_[ core ].skid_buffer_ do
      for i : 0 .. skid_buffer_NUM_ENTRIES_ENUM_CONST do
        skid.entries[ i ].instruction.op := inval;
        skid.entries[ i ].instruction.seq_num := 0;
        skid.entries[ i ].state := skid_buffer_await_creation;
      endfor;
      skid.head := 0;
      skid.tail := 0;
      skid.num_entries := 0;
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
        rob.entries[ i ].phys_addr := 0;
        rob.entries[ i ].write_value := 0;
        rob.entries[ i ].state := rob_await_creation;
      endfor;
      rob.head := 0;
      rob.tail := 0;
      rob.num_entries := 0;
    end;
    alias listen_handler : init_state.core_[core].invalidation_listener_ do
      listen_handler.invalidation_seq_num := 0;
      listen_handler.invalidation_address := 0;
      listen_handler.state := await_invalidation;
    end;
    alias lat : init_state.core_[core].load_address_table_ do
      for i : load_address_table_idx_t do
        lat.entries[i].lat_seq_num := 0;
        lat.entries[i].lat_address := 0;
        lat.entries[i].state := load_address_table_await_insert_remove;
        lat.entries[i].valid := false;
      endfor;
      lat.num_entries := 0;
    end;
    alias mem_stage_one : init_state.core_[ core ].memory_unit_sender_ do
      for i : 0 .. CORE_INST_NUM do
        mem_stage_one.instruction.op := inval;
        mem_stage_one.instruction.seq_num := 0;
        mem_stage_one.phys_addr := 0;
        mem_stage_one.state := mem_unit_send_get_input;
      endfor;
    end;
    alias mem_stage_two : init_state.core_[ core ].second_memory_stage_ do
      for i : 0 .. CORE_INST_NUM do
        mem_stage_two.instruction.op := inval;
        mem_stage_two.instruction.seq_num := 0;
        mem_stage_two.phys_addr := 0;
        mem_stage_two.state := second_mem_unit_get_inputs;
      endfor;
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
  -- manually edited, don't touch..
  -- var lq : LSQ;
  var rob : ROB;
  var rob_id : ROB_idx_t;
  var mem_interface : MEM_INTERFACE;

begin
  next_state := Sta;
  put next_state.core_[ j ].memory_unit_sender_.state;
  -- lq := Sta.core_[ j ].LSQ_;
  rob := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  -- Do this based on the stage state
  put "CORE-ACK: About to check if 1st mem unit or 2nd mem unit is the recipient..\n";
  if (mem_interface.in_msg.r_w = read)
    then
    put "CORE-ACK: 1st Mem unit is the recipient, check if load or store..\n";
    -- if (mem_interface.in_msg.r_w = read) then
    if
    (Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver
    -- | Sta.core_[j].memory_unit_sender_.state = replay_generated_memory_unit_receiver
    ) &
    (Sta.core_[j].memory_unit_sender_.instruction.seq_num = mem_interface.in_msg.seq_num)
      then
      rob_id := search_ROB_seq_num_idx(rob, Sta.core_[j].memory_unit_sender_.instruction.seq_num);
      -- put "CORE-ACK: Rob id: (";
      -- put rob_id;
      -- put ")\n";
      -- put "CORE-ACK: mem_seq_num in: (";
      -- put mem_interface.in_msg.seq_num;
      -- put ")\n";
      -- put "CORE-ACK: mem_unit_sender seq_num: (";
      -- put Sta.core_[j].memory_unit_sender_.instruction.seq_num;
      -- put ")\n";
      if Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver then
        assert(rob.entries[rob_id].state = rob_await_is_executed);
      -- else
      --   assert rob.entries[rob_id].state = ROB_await_replay_complete;
      endif;
      -- assert();
      -- rob.entries[rob_id].state := rob_commit_if_head;
      -- rob.entries[rob_id].is_executed := true;
      if (Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver) then
        next_state.core_[j].memory_unit_sender_.read_value := mem_interface.in_msg.value;
        next_state.core_[j].memory_unit_sender_.state := memory_unit_load_result_write;
      -- elsif (Sta.core_[j].memory_unit_sender_.state = replay_generated_memory_unit_receiver) then
      --   next_state.core_[j].memory_unit_sender_.replay_value := mem_interface.in_msg.value;
      --   next_state.core_[j].memory_unit_sender_.state := replay_compare_and_check_state;
      endif;
      -- elsif (Sta.core_[j].memory_unit_sender_.state = squashed_memory_unit_receiver) then
      --   -- consume the stale message
      --   -- go to reset state
      --   next_state.core_[j].memory_unit_sender_.state := memory_unit_stage_send;
      mem_interface.in_busy := false;
    elsif (Sta.core_[j].memory_unit_sender_.instruction.seq_num != mem_interface.in_msg.seq_num) then
      if (mem_interface.in_msg.seq_num <= next_state.core_[j].SeqNumReg_.seq_num_counter) then
        mem_interface.in_busy := false;
        -- ignore a seq_num mismatch
      else
        error "This would have been a stale read? but it's seq num isn't less than or equal the one in the seq_num reg?";
      endif;
    end;
  elsif (mem_interface.in_msg.r_w = write) then
    if (mem_interface.in_msg.store_state = store_send_completion) &
      (Sta.core_[j].second_memory_stage_.state = second_mem_unit_receive) &
      (Sta.core_[j].second_memory_stage_.instruction.seq_num = mem_interface.in_msg.seq_num) then
        -- elsif (mem_interface.in_msg.store_state = store_send_completion) then
          rob_id := search_rob_seq_num_idx(rob, Sta.core_[j].second_memory_stage_.instruction.seq_num);
          assert(rob.entries[rob_id].state = rob_wait_store_completed);
          rob.entries[rob_id].state := rob_complete_store;
          -- sb := associative_ack_st(sb, mem_interface.in_msg);
          mem_interface.in_busy := false;
        -- end;
        put "CORE-ACK: Rob id: (";
        put rob_id;
        put ")\n";
        put "CORE-ACK: mem_seq_num in: (";
        put mem_interface.in_msg.seq_num;
        put ")\n";
        put "CORE-ACK: second_memory_stage seq_num: (";
        put Sta.core_[j].second_memory_stage_.instruction.seq_num;
        put ")\n";
      next_state.core_[j].second_memory_stage_.state := second_mem_unit_get_inputs;
      -- Directly handle the invalidation case here
    elsif (mem_interface.in_msg.store_state = await_handling) then
      -- Overview: Send back an ack for the invalidation sent..
      -- match this message with it's copy in the IC, set it's ack bool to true.

      next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
      next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
      next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;
      -- found_msg_in_ic := false;
      -- for ic_idx : ic_idx_t do
      --   if ((Sta.ic_.valid[ ic_idx ] = true) & ((Sta.ic_.buffer[ ic_idx ].seq_num = Sta.core_[ j ].mem_interface_.in_msg.seq_num) & (Sta.ic_.buffer[ ic_idx ].dest_id = Sta.core_[ j ].mem_interface_.in_msg.dest_id))) then
      --     next_state.ic_.buffer[ ic_idx ].store_inval_ackd[ j ] := true;
      --     if (found_msg_in_ic = true) then
      --       error "we found 2 matching ic entries? shouldn't happen...";
      --     elsif (found_msg_in_ic = false) then
      --       found_msg_in_ic := true;
      --     end;
      --   end;
      -- endfor;
      -- assert (found_msg_in_ic = true) "Should have found a msg in the IC? Otherwise we wouldn't be performing this invalidation's squash.";
      -- assert (Sta.core_[ j ].mem_interface_.in_busy = true) "The memory interface of this core should be busy, this is the msg we're processing.";
      -- next_state.core_[ j ].mem_interface_.in_busy := false;
      -- mem_interface.in_busy := false;
    endif;
  else
    error "CORE-ACK: Got a message but don't know what to do with it!\n";
  endif;
  put "CORE-ACK: Reached end of if stmt..\n";
  -- next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := rob;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end;

rule "reset" 
((Sta.core_[ 0 ].RENAME_.num_entries = 0) &
(Sta.core_[ 0 ].skid_buffer_.num_entries = 0) &
((Sta.core_[ 0 ].ROB_.num_entries = 0) &
((Sta.core_[ 0 ].IQ_.num_entries = 0) &
((Sta.core_[ 1 ].RENAME_.num_entries = 0) &
(Sta.core_[ 1 ].skid_buffer_.num_entries = 0) &
((Sta.core_[ 1 ].ROB_.num_entries = 0) &
((Sta.core_[ 1 ].IQ_.num_entries = 0)
))))))
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


ruleset j : cores_t do 
  rule "invalidation_listener init_inval_listener ===> await_invalidation" 
(Sta.core_[ j ].invalidation_listener_.state = init_inval_listener)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := 0;
  next_state.core_[ j ].invalidation_listener_.invalidation_address := 0;
  next_state.core_[ j ].invalidation_listener_.state := await_invalidation;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "invalidation_listener squash_speculative_loads ===> await_invalidation" 
(Sta.core_[ j ].invalidation_listener_.state = squash_speculative_loads)
==>
 
  var remove_key_dest_already_found : boolean;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_squash_curr_idx : ROB_idx_t;
  var ROB_squash_remove_count : ROB_count_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : load_address_table_idx_t;
  var next_state : STATE;
  var found_msg_in_ic : boolean;
  var violating_seq_num : inst_count_t;

begin
  next_state := Sta;
  found_entry := false;
  for load_address_table_iter : load_address_table_idx_t do
    if next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].valid then
      if ((next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].valid = true) & (next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].lat_address = next_state.core_[ j ].invalidation_listener_.invalidation_address)) then
        if (found_entry = false) then
          found_element := next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].lat_seq_num;
          found_idx := load_address_table_iter;
        else
          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].lat_seq_num < found_element) then
            found_element := next_state.core_[ j ].load_address_table_.entries[ load_address_table_iter ].lat_seq_num;
            found_idx := load_address_table_iter;
          end;
        end;
        found_entry := true;
      end;
    end;
  endfor;
  if (found_entry = false) then
  elsif (found_entry = true) then
    for load_address_table_squash_idx : load_address_table_idx_t do
      if next_state.core_[ j ].load_address_table_.entries[ load_address_table_squash_idx ].valid then
        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_squash_idx ].state = load_address_table_await_insert_remove) then
          violating_seq_num := next_state.core_[ j ].load_address_table_.entries[ found_idx ].lat_seq_num;
          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_squash_idx ].lat_seq_num >= violating_seq_num) then
            remove_key_dest_already_found := false;
            for remove_load_address_table_idx : load_address_table_idx_t do
              if next_state.core_[ j ].load_address_table_.entries[ remove_load_address_table_idx ].valid then
                if (next_state.core_[ j ].load_address_table_.entries[ remove_load_address_table_idx ].lat_seq_num = next_state.core_[ j ].load_address_table_.entries[ load_address_table_squash_idx ].lat_seq_num) then
                  if remove_key_dest_already_found then
                    error "Error: Found multiple entries with same key in remove_key API func";
                  elsif !(remove_key_dest_already_found) then
                    next_state.core_[ j ].load_address_table_.entries[ remove_load_address_table_idx ].valid := false;
                    next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries - 1);
                    next_state.core_[ j ].load_address_table_.entries[ remove_load_address_table_idx ].state := load_address_table_await_insert_remove;
                  else
                    error "Unreachable.. Just to make Murphi metaprogramming parser parse the stmts...";
                  end;
                end;
              end;
            endfor;
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
                if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_await_is_executed) then
                  violating_seq_num := violating_seq_num;
                  if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write) then
                      violating_seq_num := violating_seq_num;
                      if (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num >= violating_seq_num) then
                        next_state.core_[ j ].memory_unit_sender_.state := mem_unit_send_get_input;
                      end;
                    elsif (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) then
                      violating_seq_num := violating_seq_num;
                      if (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num >= violating_seq_num) then
                        next_state.core_[ j ].memory_unit_sender_.state := mem_unit_send_get_input;
                      end;
                     elsif (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) then
                      violating_seq_num := violating_seq_num;
                      if (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num >= violating_seq_num) then
                        next_state.core_[ j ].memory_unit_sender_.state := mem_unit_send_get_input;
                      end;
                     elsif (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      violating_seq_num := violating_seq_num;
                    else
                      error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (memory_unit_sender)";
                    end;
                    for IQ_squash_idx : IQ_idx_t do
                      if next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid then
                        if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_schedule_inst) then
                          violating_seq_num := violating_seq_num;
                          if (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].instruction.seq_num >= violating_seq_num) then
                            next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                            next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].valid := false;
                            next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state := iq_await_creation;
                          end;
                        elsif (next_state.core_[ j ].IQ_.entries[ IQ_squash_idx ].state = iq_await_creation) then
                          violating_seq_num := violating_seq_num;
                        else
                          error "Controller is not on an expected state for a msg: (squash) from: (ROB) to: (IQ)";
                        end;
                      end;
                    endfor;
                    ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].state := skid_issue_if_head;
                    next_state.core_[ j ].skid_buffer_.tail := ((next_state.core_[ j ].skid_buffer_.tail + 1) % skid_buffer_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].skid_buffer_.num_entries := (next_state.core_[ j ].skid_buffer_.num_entries + 1);
                    next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                  end;
                elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_complete_store) then
                  violating_seq_num := violating_seq_num;
                 elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_wait_store_completed) then
                  violating_seq_num := violating_seq_num;
                 elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_commit_based_on_inst) then
                  violating_seq_num := violating_seq_num;
                  if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                    ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                    next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].is_executed := false;
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].state := skid_issue_if_head;
                    next_state.core_[ j ].skid_buffer_.tail := ((next_state.core_[ j ].skid_buffer_.tail + 1) % skid_buffer_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].skid_buffer_.num_entries := (next_state.core_[ j ].skid_buffer_.num_entries + 1);
                    next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                  end;
                 elsif (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state = rob_commit_if_head) then
                  violating_seq_num := violating_seq_num;
                  if (next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction.seq_num >= violating_seq_num) then
                    ROB_squash_remove_count := (ROB_squash_remove_count + 1);
                    next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].is_executed := false;
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].instruction := next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].instruction;
                    next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.tail ].state := skid_issue_if_head;
                    next_state.core_[ j ].skid_buffer_.tail := ((next_state.core_[ j ].skid_buffer_.tail + 1) % skid_buffer_NUM_ENTRIES_CONST);
                    next_state.core_[ j ].skid_buffer_.num_entries := (next_state.core_[ j ].skid_buffer_.num_entries + 1);
                    next_state.core_[ j ].ROB_.entries[ ROB_squash_curr_idx ].state := rob_await_creation;
                  end;
                else
                  error "Controller is not on an expected state for a msg: (squash) from: (load_address_table) to: (ROB)";
                end;
              end;
              if (ROB_offset < ROB_difference) then
                ROB_offset := (ROB_offset + 1);
              end;
            end;
            next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - ROB_squash_remove_count)) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - ROB_squash_remove_count);
            next_state.core_[ j ].load_address_table_.entries[ load_address_table_squash_idx ].state := load_address_table_await_insert_remove;
          end;
        else
          error "Controller is not on an expected state for a msg: (squash) from: (invalidation_listener) to: (load_address_table)";
        end;
      end;
    endfor;
  end;
  next_state.core_[ j ].invalidation_listener_.state := await_invalidation;

      found_msg_in_ic := false;
      for ic_idx : ic_idx_t do
        -- if msg entry is valid & seq num matches, use this...
        -- ..and ack the IC entry
        if (Sta.ic_.valid[ic_idx] = true) & (Sta.ic_.buffer[ic_idx].seq_num = Sta.core_[j].mem_interface_.in_msg.seq_num)
          & (Sta.ic_.buffer[ic_idx].dest_id = Sta.core_[j].mem_interface_.in_msg.dest_id) then

          -- (1) send ack
          next_state.ic_.buffer[ic_idx].store_inval_ackd[j] := true;

          put next_state.ic_.buffer[ic_idx].store_inval_ackd[j];

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

  Sta := next_state;
end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_complete_store ===> rob_await_creation" 
(Sta.core_[ j ].ROB_.entries[ i ].state = rob_complete_store)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_commit_based_on_inst ===> rob_commit_based_on_inst || rob_await_creation || rob_wait_store_completed" 
(Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_based_on_inst)
==>
 
  var ROB_Load_is_in_state_set : boolean;
  var IQ_Load_is_in_state_set : boolean;
  var memory_unit_sender_Load_is_in_state_set : boolean;
  var ROB_Store_is_in_state_set : boolean;
  var second_memory_stage_Store_is_in_state_set : boolean;
  var IQ_Store_is_in_state_set : boolean;
  var memory_unit_sender_Store_is_in_state_set : boolean;
  var is_instruction_on_any_state : boolean;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : IQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
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
      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
    if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
      IQ_Load_is_in_state_set := true;
    else
      IQ_Load_is_in_state_set := false;
    end;
  end;
  memory_unit_sender_Load_is_in_state_set := false;
  if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
    if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
      memory_unit_sender_Load_is_in_state_set := true;
    else
      memory_unit_sender_Load_is_in_state_set := false;
    end;
  else
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
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
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst))))) then
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
  second_memory_stage_Store_is_in_state_set := false;
  if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
    if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send))) then
      second_memory_stage_Store_is_in_state_set := true;
    else
      second_memory_stage_Store_is_in_state_set := false;
    end;
  else
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
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
    if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
      IQ_Store_is_in_state_set := true;
    else
      IQ_Store_is_in_state_set := false;
    end;
  end;
  memory_unit_sender_Store_is_in_state_set := false;
  if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
    if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
      memory_unit_sender_Store_is_in_state_set := true;
    else
      memory_unit_sender_Store_is_in_state_set := false;
    end;
  else
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
  end;
  is_instruction_on_any_state := (ROB_Load_is_in_state_set | (IQ_Load_is_in_state_set | (memory_unit_sender_Load_is_in_state_set | (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | memory_unit_sender_Store_is_in_state_set))))));
  if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) then
    if is_instruction_on_any_state then
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
    else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst))))) then
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
      second_memory_stage_Store_is_in_state_set := false;
      if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
        if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send))) then
          second_memory_stage_Store_is_in_state_set := true;
        else
          second_memory_stage_Store_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
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
      memory_unit_sender_Store_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
          memory_unit_sender_Store_is_in_state_set := true;
        else
          memory_unit_sender_Store_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
      end;
      is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | memory_unit_sender_Store_is_in_state_set)));
      if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
        else
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
              if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
            if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
              IQ_Load_is_in_state_set := true;
            else
              IQ_Load_is_in_state_set := false;
            end;
          end;
          memory_unit_sender_Load_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
              memory_unit_sender_Load_is_in_state_set := true;
            else
              memory_unit_sender_Load_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
          end;
          is_instruction_on_any_state := (ROB_Load_is_in_state_set | (IQ_Load_is_in_state_set | memory_unit_sender_Load_is_in_state_set));
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
                next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                  next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                  next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                  next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
                else
                  if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                    if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                      next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                      next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                      next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                      next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                    else
                      error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                    end;
                    next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
                  end;
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
              next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                  if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                    next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                    next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                    next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                    next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                  end;
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
                end;
              end;
            end;
          end;
        end;
      else
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
            if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
            IQ_Load_is_in_state_set := true;
          else
            IQ_Load_is_in_state_set := false;
          end;
        end;
        memory_unit_sender_Load_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
            memory_unit_sender_Load_is_in_state_set := true;
          else
            memory_unit_sender_Load_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
        end;
        is_instruction_on_any_state := (ROB_Load_is_in_state_set | (IQ_Load_is_in_state_set | memory_unit_sender_Load_is_in_state_set));
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
              next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                  if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                    next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                    next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                    next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                    next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                  end;
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
                end;
              end;
            end;
          end;
        else
          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
            next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                  next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                  next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                  next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                end;
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
              end;
            end;
          end;
        end;
      end;
    end;
  else
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst))))) then
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
    second_memory_stage_Store_is_in_state_set := false;
    if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
      if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send))) then
        second_memory_stage_Store_is_in_state_set := true;
      else
        second_memory_stage_Store_is_in_state_set := false;
      end;
    else
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
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
    memory_unit_sender_Store_is_in_state_set := false;
    if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
      if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
        memory_unit_sender_Store_is_in_state_set := true;
      else
        memory_unit_sender_Store_is_in_state_set := false;
      end;
    else
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
    end;
    is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | memory_unit_sender_Store_is_in_state_set)));
    if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st) then
      if is_instruction_on_any_state then
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
      else
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
            if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
            IQ_Load_is_in_state_set := true;
          else
            IQ_Load_is_in_state_set := false;
          end;
        end;
        memory_unit_sender_Load_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
            memory_unit_sender_Load_is_in_state_set := true;
          else
            memory_unit_sender_Load_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
        end;
        is_instruction_on_any_state := (ROB_Load_is_in_state_set | (IQ_Load_is_in_state_set | memory_unit_sender_Load_is_in_state_set));
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
              next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              else
                if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                  if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                    next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                    next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                    next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                    next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                  end;
                  next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
                end;
              end;
            end;
          end;
        else
          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
            next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                  next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                  next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                  next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                end;
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
              end;
            end;
          end;
        end;
      end;
    else
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
          if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
        if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
          IQ_Load_is_in_state_set := true;
        else
          IQ_Load_is_in_state_set := false;
        end;
      end;
      memory_unit_sender_Load_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
          memory_unit_sender_Load_is_in_state_set := true;
        else
          memory_unit_sender_Load_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
      end;
      is_instruction_on_any_state := (ROB_Load_is_in_state_set | (IQ_Load_is_in_state_set | memory_unit_sender_Load_is_in_state_set));
      if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
        else
          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
            next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
          else
            if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
              next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
              next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
              next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            else
              if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
                if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                  next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                  next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                  next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
                end;
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
              end;
            end;
          end;
        end;
      else
        if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = dmb_st)))) then
          next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
          next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
          next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
        else
          if ((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ldar)) then
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((next_state.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
          else
            if (((next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = stlr)) & (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs)) then
              if (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_get_inputs) then
                next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.entries[ i ].instruction;
                next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.entries[ i ].instruction.imm;
                next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.entries[ i ].instruction.write_value;
                next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
              else
                error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (ROB) to: (second_memory_stage)";
              end;
              next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
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
  rule "ROB rob_commit_if_head ===> rob_commit_based_on_inst || rob_await_is_executed || rob_await_creation" 
((Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_if_head) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].ROB_.head = i) then
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_based_on_inst;
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
  next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "second_memory_stage second_mem_unit_send ===> second_mem_unit_send || second_mem_unit_receive" 
(((((((((Sta.core_[ j ].second_memory_stage_.state = second_mem_unit_send) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var ROB_dmb_sy_is_in_state_set : boolean;
  var IQ_dmb_sy_is_in_state_set : boolean;
  var memory_unit_sender_dmb_sy_is_in_state_set : boolean;
  var is_instruction_on_any_state : boolean;
  var ROB_dmb_st_is_in_state_set : boolean;
  var IQ_dmb_st_is_in_state_set : boolean;
  var memory_unit_sender_dmb_st_is_in_state_set : boolean;
  var ROB_Store_is_in_state_set : boolean;
  var second_memory_stage_Store_is_in_state_set : boolean;
  var IQ_Store_is_in_state_set : boolean;
  var memory_unit_sender_Store_is_in_state_set : boolean;
  var ROB_STLR_is_in_state_set : boolean;
  var second_memory_stage_STLR_is_in_state_set : boolean;
  var IQ_STLR_is_in_state_set : boolean;
  var memory_unit_sender_STLR_is_in_state_set : boolean;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : IQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
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
    if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_sy)) then
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
  IQ_dmb_sy_is_in_state_set := false;
  found_entry := false;
  for IQ_iter : IQ_idx_t do
    if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
      if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_sy)) then
        if (found_entry = false) then
          found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
          found_idx := IQ_iter;
        else
          if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
            found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
            found_idx := IQ_iter;
          end;
        end;
        found_entry := true;
      end;
    end;
  endfor;
  if (found_entry = false) then
  elsif (found_entry = true) then
    if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
      IQ_dmb_sy_is_in_state_set := true;
    else
      IQ_dmb_sy_is_in_state_set := false;
    end;
  end;
  memory_unit_sender_dmb_sy_is_in_state_set := false;
  if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_sy) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
    if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
      memory_unit_sender_dmb_sy_is_in_state_set := true;
    else
      memory_unit_sender_dmb_sy_is_in_state_set := false;
    end;
  else
    next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
  end;
  is_instruction_on_any_state := (ROB_dmb_sy_is_in_state_set | (IQ_dmb_sy_is_in_state_set | memory_unit_sender_dmb_sy_is_in_state_set));
  if (next_state.core_[ j ].second_memory_stage_.instruction.op = st) then
    if is_instruction_on_any_state then
      next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
    else
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_st)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
      IQ_dmb_st_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_st)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          IQ_dmb_st_is_in_state_set := true;
        else
          IQ_dmb_st_is_in_state_set := false;
        end;
      end;
      memory_unit_sender_dmb_st_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
          memory_unit_sender_dmb_st_is_in_state_set := true;
        else
          memory_unit_sender_dmb_st_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
      end;
      is_instruction_on_any_state := (ROB_dmb_st_is_in_state_set | (IQ_dmb_st_is_in_state_set | memory_unit_sender_dmb_st_is_in_state_set));
      if (next_state.core_[ j ].second_memory_stage_.instruction.op = st) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        else
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
          second_memory_stage_Store_is_in_state_set := false;
          if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
            if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
              second_memory_stage_Store_is_in_state_set := true;
            else
              second_memory_stage_Store_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          end;
          IQ_Store_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_Store_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
              memory_unit_sender_Store_is_in_state_set := true;
            else
              memory_unit_sender_Store_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
          second_memory_stage_STLR_is_in_state_set := false;
          if ((next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
            if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
              second_memory_stage_STLR_is_in_state_set := true;
            else
              second_memory_stage_STLR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          end;
          IQ_STLR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_STLR_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = stlr) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
              memory_unit_sender_STLR_is_in_state_set := true;
            else
              memory_unit_sender_STLR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          end;
          is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | (memory_unit_sender_Store_is_in_state_set | (ROB_STLR_is_in_state_set | (second_memory_stage_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | memory_unit_sender_STLR_is_in_state_set)))))));
          if (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
            else
              if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
                if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
                  next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
                  next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                  next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
                  next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                  next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                  next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                  next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
                  next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                  for core_idx : cores_t do
                    next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                    next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                  endfor;
                  next_state.core_[ j ].mem_interface_.out_busy := true;
                  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
                end;
              end;
            end;
          else
            if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
              if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                for core_idx : cores_t do
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                endfor;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
              end;
            end;
          end;
        end;
      else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
        second_memory_stage_Store_is_in_state_set := false;
        if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
            second_memory_stage_Store_is_in_state_set := true;
          else
            second_memory_stage_Store_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        IQ_Store_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_Store_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
            memory_unit_sender_Store_is_in_state_set := true;
          else
            memory_unit_sender_Store_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
        second_memory_stage_STLR_is_in_state_set := false;
        if ((next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
            second_memory_stage_STLR_is_in_state_set := true;
          else
            second_memory_stage_STLR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        IQ_STLR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_STLR_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = stlr) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
            memory_unit_sender_STLR_is_in_state_set := true;
          else
            memory_unit_sender_STLR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | (memory_unit_sender_Store_is_in_state_set | (ROB_STLR_is_in_state_set | (second_memory_stage_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | memory_unit_sender_STLR_is_in_state_set)))))));
        if (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          else
            if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
              if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                for core_idx : cores_t do
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                endfor;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
              end;
            end;
          end;
        else
          if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
            if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
              next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
              for core_idx : cores_t do
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
              endfor;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
            end;
          end;
        end;
      end;
    end;
  else
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
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_st)) then
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
    IQ_dmb_st_is_in_state_set := false;
    found_entry := false;
    for IQ_iter : IQ_idx_t do
      if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
        if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_st)) then
          if (found_entry = false) then
            found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
            found_idx := IQ_iter;
          else
            if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
              found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        IQ_dmb_st_is_in_state_set := true;
      else
        IQ_dmb_st_is_in_state_set := false;
      end;
    end;
    memory_unit_sender_dmb_st_is_in_state_set := false;
    if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
      if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
        memory_unit_sender_dmb_st_is_in_state_set := true;
      else
        memory_unit_sender_dmb_st_is_in_state_set := false;
      end;
    else
      next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
    end;
    is_instruction_on_any_state := (ROB_dmb_st_is_in_state_set | (IQ_dmb_st_is_in_state_set | memory_unit_sender_dmb_st_is_in_state_set));
    if (next_state.core_[ j ].second_memory_stage_.instruction.op = st) then
      if is_instruction_on_any_state then
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
      else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
        second_memory_stage_Store_is_in_state_set := false;
        if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
            second_memory_stage_Store_is_in_state_set := true;
          else
            second_memory_stage_Store_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        IQ_Store_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_Store_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
            memory_unit_sender_Store_is_in_state_set := true;
          else
            memory_unit_sender_Store_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
        second_memory_stage_STLR_is_in_state_set := false;
        if ((next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
            second_memory_stage_STLR_is_in_state_set := true;
          else
            second_memory_stage_STLR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        IQ_STLR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_STLR_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = stlr) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
            memory_unit_sender_STLR_is_in_state_set := true;
          else
            memory_unit_sender_STLR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        end;
        is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | (memory_unit_sender_Store_is_in_state_set | (ROB_STLR_is_in_state_set | (second_memory_stage_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | memory_unit_sender_STLR_is_in_state_set)))))));
        if (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          else
            if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
              if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
                next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
                next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
                next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
                next_state.core_[ j ].mem_interface_.out_msg.valid := true;
                next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
                next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
                next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
                next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
                for core_idx : cores_t do
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                  next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
                endfor;
                next_state.core_[ j ].mem_interface_.out_busy := true;
                next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
              end;
            end;
          end;
        else
          if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
            if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
              next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
              for core_idx : cores_t do
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
              endfor;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
            end;
          end;
        end;
      end;
    else
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = st)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
      second_memory_stage_Store_is_in_state_set := false;
      if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
        if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
          second_memory_stage_Store_is_in_state_set := true;
        else
          second_memory_stage_Store_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
      end;
      IQ_Store_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = st)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
      memory_unit_sender_Store_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = st) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
          memory_unit_sender_Store_is_in_state_set := true;
        else
          memory_unit_sender_Store_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = stlr)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_wait_store_completed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed)))) then
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
      second_memory_stage_STLR_is_in_state_set := false;
      if ((next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) & (next_state.core_[ j ].second_memory_stage_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
        if ((next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_send) | (next_state.core_[ j ].second_memory_stage_.state = second_mem_unit_receive)) then
          second_memory_stage_STLR_is_in_state_set := true;
        else
          second_memory_stage_STLR_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
      end;
      IQ_STLR_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = stlr)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].second_memory_stage_.instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
      memory_unit_sender_STLR_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = stlr) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].second_memory_stage_.instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
          memory_unit_sender_STLR_is_in_state_set := true;
        else
          memory_unit_sender_STLR_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
      end;
      is_instruction_on_any_state := (ROB_Store_is_in_state_set | (second_memory_stage_Store_is_in_state_set | (IQ_Store_is_in_state_set | (memory_unit_sender_Store_is_in_state_set | (ROB_STLR_is_in_state_set | (second_memory_stage_STLR_is_in_state_set | (IQ_STLR_is_in_state_set | memory_unit_sender_STLR_is_in_state_set)))))));
      if (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
        else
          if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
            if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
              next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
              next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
              next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
              next_state.core_[ j ].mem_interface_.out_msg.valid := true;
              next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
              next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
              next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
              next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
              for core_idx : cores_t do
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
                next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
              endfor;
              next_state.core_[ j ].mem_interface_.out_busy := true;
              next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
            end;
          end;
        end;
      else
        if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
          if ((next_state.core_[ j ].second_memory_stage_.instruction.op = st) | (next_state.core_[ j ].second_memory_stage_.instruction.op = stlr)) then
            next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].second_memory_stage_.phys_addr;
            next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
            next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].second_memory_stage_.write_value;
            next_state.core_[ j ].mem_interface_.out_msg.valid := true;
            next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
            next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
            next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].second_memory_stage_.instruction.seq_num;
            next_state.core_[ j ].mem_interface_.out_msg.store_state := await_handling;
            for core_idx : cores_t do
              next_state.core_[ j ].mem_interface_.out_msg.store_inval_sent[ core_idx ] := false;
              next_state.core_[ j ].mem_interface_.out_msg.store_inval_ackd[ core_idx ] := false;
            endfor;
            next_state.core_[ j ].mem_interface_.out_busy := true;
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
          end;
        end;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "second_memory_stage secnd_mem_init ===> second_mem_unit_get_inputs" 
(Sta.core_[ j ].second_memory_stage_.state = secnd_mem_init)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].second_memory_stage_.phys_addr := 0;
  next_state.core_[ j ].second_memory_stage_.write_value := 0;
  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_get_inputs;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : IQ_idx_t do 
  rule "IQ iq_schedule_inst ===> iq_schedule_inst || iq_await_creation" 
(Sta.core_[ j ].IQ_.entries[ i ].state = iq_schedule_inst)
==>
 
  var ROB_dmb_sy_is_in_state_set : boolean;
  var IQ_dmb_sy_is_in_state_set : boolean;
  var memory_unit_sender_dmb_sy_is_in_state_set : boolean;
  var is_instruction_on_any_state : boolean;
  var ROB_dmb_ld_is_in_state_set : boolean;
  var IQ_dmb_ld_is_in_state_set : boolean;
  var memory_unit_sender_dmb_ld_is_in_state_set : boolean;
  var ROB_LDAR_is_in_state_set : boolean;
  var IQ_LDAR_is_in_state_set : boolean;
  var memory_unit_sender_LDAR_is_in_state_set : boolean;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_count_t;
  var ROB_offset : ROB_count_t;
  var ROB_curr_idx : ROB_idx_t;
  var found_entry : boolean;
  var found_element : inst_count_t;
  var found_idx : IQ_idx_t;
  var insert_key_check_found : boolean;
  var found_double_key_check : boolean;
  var load_address_table_loop_break : boolean;
  var load_address_table_entry_idx : load_address_table_idx_t;
  var load_address_table_found_entry : boolean;
  var load_address_table_difference : load_address_table_idx_t;
  var load_address_table_offset : load_address_table_idx_t;
  var load_address_table_curr_idx : load_address_table_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
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
    if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_sy)) then
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
  IQ_dmb_sy_is_in_state_set := false;
  found_entry := false;
  for IQ_iter : IQ_idx_t do
    if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
      if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_sy)) then
        if (found_entry = false) then
          found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
          found_idx := IQ_iter;
        else
          if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
            found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
            found_idx := IQ_iter;
          end;
        end;
        found_entry := true;
      end;
    end;
  endfor;
  if (found_entry = false) then
  elsif (found_entry = true) then
    if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
      IQ_dmb_sy_is_in_state_set := true;
    else
      IQ_dmb_sy_is_in_state_set := false;
    end;
  end;
  memory_unit_sender_dmb_sy_is_in_state_set := false;
  if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_sy) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
    if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
      memory_unit_sender_dmb_sy_is_in_state_set := true;
    else
      memory_unit_sender_dmb_sy_is_in_state_set := false;
    end;
  else
    next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
  end;
  is_instruction_on_any_state := (ROB_dmb_sy_is_in_state_set | (IQ_dmb_sy_is_in_state_set | memory_unit_sender_dmb_sy_is_in_state_set));
  if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
    if is_instruction_on_any_state then
      next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
    else
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_ld)) then
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
      IQ_dmb_ld_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_ld)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              end;
            end;
            found_entry := true;
          end;
        end;
      endfor;
      if (found_entry = false) then
      elsif (found_entry = true) then
        if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
          IQ_dmb_ld_is_in_state_set := true;
        else
          IQ_dmb_ld_is_in_state_set := false;
        end;
      end;
      memory_unit_sender_dmb_ld_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
          memory_unit_sender_dmb_ld_is_in_state_set := true;
        else
          memory_unit_sender_dmb_ld_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
      end;
      is_instruction_on_any_state := (ROB_dmb_ld_is_in_state_set | (IQ_dmb_ld_is_in_state_set | memory_unit_sender_dmb_ld_is_in_state_set));
      if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
        else
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_LDAR_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
              memory_unit_sender_LDAR_is_in_state_set := true;
            else
              memory_unit_sender_LDAR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          end;
          is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
          if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            else
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
                if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
              IQ_LDAR_is_in_state_set := false;
              found_entry := false;
              for IQ_iter : IQ_idx_t do
                if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                  if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                    if (found_entry = false) then
                      found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                      found_idx := IQ_iter;
                    else
                      if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                        found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
              memory_unit_sender_LDAR_is_in_state_set := false;
              if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
                if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
                  memory_unit_sender_LDAR_is_in_state_set := true;
                else
                  memory_unit_sender_LDAR_is_in_state_set := false;
                end;
              else
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
              end;
              is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
              if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
                if is_instruction_on_any_state then
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                        next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                        next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                        if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                          insert_key_check_found := false;
                          found_double_key_check := false;
                          for load_address_table_key_check_idx : load_address_table_idx_t do
                            if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                                if (insert_key_check_found = false) then
                                  insert_key_check_found := true;
                                else
                                  found_double_key_check := true;
                                end;
                              end;
                            end;
                          endfor;
                          if (found_double_key_check = true) then
                            error "Found two entries with the same key? Was this intentional?";
                          elsif (insert_key_check_found = false) then
                            load_address_table_loop_break := false;
                            if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                              load_address_table_loop_break := true;
                            end;
                            load_address_table_entry_idx := 0;
                            load_address_table_found_entry := false;
                            load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                            load_address_table_offset := 0;
                            while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                              load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                                if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                                  next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                  next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                  next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                                  next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                                  load_address_table_found_entry := true;
                                end;
                              end;
                              if (load_address_table_offset != load_address_table_difference) then
                                load_address_table_offset := (load_address_table_offset + 1);
                              else
                                load_address_table_loop_break := true;
                              end;
                            end;
                            next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                            if (load_address_table_found_entry = false) then
                              error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                            end;
                          end;
                        end;
                        next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                      else
                        error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                      end;
                      next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                      next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                      next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                    end;
                  else
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                          if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                            next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                            next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                          else
                            error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                          end;
                          ROB_found_entry := true;
                        end;
                        if (ROB_offset < ROB_difference) then
                          ROB_offset := (ROB_offset + 1);
                        end;
                      end;
                      if (ROB_found_entry = false) then
                      end;
                      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                      next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                      next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                      next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                    end;
                  end;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                      next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                      if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                        insert_key_check_found := false;
                        found_double_key_check := false;
                        for load_address_table_key_check_idx : load_address_table_idx_t do
                          if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                              if (insert_key_check_found = false) then
                                insert_key_check_found := true;
                              else
                                found_double_key_check := true;
                              end;
                            end;
                          end;
                        endfor;
                        if (found_double_key_check = true) then
                          error "Found two entries with the same key? Was this intentional?";
                        elsif (insert_key_check_found = false) then
                          load_address_table_loop_break := false;
                          if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                            load_address_table_loop_break := true;
                          end;
                          load_address_table_entry_idx := 0;
                          load_address_table_found_entry := false;
                          load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                          load_address_table_offset := 0;
                          while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                            load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                                load_address_table_found_entry := true;
                              end;
                            end;
                            if (load_address_table_offset != load_address_table_difference) then
                              load_address_table_offset := (load_address_table_offset + 1);
                            else
                              load_address_table_loop_break := true;
                            end;
                          end;
                          next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                          if (load_address_table_found_entry = false) then
                            error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                          end;
                        end;
                      end;
                      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                    else
                      error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                    end;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                        else
                          error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                        end;
                        ROB_found_entry := true;
                      end;
                      if (ROB_offset < ROB_difference) then
                        ROB_offset := (ROB_offset + 1);
                      end;
                    end;
                    if (ROB_found_entry = false) then
                    end;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                end;
              end;
            end;
          else
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
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
            memory_unit_sender_LDAR_is_in_state_set := false;
            if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
              if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
                memory_unit_sender_LDAR_is_in_state_set := true;
              else
                memory_unit_sender_LDAR_is_in_state_set := false;
              end;
            else
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            end;
            is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
            if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                      next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                      if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                        insert_key_check_found := false;
                        found_double_key_check := false;
                        for load_address_table_key_check_idx : load_address_table_idx_t do
                          if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                              if (insert_key_check_found = false) then
                                insert_key_check_found := true;
                              else
                                found_double_key_check := true;
                              end;
                            end;
                          end;
                        endfor;
                        if (found_double_key_check = true) then
                          error "Found two entries with the same key? Was this intentional?";
                        elsif (insert_key_check_found = false) then
                          load_address_table_loop_break := false;
                          if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                            load_address_table_loop_break := true;
                          end;
                          load_address_table_entry_idx := 0;
                          load_address_table_found_entry := false;
                          load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                          load_address_table_offset := 0;
                          while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                            load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                                load_address_table_found_entry := true;
                              end;
                            end;
                            if (load_address_table_offset != load_address_table_difference) then
                              load_address_table_offset := (load_address_table_offset + 1);
                            else
                              load_address_table_loop_break := true;
                            end;
                          end;
                          next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                          if (load_address_table_found_entry = false) then
                            error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                          end;
                        end;
                      end;
                      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                    else
                      error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                    end;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                        else
                          error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                        end;
                        ROB_found_entry := true;
                      end;
                      if (ROB_offset < ROB_difference) then
                        ROB_offset := (ROB_offset + 1);
                      end;
                    end;
                    if (ROB_found_entry = false) then
                    end;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          end;
        end;
      else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_LDAR_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
            memory_unit_sender_LDAR_is_in_state_set := true;
          else
            memory_unit_sender_LDAR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
        end;
        is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
        if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          else
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
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
            memory_unit_sender_LDAR_is_in_state_set := false;
            if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
              if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
                memory_unit_sender_LDAR_is_in_state_set := true;
              else
                memory_unit_sender_LDAR_is_in_state_set := false;
              end;
            else
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            end;
            is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
            if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                      next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                      if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                        insert_key_check_found := false;
                        found_double_key_check := false;
                        for load_address_table_key_check_idx : load_address_table_idx_t do
                          if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                              if (insert_key_check_found = false) then
                                insert_key_check_found := true;
                              else
                                found_double_key_check := true;
                              end;
                            end;
                          end;
                        endfor;
                        if (found_double_key_check = true) then
                          error "Found two entries with the same key? Was this intentional?";
                        elsif (insert_key_check_found = false) then
                          load_address_table_loop_break := false;
                          if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                            load_address_table_loop_break := true;
                          end;
                          load_address_table_entry_idx := 0;
                          load_address_table_found_entry := false;
                          load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                          load_address_table_offset := 0;
                          while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                            load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                                load_address_table_found_entry := true;
                              end;
                            end;
                            if (load_address_table_offset != load_address_table_difference) then
                              load_address_table_offset := (load_address_table_offset + 1);
                            else
                              load_address_table_loop_break := true;
                            end;
                          end;
                          next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                          if (load_address_table_found_entry = false) then
                            error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                          end;
                        end;
                      end;
                      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                    else
                      error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                    end;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                        else
                          error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                        end;
                        ROB_found_entry := true;
                      end;
                      if (ROB_offset < ROB_difference) then
                        ROB_offset := (ROB_offset + 1);
                      end;
                    end;
                    if (ROB_found_entry = false) then
                    end;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          end;
        else
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_LDAR_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
              memory_unit_sender_LDAR_is_in_state_set := true;
            else
              memory_unit_sender_LDAR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          end;
          is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
          if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
              if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                  next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                    insert_key_check_found := false;
                    found_double_key_check := false;
                    for load_address_table_key_check_idx : load_address_table_idx_t do
                      if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                          if (insert_key_check_found = false) then
                            insert_key_check_found := true;
                          else
                            found_double_key_check := true;
                          end;
                        end;
                      end;
                    endfor;
                    if (found_double_key_check = true) then
                      error "Found two entries with the same key? Was this intentional?";
                    elsif (insert_key_check_found = false) then
                      load_address_table_loop_break := false;
                      if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                        load_address_table_loop_break := true;
                      end;
                      load_address_table_entry_idx := 0;
                      load_address_table_found_entry := false;
                      load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                      load_address_table_offset := 0;
                      while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                        load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                            load_address_table_found_entry := true;
                          end;
                        end;
                        if (load_address_table_offset != load_address_table_difference) then
                          load_address_table_offset := (load_address_table_offset + 1);
                        else
                          load_address_table_loop_break := true;
                        end;
                      end;
                      next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                      if (load_address_table_found_entry = false) then
                        error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                      end;
                    end;
                  end;
                  next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                end;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                    else
                      error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                    end;
                    ROB_found_entry := true;
                  end;
                  if (ROB_offset < ROB_difference) then
                    ROB_offset := (ROB_offset + 1);
                  end;
                end;
                if (ROB_found_entry = false) then
                end;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            end;
          end;
        end;
      end;
    end;
  else
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
      if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = dmb_ld)) then
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_if_head) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst) | ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) | (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_commit_based_on_inst)))) then
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
    IQ_dmb_ld_is_in_state_set := false;
    found_entry := false;
    for IQ_iter : IQ_idx_t do
      if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
        if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = dmb_ld)) then
          if (found_entry = false) then
            found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
            found_idx := IQ_iter;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
              found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            end;
          end;
          found_entry := true;
        end;
      end;
    endfor;
    if (found_entry = false) then
    elsif (found_entry = true) then
      if ((next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst) | (next_state.core_[ j ].IQ_.entries[ found_idx ].state = iq_schedule_inst)) then
        IQ_dmb_ld_is_in_state_set := true;
      else
        IQ_dmb_ld_is_in_state_set := false;
      end;
    end;
    memory_unit_sender_dmb_ld_is_in_state_set := false;
    if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = dmb_ld) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
      if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write))) then
        memory_unit_sender_dmb_ld_is_in_state_set := true;
      else
        memory_unit_sender_dmb_ld_is_in_state_set := false;
      end;
    else
      next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
    end;
    is_instruction_on_any_state := (ROB_dmb_ld_is_in_state_set | (IQ_dmb_ld_is_in_state_set | memory_unit_sender_dmb_ld_is_in_state_set));
    if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
      if is_instruction_on_any_state then
        next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
      else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_LDAR_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
            memory_unit_sender_LDAR_is_in_state_set := true;
          else
            memory_unit_sender_LDAR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
        end;
        is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
        if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          else
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
              if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
                if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
            IQ_LDAR_is_in_state_set := false;
            found_entry := false;
            for IQ_iter : IQ_idx_t do
              if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
                if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                  if (found_entry = false) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                    found_idx := IQ_iter;
                  else
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                      found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
            memory_unit_sender_LDAR_is_in_state_set := false;
            if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
              if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
                memory_unit_sender_LDAR_is_in_state_set := true;
              else
                memory_unit_sender_LDAR_is_in_state_set := false;
              end;
            else
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            end;
            is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
            if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
              if is_instruction_on_any_state then
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                      next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                      next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                      if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                        insert_key_check_found := false;
                        found_double_key_check := false;
                        for load_address_table_key_check_idx : load_address_table_idx_t do
                          if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                              if (insert_key_check_found = false) then
                                insert_key_check_found := true;
                              else
                                found_double_key_check := true;
                              end;
                            end;
                          end;
                        endfor;
                        if (found_double_key_check = true) then
                          error "Found two entries with the same key? Was this intentional?";
                        elsif (insert_key_check_found = false) then
                          load_address_table_loop_break := false;
                          if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                            load_address_table_loop_break := true;
                          end;
                          load_address_table_entry_idx := 0;
                          load_address_table_found_entry := false;
                          load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                          load_address_table_offset := 0;
                          while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                            load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                              if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                                next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                                load_address_table_found_entry := true;
                              end;
                            end;
                            if (load_address_table_offset != load_address_table_difference) then
                              load_address_table_offset := (load_address_table_offset + 1);
                            else
                              load_address_table_loop_break := true;
                            end;
                          end;
                          next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                          if (load_address_table_found_entry = false) then
                            error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                          end;
                        end;
                      end;
                      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                    else
                      error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                    end;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                        if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                          next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                        else
                          error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                        end;
                        ROB_found_entry := true;
                      end;
                      if (ROB_offset < ROB_difference) then
                        ROB_offset := (ROB_offset + 1);
                      end;
                    end;
                    if (ROB_found_entry = false) then
                    end;
                    next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                    next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                    next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                    next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                  end;
                end;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          end;
        else
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_LDAR_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
              memory_unit_sender_LDAR_is_in_state_set := true;
            else
              memory_unit_sender_LDAR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          end;
          is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
          if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
              if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                  next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                    insert_key_check_found := false;
                    found_double_key_check := false;
                    for load_address_table_key_check_idx : load_address_table_idx_t do
                      if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                          if (insert_key_check_found = false) then
                            insert_key_check_found := true;
                          else
                            found_double_key_check := true;
                          end;
                        end;
                      end;
                    endfor;
                    if (found_double_key_check = true) then
                      error "Found two entries with the same key? Was this intentional?";
                    elsif (insert_key_check_found = false) then
                      load_address_table_loop_break := false;
                      if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                        load_address_table_loop_break := true;
                      end;
                      load_address_table_entry_idx := 0;
                      load_address_table_found_entry := false;
                      load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                      load_address_table_offset := 0;
                      while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                        load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                            load_address_table_found_entry := true;
                          end;
                        end;
                        if (load_address_table_offset != load_address_table_difference) then
                          load_address_table_offset := (load_address_table_offset + 1);
                        else
                          load_address_table_loop_break := true;
                        end;
                      end;
                      next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                      if (load_address_table_found_entry = false) then
                        error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                      end;
                    end;
                  end;
                  next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                end;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                    else
                      error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                    end;
                    ROB_found_entry := true;
                  end;
                  if (ROB_offset < ROB_difference) then
                    ROB_offset := (ROB_offset + 1);
                  end;
                end;
                if (ROB_found_entry = false) then
                end;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            end;
          end;
        end;
      end;
    else
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
        if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
          if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
      IQ_LDAR_is_in_state_set := false;
      found_entry := false;
      for IQ_iter : IQ_idx_t do
        if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
          if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
            if (found_entry = false) then
              found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
              found_idx := IQ_iter;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
      memory_unit_sender_LDAR_is_in_state_set := false;
      if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
        if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
          memory_unit_sender_LDAR_is_in_state_set := true;
        else
          memory_unit_sender_LDAR_is_in_state_set := false;
        end;
      else
        next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
      end;
      is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
      if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
        if is_instruction_on_any_state then
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
        else
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
            if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
              if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
          IQ_LDAR_is_in_state_set := false;
          found_entry := false;
          for IQ_iter : IQ_idx_t do
            if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
              if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
                if (found_entry = false) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                  found_idx := IQ_iter;
                else
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                    found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
          memory_unit_sender_LDAR_is_in_state_set := false;
          if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
            if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
              memory_unit_sender_LDAR_is_in_state_set := true;
            else
              memory_unit_sender_LDAR_is_in_state_set := false;
            end;
          else
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          end;
          is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
          if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
            if is_instruction_on_any_state then
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                    next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                    next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                    if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                      insert_key_check_found := false;
                      found_double_key_check := false;
                      for load_address_table_key_check_idx : load_address_table_idx_t do
                        if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                            if (insert_key_check_found = false) then
                              insert_key_check_found := true;
                            else
                              found_double_key_check := true;
                            end;
                          end;
                        end;
                      endfor;
                      if (found_double_key_check = true) then
                        error "Found two entries with the same key? Was this intentional?";
                      elsif (insert_key_check_found = false) then
                        load_address_table_loop_break := false;
                        if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                          load_address_table_loop_break := true;
                        end;
                        load_address_table_entry_idx := 0;
                        load_address_table_found_entry := false;
                        load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                        load_address_table_offset := 0;
                        while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                          load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                            if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                              next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                              load_address_table_found_entry := true;
                            end;
                          end;
                          if (load_address_table_offset != load_address_table_difference) then
                            load_address_table_offset := (load_address_table_offset + 1);
                          else
                            load_address_table_loop_break := true;
                          end;
                        end;
                        next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                        if (load_address_table_found_entry = false) then
                          error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                        end;
                      end;
                    end;
                    next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                  else
                    error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                  end;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                      else
                        error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                      end;
                      ROB_found_entry := true;
                    end;
                    if (ROB_offset < ROB_difference) then
                      ROB_offset := (ROB_offset + 1);
                    end;
                  end;
                  if (ROB_found_entry = false) then
                  end;
                  next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                  next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                  next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                  next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
                end;
              end;
            end;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
              if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                  next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                    insert_key_check_found := false;
                    found_double_key_check := false;
                    for load_address_table_key_check_idx : load_address_table_idx_t do
                      if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                          if (insert_key_check_found = false) then
                            insert_key_check_found := true;
                          else
                            found_double_key_check := true;
                          end;
                        end;
                      end;
                    endfor;
                    if (found_double_key_check = true) then
                      error "Found two entries with the same key? Was this intentional?";
                    elsif (insert_key_check_found = false) then
                      load_address_table_loop_break := false;
                      if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                        load_address_table_loop_break := true;
                      end;
                      load_address_table_entry_idx := 0;
                      load_address_table_found_entry := false;
                      load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                      load_address_table_offset := 0;
                      while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                        load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                            load_address_table_found_entry := true;
                          end;
                        end;
                        if (load_address_table_offset != load_address_table_difference) then
                          load_address_table_offset := (load_address_table_offset + 1);
                        else
                          load_address_table_loop_break := true;
                        end;
                      end;
                      next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                      if (load_address_table_found_entry = false) then
                        error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                      end;
                    end;
                  end;
                  next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                end;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                    else
                      error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                    end;
                    ROB_found_entry := true;
                  end;
                  if (ROB_offset < ROB_difference) then
                    ROB_offset := (ROB_offset + 1);
                  end;
                end;
                if (ROB_found_entry = false) then
                end;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            end;
          end;
        end;
      else
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
          if ((next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.op = ldar)) then
            if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
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
        IQ_LDAR_is_in_state_set := false;
        found_entry := false;
        for IQ_iter : IQ_idx_t do
          if next_state.core_[ j ].IQ_.entries[ IQ_iter ].valid then
            if ((next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.op = ldar)) then
              if (found_entry = false) then
                found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
                found_idx := IQ_iter;
              else
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num) < found_element) then
                  found_element := (next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].IQ_.entries[ IQ_iter ].instruction.seq_num);
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
        memory_unit_sender_LDAR_is_in_state_set := false;
        if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar) & (next_state.core_[ j ].memory_unit_sender_.instruction.seq_num < next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num)) then
          if ((next_state.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) | (next_state.core_[ j ].memory_unit_sender_.state = memory_unit_receiver)) then
            memory_unit_sender_LDAR_is_in_state_set := true;
          else
            memory_unit_sender_LDAR_is_in_state_set := false;
          end;
        else
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
        end;
        is_instruction_on_any_state := (ROB_LDAR_is_in_state_set | (IQ_LDAR_is_in_state_set | memory_unit_sender_LDAR_is_in_state_set));
        if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar) then
          if is_instruction_on_any_state then
            next_state.core_[ j ].IQ_.entries[ i ].state := iq_schedule_inst;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
              if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                  next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                  next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                  if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                    insert_key_check_found := false;
                    found_double_key_check := false;
                    for load_address_table_key_check_idx : load_address_table_idx_t do
                      if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                          if (insert_key_check_found = false) then
                            insert_key_check_found := true;
                          else
                            found_double_key_check := true;
                          end;
                        end;
                      end;
                    endfor;
                    if (found_double_key_check = true) then
                      error "Found two entries with the same key? Was this intentional?";
                    elsif (insert_key_check_found = false) then
                      load_address_table_loop_break := false;
                      if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                        load_address_table_loop_break := true;
                      end;
                      load_address_table_entry_idx := 0;
                      load_address_table_found_entry := false;
                      load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                      load_address_table_offset := 0;
                      while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                        load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                          if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                            next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                            load_address_table_found_entry := true;
                          end;
                        end;
                        if (load_address_table_offset != load_address_table_difference) then
                          load_address_table_offset := (load_address_table_offset + 1);
                        else
                          load_address_table_loop_break := true;
                        end;
                      end;
                      next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                      if (load_address_table_found_entry = false) then
                        error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                      end;
                    end;
                  end;
                  next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
                else
                  error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
                end;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            else
              if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                    else
                      error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                    end;
                    ROB_found_entry := true;
                  end;
                  if (ROB_offset < ROB_difference) then
                    ROB_offset := (ROB_offset + 1);
                  end;
                end;
                if (ROB_found_entry = false) then
                end;
                next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
                next_state.core_[ j ].IQ_.entries[ i ].valid := false;
                next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
                next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
              end;
            end;
          end;
        else
          if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
            if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
              if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
                next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.entries[ i ].instruction;
                next_state.core_[ j ].memory_unit_sender_.phys_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
                if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ldar)) then
                  insert_key_check_found := false;
                  found_double_key_check := false;
                  for load_address_table_key_check_idx : load_address_table_idx_t do
                    if next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].valid then
                      if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                        next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                        next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                        next_state.core_[ j ].load_address_table_.entries[ load_address_table_key_check_idx ].state := load_address_table_await_insert_remove;
                        if (insert_key_check_found = false) then
                          insert_key_check_found := true;
                        else
                          found_double_key_check := true;
                        end;
                      end;
                    end;
                  endfor;
                  if (found_double_key_check = true) then
                    error "Found two entries with the same key? Was this intentional?";
                  elsif (insert_key_check_found = false) then
                    load_address_table_loop_break := false;
                    if (next_state.core_[ j ].load_address_table_.num_entries = load_address_table_NUM_ENTRIES_CONST) then
                      load_address_table_loop_break := true;
                    end;
                    load_address_table_entry_idx := 0;
                    load_address_table_found_entry := false;
                    load_address_table_difference := (load_address_table_NUM_ENTRIES_CONST - 1);
                    load_address_table_offset := 0;
                    while ((load_address_table_offset <= load_address_table_difference) & ((load_address_table_loop_break = false) & ((load_address_table_found_entry = false) & (load_address_table_difference >= 0)))) do
                      load_address_table_curr_idx := ((load_address_table_entry_idx + load_address_table_offset) % load_address_table_NUM_ENTRIES_CONST);
                      if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid = false) then
                        if (next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state = load_address_table_await_insert_remove) then
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].lat_address := next_state.core_[ j ].memory_unit_sender_.phys_addr;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].state := load_address_table_await_insert_remove;
                          next_state.core_[ j ].load_address_table_.entries[ load_address_table_curr_idx ].valid := true;
                          load_address_table_found_entry := true;
                        end;
                      end;
                      if (load_address_table_offset != load_address_table_difference) then
                        load_address_table_offset := (load_address_table_offset + 1);
                      else
                        load_address_table_loop_break := true;
                      end;
                    end;
                    next_state.core_[ j ].load_address_table_.num_entries := (next_state.core_[ j ].load_address_table_.num_entries + 1);
                    if (load_address_table_found_entry = false) then
                      error "Couldn't find an empty entry to insert (insert_key) from (£ctrler_name) to (£dest_ctrler_name) into";
                    end;
                  end;
                end;
                next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
              else
                error "Controller is not on an expected state for a msg: (pass_pipe_regs) from: (IQ) to: (memory_unit_sender)";
              end;
              next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
              next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
              next_state.core_[ j ].IQ_.entries[ i ].valid := false;
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
            end;
          else
            if ((next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) | (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = stlr)) then
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
                if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
                  if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
                    next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
                    next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
                  else
                    error "Controller is not on an expected state for a msg: (executed) from: (IQ) to: (ROB)";
                  end;
                  ROB_found_entry := true;
                end;
                if (ROB_offset < ROB_difference) then
                  ROB_offset := (ROB_offset + 1);
                end;
              end;
              if (ROB_found_entry = false) then
              end;
              next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
              next_state.core_[ j ].IQ_.entries[ i ].valid := false;
              next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num := 0;
              next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
            end;
          end;
        end;
      end;
    end;
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


ruleset j : cores_t do 
  rule "memory_unit_sender memory_unit_load_result_write ===> mem_unit_send_get_input" 
(Sta.core_[ j ].memory_unit_sender_.state = memory_unit_load_result_write)
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
  next_state.core_[ j ].rf_.rf[ next_state.core_[ j ].memory_unit_sender_.instruction.dest_reg ] := next_state.core_[ j ].memory_unit_sender_.read_value;
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
    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].memory_unit_sender_.instruction.seq_num) then
      if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state = rob_await_is_executed) then
        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
        next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
      else
        error "Controller is not on an expected state for a msg: (executed) from: (memory_unit_sender) to: (ROB)";
      end;
      next_state.core_[ j ].memory_unit_sender_.instruction.seq_num := 0;
      ROB_found_entry := true;
    end;
    if (ROB_offset < ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  next_state.core_[ j ].memory_unit_sender_.state := mem_unit_send_get_input;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "memory_unit_sender memory_unit_stage_send ===> memory_unit_receiver || mem_unit_send_get_input" 
((Sta.core_[ j ].memory_unit_sender_.state = memory_unit_stage_send) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if !((next_state.core_[ j ].mem_interface_.out_busy = true)) then
    if ((next_state.core_[ j ].memory_unit_sender_.instruction.op = ld) | (next_state.core_[ j ].memory_unit_sender_.instruction.op = ldar)) then
      next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].memory_unit_sender_.phys_addr;
      next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
      next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].memory_unit_sender_.instruction.seq_num;
      next_state.core_[ j ].mem_interface_.out_busy := true;
      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_receiver;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  rule "memory_unit_sender memory_unit_send_init ===> mem_unit_send_get_input" 
(Sta.core_[ j ].memory_unit_sender_.state = memory_unit_send_init)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].memory_unit_sender_.phys_addr := 0;
  next_state.core_[ j ].memory_unit_sender_.read_value := 0;
  next_state.core_[ j ].memory_unit_sender_.state := mem_unit_send_get_input;
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
  if ((next_state.core_[ j ].RENAME_.head = i) & (!((next_state.core_[ j ].RENAME_.num_entries = 0)) & (!((next_state.core_[ j ].ROB_.num_entries = ROB_NUM_ENTRIES_CONST)) & (next_state.core_[ j ].skid_buffer_.num_entries = 0)))) then
    if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st)))) then
      next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
      if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
        next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
        next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
      else
        error "Controller is not on an expected state for a msg: (increment) from: (RENAME) to: (SeqNumReg)";
      end;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].phys_addr := next_state.core_[ j ].RENAME_.entries[ i ].instruction.imm;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].write_value := next_state.core_[ j ].RENAME_.entries[ i ].instruction.write_value;
      if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st)))) then
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
      else
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_is_executed;
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
      if !((next_state.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST)) then
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
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].phys_addr := next_state.core_[ j ].RENAME_.entries[ i ].instruction.imm;
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].write_value := next_state.core_[ j ].RENAME_.entries[ i ].instruction.write_value;
        if ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = dmb_st)))) then
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
        else
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_is_executed;
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
        next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
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


ruleset j : cores_t; i : skid_buffer_idx_t do 
  rule "skid_buffer skid_issue_if_head ===> skid_buffer_await_creation" 
((Sta.core_[ j ].skid_buffer_.entries[ i ].state = skid_issue_if_head) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST))
==>
 
  var IQ_loop_break : boolean;
  var IQ_entry_idx : IQ_idx_t;
  var IQ_found_entry : boolean;
  var IQ_offset : IQ_count_t;
  var IQ_curr_idx : IQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if ((next_state.core_[ j ].skid_buffer_.head = i) & (!((next_state.core_[ j ].skid_buffer_.num_entries = 0)) & !((next_state.core_[ j ].ROB_.num_entries = ROB_NUM_ENTRIES_CONST)))) then
    if ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_st)))) then
      next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
      if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
        next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
        next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
      else
        error "Controller is not on an expected state for a msg: (increment) from: (skid_buffer) to: (SeqNumReg)";
      end;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].phys_addr := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.imm;
      next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].write_value := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.write_value;
      if ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_st)))) then
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
      else
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_is_executed;
      end;
      next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
      next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
      next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].instruction.op := inval;
      next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].instruction.seq_num := 0;
      next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].state := skid_buffer_await_creation;
      next_state.core_[ j ].skid_buffer_.head := ((next_state.core_[ j ].skid_buffer_.head + 1) % skid_buffer_NUM_ENTRIES_CONST);
      next_state.core_[ j ].skid_buffer_.num_entries := (next_state.core_[ j ].skid_buffer_.num_entries - 1);
      next_state.core_[ j ].skid_buffer_.entries[ i ].state := skid_buffer_await_creation;
    else
      if !((next_state.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST)) then
        next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.seq_num := next_state.core_[ j ].SeqNumReg_.seq_num_counter;
        if (next_state.core_[ j ].SeqNumReg_.state = seq_num_interface) then
          next_state.core_[ j ].SeqNumReg_.seq_num_counter := (next_state.core_[ j ].SeqNumReg_.seq_num_counter + 1);
          next_state.core_[ j ].SeqNumReg_.state := seq_num_interface;
        else
          error "Controller is not on an expected state for a msg: (increment) from: (skid_buffer) to: (SeqNumReg)";
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
            next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].instruction := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction;
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
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction;
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].phys_addr := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.imm;
        next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].write_value := next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.write_value;
        if ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = mfence) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_sy) | ((next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_ld) | (next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op = dmb_st)))) then
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
        else
          next_state.core_[ j ].ROB_.entries[ next_state.core_[ j ].ROB_.tail ].state := rob_await_is_executed;
        end;
        next_state.core_[ j ].ROB_.tail := ((next_state.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
        next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries + 1);
        next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].instruction.op := inval;
        next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].instruction.seq_num := 0;
        next_state.core_[ j ].skid_buffer_.entries[ next_state.core_[ j ].skid_buffer_.head ].state := skid_buffer_await_creation;
        next_state.core_[ j ].skid_buffer_.head := ((next_state.core_[ j ].skid_buffer_.head + 1) % skid_buffer_NUM_ENTRIES_CONST);
        next_state.core_[ j ].skid_buffer_.num_entries := (next_state.core_[ j ].skid_buffer_.num_entries - 1);
        next_state.core_[ j ].skid_buffer_.entries[ i ].state := skid_buffer_await_creation;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : skid_buffer_idx_t do 
  rule "skid_buffer init_skid_buffer ===> skid_buffer_await_creation" 
(Sta.core_[ j ].skid_buffer_.entries[ i ].state = init_skid_buffer)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].skid_buffer_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].skid_buffer_.entries[ i ].state := skid_buffer_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : load_address_table_idx_t do 
  rule "load_address_table init_table_load_address_table ===> load_address_table_await_insert_remove" 
(Sta.core_[ j ].load_address_table_.entries[ i ].state = init_table_load_address_table)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].load_address_table_.entries[ i ].lat_seq_num := 0;
  next_state.core_[ j ].load_address_table_.entries[ i ].lat_address := 0;
  next_state.core_[ j ].load_address_table_.entries[ i ].state := load_address_table_await_insert_remove;
  Sta := next_state;

end;
end;

invariant "Dekker"
((((Sta.core_[ 0 ].RENAME_.num_entries = 0) &
(Sta.core_[ 0 ].skid_buffer_.num_entries = 0) &
(Sta.core_[ 0 ].ROB_.num_entries = 0)) &
((Sta.core_[ 1 ].RENAME_.num_entries = 0) &
(Sta.core_[ 1 ].skid_buffer_.num_entries = 0) &
(Sta.core_[ 1 ].ROB_.num_entries = 0)))
->
!((((Sta.core_[ 0 ].rf_.rf[ 0 ] = 0) &
(Sta.core_[ 0 ].rf_.rf[ 1 ] = 0)) &
((Sta.core_[ 1 ].rf_.rf[ 0 ] = 0) &
(Sta.core_[ 1 ].rf_.rf[ 1 ] = 0)))));

