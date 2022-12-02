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

type
val_t : 0 .. MAX_VALUE;
inst_idx_t : 0 .. CORE_INST_NUM;
inst_count_t : 0 .. (CORE_INST_NUM + 1);
addr_idx_t : 0 .. ADDR_NUM;
reg_idx_t : 0 .. REG_NUM;
ic_idx_t : 0 .. IC_ENTRY_NUM;
ic_count_t : 0 .. (IC_ENTRY_NUM + 1);
cores_t : 0 .. CORE_NUM;
MSG_DEST : enum {core, mem};
INST_TYPE : enum {ld, st, inval};
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
ROB_state : enum {rob_wait_load_replay, rob_commit_if_head, rob_await_creation, init_rob_entry};
ROB_idx_t : 0 .. ROB_NUM_ENTRIES_ENUM_CONST;
ROB_count_t : 0 .. ROB_NUM_ENTRIES_CONST;
ROB_entry_values : record
  instruction : INST;
  seq_num : inst_idx_t;
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
  seq_num : inst_idx_t;
  state : IQ_state;
end;
IQ : record
  entries : array [IQ_idx_t] of IQ_entry_values;
  num_entries : IQ_count_t;
end;
memory_unit_sender_state : enum {memory_unit_receiver, memory_unit_stage_send, mem_unit_send_get_input, memory_unit_send_init};
memory_unit_sender : record
  instruction : INST;
  phys_addr : addr_idx_t;
  state : memory_unit_sender_state;
end;
RENAME_state : enum {rename_await_creation, issue_if_head, init_rename_entry};
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
R_W : enum {read, write};
MEM_REQ : record
  addr : addr_idx_t;
  r_w : R_W;
  value : val_t;
  valid : boolean;
  dest : MSG_DEST;
  dest_id : cores_t;
  seq_num : inst_count_t;
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
  ROB_ : ROB;
  second_memory_stage_ : second_memory_stage;
  IQ_ : IQ;
  memory_unit_sender_ : memory_unit_sender;
  RENAME_ : RENAME;
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
      -- assert (curr_entry.state = rob_commit_time_await_st_mem_resp) "ACK ROB: Should be in await mem resp?";
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
    alias iq : init_state.core_[ core ].IQ_ do
      for i : 0 .. CORE_INST_NUM do
        iq.entries[ i ].instruction.op := inval;
        iq.entries[ i ].instruction.seq_num := 0;
        iq.entries[ i ].state := iq_await_creation;
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
  endfor;
  alias rename_c0 : init_state.core_[ 0 ].RENAME_ do
    rename_c0.entries[ 0 ].instruction.op := ld;
    rename_c0.entries[ 0 ].instruction.seq_num := 1;
    rename_c0.entries[ 0 ].instruction.dest_reg := 0;
    rename_c0.entries[ 0 ].instruction.imm := 1;
    rename_c0.entries[ 0 ].instruction.write_value := 0;
    rename_c0.entries[ 1 ].instruction.op := ld;
    rename_c0.entries[ 1 ].instruction.seq_num := 2;
    rename_c0.entries[ 1 ].instruction.dest_reg := 1;
    rename_c0.entries[ 1 ].instruction.imm := 0;
    rename_c0.entries[ 1 ].instruction.write_value := 0;
    rename_c0.head := 0;
    rename_c0.tail := 2 % (CORE_INST_NUM + 1);
    rename_c0.num_entries := 2;
  end;
  alias rename_c1 : init_state.core_[ 1 ].RENAME_ do
    rename_c1.entries[ 0 ].instruction.op := st;
    rename_c1.entries[ 0 ].instruction.seq_num := 1;
    rename_c1.entries[ 0 ].instruction.dest_reg := 0;
    rename_c1.entries[ 0 ].instruction.imm := 0;
    rename_c1.entries[ 0 ].instruction.write_value := 1;
    rename_c1.entries[ 1 ].instruction.op := st;
    rename_c1.entries[ 1 ].instruction.seq_num := 2;
    rename_c1.entries[ 1 ].instruction.dest_reg := 1;
    rename_c1.entries[ 1 ].instruction.imm := 1;
    rename_c1.entries[ 1 ].instruction.write_value := 1;
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

begin
  next_state := Sta;
  ic := Sta.ic_;
  mem := Sta.mem_;
  addr := ic.buffer[ i ].addr;
  if (ic.buffer[ i ].r_w = read) then
    ic.buffer[ i ].value := mem.arr[ addr ];
  elsif (ic.buffer[ i ].r_w = write) then
    mem.arr[ addr ] := ic.buffer[ i ].value;
  end;
  ic.buffer[ i ].dest := core;
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
  -- var lq : LSQ;
  var rob : ROB;
  var rob_id : ROB_idx_t;
  var mem_interface : MEM_INTERFACE;

begin
  next_state := Sta;
  -- lq := Sta.core_[ j ].LSQ_;
  rob := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  -- Do this based on the stage state
  if (Sta.core_[j].memory_unit_sender_.state = mem_unit_receiver) &
    (Sta.core_[j].memory_unit_sender_.instruction.seq_num = mem_interface.in_msg.seq_num) then
    if (mem_interface.in_msg.r_w = read) then
      rob_id := search_rob_seq_num_idx(rob, Sta.core_[j].second_memory_stage_.instruction.seq_num);
      assert(rob.entries[rob_id].state = rob_commit_if_head);
      rob.entries[rob_id].is_executed := true;
      next_state.core[j].memory_unit_sender_.state := mem_unit_send_get_input;
    elsif (mem_interface.in_msg.r_w = write) then
      error "first memory stage got a write, but it doesn't handle stores...";
    end;
  elsif (Sta.core_[j].second_memory_stage_.state = second_mem_unit_receive) &
    (Sta.core_[j].second_memory_stage_.instruction.seq_num = mem_interface.in_msg.seq_num) then
    if (mem_interface.in_msg.r_w = read) then
      rob_id := search_rob_seq_num_idx(rob, Sta.core_[j].second_memory_stage_.instruction.seq_num);
      assert(rob.entries[rob_id].state = rob_wait_load_replay);
      rob.entries[rob_id].replay_value := mem_interface.in_msg.value;
      rob.entries[rob_id].state := rob_received_load_replay;
    elsif (mem_interface.in_msg.r_w = write) then
      rob_id := search_rob_seq_num_idx(rob, Sta.core_[j].second_memory_stage_.instruction.seq_num);
      assert(rob.entries[rob_id].state = rob_wait_store_complete);
      rob.entries[rob_id].state := rob_complete_store;
    end;
    next_state.core_[j].second_memory_stage_.state := second_mem_unit_get_inputs;
  endif;
  mem_interface.in_busy := false;
  -- next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := rob;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end;


ruleset j : cores_t do 
  invariant "test_invariant"
((Sta.core_[ j ].LSQ_.num_entries = 1) -> (Sta.core_[ j ].LSQ_.tail = ((Sta.core_[ j ].LSQ_.head + 1) % LSQ_NUM_ENTRIES_CONST)));
end;

rule "reset" 
((Sta.core_[ 0 ].RENAME_.num_entries = 0) & ((Sta.core_[ 0 ].ROB_.num_entries = 0) & ((Sta.core_[ 0 ].IQ_.num_entries = 0) & ((Sta.core_[ 0 ].LSQ_.num_entries = 0) & ((Sta.core_[ 1 ].RENAME_.num_entries = 0) & ((Sta.core_[ 1 ].ROB_.num_entries = 0) & ((Sta.core_[ 1 ].IQ_.num_entries = 0) & (Sta.core_[ 1 ].LSQ_.num_entries = 0))))))))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  Sta := init_state_fn();

end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_commit_if_head ===> rob_commit_if_head || rob_commit_if_head || rob_wait_load_replay || rob_wait_store_completed || rob_commit_if_head" 
((Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_if_head) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].ROB_.entries[ i ].is_executed = false) then
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
  else
    if (next_state.core_[ j ].ROB_.head = i) then
      if (next_state.core_[ j ].second_memory_stage_.state != second_mem_unit_get_inputs) then
        next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
      else
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) then
          next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.instruction;
          next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.phys_addr;
          next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.write_value;
          next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
          next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_load_replay;
        else
          if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) then
            next_state.core_[ j ].second_memory_stage_.instruction := next_state.core_[ j ].ROB_.instruction;
            next_state.core_[ j ].second_memory_stage_.phys_addr := next_state.core_[ j ].ROB_.phys_addr;
            next_state.core_[ j ].second_memory_stage_.write_value := next_state.core_[ j ].ROB_.write_value;
            next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_send;
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_wait_store_completed;
          end;
        end;
      end;
    else
      next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
    end;
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


ruleset j : cores_t; i : second_memory_stage_idx_t do 
  rule "second_memory_stage second_mem_unit_send ===> second_mem_unit_receive || second_mem_unit_receive" 
(((Sta.core_[ j ].second_memory_stage_.entries[ i ].state = second_mem_unit_send) & !(Sta.core_[ j ].mem_interface_.out_busy)) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
    if (instruction.op = ld) then
      next_state.core_[ j ].mem_interface_.out_msg.addr := phys_addr;
      next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
      next_state.core_[ j ].mem_interface_.out_msg.seq_num := instruction.seq_num;
      next_state.core_[ j ].mem_interface_.out_busy := true;
      next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
    else
      if (instruction.op = st) then
        next_state.core_[ j ].mem_interface_.out_msg.addr := phys_addr;
        next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
        next_state.core_[ j ].mem_interface_.out_msg.value := write_value;
        next_state.core_[ j ].mem_interface_.out_msg.valid := true;
        next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
        next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
        next_state.core_[ j ].mem_interface_.out_msg.seq_num := instruction.seq_num;
        next_state.core_[ j ].mem_interface_.out_busy := true;
        next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_receive;
      end;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : second_memory_stage_idx_t do 
  rule "second_memory_stage secnd_mem_init ===> second_mem_unit_get_inputs" 
(Sta.core_[ j ].second_memory_stage_.entries[ i ].state = secnd_mem_init)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  phys_addr := 0;
  write_value := 0;
  next_state.core_[ j ].second_memory_stage_.state := second_mem_unit_get_inputs;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : IQ_idx_t do 
  rule "IQ iq_schedule_inst ===> iq_await_creation || iq_await_creation" 
(Sta.core_[ j ].IQ_.entries[ i ].state = iq_schedule_inst)
==>
 
  var rob : ROB;
  var rob_id : ROB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
    if (next_state.core_[ j ].memory_unit_sender_.state = mem_unit_send_get_input) then
      next_state.core_[ j ].memory_unit_sender_.instruction := next_state.core_[ j ].IQ_.instruction;
      next_state.core_[ j ].memory_unit_sender_.phys_addr := phys_addr;
      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_stage_send;
      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
      next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
    end;
  else
    if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) then
      rob := next_state.core_[ j ].ROB_;
      rob_id := search_ROB_seq_num_idx(rob, next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num);
      assert (rob.entries[ rob_id ].is_executed = false) "why isn't it false?";
      rob.entries[ rob_id ].is_executed := true;
      next_state.core_[ j ].ROB_ := rob;
      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
      next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
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


ruleset j : cores_t; i : memory_unit_sender_idx_t do 
  rule "memory_unit_sender memory_unit_stage_send ===> memory_unit_receiver" 
((Sta.core_[ j ].memory_unit_sender_.entries[ i ].state = memory_unit_stage_send) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].second_memory_stage_.state != second_mem_unit_receive) then
    if (instruction.op = ld) then
      next_state.core_[ j ].mem_interface_.out_msg.addr := phys_addr;
      next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
      next_state.core_[ j ].mem_interface_.out_msg.valid := true;
      next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
      next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
      next_state.core_[ j ].mem_interface_.out_msg.seq_num := instruction.seq_num;
      next_state.core_[ j ].mem_interface_.out_busy := true;
      next_state.core_[ j ].memory_unit_sender_.state := memory_unit_receiver;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : memory_unit_sender_idx_t do 
  rule "memory_unit_sender memory_unit_send_init ===> mem_unit_send_get_input" 
(Sta.core_[ j ].memory_unit_sender_.entries[ i ].state = memory_unit_send_init)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  phys_addr := 0;
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
  var IQ_difference : IQ_idx_t;
  var IQ_offset : IQ_idx_t;
  var IQ_curr_idx : IQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].RENAME_.head = i) then
    if (!((Sta.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST)) & (!((Sta.core_[ j ].ROB_.num_entries = ROB_NUM_ENTRIES_CONST)) & !((Sta.core_[ j ].LSQ_.num_entries = LSQ_NUM_ENTRIES_CONST)))) then
      if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = st) then
      end;
      IQ_loop_break := false;
      if (next_state.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST) then
        IQ_loop_break := true;
      end;
      IQ_entry_idx := 0;
      IQ_found_entry := false;
      IQ_difference := (IQ_NUM_ENTRIES_CONST - 1);
      IQ_offset := 0;
      while ((IQ_offset <= IQ_difference) & ((IQ_loop_break = false) & ((IQ_found_entry = false) & (IQ_difference >= 0)))) do
        IQ_curr_idx := ((IQ_entry_idx + IQ_offset) % IQ_NUM_ENTRIES_CONST);
        if (next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].state = iq_await_creation) then
          next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
          next_state.core_[ j ].IQ_.entries[ IQ_curr_idx ].state := iq_schedule_inst;
          IQ_found_entry := true;
        end;
        if (IQ_offset != IQ_difference) then
          IQ_offset := (IQ_offset + 1);
        else
          IQ_loop_break := true;
        end;
      end;
      next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries + 1);
      if (IQ_found_entry = false) then
        error "Couldn't find an empty entry to insert into";
      end;
      next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
      next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.tail ].state := rob_commit_if_head;
      next_state.core_[ j ].ROB_.tail := ((Sta.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
      next_state.core_[ j ].ROB_.num_entries := (Sta.core_[ j ].ROB_.num_entries + 1);
      next_state.core_[ j ].LSQ_.tail := ((Sta.core_[ j ].LSQ_.tail + 1) % LSQ_NUM_ENTRIES_CONST);
      next_state.core_[ j ].LSQ_.num_entries := (Sta.core_[ j ].LSQ_.num_entries + 1);
      next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.op := inval;
      next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
      next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].state := issue_if_head;
      next_state.core_[ j ].RENAME_.head := ((Sta.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
      next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
      next_state.core_[ j ].RENAME_.entries[ i ].state := rename_await_creation;
    else
      next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : RENAME_idx_t do 
  rule "RENAME init_rename_entry ===> issue_if_head" 
(Sta.core_[ j ].RENAME_.entries[ i ].state = init_rename_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].RENAME_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].RENAME_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
  Sta := next_state;

end;
end;

invariant "amd1"
((((Sta.core_[ 0 ].RENAME_.num_entries = 0) & (Sta.core_[ 0 ].ROB_.num_entries = 0)) & ((Sta.core_[ 1 ].RENAME_.num_entries = 0) & (Sta.core_[ 1 ].ROB_.num_entries = 0))) -> !(((Sta.core_[ 0 ].rf_.rf[ 0 ] = 1) & (Sta.core_[ 0 ].rf_.rf[ 1 ] = 0))));

