const
CORE_INST_NUM : 1;
IC_ENTRY_NUM : 1;
CORE_NUM : 1;
ADDR_NUM : 1;
REG_NUM : 1;
MAX_VALUE : 1;
LQ_NUM_ENTRIES_ENUM_CONST : 1;
LQ_NUM_ENTRIES_CONST : 2;
SQ_NUM_ENTRIES_ENUM_CONST : 1;
SQ_NUM_ENTRIES_CONST : 2;
SB_NUM_ENTRIES_ENUM_CONST : 1;
SB_NUM_ENTRIES_CONST : 2;
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
LQ_state : enum {squashed_await_ld_mem_resp, await_committed, write_result, await_mem_response, build_packet_send_mem_request, await_sb_fwd_check_response, await_fwd_check, await_translation, await_scheduled, await_creation, init_entry_with_default_state, LQ_stall_build_packet_send_mem_request};
LQ_idx_t : 0 .. LQ_NUM_ENTRIES_ENUM_CONST;
LQ_count_t : 0 .. LQ_NUM_ENTRIES_CONST;
LQ_entry_values : record
  virt_addr : addr_idx_t;
  phys_addr : addr_idx_t;
  read_value : val_t;
  st_seq_num : inst_count_t;
  seq_num : inst_count_t;
  instruction : INST;
  state : LQ_state;
end;
LQ : record
  entries : array [LQ_idx_t] of LQ_entry_values;
  num_entries : LQ_count_t;
  head : LQ_idx_t;
  tail : LQ_idx_t;
end;
SQ_state : enum {sq_await_committed, sq_await_translation, sq_await_scheduled, sq_await_creation, init_store_entry};
SQ_idx_t : 0 .. SQ_NUM_ENTRIES_ENUM_CONST;
SQ_count_t : 0 .. SQ_NUM_ENTRIES_CONST;
SQ_entry_values : record
  virt_addr : addr_idx_t;
  phys_addr : addr_idx_t;
  write_value : val_t;
  instruction : INST;
  ld_seq_num : inst_count_t;
  state : SQ_state;
end;
SQ : record
  entries : array [SQ_idx_t] of SQ_entry_values;
  num_entries : SQ_count_t;
  head : SQ_idx_t;
  tail : SQ_idx_t;
end;
SB_state : enum {sb_await_mem_response, sb_await_send_mem_req, sb_await_creation, init_SB_entry};
SB_idx_t : 0 .. SB_NUM_ENTRIES_ENUM_CONST;
SB_count_t : 0 .. SB_NUM_ENTRIES_CONST;
SB_entry_values : record
  virt_addr : addr_idx_t;
  phys_addr : addr_idx_t;
  write_value : val_t;
  instruction : INST;
  state : SB_state;
end;
SB : record
  entries : array [SB_idx_t] of SB_entry_values;
  num_entries : SB_count_t;
end;
ROB_state : enum {rob_commit_if_head, rob_await_executed, rob_await_creation, init_rob_entry};
ROB_idx_t : 0 .. ROB_NUM_ENTRIES_ENUM_CONST;
ROB_count_t : 0 .. ROB_NUM_ENTRIES_CONST;
ROB_entry_values : record
  instruction : INST;
  seq_num : inst_count_t;
  is_executed : boolean;
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
end;
IQ : record
  entries : array [IQ_idx_t] of IQ_entry_values;
  num_entries : IQ_count_t;
end;
RENAME_state : enum {issue_if_head, init_rename_entry};
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
  LQ_ : LQ;
  SQ_ : SQ;
  SB_ : SB;
  ROB_ : ROB;
  IQ_ : IQ;
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

function search_lq_seq_num_idx (
  lq : LQ;
  seq_num : inst_count_t
) : LQ_idx_t;
  var lq_new : LQ;
  var lq_idx : LQ_idx_t;
begin
  for i : LQ_idx_t do
    if (lq.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "LQ Search: didn't find it? how? bad seq_num idx?";
end;

function search_sq_seq_num_idx (
  sq : SQ;
  seq_num : inst_count_t
) : SQ_idx_t;
  var sq_new : SQ;
  var sq_idx : SQ_idx_t;
begin
  for i : SQ_idx_t do
    if (sq.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "SQ Search: didn't find it? how? bad seq_num idx?";
end;

function sq_clear_head (
  sq : SQ
) : SQ;
  var sq_new : SQ;
  var curr_head : SQ_idx_t;
begin
  sq_new := sq;
  curr_head := sq.head;
  sq_new.entries[ curr_head ].instruction.seq_num := 0;
  sq_new.entries[ curr_head ].instruction.op := inval;
  sq_new.entries[ curr_head ].instruction.dest_reg := 0;
  sq_new.entries[ curr_head ].instruction.imm := 0;
  sq_new.entries[ curr_head ].state := sq_await_creation;
  sq_new.entries[ curr_head ].write_value := 0;
  sq_new.entries[ curr_head ].virt_addr := 0;
  sq_new.entries[ curr_head ].phys_addr := 0;
  sq_new.head := ((curr_head + 1) % SQ_NUM_ENTRIES_CONST);
  sq_new.num_entries := (sq_new.num_entries - 1);
  return sq_new;
end;

function sq_commit_head (
  sq : SQ
) : SQ;
  var sq_new : SQ;
  var sq_idx : SQ_idx_t;
  var sq_entry : SQ_entry_values;
begin
  sq_new := sq;
  if (sq.entries[ sq.head ].state = sq_await_committed) then
    sq_new := sq_clear_head(sq_new);
  else
    error "SQ entry should be on sq_await_committed";
  end;
  return sq_new;
end;

function sb_insert (
  sb : SB;
  sq_entry : SQ_entry_values
) : SB;
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
end;

function sb_clear_entry (
  sb : SB;
  seq_num : inst_count_t
) : SB;
  var sb_new : SB;
  var curr_head : SB_idx_t;
begin
  sb_new := sb;
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
      sb_new.num_entries := (sb_new.num_entries - 1);
      return sb_new;
    end;
  endfor;
  error "Couldn't find the SB entry to clear!!!";
end;

function insert_ld_in_mem_interface (
  ld_entry : LQ_entry_values;
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
  sb_entry : SB_entry_values;
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

function associative_assign_lq (
  lq : LQ;
  msg : MEM_REQ
) : LQ;
  var lq_new : LQ;
  var lq_iter : LQ_idx_t;
  var lq_count : LQ_count_t;
  var curr_entry : LQ_entry_values;
  var curr_entry_id : LQ_idx_t;
  var seq_num : inst_count_t;
begin
  lq_new := lq;
  lq_iter := lq.head;
  lq_count := lq.num_entries;
  seq_num := msg.seq_num;
  for i : 0 .. LQ_NUM_ENTRIES_ENUM_CONST do
    curr_entry_id := ((lq_iter + i) % LQ_NUM_ENTRIES_CONST);
    curr_entry := lq_new.entries[ curr_entry_id ];
    if (curr_entry.instruction.seq_num = seq_num) then
      assert ((curr_entry.state = squashed_await_ld_mem_resp) | (curr_entry.state = await_mem_response)) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";
      if (curr_entry.state = await_mem_response) then
        curr_entry.state := write_result;
      elsif (curr_entry.state = squashed_await_ld_mem_resp) then
        curr_entry.state := await_fwd_check;
      end;
      curr_entry.read_value := msg.value;
      lq_new.entries[ curr_entry_id ] := curr_entry;
      return lq_new;
    end;
  endfor;
  return lq_new;
end;

function associative_ack_sb (
  sb : SB;
  msg : MEM_REQ
) : SB;
  var sb_new : SB;
  var sb_iter : SB_idx_t;
  var sb_count : SB_count_t;
  var curr_entry : SB_entry_values;
  var curr_entry_id : SB_idx_t;
  var seq_num : inst_count_t;
begin
  sb_new := sb;
  sb_iter := 0;
  sb_count := sb.num_entries;
  seq_num := msg.seq_num;
  for i : 0 .. SB_NUM_ENTRIES_ENUM_CONST do
    curr_entry_id := ((sb_iter + i) % SB_NUM_ENTRIES_CONST);
    curr_entry := sb_new.entries[ curr_entry_id ];
    if (curr_entry.instruction.seq_num = seq_num) then
      assert (curr_entry.state = sb_await_mem_response) "ACK SB: Should be in await mem resp?";
      sb_new := sb_clear_entry(sb_new, seq_num);
      return sb_new;
    end;
  endfor;
  error "didn't find the Load to write the read val into?";
  return sb_new;
end;

function store_queue_match_phys_addr_younger_than_seq_num_request (
  sq : SQ;
  st_seq_num : inst_count_t;
  phys_addr : addr_idx_t;
  ld_seq_num : inst_count_t
) : SQ;
  var sq_new : SQ;
begin
  sq_new := sq;
  return sq_new;
end;

function store_buffer_match_phys_addr_younger_than_seq_num_request (
  sb : SB;
  st_seq_num : inst_count_t;
  phys_addr : addr_idx_t;
  ld_seq_num : inst_count_t
) : SB;
  var sb_new : SB;
begin
  sb_new := sb;
  return sb_new;
end;

function find_st_idx_of_seq_num (
  sq : SQ;
  seq_num : inst_count_t
) : SQ_count_t;
  var sq_entry : SQ_entry_values;
begin
  for i : SQ_idx_t do
    sq_entry := sq.entries[ i ];
    if (sq_entry.instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  return 0;
end;

function set_load_seq_num_entry_read (
  lq : LQ;
  seq_num : inst_count_t;
  value : val_t
) : LQ;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  for i : LQ_idx_t do
    ld_entry := lq.entries[ i ];
    if (ld_entry.instruction.seq_num = seq_num) then
      lq_new.entries[ i ].read_value := value;
      return lq_new;
    end;
  endfor;
  return lq_new;
end;

function set_load_state_to_state (
  lq : LQ;
  seq_num : inst_count_t;
  state : LQ_state
) : LQ;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  for i : LQ_idx_t do
    ld_entry := lq.entries[ i ];
    if (ld_entry.instruction.seq_num = seq_num) then
      lq_new.entries[ i ].state := state;
      return lq_new;
    end;
  endfor;
  return lq_new;
end;

function assert_load_state_is_state (
  lq : LQ;
  seq_num : inst_count_t;
  state : LQ_state
) : boolean;
  var lq_new : LQ;
  var ld_entry : LQ_entry_values;
begin
  lq_new := lq;
  for i : LQ_idx_t do
    ld_entry := lq.entries[ i ];
    if (ld_entry.instruction.seq_num = seq_num) then
      assert (lq_new.entries[ i ].state = state) "bad state";
      return false;
    end;
  endfor;
  return true;
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
    alias lq : init_state.core_[ core ].LQ_ do
      for i : LQ_idx_t do
        lq.entries[ i ].instruction.seq_num := 0;
        lq.entries[ i ].instruction.op := inval;
        lq.entries[ i ].instruction.imm := 0;
        lq.entries[ i ].instruction.dest_reg := 0;
        lq.entries[ i ].read_value := 0;
        lq.entries[ i ].virt_addr := 0;
        lq.entries[ i ].phys_addr := 0;
        lq.entries[ i ].st_seq_num := 0;
        lq.entries[ i ].state := await_creation;
      endfor;
      lq.head := 0;
      lq.tail := 0;
      lq.num_entries := 0;
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
    alias sq : init_state.core_[ core ].SQ_ do
      for i : SQ_idx_t do
        sq.entries[ i ].instruction.seq_num := 0;
        sq.entries[ i ].instruction.op := inval;
        sq.entries[ i ].instruction.imm := 0;
        sq.entries[ i ].instruction.dest_reg := 0;
        sq.entries[ i ].write_value := 0;
        sq.entries[ i ].virt_addr := 0;
        sq.entries[ i ].phys_addr := 0;
        sq.entries[ i ].state := sq_await_creation;
      endfor;
      sq.head := 0;
      sq.tail := 0;
      sq.num_entries := 0;
    end;
    alias sb : init_state.core_[ core ].SB_ do
      for i : SB_idx_t do
        sb.entries[ i ].instruction.seq_num := 0;
        sb.entries[ i ].instruction.op := inval;
        sb.entries[ i ].instruction.imm := 0;
        sb.entries[ i ].instruction.dest_reg := 0;
        sb.entries[ i ].write_value := 0;
        sb.entries[ i ].virt_addr := 0;
        sb.entries[ i ].phys_addr := 0;
        sb.entries[ i ].state := sb_await_creation;
      endfor;
      sb.num_entries := 0;
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

function search_LQ_seq_num_idx (
  LQ_ : LQ;
  seq_num : inst_count_t
) : LQ_idx_t;
begin
  for i : LQ_idx_t do
    if (LQ_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;

function search_SQ_seq_num_idx (
  SQ_ : SQ;
  seq_num : inst_count_t
) : SQ_idx_t;
begin
  for i : SQ_idx_t do
    if (SQ_.entries[ i ].instruction.seq_num = seq_num) then
      return i;
    end;
  endfor;
  error "£ctrler_name Search: didn't find it? how? bad seq_num idx?";
end;

function search_SB_seq_num_idx (
  SB_ : SB;
  seq_num : inst_count_t
) : SB_idx_t;
begin
  for i : SB_idx_t do
    if (SB_.entries[ i ].instruction.seq_num = seq_num) then
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
  var lq : LQ;
  var sb : SB;
  var mem_interface : MEM_INTERFACE;

begin
  next_state := Sta;
  lq := Sta.core_[ j ].LQ_;
  sb := Sta.core_[ j ].SB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  if (mem_interface.in_msg.r_w = read) then
    lq := associative_assign_lq(lq, mem_interface.in_msg);
  elsif (mem_interface.in_msg.r_w = write) then
    sb := associative_ack_sb(sb, mem_interface.in_msg);
  end;
  mem_interface.in_busy := false;
  next_state.core_[ j ].LQ_ := lq;
  next_state.core_[ j ].SB_ := sb;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end;


rule "reset" 
((Sta.core_[ 0 ].RENAME_.num_entries = 0) &
((Sta.core_[ 0 ].ROB_.num_entries = 0) &
((Sta.core_[ 0 ].IQ_.num_entries = 0) &
(Sta.core_[ 0 ].SB_.num_entries = 0) &
(Sta.core_[ 0 ].SQ_.num_entries = 0) &
((Sta.core_[ 0 ].LQ_.num_entries = 0) &
((Sta.core_[ 1 ].RENAME_.num_entries = 0) &
((Sta.core_[ 1 ].ROB_.num_entries = 0) &
((Sta.core_[ 1 ].IQ_.num_entries = 0) &
(Sta.core_[ 1 ].LQ_.num_entries = 0)&
(Sta.core_[ 1 ].SB_.num_entries = 0) &
(Sta.core_[ 1 ].SQ_.num_entries = 0)
)))))))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  Sta := init_state_fn();

end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ write_result ===> await_committed || await_fwd_check" 
(Sta.core_[ j ].LQ_.entries[ i ].state = write_result)
==>
 
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_idx_t;
  var ROB_offset : ROB_idx_t;
  var ROB_curr_idx : ROB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].rf_.rf[ next_state.core_[ j ].LQ_.entries[ i ].instruction.dest_reg ] := next_state.core_[ j ].LQ_.entries[ i ].read_value;
  ROB_while_break := false;
  ROB_found_entry := false;
  if (next_state.core_[ j ].ROB_.num_entries = 0) then
    ROB_while_break := true;
  end;
  ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
  ROB_difference := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - next_state.core_[ j ].ROB_.head)) % ROB_NUM_ENTRIES_CONST);
  ROB_offset := 0;
  while ((ROB_offset <= ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
    ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num) then
      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
      ROB_found_entry := true;
    end;
    if (ROB_offset != ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    else
      ROB_while_break := true;
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  next_state.core_[ j ].LQ_.entries[ i ].state := await_committed;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ build_packet_send_mem_request ===> await_mem_response || await_fwd_check" 
((Sta.core_[ j ].LQ_.entries[ i ].state = build_packet_send_mem_request) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].LQ_.entries[ i ].phys_addr;
  next_state.core_[ j ].mem_interface_.out_msg.r_w := read;
  next_state.core_[ j ].mem_interface_.out_msg.valid := true;
  next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
  next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
  next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num;
  next_state.core_[ j ].mem_interface_.out_busy := true;
  next_state.core_[ j ].LQ_.entries[ i ].state := await_mem_response;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ await_sb_fwd_check_response ===> LQ_stall_build_packet_send_mem_request || write_result || await_fwd_check" 
(Sta.core_[ j ].LQ_.entries[ i ].state = await_sb_fwd_check_response)
==>
 
  var found_entry : boolean;
  var found_element : val_t;
  var found_idx : SB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  found_entry := false;
  for SB_iter : SB_idx_t do
    if (Sta.core_[ j ].SB_.entries[ SB_iter ].instruction.seq_num != 0) then
      if ((next_state.core_[ j ].SB_.entries[ SB_iter ].phys_addr = next_state.core_[ j ].LQ_.entries[ i ].phys_addr) & (next_state.core_[ j ].SB_.entries[ SB_iter ].instruction.seq_num < next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num)) then
        if (found_entry = false) then
          found_element := (next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].SB_.entries[ SB_iter ].instruction.seq_num);
          found_idx := SB_iter;
        else
          if ((next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].SB_.entries[ SB_iter ].instruction.seq_num) < found_element) then
            found_element := (next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].SB_.entries[ SB_iter ].instruction.seq_num);
            found_idx := SB_iter;
          end;
        end;
        found_entry := true;
      end;
    end;
  endfor;
  if (found_entry = false) then
    next_state.core_[ j ].LQ_.entries[ i ].state := LQ_stall_build_packet_send_mem_request;
  elsif (found_entry = true) then
    next_state.core_[ j ].LQ_.entries[ i ].read_value := next_state.core_[ j ].SB_.entries[ found_idx ].write_value;
    next_state.core_[ j ].LQ_.entries[ i ].state := write_result;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ await_fwd_check ===> await_sb_fwd_check_response || write_result || await_sb_fwd_check_response || write_result" 
(Sta.core_[ j ].LQ_.entries[ i ].state = await_fwd_check)
==>
 
  var SQ_while_break : boolean;
  var SQ_found_entry : boolean;
  var SQ_entry_idx : SQ_idx_t;
  var SQ_difference : SQ_idx_t;
  var SQ_offset : SQ_idx_t;
  var SQ_curr_idx : SQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].LQ_.entries[ i ].st_seq_num != 0) then
    SQ_while_break := false;
    SQ_found_entry := false;
    if (next_state.core_[ j ].SQ_.num_entries = 0) then
      SQ_while_break := true;
    end;
    SQ_entry_idx := ((next_state.core_[ j ].SQ_.tail + (SQ_NUM_ENTRIES_CONST - 1)) % SQ_NUM_ENTRIES_CONST);
    SQ_difference := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - next_state.core_[ j ].SQ_.head)) % SQ_NUM_ENTRIES_CONST);
    SQ_offset := 0;
    while ((SQ_offset <= SQ_difference) & ((SQ_while_break = false) & (SQ_found_entry = false))) do
      SQ_curr_idx := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - SQ_offset)) % SQ_NUM_ENTRIES_CONST);
      if ((next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].phys_addr = next_state.core_[ j ].LQ_.entries[ i ].phys_addr) & (next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num)) then
        next_state.core_[ j ].LQ_.entries[ i ].read_value := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].write_value;
        next_state.core_[ j ].LQ_.entries[ i ].state := write_result;
        SQ_found_entry := true;
      end;
      if (SQ_offset != SQ_difference) then
        SQ_offset := (SQ_offset + 1);
      else
        SQ_while_break := true;
      end;
    end;
    if (SQ_found_entry = false) then
      next_state.core_[ j ].LQ_.entries[ i ].state := await_sb_fwd_check_response;
    end;
  else
    SQ_while_break := false;
    SQ_found_entry := false;
    if (next_state.core_[ j ].SQ_.num_entries = 0) then
      SQ_while_break := true;
    end;
    SQ_entry_idx := ((next_state.core_[ j ].SQ_.tail + (SQ_NUM_ENTRIES_CONST - 1)) % SQ_NUM_ENTRIES_CONST);
    SQ_difference := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - next_state.core_[ j ].SQ_.head)) % SQ_NUM_ENTRIES_CONST);
    SQ_offset := 0;
    while ((SQ_offset <= SQ_difference) & ((SQ_while_break = false) & (SQ_found_entry = false))) do
      SQ_curr_idx := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - SQ_offset)) % SQ_NUM_ENTRIES_CONST);
      if (next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].phys_addr = next_state.core_[ j ].LQ_.entries[ i ].phys_addr) then
        next_state.core_[ j ].LQ_.entries[ i ].read_value := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].write_value;
        next_state.core_[ j ].LQ_.entries[ i ].state := write_result;
        SQ_found_entry := true;
      end;
      if (SQ_offset != SQ_difference) then
        SQ_offset := (SQ_offset + 1);
      else
        SQ_while_break := true;
      end;
    end;
    if (SQ_found_entry = false) then
      next_state.core_[ j ].LQ_.entries[ i ].state := await_sb_fwd_check_response;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ await_translation ===> await_fwd_check" 
(Sta.core_[ j ].LQ_.entries[ i ].state = await_translation)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].LQ_.entries[ i ].phys_addr := next_state.core_[ j ].LQ_.entries[ i ].virt_addr;
  next_state.core_[ j ].LQ_.entries[ i ].state := await_fwd_check;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ init_entry_with_default_state ===> await_creation" 
(Sta.core_[ j ].LQ_.entries[ i ].state = init_entry_with_default_state)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].LQ_.entries[ i ].virt_addr := 0;
  next_state.core_[ j ].LQ_.entries[ i ].phys_addr := 0;
  next_state.core_[ j ].LQ_.entries[ i ].read_value := 0;
  next_state.core_[ j ].LQ_.entries[ i ].st_seq_num := 0;
  next_state.core_[ j ].LQ_.entries[ i ].seq_num := 0;
  next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].LQ_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].LQ_.entries[ i ].state := await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : LQ_idx_t do 
  rule "LQ LQ_stall_build_packet_send_mem_request ===> LQ_stall_build_packet_send_mem_request || build_packet_send_mem_request || build_packet_send_mem_request || await_fwd_check" 
(Sta.core_[ j ].LQ_.entries[ i ].state = LQ_stall_build_packet_send_mem_request)
==>
 
  var found_entry : boolean;
  var found_element : val_t;
  var found_idx : ROB_idx_t;
  var next_state : STATE;

begin
  -- put "RUN THIS RULE";
  -- put "Core: ";
  -- put j;
  -- put "\nEntry: ";
  -- put i;
  -- put "\n";
  next_state := Sta;
  found_entry := false;
  for ROB_iter : ROB_idx_t do
    if (Sta.core_[ j ].ROB_.entries[ ROB_iter ].instruction.seq_num != 0) then
      if (
      (ROB_iter < next_state.core_[ j ].ROB_.tail) &
      (ROB_iter >= next_state.core_[ j ].ROB_.head) &
      (next_state.core_[ j ].ROB_.entries[ ROB_iter ].instruction.seq_num < next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num) & (next_state.core_[ j ].ROB_.entries[ ROB_iter ].instruction.op = ld)) then
        if (found_entry = false) then
          found_element := (next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].ROB_.entries[ ROB_iter ].instruction.seq_num);
          found_idx := ROB_iter;
        else
          if ((next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].ROB_.entries[ ROB_iter ].instruction.seq_num) < found_element) then
            found_element := (next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num - next_state.core_[ j ].ROB_.entries[ ROB_iter ].instruction.seq_num);
            found_idx := ROB_iter;
          end;
        end;
        found_entry := true;
      end;
    end;
  endfor;
  if (found_entry = false) then
    -- put "Progress fwd, no match\n";
    next_state.core_[ j ].LQ_.entries[ i ].state := build_packet_send_mem_request;
  elsif (found_entry = true) then
    if !((next_state.core_[ j ].ROB_.entries[ found_idx ].is_executed = True)) then
      -- put "Stall, found match\n";
      next_state.core_[ j ].LQ_.entries[ i ].state := LQ_stall_build_packet_send_mem_request;
    else
      -- put "Progress fwd, found match\n";
      next_state.core_[ j ].LQ_.entries[ i ].state := build_packet_send_mem_request;
    end;
  end;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : SQ_idx_t do 
  rule "SQ sq_await_translation ===> sq_await_committed" 
(Sta.core_[ j ].SQ_.entries[ i ].state = sq_await_translation)
==>
 
  var loop_break : boolean;
  var difference : LQ_idx_t;
  var entry_idx : LQ_idx_t;
  var offset : LQ_idx_t;
  var curr_idx : LQ_idx_t;
  var violating_seq_num : ROB_count_t;
  var rob_idx : ROB_idx_t;
  var squash_diff : ROB_idx_t;
  var squash_offset : ROB_count_t;
  var rob : ROB;
  var squash_idx : ROB_idx_t;
  var squash_ld_id : LQ_idx_t;
  var squash_st_id : SQ_idx_t;
  var curr_rob_inst : INST;
  var rob_id : ROB_idx_t;
  var ROB_while_break : boolean;
  var ROB_found_entry : boolean;
  var ROB_entry_idx : ROB_idx_t;
  var ROB_difference : ROB_idx_t;
  var ROB_offset : ROB_idx_t;
  var ROB_curr_idx : ROB_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].SQ_.entries[ i ].phys_addr := next_state.core_[ j ].SQ_.entries[ i ].virt_addr;
  rob := Sta.core_[ j ].ROB_;
  loop_break := false;
  if (next_state.core_[ j ].LQ_.num_entries = 0) then
    loop_break := true;
  end;
  entry_idx := next_state.core_[ j ].LQ_.head;
  difference := ((((next_state.core_[ j ].LQ_.tail + LQ_NUM_ENTRIES_CONST) - 1) - entry_idx) % LQ_NUM_ENTRIES_CONST);
  offset := 0;
  while ((offset <= difference) & ((loop_break = false) & (difference >= 0))) do
    curr_idx := ((entry_idx + offset) % LQ_NUM_ENTRIES_CONST);
    if ((next_state.core_[ j ].LQ_.entries[ curr_idx ].phys_addr = next_state.core_[ j ].SQ_.entries[ i ].phys_addr) & (next_state.core_[ j ].LQ_.entries[ curr_idx ].instruction.seq_num > next_state.core_[ j ].SQ_.entries[ i ].instruction.seq_num)) then
      violating_seq_num := next_state.core_[ j ].LQ_.entries[ curr_idx ].instruction.seq_num;
      rob_idx := (search_rob_seq_num_idx(rob, violating_seq_num) % (CORE_INST_NUM + 1));
      squash_diff := (((rob.tail + ((CORE_INST_NUM + 1) - 1)) - rob_idx) % (CORE_INST_NUM + 1));
      squash_offset := 0;
      while ((squash_offset <= squash_diff) & (squash_diff >= 0)) do
        squash_idx := ((rob_idx + squash_offset) % (CORE_INST_NUM + 1));
        curr_rob_inst := rob.entries[ squash_idx ].instruction;
        if (curr_rob_inst.op = ld) then
          squash_ld_id := search_LQ_seq_num_idx(next_state.core_[ j ].LQ_, curr_rob_inst.seq_num);
          if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = await_committed) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              rob := next_state.core_[ j ].ROB_;
              rob_id := search_rob_seq_num_idx(rob, next_state.core_[ j ].LQ_.entries[ i ].instruction.seq_num);
              assert (rob.entries[ rob_id ].is_executed = true) "why isn't it true?";
              rob.entries[ rob_id ].is_executed := false;
              next_state.core_[ j ].ROB_ := rob;
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := await_fwd_check;
            end;
          elsif (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = write_result) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := await_fwd_check;
            end;
           elsif (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = await_mem_response) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := squashed_await_ld_mem_resp;
            end;
           elsif (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = build_packet_send_mem_request) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := await_fwd_check;
            end;
           elsif (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = await_sb_fwd_check_response) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := await_fwd_check;
            end;
           elsif (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state = LQ_stall_build_packet_send_mem_request) then
            if (next_state.core_[ j ].LQ_.entries[ squash_ld_id ].instruction.seq_num >= violating_seq_num) then
              next_state.core_[ j ].LQ_.entries[ squash_ld_id ].state := await_fwd_check;
            end;
          end;
        elsif (curr_rob_inst.op = st) then
          squash_st_id := search_SQ_seq_num_idx(next_state.core_[ j ].SQ_, curr_rob_inst.seq_num);
        else
          error "inst should be either ld or st";
        end;
        squash_offset := (squash_offset + 1);
      end;
      loop_break := true;
    end;
    if (offset != difference) then
      offset := (offset + 1);
    else
      loop_break := true;
    end;
  end;
  next_state.core_[ j ].ROB_ := rob;
  ROB_while_break := false;
  ROB_found_entry := false;
  if (next_state.core_[ j ].ROB_.num_entries = 0) then
    ROB_while_break := true;
  end;
  ROB_entry_idx := ((next_state.core_[ j ].ROB_.tail + (ROB_NUM_ENTRIES_CONST - 1)) % ROB_NUM_ENTRIES_CONST);
  ROB_difference := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - next_state.core_[ j ].ROB_.head)) % ROB_NUM_ENTRIES_CONST);
  ROB_offset := 0;
  while ((ROB_offset <= ROB_difference) & ((ROB_while_break = false) & (ROB_found_entry = false))) do
    ROB_curr_idx := ((ROB_entry_idx + (ROB_NUM_ENTRIES_CONST - ROB_offset)) % ROB_NUM_ENTRIES_CONST);
    if (next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].instruction.seq_num = next_state.core_[ j ].SQ_.entries[ i ].instruction.seq_num) then
      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].is_executed := true;
      next_state.core_[ j ].ROB_.entries[ ROB_curr_idx ].state := rob_commit_if_head;
      ROB_found_entry := true;
    end;
    if (ROB_offset != ROB_difference) then
      ROB_offset := (ROB_offset + 1);
    else
      ROB_while_break := true;
    end;
  end;
  if (ROB_found_entry = false) then
  end;
  next_state.core_[ j ].SQ_.entries[ i ].state := sq_await_committed;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : SQ_idx_t do 
  rule "SQ init_store_entry ===> sq_await_creation" 
(Sta.core_[ j ].SQ_.entries[ i ].state = init_store_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].SQ_.entries[ i ].virt_addr := 0;
  next_state.core_[ j ].SQ_.entries[ i ].phys_addr := 0;
  next_state.core_[ j ].SQ_.entries[ i ].write_value := 0;
  next_state.core_[ j ].SQ_.entries[ i ].ld_seq_num := 0;
  next_state.core_[ j ].SQ_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].SQ_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].SQ_.entries[ i ].state := sq_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : SB_idx_t do 
  rule "SB sb_await_send_mem_req ===> sb_await_mem_response" 
((Sta.core_[ j ].SB_.entries[ i ].state = sb_await_send_mem_req) & !(Sta.core_[ j ].mem_interface_.out_busy))
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].mem_interface_.out_msg.addr := next_state.core_[ j ].SB_.entries[ i ].phys_addr;
  next_state.core_[ j ].mem_interface_.out_msg.r_w := write;
  next_state.core_[ j ].mem_interface_.out_msg.value := next_state.core_[ j ].SB_.entries[ i ].write_value;
  next_state.core_[ j ].mem_interface_.out_msg.valid := true;
  next_state.core_[ j ].mem_interface_.out_msg.dest := mem;
  next_state.core_[ j ].mem_interface_.out_msg.dest_id := j;
  next_state.core_[ j ].mem_interface_.out_msg.seq_num := next_state.core_[ j ].SB_.entries[ i ].instruction.seq_num;
  next_state.core_[ j ].mem_interface_.out_busy := true;
  next_state.core_[ j ].SB_.entries[ i ].state := sb_await_mem_response;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : SB_idx_t do 
  rule "SB init_SB_entry ===> sb_await_creation" 
(Sta.core_[ j ].SB_.entries[ i ].state = init_SB_entry)
==>
 
  var next_state : STATE;

begin
  next_state := Sta;
  next_state.core_[ j ].SB_.entries[ i ].virt_addr := 0;
  next_state.core_[ j ].SB_.entries[ i ].phys_addr := 0;
  next_state.core_[ j ].SB_.entries[ i ].write_value := 0;
  next_state.core_[ j ].SB_.entries[ i ].instruction.seq_num := 0;
  next_state.core_[ j ].SB_.entries[ i ].instruction.op := inval;
  next_state.core_[ j ].SB_.entries[ i ].state := sb_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : ROB_idx_t do 
  rule "ROB rob_commit_if_head ===> rob_commit_if_head || rob_await_creation || rob_commit_if_head || rob_await_creation || rob_commit_if_head" 
((Sta.core_[ j ].ROB_.entries[ i ].state = rob_commit_if_head) & (Sta.core_[ j ].SB_.num_entries < SB_NUM_ENTRIES_CONST))
==>
 
  var inst_from_sq : INST;
  var write_value_from_sq : val_t;
  var phys_addr_from_sq : addr_idx_t;
  var LQ_while_break : boolean;
  var LQ_found_entry : boolean;
  var LQ_entry_idx : LQ_idx_t;
  var LQ_difference : LQ_idx_t;
  var LQ_offset : LQ_idx_t;
  var LQ_curr_idx : LQ_idx_t;
  var SB_loop_break : boolean;
  var SB_entry_idx : SB_idx_t;
  var SB_found_entry : boolean;
  var SB_difference : SB_idx_t;
  var SB_offset : SB_idx_t;
  var SB_curr_idx : SB_idx_t;
  var SQ_while_break : boolean;
  var SQ_found_entry : boolean;
  var SQ_entry_idx : SQ_idx_t;
  var SQ_difference : SQ_idx_t;
  var SQ_offset : SQ_idx_t;
  var SQ_curr_idx : SQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].ROB_.entries[ i ].is_executed = false) then
    next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
  else
    if (next_state.core_[ j ].ROB_.head = i) then
      if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = ld) then
        LQ_while_break := false;
        LQ_found_entry := false;
        if (next_state.core_[ j ].LQ_.num_entries = 0) then
          LQ_while_break := true;
        end;
        LQ_entry_idx := ((next_state.core_[ j ].LQ_.tail + (LQ_NUM_ENTRIES_CONST - 1)) % LQ_NUM_ENTRIES_CONST);
        LQ_difference := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - next_state.core_[ j ].LQ_.head)) % LQ_NUM_ENTRIES_CONST);
        LQ_offset := 0;
        while ((LQ_offset <= LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
          LQ_curr_idx := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - LQ_offset)) % LQ_NUM_ENTRIES_CONST);
          if (next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].virt_addr := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].phys_addr := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].read_value := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].st_seq_num := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].seq_num := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].instruction.op := inval;
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.head ].state := await_creation;
            next_state.core_[ j ].LQ_.head := ((Sta.core_[ j ].LQ_.head + 1) % LQ_NUM_ENTRIES_CONST);
            next_state.core_[ j ].LQ_.num_entries := (next_state.core_[ j ].LQ_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].seq_num := 0;
            next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].is_executed := false;
            next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].state := rob_await_creation;
            next_state.core_[ j ].ROB_.head := ((Sta.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
            next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
            LQ_found_entry := true;
          end;
          if (LQ_offset != LQ_difference) then
            LQ_offset := (LQ_offset + 1);
          else
            LQ_while_break := true;
          end;
        end;
        if (LQ_found_entry = false) then
        end;
      else
        if (next_state.core_[ j ].ROB_.entries[ i ].instruction.op = st) then
          SQ_while_break := false;
          SQ_found_entry := false;
          if (next_state.core_[ j ].SQ_.num_entries = 0) then
            SQ_while_break := true;
          end;
          SQ_entry_idx := ((next_state.core_[ j ].SQ_.tail + (SQ_NUM_ENTRIES_CONST - 1)) % SQ_NUM_ENTRIES_CONST);
          SQ_difference := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - next_state.core_[ j ].SQ_.head)) % SQ_NUM_ENTRIES_CONST);
          SQ_offset := 0;
          while ((SQ_offset <= SQ_difference) & ((SQ_while_break = false) & (SQ_found_entry = false))) do
            SQ_curr_idx := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - SQ_offset)) % SQ_NUM_ENTRIES_CONST);
            if (next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].ROB_.entries[ i ].instruction.seq_num) then
              if (Sta.core_[ j ].SB_.num_entries = SB_NUM_ENTRIES_CONST) then
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_commit_if_head;
              else
                inst_from_sq := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction;
                write_value_from_sq := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].write_value;
                phys_addr_from_sq := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].phys_addr;
                SB_loop_break := false;
                if (next_state.core_[ j ].SB_.num_entries = SB_NUM_ENTRIES_CONST) then
                  SB_loop_break := true;
                end;
                SB_entry_idx := 0;
                SB_found_entry := false;
                SB_difference := (SB_NUM_ENTRIES_CONST - 1);
                SB_offset := 0;
                while ((SB_offset <= SB_difference) & ((SB_loop_break = false) & ((SB_found_entry = false) & (SB_difference >= 0)))) do
                  SB_curr_idx := ((SB_entry_idx + SB_offset) % SB_NUM_ENTRIES_CONST);
                  if (next_state.core_[ j ].SB_.entries[ SB_curr_idx ].state = sb_await_creation) then
                    next_state.core_[ j ].SB_.entries[ SB_curr_idx ].phys_addr := phys_addr_from_sq;
                    next_state.core_[ j ].SB_.entries[ SB_curr_idx ].write_value := write_value_from_sq;
                    next_state.core_[ j ].SB_.entries[ SB_curr_idx ].instruction.seq_num := inst_from_sq.seq_num;
                    next_state.core_[ j ].SB_.entries[ SB_curr_idx ].instruction := inst_from_sq;
                    next_state.core_[ j ].SB_.entries[ SB_curr_idx ].state := sb_await_send_mem_req;
                    SB_found_entry := true;
                  end;
                  if (SB_offset != SB_difference) then
                    SB_offset := (SB_offset + 1);
                  else
                    SB_loop_break := true;
                  end;
                end;
                next_state.core_[ j ].SB_.num_entries := (next_state.core_[ j ].SB_.num_entries + 1);
                if (SB_found_entry = false) then
                  error "Couldn't find an empty entry to insert into";
                end;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].virt_addr := 0;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].phys_addr := 0;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].write_value := 0;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].ld_seq_num := 0;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].instruction.seq_num := 0;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].instruction.op := inval;
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.head ].state := sq_await_creation;
                next_state.core_[ j ].SQ_.head := ((Sta.core_[ j ].SQ_.head + 1) % SQ_NUM_ENTRIES_CONST);
                next_state.core_[ j ].SQ_.num_entries := (next_state.core_[ j ].SQ_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].seq_num := 0;
                next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].is_executed := false;
                next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.head ].state := rob_await_creation;
                next_state.core_[ j ].ROB_.head := ((Sta.core_[ j ].ROB_.head + 1) % ROB_NUM_ENTRIES_CONST);
                next_state.core_[ j ].ROB_.num_entries := (next_state.core_[ j ].ROB_.num_entries - 1);
                next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
              end;
              SQ_found_entry := true;
            end;
            if (SQ_offset != SQ_difference) then
              SQ_offset := (SQ_offset + 1);
            else
              SQ_while_break := true;
            end;
          end;
          if (SQ_found_entry = false) then
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
  next_state.core_[ j ].ROB_.entries[ i ].state := rob_await_creation;
  Sta := next_state;

end;
end;


ruleset j : cores_t; i : IQ_idx_t do 
  rule "IQ iq_schedule_inst ===> iq_await_creation || iq_await_creation" 
(Sta.core_[ j ].IQ_.entries[ i ].state = iq_schedule_inst)
==>
 
  var LQ_while_break : boolean;
  var LQ_found_entry : boolean;
  var LQ_entry_idx : LQ_idx_t;
  var LQ_difference : LQ_idx_t;
  var LQ_offset : LQ_idx_t;
  var LQ_curr_idx : LQ_idx_t;
  var SQ_while_break : boolean;
  var SQ_found_entry : boolean;
  var SQ_entry_idx : SQ_idx_t;
  var SQ_difference : SQ_idx_t;
  var SQ_offset : SQ_idx_t;
  var SQ_curr_idx : SQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = ld) then
    LQ_while_break := false;
    LQ_found_entry := false;
    if (next_state.core_[ j ].LQ_.num_entries = 0) then
      LQ_while_break := true;
    end;
    LQ_entry_idx := ((next_state.core_[ j ].LQ_.tail + (LQ_NUM_ENTRIES_CONST - 1)) % LQ_NUM_ENTRIES_CONST);
    LQ_difference := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - next_state.core_[ j ].LQ_.head)) % LQ_NUM_ENTRIES_CONST);
    LQ_offset := 0;
    while ((LQ_offset <= LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
      LQ_curr_idx := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - LQ_offset)) % LQ_NUM_ENTRIES_CONST);
      if (next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
        next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].virt_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
        next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].state := await_translation;
        next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
        next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
        LQ_found_entry := true;
      end;
      if (LQ_offset != LQ_difference) then
        LQ_offset := (LQ_offset + 1);
      else
        LQ_while_break := true;
      end;
    end;
    if (LQ_found_entry = false) then
    end;
  else
    if (next_state.core_[ j ].IQ_.entries[ i ].instruction.op = st) then
      SQ_while_break := false;
      SQ_found_entry := false;
      if (next_state.core_[ j ].SQ_.num_entries = 0) then
        SQ_while_break := true;
      end;
      SQ_entry_idx := ((next_state.core_[ j ].SQ_.tail + (SQ_NUM_ENTRIES_CONST - 1)) % SQ_NUM_ENTRIES_CONST);
      SQ_difference := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - next_state.core_[ j ].SQ_.head)) % SQ_NUM_ENTRIES_CONST);
      SQ_offset := 0;
      while ((SQ_offset <= SQ_difference) & ((SQ_while_break = false) & (SQ_found_entry = false))) do
        SQ_curr_idx := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - SQ_offset)) % SQ_NUM_ENTRIES_CONST);
        if (next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction.seq_num = next_state.core_[ j ].IQ_.entries[ i ].instruction.seq_num) then
          next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].write_value := next_state.core_[ j ].IQ_.entries[ i ].instruction.write_value;
          next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].virt_addr := next_state.core_[ j ].IQ_.entries[ i ].instruction.imm;
          next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].state := sq_await_translation;
          next_state.core_[ j ].IQ_.num_entries := (next_state.core_[ j ].IQ_.num_entries - 1);
          next_state.core_[ j ].IQ_.entries[ i ].state := iq_await_creation;
          SQ_found_entry := true;
        end;
        if (SQ_offset != SQ_difference) then
          SQ_offset := (SQ_offset + 1);
        else
          SQ_while_break := true;
        end;
      end;
      if (SQ_found_entry = false) then
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


ruleset j : cores_t; i : RENAME_idx_t do 
  rule "RENAME issue_if_head ===> issue_if_head || issue_if_head || issue_if_head || issue_if_head" 
(((Sta.core_[ j ].RENAME_.entries[ i ].state = issue_if_head) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST)) & (Sta.core_[ j ].IQ_.num_entries < IQ_NUM_ENTRIES_CONST))
==>
 
  var IQ_loop_break : boolean;
  var IQ_entry_idx : IQ_idx_t;
  var IQ_found_entry : boolean;
  var IQ_difference : IQ_idx_t;
  var IQ_offset : IQ_idx_t;
  var IQ_curr_idx : IQ_idx_t;
  var SQ_while_break : boolean;
  var SQ_found_entry : boolean;
  var SQ_entry_idx : SQ_idx_t;
  var SQ_difference : SQ_idx_t;
  var SQ_offset : SQ_idx_t;
  var SQ_curr_idx : SQ_idx_t;
  var LQ_while_break : boolean;
  var LQ_found_entry : boolean;
  var LQ_entry_idx : LQ_idx_t;
  var LQ_difference : LQ_idx_t;
  var LQ_offset : LQ_idx_t;
  var LQ_curr_idx : LQ_idx_t;
  var next_state : STATE;

begin
  next_state := Sta;
  if (next_state.core_[ j ].RENAME_.head = i) then
    if (!((Sta.core_[ j ].IQ_.num_entries = IQ_NUM_ENTRIES_CONST)) & !((Sta.core_[ j ].ROB_.num_entries = ROB_NUM_ENTRIES_CONST))) then
      if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = ld) then
        if !((Sta.core_[ j ].LQ_.num_entries = LQ_NUM_ENTRIES_CONST)) then
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
          next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.tail ].state := rob_await_executed;
          next_state.core_[ j ].ROB_.tail := ((Sta.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
          next_state.core_[ j ].ROB_.num_entries := (Sta.core_[ j ].ROB_.num_entries + 1);
          SQ_while_break := false;
          SQ_found_entry := false;
          if (next_state.core_[ j ].SQ_.num_entries = 0) then
            SQ_while_break := true;
          end;
          SQ_entry_idx := ((next_state.core_[ j ].SQ_.tail + (SQ_NUM_ENTRIES_CONST - 1)) % SQ_NUM_ENTRIES_CONST);
          SQ_difference := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - next_state.core_[ j ].SQ_.head)) % SQ_NUM_ENTRIES_CONST);
          SQ_offset := 0;
          while ((SQ_offset <= SQ_difference) & ((SQ_while_break = false) & (SQ_found_entry = false))) do
            SQ_curr_idx := ((SQ_entry_idx + (SQ_NUM_ENTRIES_CONST - SQ_offset)) % SQ_NUM_ENTRIES_CONST);
            if (next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.tail ].instruction.seq_num) then
              next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.tail ].st_seq_num := next_state.core_[ j ].SQ_.entries[ SQ_curr_idx ].instruction.seq_num;
              SQ_found_entry := true;
            end;
            if (SQ_offset != SQ_difference) then
              SQ_offset := (SQ_offset + 1);
            else
              SQ_while_break := true;
            end;
          end;
          if (SQ_found_entry = false) then
            next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.tail ].st_seq_num := 0;
          end;
          next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
          next_state.core_[ j ].LQ_.entries[ Sta.core_[ j ].LQ_.tail ].state := await_scheduled;
          next_state.core_[ j ].LQ_.tail := ((Sta.core_[ j ].LQ_.tail + 1) % LQ_NUM_ENTRIES_CONST);
          next_state.core_[ j ].LQ_.num_entries := (Sta.core_[ j ].LQ_.num_entries + 1);
          next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.op := inval;
          next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
          next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].state := issue_if_head;
          next_state.core_[ j ].RENAME_.head := ((Sta.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
          next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
          next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
        else
          next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
        end;
      else
        if (next_state.core_[ j ].RENAME_.entries[ i ].instruction.op = st) then
          if !((Sta.core_[ j ].SQ_.num_entries = SQ_NUM_ENTRIES_CONST)) then
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
            next_state.core_[ j ].ROB_.entries[ Sta.core_[ j ].ROB_.tail ].state := rob_await_executed;
            next_state.core_[ j ].ROB_.tail := ((Sta.core_[ j ].ROB_.tail + 1) % ROB_NUM_ENTRIES_CONST);
            next_state.core_[ j ].ROB_.num_entries := (Sta.core_[ j ].ROB_.num_entries + 1);
            LQ_while_break := false;
            LQ_found_entry := false;
            if (next_state.core_[ j ].LQ_.num_entries = 0) then
              LQ_while_break := true;
            end;
            LQ_entry_idx := ((next_state.core_[ j ].LQ_.tail + (LQ_NUM_ENTRIES_CONST - 1)) % LQ_NUM_ENTRIES_CONST);
            LQ_difference := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - next_state.core_[ j ].LQ_.head)) % LQ_NUM_ENTRIES_CONST);
            LQ_offset := 0;
            while ((LQ_offset <= LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
              LQ_curr_idx := ((LQ_entry_idx + (LQ_NUM_ENTRIES_CONST - LQ_offset)) % LQ_NUM_ENTRIES_CONST);
              if (next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].instruction.seq_num < next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.tail ].instruction.seq_num) then
                next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.tail ].ld_seq_num := next_state.core_[ j ].LQ_.entries[ LQ_curr_idx ].instruction.seq_num;
                LQ_found_entry := true;
              end;
              if (LQ_offset != LQ_difference) then
                LQ_offset := (LQ_offset + 1);
              else
                LQ_while_break := true;
              end;
            end;
            if (LQ_found_entry = false) then
              next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.tail ].ld_seq_num := 0;
            end;
            next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.tail ].instruction := next_state.core_[ j ].RENAME_.entries[ i ].instruction;
            next_state.core_[ j ].SQ_.entries[ Sta.core_[ j ].SQ_.tail ].state := sq_await_scheduled;
            next_state.core_[ j ].SQ_.tail := ((Sta.core_[ j ].SQ_.tail + 1) % SQ_NUM_ENTRIES_CONST);
            next_state.core_[ j ].SQ_.num_entries := (Sta.core_[ j ].SQ_.num_entries + 1);
            next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.op := inval;
            next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].instruction.seq_num := 0;
            next_state.core_[ j ].RENAME_.entries[ Sta.core_[ j ].RENAME_.head ].state := issue_if_head;
            next_state.core_[ j ].RENAME_.head := ((Sta.core_[ j ].RENAME_.head + 1) % RENAME_NUM_ENTRIES_CONST);
            next_state.core_[ j ].RENAME_.num_entries := (next_state.core_[ j ].RENAME_.num_entries - 1);
            next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
          else
            next_state.core_[ j ].RENAME_.entries[ i ].state := issue_if_head;
          end;
        end;
      end;
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
((((Sta.core_[ 0 ].RENAME_.num_entries = 0) &
(Sta.core_[ 0 ].ROB_.num_entries = 0)) &
(Sta.core_[ 0 ].SB_.num_entries = 0) &
((Sta.core_[ 1 ].RENAME_.num_entries = 0) &
(Sta.core_[ 1 ].ROB_.num_entries = 0)&
(Sta.core_[ 1 ].SB_.num_entries = 0)
)) -> !(((Sta.core_[ 0 ].rf_.rf[ 0 ] = 1) &
(Sta.core_[ 0 ].rf_.rf[ 1 ] = 0))));

