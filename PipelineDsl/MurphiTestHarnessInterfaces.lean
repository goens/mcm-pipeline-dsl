
import PipelineDsl.AnalysisHelpers

import Murphi

import PipelineDsl.AST

import PipelineDsl.LitmusTests

import PipelineDsl.Translation

import PipelineDsl.EmitMurphi

set_option maxHeartbeats 500000
open Pipeline
open Murϕ

/- ----------- BEGIN Single LSQ Await Mem Resp Functions ---------------- -/

  /- NOTE: Single LSQ In-Order & Invalidaiton Tracking Await mem response. -/
  /- NOTE: Same as with In-Order only. -/
  /-
  def lsq_inval_await_load_mem_resp_function :=
    [murϕ_proc_decl|
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
      assert (curr_entry.state = lsq_await_load_mem_response) "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = lsq_await_load_mem_response) then
        curr_entry.state := lsq_ld_write_result;
      -- elsif (curr_entry.state = lsq_squashed_await_ld_mem_resp) then
      --   curr_entry.state := lsq_ld_st_fwd_branch;
      end;
      curr_entry.read_value := msg.value;
      lq_new.entries[ LSQ_curr_idx ] := curr_entry;
      return lq_new;
    end;

    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  return lq_new;
end
]
-/

  /- NOTE: Single LSQ In-Order & Load-Replay. Await mem response. -/
  def lsq_replay_await_load_mem_resp_function :=
    [murϕ_proc_decl|
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
        | curr_entry.state = replay_generated_lsq_await_load_mem_response
        )
        "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = lsq_await_load_mem_response) then
        curr_entry.state := lsq_ld_write_result;
        curr_entry.read_value := msg.value;
      elsif (curr_entry.state = replay_generated_lsq_await_load_mem_response) then
        curr_entry.state := replay_compare_and_check_state;
        curr_entry.replay_value := msg.value;
      end;
      lq_new.entries[ LSQ_curr_idx ] := curr_entry;
      return lq_new;
    end;

    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  return lq_new;
end
]

  /- NOTE: Single LSQ In-Order. Await mem response. -/
  def lsq_await_load_mem_resp_function :=
    [murϕ_proc_decl|
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
      assert (curr_entry.state = lsq_await_load_mem_response) "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = lsq_await_load_mem_response) then
        curr_entry.state := lsq_ld_write_result;
      -- elsif (curr_entry.state = lsq_squashed_await_ld_mem_resp) then
      --   curr_entry.state := lsq_ld_st_fwd_branch;
      end;
      curr_entry.read_value := msg.value;
      lq_new.entries[ LSQ_curr_idx ] := curr_entry;
      return lq_new;
    end;

    if (LSQ_offset < LSQ_difference) then
      LSQ_offset := (LSQ_offset + 1);
    end;
  end;
  return lq_new;
end
  ]

  def lsq_store_await_mem_resp_function :=
    [murϕ_proc_decl|
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
end
  ]

/- ----------- END Single LSQ Await Mem Resp Functions ---------------- -/

/- ----------- BEGIN HP LSQ Await Mem Resp Functions ---------------- -/

  def hp_load_replay_await_load_mem_resp_function :=
    [murϕ_proc_decl|
function associative_assign_lq (
  lq : LQ;
  msg : MEM_REQ
) : LQ;
  var lq_new : LQ;
  var lq_iter : LQ_idx_t;
  var lq_count : LQ_count_t;
  var curr_entry : LQ_entry_values;
  -- var curr_entry_id : LQ_idx_t;
  var seq_num : inst_count_t;
  var LQ_while_break : boolean;
  var LQ_found_entry : boolean;
  var LQ_entry_idx : LQ_idx_t;
  var LQ_difference : LQ_count_t;
  var LQ_offset : LQ_count_t;
  var LQ_curr_idx : LQ_idx_t;
  var LQ_squash_remove_count : LQ_count_t;
begin
  lq_new := lq;
  lq_iter := lq.head;
  lq_count := lq.num_entries;
  seq_num := msg.seq_num;
  LQ_while_break := false;
  LQ_found_entry := false;
  if (lq.num_entries = 0) then
    LQ_while_break := true;
  end;
  LQ_entry_idx := lq.head;
  LQ_difference := lq.num_entries;
  LQ_offset := 0;
  LQ_squash_remove_count := 0;
  while ((LQ_offset < LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
    LQ_curr_idx := ((LQ_entry_idx + LQ_offset) % LQ_NUM_ENTRIES_CONST);
    curr_entry := lq_new.entries[ LQ_curr_idx ];
    if (curr_entry.instruction.seq_num = seq_num) then
      -- assert ((curr_entry.state = lsq_squashed_await_ld_mem_resp) | (curr_entry.state = lsq_await_load_mem_response)) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";
      assert (curr_entry.state = await_mem_response
      | curr_entry.state = replay_generated_await_mem_response
      ) "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
      if (curr_entry.state = await_mem_response) then
        curr_entry.state := write_result;
        curr_entry.read_value := msg.value;
      elsif (curr_entry.state = replay_generated_await_mem_response) then
        curr_entry.state := replay_compare_and_check_state;
        curr_entry.replay_value := msg.value;
      end;
      lq_new.entries[ LQ_curr_idx ] := curr_entry;
      return lq_new;
    end;
    if (LQ_offset < LQ_difference) then
      LQ_offset := (LQ_offset + 1);
    end;
  end;
  return lq_new;
end

  ]

  /- Functions for awaiting a response from the memory -/
  /- Use for HP In-Order & IO + Inval Tracking -/
  def hp_in_order_await_load_mem_resp_function :=
    [murϕ_proc_decl|
    function associative_assign_lq (
      lq : LQ;
      msg : MEM_REQ
    ) : LQ;
      var lq_new : LQ;
      var lq_iter : LQ_idx_t;
      var lq_count : LQ_count_t;
      var curr_entry : LQ_entry_values;
      var seq_num : inst_count_t;
      var LQ_while_break : boolean;
      var LQ_found_entry : boolean;
      var LQ_entry_idx : LQ_idx_t;
      var LQ_difference : LQ_count_t;
      var LQ_offset : LQ_count_t;
      var LQ_curr_idx : LQ_idx_t;
      var LQ_squash_remove_count : LQ_count_t;
    begin
      lq_new := lq;
      lq_iter := lq.head;
      lq_count := lq.num_entries;
      seq_num := msg.seq_num;
      LQ_while_break := false;
      LQ_found_entry := false;
      if (lq.num_entries = 0) then
        LQ_while_break := true;
      end;
      LQ_entry_idx := lq.head;
      LQ_difference := lq.num_entries;
      LQ_offset := 0;
      LQ_squash_remove_count := 0;
      while ((LQ_offset < LQ_difference) & ((LQ_while_break = false) & (LQ_found_entry = false))) do
        LQ_curr_idx := ((LQ_entry_idx + LQ_offset) % LQ_NUM_ENTRIES_CONST);
        curr_entry := lq_new.entries[ LQ_curr_idx ];
        if (curr_entry.instruction.seq_num = seq_num) then
          -- assert ((curr_entry.state = lsq_squashed_await_ld_mem_resp) | (curr_entry.state = lsq_await_load_mem_response)) "ASSN LQ: Should be in await mem resp? or squashed and await collect the mem resp?";
          assert (curr_entry.state = await_mem_response
          --| curr_entry.state = replay_generated_await_mem_response
          ) "ASSN LQ: Should be in await mem resp? and await collect the mem resp?";
          if (curr_entry.state = await_mem_response) then
            curr_entry.state := write_result;
            curr_entry.read_value := msg.value;
          end;
          lq_new.entries[ LQ_curr_idx ] := curr_entry;
          return lq_new;
        end;
        if (LQ_offset < LQ_difference) then
          LQ_offset := (LQ_offset + 1);
        end;
      end;
      return lq_new;
    end]

    def hp_await_store_mem_response_fn :=
    [murϕ_proc_decl|
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
    end
    ]
/- ----------- END HP LSQ Await Mem Resp Functions ---------------- -/

  /- NOTE: Structure Initialization... -/
  def listen_handler_init := [murϕ_statement|
    alias listen_handler : init_state.core_[core].invalidation_listener_ do
      listen_handler.invalidation_seq_num := 0;
      listen_handler.invalidation_address := 0;
      listen_handler.state := await_invalidation;
    end]

  def lat_alias_init := [murϕ_statement|
    alias lat : init_state.core_[core].load_address_table_ do
      for i : load_address_table_idx_t do
        lat.entries[i].lat_seq_num := 0;
        lat.entries[i].lat_address := 0;
        lat.entries[i].state := load_address_table_await_insert_remove;
        lat.entries[i].valid := false;
      endfor;
      lat.num_entries := 0;
    end]


/- ================= ================================= ===================== -/
/- ================= BEGIN Core Gets Mem Message Rules ===================== -/
/- ================= ================================= ===================== -/

  /- NOTE: LSQ + Invalidation Tracking -/
  def lsq_core_gets_inv_msg_rule :=
    [murϕ_rule|
ruleset j : cores_t do
  rule "core_sends_in_msg_ack_to_structures"
(Sta.core_[ j ].mem_interface_.in_busy = true)
==>

  var next_state : STATE;
  var lq : LSQ;
  var sb : ROB;
  var mem_interface : MEM_INTERFACE;

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
      -- let invalidation handler handle sending ack...
      next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
      next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
      next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;


      assert mem_interface.in_busy = true "This should still be busy...";

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
end
]

  /- NOTE: LSQ Core gets mem response. Same for IO and IO+LR, different for IT -/
  def lsq_core_gets_msg_rule :=
    [murϕ_rule|
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
end
]

/- NOTE: Core receive Message, for NoSQ-- / Load Buffer. -/
  /- Load Buffer + Invalidation-Tracking & In-Order Transform -/
  def load_buffer_core_gets_msg_including_inval_rule :=
    [murϕ_rule|
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
  --put next_state.core_[ j ].memory_unit_sender_.state;
  -- lq := Sta.core_[ j ].LSQ_;
  rob := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  -- Do this based on the stage state
  --put "CORE-ACK: About to check if 1st mem unit or 2nd mem unit is the recipient..\n";
  if (mem_interface.in_msg.r_w = read)
    then
    --put "CORE-ACK: 1st Mem unit is the recipient, check if load or store..\n";
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
        --put "CORE-ACK: Rob id: (";
        --put rob_id;
        --put ")\n";
        --put "CORE-ACK: mem_seq_num in: (";
        --put mem_interface.in_msg.seq_num;
        --put ")\n";
        --put "CORE-ACK: second_memory_stage seq_num: (";
        --put Sta.core_[j].second_memory_stage_.instruction.seq_num;
        --put ")\n";
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
  --put "CORE-ACK: Reached end of if stmt..\n";
  -- next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := rob;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end
    ]

  /- Load Buffer + Load-Replay & In-Order Transform -/
  def load_buffer_core_gets_msg_including_load_replay_rule :=
    [murϕ_rule|
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
  var found_msg_in_ic : boolean;

begin
  next_state := Sta;
  --put next_state.core_[ j ].memory_unit_sender_.state;
  -- lq := Sta.core_[ j ].LSQ_;
  rob := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  -- Do this based on the stage state
  --put "CORE-ACK: About to check if 1st mem unit or 2nd mem unit is the recipient..\n";
  if (mem_interface.in_msg.r_w = read)
    then
    --put "CORE-ACK: 1st Mem unit is the recipient, check if load or store..\n";
    -- if (mem_interface.in_msg.r_w = read) then
    if
    (Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver
    | Sta.core_[j].memory_unit_sender_.state = replay_generated_memory_unit_receiver
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
      else
        assert rob.entries[rob_id].state = ROB_await_replay_complete;
      endif;
      -- assert();
      -- rob.entries[rob_id].state := rob_commit_if_head;
      -- rob.entries[rob_id].is_executed := true;
      if (Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver) then
        next_state.core_[j].memory_unit_sender_.read_value := mem_interface.in_msg.value;
        next_state.core_[j].memory_unit_sender_.state := memory_unit_load_result_write;
      elsif (Sta.core_[j].memory_unit_sender_.state = replay_generated_memory_unit_receiver) then
        next_state.core_[j].memory_unit_sender_.replay_value := mem_interface.in_msg.value;
        next_state.core_[j].memory_unit_sender_.state := replay_compare_and_check_state;
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
        --put "CORE-ACK: Rob id: (";
        --put rob_id;
        --put ")\n";
        --put "CORE-ACK: mem_seq_num in: (";
        --put mem_interface.in_msg.seq_num;
        --put ")\n";
        --put "CORE-ACK: second_memory_stage seq_num: (";
        --put Sta.core_[j].second_memory_stage_.instruction.seq_num;
        --put ")\n";
      next_state.core_[j].second_memory_stage_.state := second_mem_unit_get_inputs;
      -- Directly handle the invalidation case here
    elsif (mem_interface.in_msg.store_state = await_handling) then
      -- Overview: Send back an ack for the invalidation sent..
      -- match this message with it's copy in the IC, set it's ack bool to true.

      -- next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;
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
    endif;
  else
    error "CORE-ACK: Got a message but don't know what to do with it!\n";
  endif;
  --put "CORE-ACK: Reached end of if stmt..\n";
  -- next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := rob;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end
    ]

  /- Load Buffer + In-Order Transform -/
  def load_buffer_core_gets_msg_rule :=
    [murϕ_rule|
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
  var found_msg_in_ic : boolean;

begin
  next_state := Sta;
  -- put next_state.core_[ j ].memory_unit_sender_.state;
  -- lq := Sta.core_[ j ].LSQ_;
  rob := Sta.core_[ j ].ROB_;
  mem_interface := Sta.core_[ j ].mem_interface_;
  -- Do this based on the stage state
  -- put "CORE-ACK: About to check if 1st mem unit or 2nd mem unit is the recipient..\n";
  if (mem_interface.in_msg.r_w = read)
    then
    -- put "CORE-ACK: 1st Mem unit is the recipient, check if load or store..\n";
    -- if (mem_interface.in_msg.r_w = read) then
    if
    (Sta.core_[j].memory_unit_sender_.state = memory_unit_receiver
    --| Sta.core_[j].memory_unit_sender_.state = replay_generated_memory_unit_receiver
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
        -- put "CORE-ACK: Rob id: (";
        -- put rob_id;
        -- put ")\n";
        -- put "CORE-ACK: mem_seq_num in: (";
        -- put mem_interface.in_msg.seq_num;
        -- put ")\n";
        -- put "CORE-ACK: second_memory_stage seq_num: (";
        -- put Sta.core_[j].second_memory_stage_.instruction.seq_num;
        -- put ")\n";
      next_state.core_[j].second_memory_stage_.state := second_mem_unit_get_inputs;
      -- Directly handle the invalidation case here
    elsif (mem_interface.in_msg.store_state = await_handling) then
      -- Overview: Send back an ack for the invalidation sent..
      -- match this message with it's copy in the IC, set it's ack bool to true.

      -- next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
      -- next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;
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
    endif;
  else
    error "CORE-ACK: Got a message but don't know what to do with it!\n";
  endif;
  -- put "CORE-ACK: Reached end of if stmt..\n";
  -- next_state.core_[ j ].LSQ_ := lq;
  next_state.core_[ j ].ROB_ := rob;
  next_state.core_[ j ].mem_interface_ := mem_interface;
  Sta := next_state;

end;
end
]

/- NOTE: Core receive Message, for HP LSQ. -/
  /- NOTE: Use this for inval tests.. -/
  def core_gets_msg_including_inval_rule :=
    [murϕ_rule|
    ruleset j : cores_t do
      rule "core_sends_in_msg_ack_to_structures"
    (Sta.core_[ j ].mem_interface_.in_busy = true)
    ==>

      var next_state : STATE;
      var lq : LQ;
      var sb : SB;
      var mem_interface : MEM_INTERFACE;
      var found_msg_in_ic : boolean;

    begin
      next_state := Sta;
      lq := Sta.core_[ j ].LQ_;
      sb := Sta.core_[ j ].SB_;
      mem_interface := Sta.core_[ j ].mem_interface_;
      if (mem_interface.in_msg.r_w = read) then
        lq := associative_assign_lq(lq, mem_interface.in_msg);
        mem_interface.in_busy := false;
      elsif (mem_interface.in_msg.r_w = write) then
        if (mem_interface.in_msg.store_state = await_handling) then
          -- let invalidation handler handle sending ack...
          next_state.core_[ j ].invalidation_listener_.state := squash_speculative_loads;
          next_state.core_[ j ].invalidation_listener_.invalidation_seq_num := mem_interface.in_msg.seq_num;
          next_state.core_[ j ].invalidation_listener_.invalidation_address := mem_interface.in_msg.addr;


          assert mem_interface.in_busy = true "This should still be busy...";

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
        elsif (mem_interface.in_msg.store_state = store_send_completion) then
          sb := associative_ack_sb(sb, mem_interface.in_msg);
          mem_interface.in_busy := false;
        end;
      end;
      -- mem_interface.in_busy := false;
      next_state.core_[ j ].LQ_ := lq;
      next_state.core_[ j ].SB_ := sb;
      next_state.core_[ j ].mem_interface_ := mem_interface;
      Sta := next_state;

    end;
    end
    ]

  /- NOTE: Use this for HP?-/
  /- In-Order, and Load Replay. Not Inval Tracking -/
  def core_gets_msg_not_inval_rule :=
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
    ]

/- ================= =============================== ===================== -/
/- ================= END Core Gets Mem Message Rules ===================== -/
/- ================= =============================== ===================== -/
