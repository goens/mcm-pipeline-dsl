import PipelineDsl


def protogen_like_lsq_hennesy_and_patterson_load_load :=
[pipeline|

queue load_queue {
  int num_entries = 32
  searchability = [head, CAM, tail,
                  elements_older_than_input_arg]

  self.searchability.CAM.hash_based_on(load_sequence_num)



  entry_execution_refinements = load.global_perform_mem_read(
                              global_perform = in_order,
                              speculation = no_speculation)
}

controller_entry load_queue {
  load_state ld_state;

  virtual_address virt_addr;
  physical_address phys_addr;

  u32 read_value;
  packet load_packet;

  latest_store_sequence_num st_seq_num;
  seq_num load_sequence_num;
}

controller_entry store_queue {
  store_state st_state;

  virtual_address virt_addr;
  physical_address phys_addr;
  u32 write_value;
}

controller_control_flow load_queue {

  init_entry_with_default_state;

}

state_machine_sub_func init_entry_with_default_state {
  self.phys_addr = NULL;

  await_entry_creation_and_get_load_value;
}

state_machine_sub_func await_entry_creation_and_get_load_value {
await{
    when CREATE_ENTRY:
      if (!store_controller.empty()) {
        self.latest_store_sequence_num = store_controller.last_elem().seq_num
      } else {
        self.latest_store_sequence_num = None
      }

      self.load_sequence_num = CREATE_ENTRY.sequence_num;
      self.load_instruction = CREATE_ENTRY.instruction;
  }
  await {
    when SCHEDULE_LOAD:
      self.virt_addr = SCHEDULE_LOAD.virtual_addr
  }


  interface_message = TLB.TRANSLATE_ADDR( self.virt_addr )
  msg = request(TLB, interface_message);
  send(msg)

  await {
    when get_translation:
      self.phys_addr = get_translation.phys_addr;
      check_forwarding_or_load;
      break;
  }
}

state_machine_sub_func check_forwarding_or_load {

  msg = request(store_buffer, store_buffer.SEARCH, self.phys_addr)
  send(msg)

  await {
    when search_result_fail:
      self.load_packet = build_packet(self.phys_addr);
      interface_message load_mem_request = memory_interface.SEND_PACKET(self.load_packet)
      load_mem_request.meta(PRESENT_MESSAGE_TO_MEMORY_SYSTEM)

      try {
        msg = request(memory_interface, load_mem_request);
        send(msg)

        await {
          when access_completed:
            self.read_value = access_completed.value;
            self.ld_state = writeback_attempt

        }
      } catch cache_invalidate {
        bool address_overlap =
          compare_phys_and_inval_addr(self.phys_addr,
                                      cache_invalidate.address)
        if (address_overlap) {
          check_forwarding_or_load;
        }
      }
      break;
    when search_result_success:
      try {
        self.read_value = search_result_success.store_inst
        attempt_writeback;
      } catch cache_invalidate {
        check_forwarding_or_load;
      }
      break;
  }


}

state_machine_sub_func attempt_writeback {
  msg = request(writeback_queue, INSERT, self.read_value)
  send(msg)

  await {
    when insert_success:
      await {
        when ROB_committed:
          await_entry_creation_and_get_load_value;
          init_entry_with_default_state;
      }
    when insert_fail:
      attempt_writeback;
  }
}



internal_function compare_phys_and_inval_addr(physical_address phys_addr,
                                              physical_address cache_invalidate_address) {
      bool address_overlap = addr_overlap(self.phys_addr,
                                          cache_invalidate.address)
      return address_overlap;
}
]
def ex2 : Pipeline.AST := [pipeline|
internal_function compare_phys_and_inval_addr(physical_address phys_addr,
physical_address cache_invalidate_address) {
bool address_overlap = addr_overlap(self.phys_addr,
cache_invalidate.address)
return address_overlap;
}
]
