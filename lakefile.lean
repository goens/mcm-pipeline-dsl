import Lake
open Lake DSL

package «pipeline-dsl» {

  -- Uncomment the line about
  -- supporting the interpreter if on Linux
  -- See the following issue on Zulip
  -- https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/include.20lean.2Eh/near/264879668
  -- supportInterpreter := true

  -- add configuration options here
}

lean_lib PipelineDsl {
  -- add library configuration options here
}

@[defaultTarget]
lean_exe pipelinedsl {
  supportInterpreter := true
  root := `Main
}
