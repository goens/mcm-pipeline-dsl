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

@[default_target]
lean_exe aqlc {
  supportInterpreter := true
  root := `Main
}

require Murphi from git "https://github.com/goens/lean-murphi.git"@"main"
require Cli from git "https://github.com/mhuisi/lean4-cli.git" @ "nightly"
require Mathlib from git "https://github.com/leanprover-community/mathlib4" @ "e01ef88"