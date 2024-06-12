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

-- require Murphi from git "https://github.com/goens/lean-murphi.git"@"main"
-- require Cli from git "https://github.com/mhuisi/lean4-cli.git" @ "5a858c32963b6b19be0d477a30a1f4b6c120be7e"
-- require Mathlib from git "https://github.com/leanprover-community/mathlib4" @ "38dbcd8285bc4b1391619c12f158a7409f3dfc12"

require Murphi from git "https://github.com/goens/lean-murphi.git"@"e1e6ea7c3a3f46be82273dd143ae7f35e84e2f8e"
require Cli from git "https://github.com/mhuisi/lean4-cli.git" @ "5a858c32963b6b19be0d477a30a1f4b6c120be7e"
-- require Mathlib from git "https://github.com/leanprover-community/mathlib4" @ "38dbcd8285bc4b1391619c12f158a7409f3dfc12"
