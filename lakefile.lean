import Lake
open Lake DSL

package «pipeline-dsl» {

  -- Uncomment the line about
  -- supporting the interpreter if on Linux
  -- See the following issue on Zulip
  -- https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/include.20lean.2Eh/near/264879668
  -- supportInterpreter := true

  -- add configuration options here
  dependencies := #[{
  name := `Graph
  src := Source.git "https://github.com/goens/graph-library-for-lean4.git" "master"
  },
  {
  name := `MLIR
  src := Source.git "https://github.com/opencompl/lean-mlir" "master"
  }
  ]
}
