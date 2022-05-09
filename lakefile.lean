import Lake
open Lake DSL

package «pipeline-dsl» {
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
