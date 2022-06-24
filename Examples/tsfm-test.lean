-- import PipelineDsl.Preprocess
-- import Init.System
-- import Init.System.IO

-- def process_file (filename : String) : String :=
-- match filename with
-- |  name => name


import PipelineDsl
-- import PipelineDsl.AST

--open Pipeline

def ex0010 : Term := [dsl_term| hullo ]

-- This works... but I want an input object of type AST?
-- Why doesn't this accept matching on an object?
open Pipeline.AST in
def transform0001 ( input : Pipeline.AST) : Pipeline.Term :=
  match input with
  | Const => [dsl_term| "1" ]

#eval  transform0001 (AST.structure_descriptions [])

-- The above doesn't really work. It's just taking in a name it doesn't know as a variable.
-- Notice how the two get different highlighting (at least on my color scheme)
open Pipeline.AST in
def transform0003 ( input : Pipeline.AST) : Pipeline.Term :=
  match input with
  | structure_descriptions [] => [dsl_term| "1" ]
  | SomeRandomName => [dsl_term| "1" ]

-- It doesn't work because Const is not (and was not) a constructor of AST.
-- Remember that the AST type is just the root node.
-- It works if you use the Const type instead of AST.
open Pipeline.Const in
def transform0002 ( input : Pipeline.Const) :=
match input with
 | num_lit num => num + 1
 | str_lit str => 0
