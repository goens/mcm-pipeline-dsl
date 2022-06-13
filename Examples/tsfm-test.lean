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

-- This doesn't work?
open Pipeline.AST in
def transform0002 ( input : Pipeline.AST) :=
match input with
| Pipeline.Const sth => sth 