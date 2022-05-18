import PipelineDsl.AST
import PipelineDsl.Parser
open Pipeline

--- ==== AST tests =====

def ex0000 : Identifier := "hullo"
def ex0002 : Term := Term.var ex0000
def ex0003 : Expr := Expr.some_term ex0002

-- === Statement
def ex0004 : Statement := Statement.stray_expr ex0003

-- === Conditional
def ex0005 : Conditional := Conditional.if_else_statement ex0003 ex0004 ex0004

-- === await
def ex0006 : Statement := Statement.await ex0004

-- === descriptions
def ex0007 : Description := Description.controller "example_structure" ex0006

-- === AST with 1 description!
def ex0008 : AST := AST.structure_descriptions [ ex0007 ]

--- ==== Parser tests =====


-- broken by parsing to data structure (and not Lean terms)
-- def ex0010 : Term := [dsl_term| hullo ]
-- def ex0011 : Expr := [expr|a]
-- def ex0012 : Expr := [binop| a + b]
-- def ex0013 : Expr := [expr| a << 42]
-- def ex0014 : Statement := [statement| b = 42]
-- def ex0015 : Statement := [statement| a.b = c;]
-- def ex0016 : QualifiedName := [qualified_name| a . b]
-- def ex0017 : Conditional := [conditional| if (a | b) {a . b = b;} else { b = a }]
-- def ex0018 : Description := [structure_declaration| controller_entry example_structure { foo = bar; } ]
-- def ex0019 : AST := [file| controller_entry example_structure { foo = bar; } ]
-- int num_entries = 32
-- #reduce ex0019

