def Identifier := String
def TIden := Identifier
-- def FunctionName := Identifier
-- def StructureName := Identifier
-- def Var := Identifier

namespace Pipeline

mutual

inductive AST
| structure_descriptions : List Description → AST

inductive TypedIdentifier
 | mk : TIden → Identifier → TypedIdentifier

inductive Description
| structure_specification :
  /- structure -/ Identifier /- { -/ → List Statement  /- } -/ → Description
| structure_state : /- state -/ Identifier → /- { -/ List Statement → /- } -/
  Description
-- constructors have the same signature, but will use different keywords
| structure_transition :
  /- transition -/ Identifier /- { -/ → List Statement /- } -/ → Description
-- Function definition
| function_definition :
  TypedIdentifier /- ( -/ → List Expr /- ) { -/ → List Statement /- } -/ →
  Description

inductive Label
| result_write : Label

-- one or more catch blocks
inductive CatchBlocks
| catch_block :
  /- catch  ( -/  QualifiedName /- ) { -/ → Statement /- } -/ →
  CatchBlocks → CatchBlocks
| single_catch : -- one or more, this is the "one"
  QualifiedName → Statement → CatchBlocks

inductive Conditional
-- if with else
| if_else_statement :
  /- if ( -/ Expr /- ) -/ → Statement /- else -/ → Statement → Conditional
| if_statement : -- if without else
  /- if ( -/ Expr /- ) { -/ → Statement /- } -/ → Conditional

inductive Term
| mult : Term → Factor → Term
| div  : Term → Factor → Term
| some_factor : Factor → Term
-- | lsh  : Expr → Term → Expr
-- | rsh  : Expr → Term → Expr

inductive Factor
| negation: String → Factor → Factor
| var : Identifier → Factor -- variable is a lean keyword...
| const : Const → Factor -- constant is a lean keyword...
| function_call : Identifier → /- ( -/ List Expr  /- ) -/ → Factor

inductive Const
| num_lit : Nat → Const
| str_lit : String → Const

inductive QualifiedName
| mk : List Identifier → QualifiedName

-- TODO: Test this (expr) in a sandbox
inductive Expr
| add : Expr → Term → Expr
| sub : Expr → Term → Expr
| greater_than : Expr → Term → Expr
| less_than    : Expr → Term → Expr
| equal        : Expr → Term → Expr
| not_equal : Expr → Term → Expr
| some_term : Term → Expr
-- | Term : factor → Expr → Expr
-- | nothing : Expr

inductive Statement
| labelled_statement : Label → Statement → Statement
| declaration : TypedIdentifier → Statement -- declare a variable
| value_declaration : -- declare a variable with a value
  TypedIdentifier → /- = -/ Expr → Statement
| variable_assignment : -- assign a var an expr
  QualifiedName /- = -/ → Expr → Statement
| conditional_stmt : -- if statement, if else statement
  Conditional → Statement
-- function call?
| try_catch :
  /- try { -/ List Statement /- } -/ → CatchBlocks → Statement
| await :
  /- await { -/ List Statement → Statement
| when :
  /- when -/ QualifiedName /- { -/ → List Statement /- } -/ → Statement
| transition : -- transition to an explicit state
  /- transition -/ Identifier → Statement
-- should just be a function call
| stray_expr : Expr → Statement
| block : /- { -/ List Statement /- } -/ → Statement

end  -- mutual

--- ==== some tests... =====

def ex0000 : Identifier := "hullo"
def ex0002 : Factor := Factor.var ex0000
def ex0003 : Term := Term.some_factor ex0002

-- === expression
def ex0004 : Expr := Expr.some_term ex0003

-- === Statement
def ex0005 : Statement := Statement.stray_expr ex0004

-- === Conditional
def ex0008 : Conditional := Conditional.if_else_statement ex0004 ex0005 ex0005

-- === await
def ex0010 : Statement := Statement.await [ ex0005 ]

-- === descriptions
def ex0011 : Description := Description.structure_specification "example_structure" [ ex0010 ]

-- === AST with 1 description!
def ex0012 : AST := AST.structure_descriptions [ ex0011 ]

end Pipeline
