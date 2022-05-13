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
  /- structure -/ Identifier /- { -/ → Statement  /- } -/ → Description
| structure_state : /- state -/ Identifier → /- { -/ Statement → /- } -/
  Description
-- constructors have the same signature, but will use different keywords
| structure_transition :
  /- transition -/ Identifier /- { -/ → Statement /- } -/ → Description
-- Function definition
| function_definition :
  TypedIdentifier /- ( -/ → List Expr /- ) { -/ → Statement /- } -/ →
  Description

inductive Label
| result_write : Label

-- one or more catch blocks
inductive CatchBlocks
| multiple_statements :
  /- catch  ( -/  QualifiedName /- ) { -/ → List Statement /- } -/ →
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
| negation: Factor → Factor
| logical_negation: Factor → Factor
| binary_negation: Factor → Factor
| var : Identifier → Factor -- variable is a lean keyword...
| const : Const → Factor -- constant is a lean keyword...
| function_call : QualifiedName → /- ( -/ List Expr  /- ) -/ → Factor

inductive Const
| num_lit : Nat → Const
| str_lit : String → Const

inductive QualifiedName
| mk : List Identifier → QualifiedName

-- TODO: Test this (expr) in a sandbox
inductive Expr
| add : Term → Term → Expr
| sub : Term → Term → Expr
| mul : Term → Term → Expr
| div : Term → Term → Expr
| binand : Term → Term → Expr
| binor : Term → Term → Expr
| binxor : Term → Term → Expr
| leftshift : Term → Term → Expr
| rightshift : Term → Term → Expr
| greater_than : Term → Term → Expr
| less_than    : Term → Term → Expr
| leq    : Term → Term → Expr
| geq    : Term → Term → Expr
| equal        : Term → Term → Expr
| not_equal : Term → Term → Expr
-- | some_term : Term → Expr
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
  /- try { -/ Statement /- } -/ → CatchBlocks → Statement
| await :
  /- await { -/ Statement → Statement -- here the AST is "imprecise" (when could be a different inductive type)
| when :
  /- when -/ QualifiedName /- { -/ → Statement /- } -/ → Statement
| transition : -- transition to an explicit state
  /- transition -/ Identifier → Statement
-- should just be a function call
| stray_expr : Expr → Statement
| block : /- { -/ List Statement /- } -/ → Statement
| return_stmt : Expr → Statement

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
def ex0010 : Statement := Statement.await ex0005 

-- === descriptions
def ex0011 : Description := Description.structure_specification "example_structure" [ ex0010 ]

-- === AST with 1 description!
def ex0012 : AST := AST.structure_descriptions [ ex0011 ]

end Pipeline
