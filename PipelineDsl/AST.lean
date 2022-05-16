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
-- constructors have the same signature, but will use different keywords
| controller : Identifier → Statement → Description
| entry : Identifier → Statement → Description
| control_flow : Identifier → Statement → Description
| transition : Identifier → Statement → Description
-- Function definition
| function_definition :
  TypedIdentifier /- ( -/ → List TypedIdentifier /- ) { -/ → Statement /- } -/ →
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
| negation: Term → Term
| logical_negation: Term → Term
| binary_negation: Term → Term
| var : Identifier → Term -- variable is a lean keyword...
| qualified_var : QualifiedName → Term -- variable is a lean keyword...
| const : Const → Term -- constant is a lean keyword...
| function_call : QualifiedName → /- ( -/ List Expr  /- ) -/ → Term

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

mutual
private partial def constToString : Const → String
  | .num_lit n => toString n
  | .str_lit s => s

private partial def termToString : Term → String
  | .negation t => "-" ++ (termToString t)
  | .logical_negation t => "!" ++ (termToString t)
  | .binary_negation t => "~" ++ (termToString t)
  | .var i => identifierToString i
  | .qualified_var n => qualifiedNameToString n
  | .const c => constToString c
  | .function_call n es => (qualifiedNameToString n) ++ "(" ++ String.intercalate ", " (es.map exprToString)  ++ ")"

private partial def identifierToString : Identifier → String := λ x => x

private partial def qualifiedNameToString : QualifiedName → String
  | .mk l => match l with
    | [] => ""
    | n::[] => identifierToString n
    | n::ns => (identifierToString n) ++ "." ++ (qualifiedNameToString (QualifiedName.mk ns))

private partial def exprToString : Expr → String
  | .add x y => (termToString x) ++ "+" ++ (termToString y)
  | .sub x y => (termToString x) ++ "-" ++ (termToString y)
  | .mul x y => (termToString x) ++ "*" ++ (termToString y)
  | .div x y => (termToString x) ++ "/" ++ (termToString y)
  | .binand x y => (termToString x) ++ "&" ++ (termToString y)
  | .binor x y => (termToString x) ++ "|" ++ (termToString y)
  | .binxor x y => (termToString x) ++ "^" ++ (termToString y)
  | .leftshift x y => (termToString x) ++ "<<" ++ (termToString y)
  | .rightshift x y => (termToString x) ++ ">>" ++ (termToString y)
  | .greater_than x y => (termToString x) ++ ">" ++ (termToString y)
  | .less_than    x y => (termToString x) ++ "<" ++ (termToString y)
  | .leq    x y => (termToString x) ++ "<=" ++ (termToString y)
  | .geq    x y => (termToString x) ++ ">=" ++ (termToString y)
  | .equal        x y => (termToString x) ++ "==" ++ (termToString y)
  | .not_equal x y => (termToString x) ++ "!=" ++ (termToString y)
  | .some_term x => termToString x

end -- mutual

instance : ToString Const where toString := constToString
instance : ToString Term where toString := termToString
instance : ToString Expr where toString := exprToString
instance : ToString Identifier where toString := identifierToString
instance : ToString QualifiedName where toString := qualifiedNameToString
instance : Inhabited Const where default := Const.num_lit 0
instance : Inhabited Term where default := Term.const default

end Pipeline

