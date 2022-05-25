def Identifier := String deriving Inhabited, ToString
def TIden := Identifier deriving Inhabited, ToString
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
inductive CatchBlock
| mk : QualifiedName → List Identifier → Statement → CatchBlock

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
| list : List Expr → Expr
-- | Term : factor → Expr → Expr
-- | nothing : Expr

inductive Statement
| labelled_statement : Label → Statement → Statement
| variable_declaration : TypedIdentifier → Statement -- declare a variable
| value_declaration : -- declare a variable with a value
  TypedIdentifier → /- = -/ Expr → Statement
| variable_assignment : -- assign a var an expr
  QualifiedName /- = -/ → Expr → Statement
| conditional_stmt : -- if statement, if else statement
  Conditional → Statement
-- function call?
| try_catch :
  /- try { -/ Statement /- } -/ → List CatchBlock → Statement
| await :
  /- await { -/ List Statement → Statement -- here the AST is "imprecise" (when could be a different inductive type)
| when :
  /- when -/ QualifiedName → List Identifier → Statement /- } -/ → Statement
| transition : -- transition to an explicit state
  /- transition -/ Identifier → Statement
-- should just be a function call
| stray_expr : Expr → Statement
| block : /- { -/ List Statement /- } -/ → Statement
| return_stmt : Expr → Statement

end  -- mutual

mutual

private partial def astToString : AST → String
  | .structure_descriptions descs => String.intercalate "\n" (descs.map descriptionToString)

private partial def typedIdentifierToString : TypedIdentifier → String
  | .mk t id => (toString t) ++ " " ++ (toString id)

private partial def descriptionToString : Description → String
| .controller name desc => "controller " ++ (toString name) ++ " " ++ (statementToString desc)
| .entry name desc => "controller_entry" ++ (toString name) ++ " " ++ (statementToString desc)
| .control_flow name desc => "controller_control_flow" ++ (toString name) ++ " " ++ (statementToString desc)
| .transition name body => "transition" ++ (toString name) ++ (statementToString body)
-- Function definition
| .function_definition ret args body =>
  (typedIdentifierToString ret) ++ "(" ++ (String.intercalate ", " (args.map typedIdentifierToString))
  ++ (statementToString body)

private partial def labelToString : Label → String
  | .result_write => "result_write"

private partial def catchBlockToString : CatchBlock → String
| .mk name args body => "catch " ++ (qualifiedNameToString name) ++
  "(" ++ (String.intercalate ", " (args.map toString)) ++
 ")\n" ++ (statementToString body)

private partial def conditionalToString : Conditional → String
| .if_else_statement cond then_br else_br  => "if (" ++ (exprToString cond) ++ ")\n" ++ (statementToString then_br) ++ "else\n" ++ (statementToString else_br)
| .if_statement cond then_br => "if (" ++ (exprToString cond) ++ ")\n" ++ (statementToString then_br)

private partial def termToString : Term → String
  | .negation t => "-" ++ (termToString t)
  | .logical_negation t => "!" ++ (termToString t)
  | .binary_negation t => "~" ++ (termToString t)
  | .var i => toString i
  | .qualified_var n => qualifiedNameToString n
  | .const c => constToString c
  | .function_call n es => (qualifiedNameToString n) ++ "(" ++ String.intercalate ", " (es.map exprToString)  ++ ")"

private partial def constToString : Const → String
| .num_lit n => toString n
| .str_lit s => s

private partial def qualifiedNameToString : QualifiedName → String
  | .mk l => match l with
    | [] => ""
    | n::[] => toString n
    | n::ns => (toString n) ++ "." ++ (qualifiedNameToString (QualifiedName.mk ns))

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
  | .list xs => String.intercalate ", " (xs.map exprToString)
  | .some_term x => termToString x

private partial def statementToString : Statement → String
  | .labelled_statement label stmt => (labelToString label) ++ " " ++ (statementToString stmt)
  | .variable_declaration tid => (typedIdentifierToString tid)
  | .value_declaration tid val => (typedIdentifierToString tid) ++ " = " ++ (exprToString val)
  | .variable_assignment tgt val => (qualifiedNameToString tgt) ++ " = " ++ (exprToString val)
  | .conditional_stmt cond => conditionalToString cond
  | .try_catch try_block catches => (statementToString try_block) ++ "\n" ++ (String.intercalate "\n" (catches.map catchBlockToString))
  | .await whens => "await\n" ++ String.intercalate "\n" (whens.map statementToString)
  | .when msg args body => "when "  ++ (qualifiedNameToString msg) ++ "(" ++ (String.intercalate "," args) ++ ")" ++ (statementToString body)
  | .transition lbl => "transition " ++ (toString lbl)
  | .stray_expr e => exprToString e
  | .block stmts => "{" ++ (String.intercalate "\n" (stmts.map λ s => statementToString s ++ ";"))  ++ "\n}"
  | .return_stmt e => "return " ++ exprToString e

end -- mutual

instance : ToString Const where toString := constToString
instance : ToString Term where toString := termToString
instance : ToString Expr where toString := exprToString
instance : ToString AST where toString := astToString
instance : ToString Statement where toString := statementToString
instance : ToString QualifiedName where toString := qualifiedNameToString
instance : ToString Description where toString := descriptionToString
instance : Inhabited Const where default := Const.num_lit 0
instance : Inhabited Term where default := Term.const default
instance : Inhabited AST where default := AST.structure_descriptions []
instance : Inhabited Expr where default := Expr.some_term default
instance : Inhabited Statement where default := Statement.block []
instance : Inhabited Description where default := Description.controller default default

end Pipeline

