def Identifier := String deriving Inhabited, ToString, BEq
def TIden := Identifier deriving Inhabited, ToString, BEq
-- def FunctionName := Identifier
-- def StructureName := Identifier
-- def Var := Identifier

namespace Pipeline

inductive Direction
 | Previous
 | Next

def indent : Nat → String
  | Nat.zero => ""
  | Nat.succ n => "  " ++ indent n

mutual

--@[deriving BEq]
inductive AST
| structure_descriptions : List Description → AST

inductive TypedIdentifier
 | mk : TIden → Identifier → TypedIdentifier

inductive Description
-- constructors have the same signature, but will use different keywords
| controller : Identifier → Statement → Description
| entry : Identifier → Statement → Description
| control_flow : Identifier → Statement → Description
| state : Identifier → Statement → Description
-- Function definition
| function_definition :
  TypedIdentifier /- ( -/ → List TypedIdentifier /- ) { -/ → Statement /- } -/ →
  Description

inductive Label
| result_write : Label

-- one or more catch blocks
inductive HandleBlock
| mk : QualifiedName → List Identifier → Statement → HandleBlock

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
| expr : Expr → Term
| relative_entry : Direction → List Expr → Term

inductive Const
| num_lit : Nat → Const
| str_lit : String → Const

inductive QualifiedName
| mk : List Identifier → QualifiedName
deriving Inhabited

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
| variable_declaration : TypedIdentifier → Statement
| value_declaration : TypedIdentifier →  Expr → Statement
| variable_assignment : QualifiedName  → Expr → Statement
| conditional_stmt : Conditional → Statement
| listen_handle : Statement → List HandleBlock → Statement
| await : Option Term → List Statement → Statement -- here the AST is "imprecise" (when could be a different inductive type)
| when :  QualifiedName → List Identifier → Statement → Statement
| transition : Identifier → Statement
| reset : Identifier → Statement
| complete : Identifier → Statement
| stray_expr : Expr → Statement
| block : /- { -/ List Statement /- } -/ → Statement
| return_stmt : Expr → Statement
| stall : Expr → Statement
-- what about function call?

end  -- mutual

def QualifiedName.toList : QualifiedName → List Identifier
| .mk ids => ids

mutual

private partial def astToString : AST → String
  | .structure_descriptions descs => String.intercalate "\n" (descs.map descriptionToString)

private partial def typedIdentifierToString : TypedIdentifier → String
  | .mk t id => (toString t) ++ " " ++ (toString id)

private partial def descriptionToString : Description → String
| .controller name desc => "controller " ++ (toString name) ++ " " ++ (statementToString 0 desc)
| .entry name desc => "controller_entry " ++ (toString name) ++ " " ++ (statementToString 0 desc)
| .control_flow name desc => "controller_control_flow " ++ (toString name) ++ " " ++ (statementToString 0 desc)
| .state name body => "state " ++ (toString name) ++ (statementToString 0 body)
-- Function definition
| .function_definition ret args body =>
  (typedIdentifierToString ret) ++ "(" ++ (String.intercalate ", " (args.map typedIdentifierToString))
  ++ (statementToString 0 body)

private partial def labelToString : Label → String
  | .result_write => "result_write "

private partial def handleBlockToString : HandleBlock → String
| .mk name args body => "handle " ++ (qualifiedNameToString name) ++
  "(" ++ (String.intercalate ", " (args.map toString)) ++
 ")\n" ++ (statementToString 0 body)

private partial def conditionalToString : Conditional → String
| .if_else_statement cond then_br else_br  => "if (" ++ (exprToString cond) ++ ")\n" ++ (statementToString 0 then_br) ++ "else\n" ++ (statementToString 0 else_br)
| .if_statement cond then_br => "if (" ++ (exprToString cond) ++ ")\n" ++ (statementToString 0 then_br)

private partial def termToString : Term → String
  | .negation t => "-" ++ (termToString t)
  | .logical_negation t => "!" ++ (termToString t)
  | .binary_negation t => "~" ++ (termToString t)
  | .var i => toString i
  | .qualified_var n => qualifiedNameToString n
  | .const c => constToString c
  | .expr e => exprToString e
  | .relative_entry .Previous es => "prev<" ++ String.intercalate ", " (es.map exprToString) ++ ">"
  | .relative_entry .Next es => "next<" ++ String.intercalate ", " (es.map exprToString) ++ ">"
  | .function_call n es => (qualifiedNameToString n) ++ "(" ++ String.intercalate ", " (es.map exprToString)  ++ ")"

private partial def constToString : Const → String
| .num_lit n => toString n
| .str_lit s => s

private partial def qualifiedNameToString : QualifiedName → String
  | .mk l => match l.reverse with
    | [] => ""
    | n::[] => toString n
    | n::ns => (qualifiedNameToString (QualifiedName.mk ns)) ++ "." ++ (toString n)

private partial def exprToString : Expr → String
  | .add x y => (termToString x) ++ " + " ++ (termToString y)
  | .sub x y => (termToString x) ++ " - " ++ (termToString y)
  | .mul x y => (termToString x) ++ " * " ++ (termToString y)
  | .div x y => (termToString x) ++ " / " ++ (termToString y)
  | .binand x y => (termToString x) ++ " & " ++ (termToString y)
  | .binor x y => (termToString x) ++ " | " ++ (termToString y)
  | .binxor x y => (termToString x) ++ " ^ " ++ (termToString y)
  | .leftshift x y => (termToString x) ++ " << " ++ (termToString y)
  | .rightshift x y => (termToString x) ++ " >> " ++ (termToString y)
  | .greater_than x y => (termToString x) ++ " > " ++ (termToString y)
  | .less_than    x y => (termToString x) ++ " < " ++ (termToString y)
  | .leq    x y => (termToString x) ++ " <= " ++ (termToString y)
  | .geq    x y => (termToString x) ++ " >= " ++ (termToString y)
  | .equal        x y => (termToString x) ++ " == " ++ (termToString y)
  | .not_equal x y => (termToString x) ++ " != " ++ (termToString y)
  | .list xs => "[" ++ String.intercalate ", " (xs.map exprToString) ++ "]"
  | .some_term x => termToString x

private partial def statementToString (indentationLevel := 0) (inputStatement : Statement) : /-Statement →-/ String :=
  let indent_outter_nest : Nat := (indentationLevel - 1)
  let same_nesting_statementToString := statementToString (indentationLevel := indentationLevel)
  let indent_nested_statementToString := statementToString (indentationLevel := indentationLevel + 1)
  match inputStatement with
  | .labelled_statement label stmt => (labelToString label) ++ " " ++ (same_nesting_statementToString stmt)
  | .variable_declaration tid => (typedIdentifierToString tid)
  | .value_declaration tid val => (typedIdentifierToString tid) ++ " = " ++ (exprToString val)
  | .variable_assignment tgt val => (qualifiedNameToString tgt) ++ " = " ++ (exprToString val)
  | .conditional_stmt cond => conditionalToString cond
  | .listen_handle listen_block catches => "listen " ++ (indent_nested_statementToString listen_block) ++ "\n" ++ (String.intercalate "\n" (catches.map handleBlockToString))
  | .await opcall whens =>
    let call := match opcall with
      | none => ""
      | some c => termToString c ++ " "
    s!"await {call}\{\n" ++ String.intercalate "\n" (whens.map statementToString) ++ "\n}\n"
  | .when src_and_msg args body =>
    let src := src_and_msg.toList.head!
    let msg := QualifiedName.mk src_and_msg.toList.tail!
   "when "  ++ (qualifiedNameToString msg) ++ "(" ++ (String.intercalate "," args) ++ s!") from {src} " ++ (same_nesting_statementToString body)
  | .transition lbl => "transition " ++ (toString lbl)
  | .reset lbl => "reset " ++ (toString lbl)
  | .complete lbl => "complete " ++ (toString lbl)
  | .stray_expr e => exprToString e
  | .stall e => "stall ( " ++ exprToString e ++ " )"
  | .block stmts => " {\n" ++ String.join (stmts.map (λ stmt => String.join [(indent indentationLevel), indent_nested_statementToString stmt, ";\n"]))  ++ (indent indent_outter_nest) ++ "}\n"
  | .return_stmt e => "return " ++ exprToString e

end -- mutual


instance : ToString Const where toString := constToString
instance : ToString Term where toString := termToString
instance : ToString Expr where toString := exprToString
instance : ToString AST where toString := astToString
instance : ToString Statement where toString := statementToString
instance : ToString QualifiedName where toString := qualifiedNameToString
instance : ToString Description where toString := descriptionToString
instance : ToString TypedIdentifier where toString := typedIdentifierToString
instance : ToString HandleBlock where toString := handleBlockToString
instance : Inhabited Const where default := Const.num_lit 0
instance : Inhabited Term where default := Term.const default
instance : Inhabited AST where default := AST.structure_descriptions []
instance : Inhabited Expr where default := Expr.some_term default
instance : Inhabited Statement where default := Statement.block []
instance : Inhabited Description where default := Description.controller default default

end Pipeline
