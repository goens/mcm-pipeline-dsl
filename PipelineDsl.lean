import Lean.Syntax
open Lean.Syntax
namespace Pipeline

inductive AST
| Pipeline : List AST → AST
| Struct : List AST → AST
| List : List AST → AST
| Const : Nat → AST
| Assign : AST → AST → AST → AST
| Decl : AST → AST → AST
| FunDecl : AST → List AST → AST → AST
| FunCall : AST → List AST → AST
| If  : AST → List AST → AST
| IfElse  : AST → List AST → List AST → AST
| Await : List AST → AST
| When : AST → AST → AST
| Break : AST
| Access : AST → AST → AST
| Identifier : String → AST

declare_syntax_cat pipeline
declare_syntax_cat struct
declare_syntax_cat stmt
declare_syntax_cat stmt_list
declare_syntax_cat expr

-- expresions
syntax num : expr
syntax ident : expr
syntax "[" expr,* "]" : expr
syntax expr "(" expr,* ")" : expr
syntax expr "." expr : expr -- this is too permissive (should be just identifiers)

-- statements
syntax ident ident " = " expr : stmt -- definition
syntax ident ident : stmt -- declaration

syntax (stmt ";"?)* : stmt_list
syntax "if" "(" expr ")" "{" stmt_list "}" "else" "{" stmt_list "}" : stmt
syntax "if" "(" expr ")" "{" stmt_list "}" : stmt
syntax "await" "{" ("when" expr ":" stmt_list )* "}" : stmt
syntax "break" : stmt
-- syntax stmt ";" stmt : stmt

syntax struct* : pipeline
syntax ident ident "{" (stmt ";"?)* "}" : struct

syntax "[expr|" expr "]" : term
syntax "[stmt|" stmt "]" : term
syntax "[stmt_list|" stmt_list "]" : term
syntax "[struct|" struct "]" : term
syntax "[pipeline|" pipeline "]" : term

macro_rules
| `([expr| $val:numLit]) => `(AST.Const $val)
| `([expr| $name:ident]) =>
  let name_str : String := name.getId.toString
  let name_syntax := Lean.quote name_str
  `(AST.Identifier $name_syntax)
| `([expr| [$vals:expr,* ]]) => do
  let initList <- `([])
  let valList <- vals.getElems.foldrM (init := initList) fun x xs => `([expr| $x]::$xs)
  `(AST.List $valList)
| `([expr| $name:ident($args:expr,*)]) => do
  let initList <- `([])
  let argList <- args.getElems.foldrM (init := initList) fun x xs => `([expr| $x]::$xs)
  `(AST.FunCall [expr|$name] $argList)
| `([expr|$first:ident.$last:ident]) => `(AST.Access [expr|$first] [expr|$last])

macro_rules
| `([stmt_list| $[ $stmts $[;]?]*]) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([stmt| $x]::$xs)
  `($stmtList)


macro_rules
| `([stmt| $type:ident $name:ident = $e:expr]) =>
  `(AST.Assign [expr|type] [expr|name] [expr|$e])
| `([stmt| $type:ident $name:ident]) =>
  `(AST.Decl [expr|type] [expr|name])
| `([stmt| if($cond:expr){ $stmts }]) => do
  `(AST.If [expr|$cond] [stmt_list| $stmts])
| `([stmt| if($cond:expr){ $thenStmts } else { $elseStmts }]) => do
  `(AST.IfElse [expr|$cond] [stmt_list| $thenStmts] [stmt_list| $elseStmts])
| `([stmt| await{ $[when $exps : $stmt_lists]* }] ) => do
  let initList <- `([])
  let expsAndStmts := Array.zip exps stmt_lists
  let whens <- expsAndStmts.foldlM (init := initList) fun rest (exp,stmt_list) => `((AST.When [expr|$exp] [stmt_list|$stmt_list])::$rest)
  `(AST.Await $whens)

--  | `([stmt|await { $[when $expr : $[stmts $[;]?]*]*}])

#check [stmt| if(0){ int a = b  int b = c } ]
#check [stmt| if(0){ int a = b  int b = c } else { char bar = z; }]
#check [stmt| await{ when foo : int a = b  int b = c } ]
