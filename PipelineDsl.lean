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
| If  : AST → AST → AST
| IfElse  : AST → AST → AST → AST
| Await : List AST → AST
| When : AST → AST → AST
| Break : AST
| Access : AST → AST → AST
| Identifier : String → AST

declare_syntax_cat pipeline
declare_syntax_cat struct
declare_syntax_cat stmt
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

syntax "if" "(" expr ")" "{" (stmt ";"?)* "}" "else" "{" (stmt ";"?)* "}" : stmt
syntax "if" "(" expr ")" "{" (stmt ";"?)* "}" : stmt
syntax "await" "{" ("when" expr ":" stmt ";"?)* "}" : stmt
syntax "break" : stmt
-- syntax stmt ";" stmt : stmt

syntax struct* : pipeline
syntax ident ident "{" (stmt ";"?)* "}" : struct

syntax "[expr|" expr "]" : term
syntax "[stmt|" stmt "]" : term
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

-- let initNamesList <- `([]) -- ` `([[uaAccess| $name]])
-- -- dbg_trace name
-- let nameList <- names.foldlM (init := initNamesList) (fun xs x => do
-- -- dbg_trace x
-- let bar <- `(BAR)
-- let out <- `($xs ++ [ [uaAccess| $bar] ] ++ [AST.nameNode "bar"])
-- return out) 
-- 
-- | `([])

macro_rules
| `([stmt| $type:ident $name:ident = $e:expr]) =>
  `(AST.Assign [expr|type] [expr|name] [expr|$e])
| `([stmt| $type:ident $name:ident]) =>
  `(AST.Decl [expr|type] [expr|name])
| `([stmt| if($cond:expr){$thenBr*}]) =>
  `(AST.If [expr|$cond] [stmt|$thenBr])
  | `([stmt| if($cond:expr){$thenBr:stmt} {$elseBr:stmt}]) =>
  `(AST.IfElse [expr|$cond] [stmt|$thenBr] [stmt|$elseBr])
