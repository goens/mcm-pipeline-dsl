import Lean.Syntax
open Lean.Syntax
namespace Pipeline

inductive AST
| Pipeline : List AST → AST
| Struct : String → String → List AST → AST
| List : List AST → AST
| Const : Nat → AST
| Assign : AST → AST → AST
| Decl : AST → AST
| FunDecl : AST → List AST → List AST → AST
| FunCall : AST → List AST → AST
| If  : AST → List AST → AST
| IfElse  : AST → List AST → List AST → AST
| Await : List AST → AST
| When : AST → List AST → AST
| TryCatch  : List AST → AST → List AST → AST
| Break : AST
| Return : AST → AST
| UnaryOp : String → AST → AST
| Identifier : String → AST
| Access : AST → AST → AST
| TypedIdentifier : String → String → AST

declare_syntax_cat pipeline
declare_syntax_cat stmt
declare_syntax_cat stmt_list
declare_syntax_cat typed_ident
declare_syntax_cat expr

-- expresions
syntax ident : expr
syntax "[" expr,* "]" : expr
syntax expr "(" expr,* ")" : expr
syntax expr "." expr : expr
syntax  "!" expr : expr
syntax typed_ident "=" expr : expr -- definition
syntax ident "=" expr : expr -- definition
syntax num : expr

/-
NOTE 1:
Typed identifier, consider replacing
first ident with "type" that enums the return type
with some specific types
-/
syntax ident ident : typed_ident

-- statements
syntax typed_ident "(" typed_ident,* ")" "{" stmt_list "}" : stmt -- function declaration
syntax typed_ident : stmt -- declaration
syntax "if" "(" expr ")" "{" stmt_list "}" "else" "{" stmt_list "}" : stmt
syntax "if" "(" expr ")" "{" stmt_list "}" : stmt
syntax "await" "{" ("when" expr ":" stmt_list )* "}" : stmt
syntax "try" "{" stmt_list "}" "catch" expr "{" stmt_list "}" : stmt
syntax "break" : stmt
syntax "return" expr : stmt
syntax expr : stmt
-- syntax stmt ";" stmt : stmt

syntax (stmt ";"?)* : stmt_list

syntax stmt_list : pipeline
/-
NOTE 2:
Same thing here. Replace the first ident
(identifier) with an enum of allowed keywords
Or just whatever the transitions are called
-/
syntax ident ident "{" stmt_list "}" : stmt

syntax "[expr|" expr "]" : term
syntax "[stmt|" stmt "]" : term
syntax "[stmt_list|" stmt_list "]" : term
syntax "[t_ident|" typed_ident "]" : term
syntax "[ident|" ident "]" : term
syntax "[pipeline|" pipeline "]" : term

macro_rules
| `([ident|$x:ident]) => do
  let xStr : String := x.getId.toString
  let xSyn : Lean.Syntax := Lean.quote xStr
  return xSyn

-- parsing expressions
macro_rules
| `([expr| $val:numLit]) => `(AST.Const $val)
| `([expr| [$vals:expr,* ]]) => do
  let initList <- `([])
  let valList <- vals.getElems.foldrM (init := initList) fun x xs => `([expr| $x]::$xs)
  `(AST.List $valList)
| `([expr| $name:expr($args:expr,*)]) => do
  let initList <- `([])
  let argList <- args.getElems.foldrM (init := initList) fun x xs => `([expr| $x]::$xs)
  `(AST.FunCall [expr|$name] $argList)
| `([expr|$name:ident]) => `(AST.Identifier [ident|$name])
| `([expr|$first:expr . $rest:expr]) => do
  `(AST.Access [expr|$first] [expr|$rest])
| `([expr| $tname:typed_ident = $e:expr]) =>
`(AST.Assign [t_ident|$tname] [expr|$e])
| `([expr| $name:ident = $e:expr]) =>
  `([expr| untyped $name = $e])
| `([expr| !$e]) => `(AST.UnaryOp "!" [expr|$e])

-- parsing statement list
macro_rules
| `([stmt_list| $[ $stmts $[;]?]*]) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([stmt| $x]::$xs)
  `($stmtList)

-- parsing typed identifier
macro_rules
| `([t_ident| $type:ident $name:ident]) =>
  `(AST.TypedIdentifier $(Lean.quote (toString type.getId))
  $(Lean.quote (toString name.getId)))

-- parsing stmts
macro_rules
| `([stmt| $tname:typed_ident ($args,*) {$stmts}]) => do
  let initList <- `([])
  let argsList <- args.getElems.foldrM (init := initList) fun x xs => `([t_ident| $x]::$xs)
  `(AST.FunDecl [t_ident| $tname] $argsList [stmt_list| $stmts])
| `([stmt| $type:ident $name:ident { $stmts } ]) => `(AST.Struct [ident|$type] [ident|$name] [stmt_list| $stmts])
| `([stmt| $tname:typed_ident]) =>
`(AST.Decl [t_ident| $tname])
| `([stmt| if($cond:expr){ $stmts }]) => do
  `(AST.If [expr|$cond] [stmt_list| $stmts])
| `([stmt| if($cond:expr){ $thenStmts } else { $elseStmts }]) => do
  `(AST.IfElse [expr|$cond] [stmt_list| $thenStmts] [stmt_list| $elseStmts])
| `([stmt| await{ $[when $exps : $stmt_lists]* }] ) => do
  let initList <- `([])
  let expsAndStmts := Array.zip exps stmt_lists
  let whens <- expsAndStmts.foldlM (init := initList) fun rest (exp,stmt_list) => `((AST.When [expr|$exp] [stmt_list|$stmt_list])::$rest)
  `(AST.Await $whens)
| `([stmt|try { $tryStmts } catch $catchExpr { $catchStmts }]) => `(AST.TryCatch [stmt_list| $tryStmts] [expr| $catchExpr] [stmt_list| $catchStmts])
| `([stmt|break]) => `(AST.Break)
| `([stmt|return $e]) => `(AST.Return [expr|$e])
| `([stmt| $e:expr]) => `([expr| $e])

macro_rules
| `([pipeline| $stmts:stmt_list]) => `(AST.Pipeline [stmt_list|$stmts])

--  | `([stmt|await { $[when $expr : $[stmts $[;]?]*]*}])
#check [expr| foo]
#check [expr| foo a = b]
#check [expr| foo(bar,baz)]
#check [stmt| int foo]
#check [stmt| int foo(int arg1, int arg2){ int a = b; return bar}]
#check [stmt| if(0){ int a = b  int b = c } ]
#check [stmt| if(0){ int a = b  int b = c } else { char bar = z; }]
#check [stmt| await{ when foo : int a = b  int b = c } ]
#check [pipeline| internal_function compare_phys_and_inval_addr(physical_address phys_addr,
physical_address cache_invalidate_address) {
bool address_overlap = addr_overlap(self.phys_addr,
cache_invalidate.address)
return address_overlap;
}
]

#check [stmt| internal_function compare_phys_and_inval_addr(physical_address phys_addr,
physical_address cache_invalidate_address) {
bool address_overlap = addr_overlap(self.phys_addr,
cache_invalidate.address)
return address_overlap;
}
]

/-
General NOTE:
1. Limited debugging support from lean?
2. Limited parsing error messages?
-/