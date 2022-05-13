import Lean.Syntax
import PipelineDsl.AST
open Lean.Syntax

namespace Pipeline

-- non-terminals
declare_syntax_cat file
declare_syntax_cat statement
declare_syntax_cat expr
declare_syntax_cat typed_identifier
declare_syntax_cat qualified_name
declare_syntax_cat declaration
declare_syntax_cat variable_declaration
declare_syntax_cat labeled_statement
declare_syntax_cat label
declare_syntax_cat assignment
declare_syntax_cat conditional
declare_syntax_cat block
declare_syntax_cat await_block
declare_syntax_cat when_block
declare_syntax_cat try_catch
declare_syntax_cat catch_block
declare_syntax_cat return_stmt
declare_syntax_cat dsl_transition
declare_syntax_cat dsl_term
declare_syntax_cat call
declare_syntax_cat unuaryop
declare_syntax_cat binop
declare_syntax_cat expr_list
declare_syntax_cat parexpr
declare_syntax_cat list
declare_syntax_cat structure_declaration
declare_syntax_cat internal_func_decl
declare_syntax_cat arg_list
declare_syntax_cat constval

-- syntax
syntax ident ("." ident)* : qualified_name
syntax num : constval
syntax str : constval
syntax ident ident : typed_identifier -- type_def ~ identifier

syntax declaration* : file
syntax structure_declaration : declaration
syntax internal_func_decl : declaration
syntax typed_identifier statement : structure_declaration

syntax typed_identifier "(" arg_list ")" "{" statement "}" : internal_func_decl
syntax typed_identifier typed_identifier,* : arg_list

syntax labeled_statement ";"? : statement
syntax dsl_transition ";"? : statement
syntax variable_declaration ";"? : statement
syntax assignment ";"? : statement
syntax conditional ";"? : statement
syntax block ";"? : statement
syntax await_block ";"? : statement
syntax try_catch ";"? : statement
syntax return_stmt ";"? : statement
syntax expr ";"? : statement

syntax typed_identifier ("=" expr)? : variable_declaration
syntax label statement : labeled_statement
syntax "result_write" : label
syntax qualified_name "=" expr : assignment -- maybe allow foo.bar?
syntax "if" "(" expr ")" statement ("else"  statement)?  : conditional
syntax "{"  statement*  "}" : block
syntax  "await"  "{"  (when_block)+  "}" : await_block
syntax "when"  "("  qualified_name  ")"  statement : when_block
syntax "try"  statement  catch_block+ : try_catch
syntax "catch"  "("  qualified_name  ")"  statement* : catch_block
syntax "return"  expr : return_stmt
syntax "transition"  ident : dsl_transition

syntax unuaryop : expr
syntax binop : expr
syntax parexpr : expr
syntax  list : expr
syntax dsl_term : expr

-- should be expression instead of access, but leaving this for now (left-recursion)
syntax "("  expr  ")" : dsl_term
syntax  call : dsl_term
syntax qualified_name : dsl_term
syntax constval : dsl_term
syntax qualified_name  "("  expr_list  ")" : call

syntax "!" dsl_term : unuaryop
syntax "~" dsl_term : unuaryop
syntax "-" dsl_term : unuaryop
syntax dsl_term "+" dsl_term : binop
syntax dsl_term "-" dsl_term : binop
syntax dsl_term "*" dsl_term : binop
syntax dsl_term "/" dsl_term : binop
syntax dsl_term "&" dsl_term : binop
syntax dsl_term "|" dsl_term : binop
syntax dsl_term "^" dsl_term : binop
syntax dsl_term "<" dsl_term : binop
syntax dsl_term ">" dsl_term : binop
syntax dsl_term "<=" dsl_term : binop
syntax dsl_term ">=" dsl_term : binop
syntax dsl_term "<<" dsl_term : binop
syntax dsl_term ">>" dsl_term : binop
syntax dsl_term "==" dsl_term : binop
syntax dsl_term "!=" dsl_term : binop

syntax expr,* : expr_list
syntax "("  expr ")" : parexpr
syntax "["  expr_list "]" : list

syntax "[file|" file "]" : term
syntax "[statement|" statement "]" : term
syntax "[expr|" expr "]" : term
syntax "[typed_identifier|" typed_identifier "]" : term
syntax "[qualified_name|" qualified_name "]" : term
syntax "[declaration|" declaration "]" : term
syntax "[variable_declaration|" variable_declaration "]" : term
syntax "[labeled_statement|" labeled_statement "]" : term
syntax "[label|" label "]" : term
syntax "[assignment|" assignment "]" : term
syntax "[conditional|" conditional "]" : term
syntax "[block|" block "]" : term
syntax "[await_block|" await_block "]" : term
syntax "[when_block|" when_block "]" : term
syntax "[try_catch|" try_catch "]" : term
syntax "[catch_block|" catch_block "]" : term
syntax "[return_stmt|" return_stmt "]" : term
syntax "[dsl_term|" dsl_term "]" : term
syntax "[call|" call "]" : term
syntax "[unuaryop|" unuaryop "]" : term
syntax "[binop|" binop "]" : term
syntax "[expr_list|" expr_list "]" : term
syntax "[parexpr|" parexpr "]" : term
syntax "[list|" list "]" : term
syntax "[structure_declaration|" structure_declaration "]" : term
syntax "[internal_func_decl|" internal_func_decl "]" : term
syntax "[arg_list|" arg_list "]" : term
syntax "[constval|" constval "]" : term
syntax "[dsl_transition|" dsl_transition "]" : term

macro_rules
| `([file| $[$decls]* ]) => do
  let initList <- `([])
  let declList <- decls.foldrM (init := initList) fun x xs => `([structure_declaration| $x]::$xs)
  `(AST.structure_descriptions $declList)

macro_rules
| `([statement| $x:labeled_statement $[;]? ]) => `([labeled_statement| $x])
| `([statement| $x:dsl_transition $[;]? ]) => `([dsl_transition| $x])
| `([statement| $x:variable_declaration $[;]? ]) => `([variable_declaration| $x])
| `([statement| $x:assignment $[;]? ]) => `([assignment| $x])
| `([statement| $x:conditional $[;]? ]) => `(Statement.conditional [conditional| $x]) -- feels unnecessary
| `([statement| $x:block $[;]? ]) => `([block| $x])
| `([statement| $x:await_block $[;]? ]) => `([await_block| $x])
| `([statement| $x:try_catch $[;]? ]) => `([try_catch| $x])
| `([statement| $x:return_stmt $[;]? ]) => `([return_stmt| $x])
| `([statement| $x:expr $[;]? ]) => `(Statement.stray_expr [expr| $x])

macro_rules
| `([expr| $x:unuaryop ]) => `([unuaryop| $x])
| `([expr| $x:binop ]) => `([binop| $x])
| `([expr| $x:parexpr ]) => `([parexpr| $x])
| `([expr| $x:list ]) => `([list| $x])
| `([expr| $x:dsl_term ]) => `([dsl_term| $x])

macro_rules
| `([typed_identifier| $t:ident $x:ident ]) => do
  let tStr : String := t.getId.toString
  let xStr : String := x.getId.toString
  let tSyn : Lean.Syntax := Lean.quote tStr
  let xSyn : Lean.Syntax := Lean.quote xStr
  `(TypedIdentifier.mk $tSyn $xSyn)

macro_rules
| `([qualified_name| $x:ident $[. $xs ]* ]) => do
  let xStr : String := x.getId.toString
  let initList <- `([$x])
  let xsList <- xs.foldrM (init := initList) fun x xs => do
    let xStr : String := x.getId.toString
    let xSyn : Lean.Syntax := Lean.quote xStr
    `( $xSyn :: $xs)
  `(QualifiedName.mk $xsList)

macro_rules
| `([declaration| $x:structure_declaration ]) => `([structure_declaration| $x])
| `([declaration| $x:internal_func_decl ]) => `([internal_func_decl| $x])

macro_rules
| `([variable_declaration| $x:typed_identifier = $e ]) => `(Statement.declaration [typed_identifier| $x] [expr|$e])
| `([variable_declaration| $x:typed_identifier]) => `(Statement.value_declaration [typed_identifier| $x])

macro_rules
| `([labeled_statement| $l:label $s ]) => `(Statement.labelled_statement [label| $l] [statement| $s])

macro_rules
| `([label| result_write ]) => `(Label.result_write)

macro_rules
| `([assignment| $q:qualified_name = $e ]) => `(Statement.variables_assignement [qualified_name| $q] [expr| $e])

macro_rules
| `([conditional| if ( $e ) $s else $es ]) => `(Conditional.if_else_statement [expr| $e] [statement| $s] [statement| $es])
| `([conditional| if ( $e ) $s ]) => `(Conditional.if_else_statement [expr| $e] [statement| $s])

macro_rules
| `([block| { $[ $stmts ]* }]) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([statement| $x]::$xs)
  `(Statement.block $stmtList)

macro_rules
| `([await_block| await { $w }]) => do
  `(Statement.await [statement|$w])

macro_rules
| `([when_block| when ($n) { $stmt }]) => do
  `(Statement.await [qualified_name| $n] [statement| $stmt])

macro_rules
| `([try_catch| try $s:statement $[ $c:catch_block ]* ]) => do -- why doesn't the '+' pattern work?
  let initList <- `([])
  let catchList <- c.foldrM (init := initList) fun x xs => `([catch_block| $x]::$xs)
  `(Statement.try_catch [statement| $s] $catchList)

macro_rules
| `([catch_block| catch ( $n ) $s:statement ]) => `(CatchBlocks.single_statement [qualified_name| $n] [statement| $s])
| `([catch_block| catch ( $n ) $[$stmts:statement]* ]) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([statement| $x]::$xs)
  `(CatchBlocks.multiple_statements [qualified_name| $n] $stmtList)

macro_rules
| `([return_stmt| return $e ]) => `(Statement.return_stmt [expr| $e])

macro_rules
| `([dsl_transition| transition $i ]) => do `(Statement.transition $(Lean.quote $ i.getId.toString))

macro_rules
| `([dsl_term| ( $e:expr )]) => `( [expr|$e])
| `([dsl_term|  $c:call ]) => `( [call|$c])
| `([dsl_term|  $n:qualified_name ]) => `( [qualified_name|$n])
| `([dsl_term|  $c:constval ]) => `( [constval|$c])

macro_rules
| `([call| $n:qualified_name ( $e:expr_list )  ]) =>
  `(Factor.function_call [qualified_name| $n] [expr_list| $e])

macro_rules
| `([unuaryop| -$x]) => `(Factor.negation [dsl_term| $x])
| `([unuaryop| ~$x]) => `(Factor.logical_negation [dsl_term| $x])
| `([unuaryop| !$x]) => `(Factor.binary_negation [dsl_term| $x])

macro_rules
| `([binop| $x:dsl_term + $y:dsl_term ]) => `(Expr.add [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term - $y:dsl_term ]) => `(Expr.sub [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term * $y:dsl_term ]) => `(Expr.mul [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term / $y:dsl_term ]) => `(Expr.div [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term & $y:dsl_term ]) => `(Expr.binand [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term | $y:dsl_term ]) => `(Expr.binor [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term ^ $y:dsl_term ]) => `(Expr.binxor [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term << $y:dsl_term ]) => `(Expr.leftshift [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term >> $y:dsl_term ]) => `(Expr.rightshift [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term < $y:dsl_term ]) => `(Expr.less_than [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term > $y:dsl_term ]) => `(Expr.greater_than [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term <= $y:dsl_term ]) => `(Expr.leq [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term >= $y:dsl_term ]) => `(Expr.geq [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term == $y:dsl_term ]) => `(Expr.equal [expr|$x] [dsl_term|$y])
| `([binop| $x:dsl_term != $y:dsl_term ]) => `(Expr.not_equal [expr|$x] [dsl_term|$y])

macro_rules
| `([expr_list| $exprs:expr,*]) => do
  let initList <- `([])
  let valList <- exprs.getElems.foldrM (init := initList) fun x xs => `([expr| $x]::$xs)
  return valList

macro_rules
| `([parexpr| ($e:expr)]) => `([expr|$e])

macro_rules
| `([list| [$e:expr_list] ]) => `([expr_list|$e])

macro_rules
| `([structure_declaration| $id:typed_identifier $s ]) => `(Declaration.structure_declaration [typed_identifier|$id] [statement|$s])

macro_rules
| `([internal_func_decl| $id:typed_identifier ( $args ){ $s }]) =>
  `(Description.function_definition [typed_identifier|$id] [arg_list|$args] [statement|$s])

macro_rules
| `([arg_list| $fst:typed_identifier $rest,*]) => do
  let initList <- `([typed_identifier|$fst])
  let argList <- rest.getElems.foldrM (init := initList) fun x xs => `([typed_identifier|$x] :: $xs)
  return argList

macro_rules
| `([constval| $x:num ]) => `(Const.num_lit $x)
| `([constval| $x:str ]) => `(Const.str_lit $x)
