import Lean
import PipelineDsl.AST
open Lean Lean.Syntax

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
syntax "controller_entry" ident statement : structure_declaration
syntax "controller" ident statement : structure_declaration
syntax "transition" ident statement : structure_declaration
syntax "controller_control_flow" ident statement : structure_declaration

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
syntax ident "=" expr : assignment 
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
syntax ident : dsl_term
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

-- parsing


/- private -/ def mkParseFun {α : Type} (syntaxcat : Name) (ntparser : Syntax → Except String α) :
String → Environment → Except String α := λ s env => do
  ntparser (← Parser.runParserCategory env syntaxcat s)

/- private -/ def mkNonTerminalParser {α : Type} [Inhabited α] (syntaxcat : Name) (ntparser : Syntax → Except String α)
(s : String) (env : Environment) : Option String × α :=
  let parseFun := mkParseFun syntaxcat ntparser
  match parseFun s env with
    | .error msg => (some msg, default)
    | .ok    p   => (none, p)

private def liftExcept {α β : Type} : (α → β) → Except String α → Except String β :=
λ f c => do
  match c with
  | .error msg => .error msg
  | .ok r => return f r

private def liftExcept2 {α β γ : Type} : (α → β → γ) → Except String α → Except String β → Except String γ :=
  λ f c d => do
    match c with
    | .error msg => .error msg
    | .ok r₁ => match d with
      | .error msg => .error msg
      | .ok r₂ => return f r₁ r₂

mutual
partial def mkConstval : Syntax → Except String Const
| `(constval| $x:num ) => return Const.num_lit x.toNat
| `(constval| $x:str ) => match x.isStrLit? with
  | some s => return Const.str_lit s
  | none   => unreachable! -- because of `s:str`
| _ => throw "error parsing constant value"


partial def mkUnuaryop : Syntax → Except String Term
  | `(unuaryop| -$x) => liftExcept Term.negation (mkTerm x)
  -- | `(unuaryop| ~$x) => Term.logical_negation x
  -- | `(unuaryop| !$x) => Term.binary_negation x
  | _ => throw "error parsing unuary operator (not implemented)"

partial def mkQualifiedName : Syntax → Except String QualifiedName
  | `(qualified_name| $x:ident $[. $xs:ident ]* ) => do
    let xStr : String := x.getId.toString
    let xsList <- xs.foldrM (init := [xStr]) fun x xs => return (x.getId.toString :: xs)
    return QualifiedName.mk xsList
  | _ => throw "error parsing qualified name"

partial def mkExprList : Syntax → Except String (List Expr)
  | `(expr_list| $exprs:expr,*) => do
    let joinFun := fun x xs => do -- lift this pattern into a HOF
      let res := mkExpr x
      match res with
        | .error msg => .error msg
        | .ok exp => return (exp::xs)
    let valList <- exprs.getElems.foldrM (init := []) joinFun
    return valList
  | _ => throw "error parsing expression list"

partial def mkExpr : Syntax → Except String Expr
  | `(expr| $x:unuaryop ) => liftExcept Expr.some_term $ mkUnuaryop x
  -- | `(expr| $x:binop ) => mkbinop x
  -- | `(expr| $x:parexpr ) => mkparexpr x
  -- | `(expr| $x:list ) => mklist x
  | `(expr| $x:dsl_term ) => liftExcept Expr.some_term $ mkTerm x
  | _ => throw "error, parser unimplemented"

partial def mkCall : Syntax → Except String Term
  | `(call| $n:qualified_name ( $e:expr_list )  ) =>
    liftExcept2 Term.function_call (mkQualifiedName n) (mkExprList e)
  | _ => throw "error parsing function call"

partial def mkTerm : Syntax → Except String Term
  | `(dsl_term|  $c:constval ) => liftExcept (λ const => Term.const const) (mkConstval c)
  | `(dsl_term|  $i:ident ) => match i.isIdent with
    | true => Except.ok $ Term.var i.getId.toString
    | false => Except.error "error parsing variable"
  | `(dsl_term|  $n:qualified_name ) => liftExcept (λ x => Term.qualified_var x) (mkQualifiedName n)
  | `(dsl_term|  $c:call ) => mkCall c
  | _ => throw "error parsing term"

end

def parseConstval := mkNonTerminalParser `constval mkConstval
def parseCall := mkNonTerminalParser `call mkCall
def parse := parseCall

/-
def mkAST : Syntax → Except String AST
  | `(file|$[$decls:declaration]*) => do
    let declList <- decls.foldrM (init := []) fun x xs => (mkDescription x)::xs
    return AST.structure_descriptions declList
  | _ => throw "error: can't parse file"

def mkStatement : Syntax → Except String Statement
| `(statement| $x:labeled_statement $[;]? ) => mkLabeledStatement x
| `(statement| $x:dsl_transition $[;]? ) => mkDslTransition x
| `(statement| $x:variable_declaration $[;]? ) => mkVariableDeclaration x
| `(statement| $x:assignment $[;]? ) => mkAssignment x
| `(statement| $x:conditional $[;]? ) => Statement.conditional (mkConditional x) -- feels unnecessary
| `(statement| $x:block $[;]? ) => mkBlock x
| `(statement| $x:await_block $[;]? ) => mkAwaitBlock x
| `(statement| $x:try_catch $[;]? ) => mkTryCatch x
| `(statement| $x:return_stmt $[;]? ) => mkReturnStmt x
| `(statement| $x:expr $[;]? ) => Statement.stray_expr (mkExpr x)


def mkCHANGEME : Syntax → Except String CHANGEME
| `(typed_identifier| $t:ident $x:ident ) => do
  let tStr : String := t.getId.toString
  let xStr : String := x.getId.toString
  let tSyn : Lean.Syntax := Lean.quote tStr
  let xSyn : Lean.Syntax := Lean.quote xStr
  `(TypedIdentifier.mk $tSyn $xSyn)

def mkDescription : Syntax → Except String Description
| `(declaration| $x:structure_declaration ) => mkStructureDeclaration x
| `(declaration| $x:internal_func_decl ) => mkinternal_func_decl x

def mkCHANGEME : Syntax → Except String CHANGEME
| `(variable_declaration| $x:typed_identifier = $e ) => `(Statement.declaration [typed_identifier| $x] [expr|$e])
| `(variable_declaration| $x:typed_identifier) => `(Statement.value_declaration [typed_identifier| $x])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(labeled_statement| $l:label $s ) => `(Statement.labelled_statement [label| $l] [statement| $s])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(label| result_write ) => `(Label.result_write)

def mkCHANGEME : Syntax → Except String CHANGEME
| `(assignment| $q:qualified_name = $e ) => `(Statement.variable_assignment [qualified_name| $q] [expr| $e])
| `(assignment| $i:ident = $e ) => `(Statement.variable_assignment (QualifiedName.mk [$(Lean.quote $ i.getId.toString)]) [expr| $e])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(conditional| if ( $e ) $s else $es ) => `(Conditional.if_else_statement [expr| $e] [statement| $s] [statement| $es])
| `(conditional| if ( $e ) $s ) => `(Conditional.if_else_statement [expr| $e] [statement| $s])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(block| { $[ $stmts ]* }) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([statement| $x]::$xs)
  `(Statement.block $stmtList)

def mkCHANGEME : Syntax → Except String CHANGEME
| `(await_block| await { $w }) => do
  `(Statement.await [statement|$w])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(when_block| when ($n) { $stmt }) => do
  `(Statement.await [qualified_name| $n] [statement| $stmt])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(try_catch| try $s:statement $[ $c:catch_block ]* ) => do -- why doesn't the '+' pattern work?
  let initList <- `([])
  let catchList <- c.foldrM (init := initList) fun x xs => `([catch_block| $x]::$xs)
  `(Statement.try_catch [statement| $s] $catchList)

def mkCHANGEME : Syntax → Except String CHANGEME
| `(catch_block| catch ( $n ) $s:statement ) => `(CatchBlocks.single_statement [qualified_name| $n] [statement| $s])
| `(catch_block| catch ( $n ) $[$stmts:statement]* ) => do
  let initList <- `([])
  let stmtList <- stmts.foldrM (init := initList) fun x xs => `([statement| $x]::$xs)
  `(CatchBlocks.multiple_statements [qualified_name| $n] $stmtList)

def mkCHANGEME : Syntax → Except String CHANGEME
| `(return_stmt| return $e ) => `(Statement.return_stmt [expr| $e])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(dsl_transition| transition $i ) => do `(Statement.transition $(Lean.quote $ i.getId.toString))


def mkCHANGEME : Syntax → Except String CHANGEME
| `(binop| $x:dsl_term + $y:dsl_term ) => `(Expr.add [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term - $y:dsl_term ) => `(Expr.sub [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term * $y:dsl_term ) => `(Expr.mul [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term / $y:dsl_term ) => `(Expr.div [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term & $y:dsl_term ) => `(Expr.binand [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term | $y:dsl_term ) => `(Expr.binor [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term ^ $y:dsl_term ) => `(Expr.binxor [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term << $y:dsl_term ) => `(Expr.leftshift [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term >> $y:dsl_term ) => `(Expr.rightshift [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term < $y:dsl_term ) => `(Expr.less_than [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term > $y:dsl_term ) => `(Expr.greater_than [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term <= $y:dsl_term ) => `(Expr.leq [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term >= $y:dsl_term ) => `(Expr.geq [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term == $y:dsl_term ) => `(Expr.equal [dsl_term|$x] [dsl_term|$y])
| `(binop| $x:dsl_term != $y:dsl_term ) => `(Expr.not_equal [dsl_term|$x] [dsl_term|$y])


def mkCHANGEME : Syntax → Except String CHANGEME
| `(parexpr| ($e:expr)) => mkexpr |$e

def mkCHANGEME : Syntax → Except String CHANGEME
| `(list| [$e:expr_list] ) => mkexpr_list |$e

def mkCHANGEME : Syntax → Except String CHANGEME
| `(structure_declaration| controller $id:ident $s ) =>
  `(Description.controller $(Lean.quote $ id.getId.toString) [statement|$s])
| `(structure_declaration| controller_entry $id:ident $s ) =>
  `(Description.entry $(Lean.quote $ id.getId.toString) [statement|$s])
| `(structure_declaration| controller_control_flow $id:ident $s ) =>
  `(Description.control_flow $(Lean.quote $ id.getId.toString) [statement|$s])
| `(structure_declaration| transition $id:ident $s ) =>
  `(Description.transition $(Lean.quote $ id.getId.toString) [statement|$s])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(internal_func_decl| $id:typed_identifier ( $args ){ $s }) =>
  `(Description.function_definition [typed_identifier|$id] [arg_list|$args] [statement|$s])

def mkCHANGEME : Syntax → Except String CHANGEME
| `(arg_list| $fst:typed_identifier $rest,*) => do
  let initList <- `([typed_identifier|$fst])
  let argList <- rest.getElems.foldrM (init := initList) fun x xs => `([typed_identifier|$x] :: $xs)
  return argList

-/
