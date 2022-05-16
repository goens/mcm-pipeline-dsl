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

private def liftExcept3 {α β γ δ : Type} : (α → β → γ → δ) → Except String α → Except String β →
Except String γ → Except String δ :=
  λ f a b c => do
  match a with
  | .error msg => .error msg
  | .ok r₁ => match b with
    | .error msg => .error msg
    | .ok r₂ => match c with
      | .error msg => .error msg
      | .ok r₃ => return f r₁ r₂ r₃

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
  | `(expr| $x:binop ) => mkBinop x
  | `(expr| $x:parexpr ) => mkParExpr x
  -- | `(expr| $x:list ) => liftExcept Expr.list mkList x
  | `(expr| $x:dsl_term ) => liftExcept Expr.some_term $ mkTerm x
  | _ => throw "error, parser unimplemented"

partial def mkParExpr : Syntax → Except String Expr
  | `(parexpr| ($e:expr)) => mkExpr e
  | _ => throw "error parsing expression in parenthesis"

partial def mkBinop : Syntax → Except String Expr
  | `(binop| $x:dsl_term + $y:dsl_term ) => liftExcept2 Expr.add (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term - $y:dsl_term ) => liftExcept2 Expr.sub (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term * $y:dsl_term ) => liftExcept2 Expr.mul (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term / $y:dsl_term ) => liftExcept2 Expr.div (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term & $y:dsl_term ) => liftExcept2 Expr.binand (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term | $y:dsl_term ) => liftExcept2 Expr.binor (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term ^ $y:dsl_term ) => liftExcept2 Expr.binxor (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term << $y:dsl_term ) => liftExcept2 Expr.leftshift (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term >> $y:dsl_term ) => liftExcept2 Expr.rightshift (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term < $y:dsl_term ) => liftExcept2 Expr.less_than (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term > $y:dsl_term ) => liftExcept2 Expr.greater_than (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term <= $y:dsl_term ) => liftExcept2 Expr.leq (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term >= $y:dsl_term ) => liftExcept2 Expr.geq (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term == $y:dsl_term ) => liftExcept2 Expr.equal (mkTerm x) (mkTerm y)
  | `(binop| $x:dsl_term != $y:dsl_term ) => liftExcept2 Expr.not_equal (mkTerm x) (mkTerm y)
  | _ => throw "error parsing binary operator"


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

partial def mkTypedIdentifier : Syntax → Except String TypedIdentifier
  | `(typed_identifier| $t:ident $x:ident ) => do
    let tStr : String := t.getId.toString
    let xStr : String := x.getId.toString
    return TypedIdentifier.mk tStr xStr
  | _ => throw "error parsing typed identifier"


partial def mkDescription : Syntax → Except String Description
  | `(declaration| $x:structure_declaration ) => mkStructureDeclaration x
  | `(declaration| $x:internal_func_decl ) => mkInternalFuncDecl x
  | _ => throw "error parsing declaration"

partial def mkVariableDeclaration : Syntax → Except String Statement
  | `(variable_declaration| $x:typed_identifier = $e ) => liftExcept2 Statement.value_declaration (mkTypedIdentifier x) (mkExpr e)
  | `(variable_declaration| $x:typed_identifier) => liftExcept Statement.variable_declaration (mkTypedIdentifier x)
  | _ => throw "error parsing variable declaration"

partial def mkLabeledStatement : Syntax → Except String Statement
  | `(labeled_statement| $l:label $s ) => liftExcept2 Statement.labelled_statement (mkLabel l) (mkStatement s)
  | _ => throw "error parsing labeled statement"

partial def mkLabel : Syntax → Except String Label
  | `(label| result_write ) => return Label.result_write
  | _ => throw "error parsing label"

partial def mkAssigmnent : Syntax → Except String Statement
  | `(assignment| $q:qualified_name = $e ) => liftExcept2 Statement.variable_assignment (mkQualifiedName q) (mkExpr e)
  | `(assignment| $i:ident = $e ) => let name := (QualifiedName.mk [i.getId.toString])
    liftExcept (Statement.variable_assignment name) (mkExpr e)
  | _ => throw "error parsing assignment"

partial def mkConditional : Syntax → Except String Conditional
  | `(conditional| if ( $e ) $s else $es ) => liftExcept3 Conditional.if_else_statement
    (mkExpr e) (mkStatement s) (mkStatement es)
  | `(conditional| if ( $e ) $s ) => liftExcept2 Conditional.if_statement (mkExpr e) (mkStatement s)
  | _ => throw "error parsing assignment"

partial def mkBlock : Syntax → Except String Statement
  | `(block| { $[ $stmts ]* }) => do
    let stmtList <- stmts.foldrM (init := []) fun x xs => do
      let s <- mkStatement x
      return s::xs
    return Statement.block stmtList
  | _ => throw "error parsing block statement"


partial def mkAwaitBlock : Syntax → Except String Statement
| `(await_block| await { $w }) => liftExcept Statement.await (mkStatement w)
| _ => throw "error parsing await block statement"

partial def mkWhenBlock : Syntax → Except String Statement
| `(when_block| when ($n) { $stmt }) => liftExcept2 Statement.when
  (mkQualifiedName n) (mkStatement stmt)
  | _ => throw "error parsing when block statement"

partial def mkTryCatch : Syntax → Except String Statement
  | `(try_catch| try $s:statement $[ $c:catch_block ]* ) => do -- why doesn't the '+' pattern work?
    let catchList <- c.foldrM (init := []) fun x xs => do
      let cb <- mkCatchBlock x
      return (cb::xs)
    liftExcept (λ stmt => Statement.try_catch stmt catchList) (mkStatement s)
  | _ => throw "error parsing try-catch statement"

partial def mkCatchBlock : Syntax → Except String CatchBlock
  | `(catch_block| catch ( $n ) $s:statement ) => liftExcept2 CatchBlock.mk
    (mkQualifiedName n) (mkStatement s)
  | _ => throw "error parsing catch block"

partial def mkStatement : Syntax → Except String Statement
| `(statement| $x:labeled_statement $[;]? ) => mkLabeledStatement x
| `(statement| $x:dsl_transition $[;]? ) => mkTransition x
| `(statement| $x:variable_declaration $[;]? ) => mkVariableDeclaration x
| `(statement| $x:assignment $[;]? ) => mkAssigmnent x
| `(statement| $x:conditional $[;]? ) => liftExcept Statement.conditional_stmt (mkConditional x) -- feels unnecessary
| `(statement| $x:block $[;]? ) => mkBlock x
| `(statement| $x:await_block $[;]? ) => mkAwaitBlock x
| `(statement| $x:try_catch $[;]? ) => mkTryCatch x
| `(statement| $x:return_stmt $[;]? ) => mkReturnStmt x
| `(statement| $x:expr $[;]? ) => liftExcept Statement.stray_expr (mkExpr x)
| _ => throw "error parsing statement"

partial def mkReturnStmt : Syntax → Except String Statement
  | `(return_stmt| return $e ) => liftExcept Statement.return_stmt (mkExpr e)
  | _ => throw "error parsing return statement"

partial def mkTransition : Syntax → Except String Statement
  | `(dsl_transition| transition $i ) => return Statement.transition i.getId.toString
  | _ => throw "error parsing transition statement"

partial def mkList : Syntax → Except String Expr
  | `(list| [$e:expr_list] ) => liftExcept Expr.list (mkExprList e)
  | _ => throw "error parsing expression list"

partial def mkStructureDeclaration : Syntax → Except String Description
  | `(structure_declaration| controller $id:ident $s ) =>
    liftExcept (Description.controller id.getId.toString) (mkStatement s)
  | `(structure_declaration| controller_entry $id:ident $s ) =>
    liftExcept (Description.entry id.getId.toString) (mkStatement s)
  | `(structure_declaration| controller_control_flow $id:ident $s ) =>
    liftExcept (Description.control_flow id.getId.toString) (mkStatement s)
  | `(structure_declaration| transition $id:ident $s ) =>
    liftExcept (Description.transition id.getId.toString) (mkStatement s)
  | _ => throw "error parsing structure declaration"

partial def mkInternalFuncDecl : Syntax → Except String Description
  | `(internal_func_decl| $id:typed_identifier ( $args ){ $s }) =>
    liftExcept3 Description.function_definition (mkTypedIdentifier id) (mkArgList args) (mkStatement s)
  | _ => throw "error parsing internal function declaration"

partial def mkArgList : Syntax → Except String (List TypedIdentifier)
  | `(arg_list| $fst:typed_identifier $rest,*) => do
      let initId <- mkTypedIdentifier fst
      let argList <- rest.getElems.foldrM (init := [initId]) fun x xs => do
        let xid <- mkTypedIdentifier x
        return (xid::xs)
      return argList
  | _ => throw "error parsing argument list"

partial def mkAST : Syntax → Except String AST
  | `(file|$[$decls:declaration]*) => do
    let declList <- decls.foldrM (init := []) fun x xs => do
      let desc <- mkDescription x
      return (desc::xs)
    return AST.structure_descriptions declList
  | _ => throw "error: can't parse file"
end

def parseConstval := mkNonTerminalParser `constval mkConstval
def parseCall := mkNonTerminalParser `call mkCall
def parseFile := mkNonTerminalParser `file mkAST
def parse := parseFile
