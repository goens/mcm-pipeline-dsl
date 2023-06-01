import Lean
import PipelineDsl.AST
import PipelineDsl.Preprocess
open Lean Lean.Syntax
open Lean.Elab
open Lean.Elab.Command
-- import Lean.Elab.Deriving.Basic
open Lean.Meta

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
declare_syntax_cat listen_handle
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
declare_syntax_cat structure_declaration (behavior := both)
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
syntax "state_queue" ident statement : structure_declaration

syntax &"state" ident statement : structure_declaration
syntax "controller_control_flow" ident statement : structure_declaration

syntax typed_identifier "(" arg_list ")" statement : internal_func_decl
syntax typed_identifier ("," typed_identifier)* : arg_list

syntax labeled_statement ";"? : statement
syntax dsl_transition ";"? : statement
syntax variable_declaration ";"? : statement
syntax assignment ";"? : statement
syntax conditional ";"? : statement
syntax "stall" "(" expr ")" : statement
syntax block ";"? : statement
syntax await_block ";"? : statement
syntax listen_handle ";"? : statement
syntax return_stmt ";"? : statement
syntax expr ";"? : statement

syntax typed_identifier ("=" expr)? : variable_declaration
syntax label statement : labeled_statement
syntax "result_write" : label
syntax "inst_source" : label
syntax "commit" : label
syntax qualified_name "=" expr : assignment -- maybe allow foo.bar?
syntax ident "=" expr : assignment
syntax "if" "(" expr ")" statement ("else"  statement)?  : conditional
syntax "{"  statement*  "}" : block
syntax  "await" (call)? "{"  (when_block)*  "}" : await_block -- this should be + but lean won't let me parse this
syntax "when"  ident "(" ident,* ")" "from" ident statement : when_block
syntax "listen"  statement  catch_block+ : listen_handle
syntax "handle"  qualified_name  "(" ident,* ")"  statement* : catch_block
syntax "return"  expr : return_stmt
syntax "transition"  ident : dsl_transition
syntax "reset"  ident : dsl_transition
syntax "complete"  ident : dsl_transition

syntax unuaryop : expr
syntax binop : expr
syntax parexpr : expr
syntax  list : expr
syntax dsl_term : expr

-- should be expression instead of access, but leaving this for now (left-recursion)
syntax "("  expr  ")" : dsl_term
syntax  call : dsl_term -- TODO: check why TLB.translate(this.virt_addr) doesn't parse
syntax qualified_name : dsl_term
-- TODO: support nested expressions in accesses
syntax constval : dsl_term
syntax ident : dsl_term
syntax qualified_name  "("  expr_list  ")" : call
syntax "prev<" expr_list ">" : dsl_term
syntax "next<" expr_list ">" : dsl_term
syntax unuaryop : dsl_term

-- TODO: add kwargs

syntax "!" dsl_term : unuaryop
syntax "~" dsl_term : unuaryop
syntax "-" dsl_term : unuaryop
syntax dsl_term "+"  dsl_term : binop
syntax dsl_term "-"  dsl_term : binop
syntax dsl_term "*"  dsl_term : binop
syntax dsl_term "/"  dsl_term : binop
syntax dsl_term "&"  dsl_term : binop
syntax dsl_term "|"  dsl_term : binop
syntax dsl_term "^"  dsl_term : binop
syntax dsl_term "<"  dsl_term : binop
syntax dsl_term ">"  dsl_term : binop
syntax dsl_term "<=" dsl_term : binop
syntax dsl_term ">=" dsl_term : binop
syntax dsl_term "<<" dsl_term : binop
syntax dsl_term ">>" dsl_term : binop
syntax dsl_term "==" dsl_term : binop
syntax dsl_term "!=" dsl_term : binop
syntax dsl_term "+"  binop : binop
syntax dsl_term "-"  binop : binop
syntax dsl_term "*"  binop : binop
syntax dsl_term "/"  binop : binop
syntax dsl_term "&"  binop : binop
syntax dsl_term "|"  binop : binop
syntax dsl_term "^"  binop : binop
syntax dsl_term "<"  binop : binop
syntax dsl_term ">"  binop : binop
syntax dsl_term "<=" binop : binop
syntax dsl_term ">=" binop : binop
syntax dsl_term "<<" binop : binop
syntax dsl_term ">>" binop : binop
syntax dsl_term "==" binop : binop
syntax dsl_term "!=" binop : binop

syntax expr,* : expr_list
syntax "("  expr ")" : parexpr
syntax "["  expr_list "]" : list

-- parsing


private def mkParseFun {α : Type} (syntaxcat : Name) (ntparser : Syntax → Except String α) :
String → Environment → Except String α := λ s env => do
  ntparser (← Parser.runParserCategory env syntaxcat s)

private def mkNonTerminalParser {α : Type} [Inhabited α] (syntaxcat : Name) (ntparser : Syntax → Except String α)
(s : String) (env : Environment) : Option String × α :=
  let parseFun := mkParseFun syntaxcat ntparser
  match parseFun s env with
    | .error msg => (some msg, default)
    | .ok    p   => (none, p)

mutual
partial def mkConstval : Syntax → Except String Const
| `(constval| $x:num ) => return Const.num_lit x.getNat
| `(constval| $x:str ) => return Const.str_lit x.getString
| _ => throw "error parsing constant value"


partial def mkUnuaryop : Syntax → Except String Term
  | `(unuaryop| -$x) => Except.map Term.negation (mkTerm x)
  | `(unuaryop| ~$x) => Except.map Term.logical_negation (mkTerm x)
  | `(unuaryop| !$x) => Except.map Term.binary_negation (mkTerm x)
  | u => throw s!"error parsing unuary operator (not implemented), {u}"

partial def mkQualifiedName : Syntax → Except String QualifiedName
  | `(qualified_name| $x:ident $[. $xs:ident ]* ) => do
    let xStr : String := x.getId.toString
    let xsList <- xs.foldlM (init := [xStr]) fun xs x => return (xs ++ [x.getId.toString])
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
  | `(expr| $x:unuaryop ) => Except.map Expr.some_term $ mkUnuaryop x
  | `(expr| $x:binop ) => mkBinop x
  | `(expr| $x:parexpr ) => mkParExpr x
  | `(expr| $x:list ) => mkList x
  | `(expr| $x:dsl_term ) => Except.map Expr.some_term $ mkTerm x
  | _ => throw "error, parser unimplemented"

partial def mkParExpr : Syntax → Except String Expr
  | `(parexpr| ($e:expr)) => mkExpr e
  | _ => throw "error parsing expression in parenthesis"

partial def mkBinop : Syntax → Except String Expr
  | `(binop| $x:dsl_term + $y:dsl_term ) =>  Expr.add <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term - $y:dsl_term ) => Expr.sub <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term * $y:dsl_term ) => Expr.mul <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term / $y:dsl_term ) => Expr.div <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term & $y:dsl_term ) => Expr.binand <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term | $y:dsl_term ) => Expr.binor <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term ^ $y:dsl_term ) => Expr.binxor <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term << $y:dsl_term ) => Expr.leftshift <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term >> $y:dsl_term ) => Expr.rightshift <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term < $y:dsl_term ) => Expr.less_than <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term > $y:dsl_term ) => Expr.greater_than <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term <= $y:dsl_term ) => Expr.leq <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term >= $y:dsl_term ) => Expr.geq <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term == $y:dsl_term ) => Expr.equal <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term != $y:dsl_term ) => Expr.not_equal <$> (mkTerm x) <*> (mkTerm y)
  | `(binop| $x:dsl_term + $y:binop ) =>  Expr.add <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term - $y:binop ) => Expr.sub <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term * $y:binop ) => Expr.mul <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term / $y:binop ) => Expr.div <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term & $y:binop ) => Expr.binand <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term | $y:binop ) => Expr.binor <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term ^ $y:binop ) => Expr.binxor <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term << $y:binop ) => Expr.leftshift <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term >> $y:binop ) => Expr.rightshift <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term < $y:binop ) => Expr.less_than <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term > $y:binop ) => Expr.greater_than <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term <= $y:binop ) => Expr.leq <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term >= $y:binop ) => Expr.geq <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term == $y:binop ) => Expr.equal <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)
  | `(binop| $x:dsl_term != $y:binop ) => Expr.not_equal <$> (mkTerm x) <*> (Term.expr <$> mkBinop y)

  | _ => throw "error parsing binary operator"


partial def mkCall : Syntax → Except String Term
  | `(call| $n:qualified_name ( $e:expr_list )  ) =>
    Term.function_call <$> (mkQualifiedName n) <*> (mkExprList e)
  | _ => throw "error parsing function call"

partial def mkTerm : Syntax → Except String Term
  | `(dsl_term|  $c:constval ) => Except.map (λ const => Term.const const) (mkConstval c)
  | `(dsl_term|  $i:ident ) => return Term.var i.getId.toString
  | `(dsl_term|  $n:qualified_name ) => Except.map (λ x => Term.qualified_var x) (mkQualifiedName n)
  | `(dsl_term|  $c:call ) => mkCall c
  | `(dsl_term|  prev< $exprs > ) => Term.relative_entry .Previous <$> mkExprList exprs
  | `(dsl_term|  next< $exprs > ) => Term.relative_entry .Next <$> mkExprList exprs
  | `(dsl_term|  ($e:expr) ) => return Term.expr (← mkExpr e)
  | other => throw s!"error parsing term {other}"

partial def mkTypedIdentifier : Syntax → Except String TypedIdentifier
  | `(typed_identifier| $t:ident $x:ident ) => do
    let tStr : String := t.getId.toString
    let xStr : String := x.getId.toString
    return TypedIdentifier.mk tStr xStr
  | _ => throw "error parsing typed identifier"

-- this is a really weird behavior, why won't it work with `(declaration | $x:structure_declaration) ?
partial def mkDescription : Syntax → Except String Description
  | `(declaration| $x:structure_declaration ) => mkStructureDeclaration x
  | `(declaration| $x:internal_func_decl ) => mkInternalFuncDecl x
  | _ => throw "error parsing declaration"

partial def mkVariableDeclaration : Syntax → Except String Statement
  | `(variable_declaration| $x:typed_identifier = $e ) => Statement.value_declaration <$> (mkTypedIdentifier x)  <*> (mkExpr e)
  | `(variable_declaration| $x:typed_identifier) => Except.map Statement.variable_declaration (mkTypedIdentifier x)
  | _ => throw "error parsing variable declaration"

partial def mkLabeledStatement : Syntax → Except String Statement
  | `(labeled_statement| $l:label $s ) => Statement.labelled_statement <$> (mkLabel l) <*> (mkStatement s)
  | _ => throw "error parsing labeled statement"

partial def mkLabel : Syntax → Except String Label
  | `(label| result_write ) => return Label.result_write
  | `(label| inst_source ) => return Label.inst_source
  | `(label| commit ) => return Label.commit
  | _ => throw "error parsing label"

partial def mkAssigmnent : Syntax → Except String Statement
  | `(assignment| $q:qualified_name = $e ) => Statement.variable_assignment <$> (mkQualifiedName q) <*> (mkExpr e)
  | `(assignment| $i:ident = $e ) => let name := (QualifiedName.mk [i.getId.toString])
    Except.map (Statement.variable_assignment name) (mkExpr e)
  | _ => throw "error parsing assignment"

partial def mkConditional : Syntax → Except String Conditional
  | `(conditional| if ( $e ) $s else $es ) => Conditional.if_else_statement <$>
    (mkExpr e) <*> (mkStatement s) <*> (mkStatement es)
  | `(conditional| if ( $e ) $s ) => Conditional.if_statement <$> (mkExpr e) <*> (mkStatement s)
  | _ => throw "error parsing assignment"

partial def mkBlock : Syntax → Except String Statement
  | `(block| { $[ $stmts ]* }) => do
    let stmtList <- stmts.foldrM (init := []) fun x xs => do
      let s <- mkStatement x
      return s::xs
    return Statement.block stmtList
  | u => throw s!"error parsing block statement, unknown: {u}"


partial def mkAwaitBlock : Syntax → Except String Statement
| `(await_block| await $(opcall)? { $[ $w:when_block ]* }) => do
  let whenList <- w.foldrM (init := []) fun x xs => do
    let wb <- mkWhenBlock x
    return (wb::xs)
  let call <-  match opcall with
    | none => Except.ok none
    | some c => do
    let callObj <- mkCall c
    Except.ok $ some callObj
  return Statement.await call whenList
| _ => throw "error parsing await block statement"

partial def mkWhenBlock : Syntax → Except String Statement
| `(when_block| when $n($[$args],*) from $src $stmt ) => do
  let argsArr := args.map (λ x => x.getId.toString)
  let createNodeFun := λ nameNode stmtNode => Statement.when nameNode  argsArr.toList stmtNode
  let qn := QualifiedName.mk [src.getId.toString, n.getId.toString]
  createNodeFun qn <$> (mkStatement stmt)
| u => throw s!"error parsing when block statement, unknown {u}"

partial def mkListenhandle : Syntax → Except String Statement
  | `(listen_handle| listen $s:statement $[ $c:catch_block ]* ) => do -- why doesn't the '+' pattern work?
    let catchList <- c.foldrM (init := []) fun x xs => do
      let cb <- mkHandleblock x
      return (cb::xs)
    Except.map (λ stmt => Statement.listen_handle stmt catchList) (mkStatement s)
  | _ => throw "error parsing try-catch statement"

partial def mkHandleblock : Syntax → Except String HandleBlock
  | `(catch_block| handle  $n( $[$args],* ) $s:statement ) =>
  let argsArr := args.map (λ x => x.getId.toString)
  let createNodeFun := λ nameNode stmtNode => HandleBlock.mk nameNode  argsArr.toList stmtNode
  createNodeFun <$> (mkQualifiedName n) <*> (mkStatement s)
  | _ => throw "error parsing catch block"

partial def mkStatement : Syntax → Except String Statement
  | `(statement| $x:labeled_statement $[;]? ) => mkLabeledStatement x
  | `(statement| $x:dsl_transition $[;]? ) => mkTransition x
  | `(statement| $x:variable_declaration $[;]? ) => mkVariableDeclaration x
  | `(statement| $x:assignment $[;]? ) => mkAssigmnent x
  | `(statement| $x:conditional $[;]? ) => Except.map Statement.conditional_stmt (mkConditional x) -- feels unnecessary
  | `(statement| $x:block $[;]? ) => mkBlock x
  | `(statement| $x:await_block $[;]? ) => mkAwaitBlock x
  | `(statement| $x:listen_handle $[;]? ) => mkListenhandle x
  | `(statement| $x:return_stmt $[;]? ) => mkReturnStmt x
  | `(statement| $x:expr $[;]? ) => Except.map Statement.stray_expr (mkExpr x)
  | u => throw s!"error parsing statement, unknown statement {u}"

partial def mkReturnStmt : Syntax → Except String Statement
  | `(return_stmt| return $e ) => Except.map Statement.return_stmt (mkExpr e)
  | _ => throw "error parsing return statement"

partial def mkTransition : Syntax → Except String Statement
  | `(dsl_transition| transition $i ) => return Statement.transition i.getId.toString
  | `(dsl_transition| reset $i ) => return Statement.reset i.getId.toString
  | `(dsl_transition| complete $i ) => return Statement.complete i.getId.toString
  | s => throw s!"error parsing transition statement: {s}"

partial def mkList : Syntax → Except String Expr
  | `(list| [$e:expr_list] ) => Except.map Expr.list (mkExprList e)
  | _ => throw "error parsing expression list"

partial def mkStructureDeclaration : Syntax → Except String Description
  | `(structure_declaration| controller $id:ident $s ) =>
    Except.map (Description.controller id.getId.toString) (mkStatement s)
  | `(structure_declaration| state_queue $id:ident $s ) => -- TODO: difference to controller?
    Except.map (Description.controller id.getId.toString) (mkStatement s)
  | `(structure_declaration| controller_entry $id:ident $s ) =>
    Except.map (Description.entry id.getId.toString) (mkStatement s)
  | `(structure_declaration| controller_control_flow $id:ident $s ) =>
    Except.map (Description.control_flow id.getId.toString) (mkStatement s)
  | `(structure_declaration| state $id:ident $s:statement) =>
    Except.map (Description.state id.getId.toString) (mkStatement s)
  | m => dbg_trace m; throw "error parsing structure declaration"

partial def mkInternalFuncDecl : Syntax → Except String Description
  | `(internal_func_decl| $id:typed_identifier ( $args ) $s ) =>
    Description.function_definition <$> (mkTypedIdentifier id) <*> (mkArgList args) <*> (mkStatement s)
  | _ => throw "error parsing internal function declaration"

partial def mkArgList : Syntax → Except String (List TypedIdentifier)
  | `(arg_list| $fst:typed_identifier $[, $rest:typed_identifier]*) => do
      let initId <- mkTypedIdentifier fst
      let argList <- rest.foldrM (init := [initId]) fun x xs => do
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

-- eDSL

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
syntax "[listen_handle|" listen_handle "]" : term
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

-- parse functions
def parseConstval := mkNonTerminalParser `constval mkConstval
def parseCall := mkNonTerminalParser `call mkCall
def parseAssignment := mkNonTerminalParser `assignment mkAssigmnent
def parseStatement := mkNonTerminalParser `statement mkStatement
def parseDescription := mkNonTerminalParser `declaration mkDescription
def parseStructureDeclaration := mkNonTerminalParser `structure_declaration mkDescription
def parseExpr := mkNonTerminalParser `expr mkExpr
def parseFile := mkNonTerminalParser `file mkAST
def parse := parseFile

-- Round-tripping
def mkRoundTripFun {α : Type} [ToString α] :
  (String → Environment → Option String × α) → Environment → α → Option String × α
  | parseFun, env, a =>
    let rtString := toString a
    let rtLines := rtString.split λ c => c == '\n'
    let rtLArray := Array.mk rtLines
    let rtPreprocessed := preprocess rtLArray
    let rt := rtPreprocessed.foldl (λ s₁ s₂ => s₁ ++ "\n" ++ s₂) ""
    --dbg_trace s!"round-trip preprocessed: {rt}"
    parseFun rt env

def roundTrip := mkRoundTripFun parse
