import Lean
open Lean
open Syntax

-- Utility:
def Monad.kleisliLeftToRight {A B C : Type} [Monad M] : (A → M B) → (B → M C) → A → M C
 | f₁, f₂, a => do
   let b <- f₁ a
   f₂ b

infixl:55 " >=> "  => Monad.kleisliLeftToRight

namespace Murϕ

abbrev ID := String
abbrev BinOp := String
-- Binops: ?: -> | & ! < <= = != >= > + - * / %

mutual

/-
<decl> ::=	const { <constdecl> ; }
         |	type { <typedecl> ; }
         |	var { <vardecl> ; }

<constdecl> ::=	<ID> : <expr>
<typedecl> ::=	<ID> : <typeExpr>
<vardecl>  ::=	<ID> { , <ID> } : <typeExpr>
-/
inductive Decl
  | const : ID → Expr → Decl
  | type  : ID → TypeExpr → Decl
  | var   : List ID → TypeExpr → Decl

/-
  <typeExpr> ::=	<ID>		-- a previously defined type.
  <typeExpr> ::=	<expr> .. <expr>	-- Integer subrange.
  <typeExpr> ::=	enum \{ <ID> {, <ID> } \} -- enumeration.
  <typeExpr> ::=	record { <vardecl> } end
  <typeExpr> ::=	array \[ <typeExpr> \] of <typeExpr>
-/
inductive TypeExpr
  | previouslyDefined : ID → TypeExpr
  | integerSubrange : Expr → Expr → TypeExpr
  | enum : List ID → TypeExpr
  | record : List Decl → TypeExpr -- Note: this is more premissive than the grammar, should be ok though
  | array : TypeExpr → TypeExpr → TypeExpr

/-
<formal> ::=	[var] <ID> { , <ID> } : <typeExpr>
-/
inductive Formal
  | mk : Bool → List ID → TypeExpr → Formal

/-
<procdecl> ::=	<procedure>
             | 	<function>

<procedure> ::=	procedure <ID> \( [ <formal> { ; <formal> } ] \) ;
                [ { <decl> } begin ] [ <stmts> ] end;

<function> ::=	function <ID> \( [ <formal> { ; <formal> } ] \)
                : <typeExpr>;
                [ { <decl> } begin ] [ <stmts> ] end;
-/
inductive ProcDecl
  | procedure : ID → List Formal → List Decl → List Statement → ProcDecl
  | function : ID → List Formal → TypeExpr → List Decl → List Statement → ProcDecl

/-
<expr> :=  \( expr \)
| <designator>
| <integer-constant>
| <ID> \( <actuals> \)		-- a function call.
| forall <quantifier>
    do <expr> endforall		-- universal quantification.
| exists <quantifier>
    do <expr> endexists		-- existential quantification.
| <expr> + <expr>
| <expr> - <expr>
| <expr> * <expr>		-- multiplication.
| <expr> / <expr>		-- integer division.
| <expr> % <expr>		-- remainder.
| ! <expr>			-- logical negation.
| <expr> | <expr>		-- logical disjunction.
| <expr> & <expr>		-- logical conjunction.
| <expr> -> <expr>		-- logical implication.
| <expr> < <expr>
| <expr> <= <expr>
| <expr> > <expr>
| <expr> >= <expr>
| <expr> = <expr>
| <expr> != <expr>
| <expr> ? <expr> : <expr>	-- C-style conditional expression.

-/

/-
 I have no idea what an "actual" is, the manual doesn't describe it either.
 Otherwise, the `call` constructor should be something like:

   | call : ID → List Actual → Expr
-/
inductive Expr
  | designator : Designator → Expr
  | integerConst : Int → Expr
  | call : ID → List Expr → Expr
  | universal : Quantifier → Expr → Expr
  | existential : Quantifier → Expr → Expr
  | binop : BinOp → Expr → Expr → Expr
  | negation : Expr → Expr
  | conditional : Expr → Expr → Expr → Expr

-- <designator> :=	<ID> { . <ID> | \[ <expr> \] }
inductive Designator
| mk : ID → List (ID ⊕ Expr) → Designator

/-
<quantifier> ::= <ID> : <typeExpr>
  | <ID> := <expr> to <expr> [ by <expr> ]
-/

inductive Quantifier
  | simple : ID → TypeExpr → Quantifier
  | assign : ID → Expr → Expr → Option Expr → Quantifier

/-
<stmts> ::= <stmt> {; [<stmt>] }

<stmt> ::= <assignment>         /* assignment */
| <ifstmt>		/* if statement */
| <switchstmt>		/* switch statement */
| <forstmt>		/* for statement */
| <whilestmt>		/* while statement */
| <aliasstmt>		/* alias statement */
| <proccall>		/* procedure call */
| <clearstmt>		/* clear statement */
| <errorstmt>		/* error assertion */
| <assertstmt>		/* assertion */
| <putstmt>		/* output statement */
| <returnstmt>		/* function return */

<assignment> ::= <designator> := <expression>
<ifstmt> ::= if <expr> then [ <stmts> ]
              { elsif <expr> then [ <stmts> ] }
              [ else [ <stmts> ] ]
             endif
<switchstmt> ::= switch <expr>
                  { case <expr> {, expr} : [ <stmts> ] }
                  [ else [ <stmts> ] ]
                 endswitch
<forstmt> ::= for <quantifier> do [stmts] endfor
<whilestmt> ::= while <expr> do [stmts] end
<aliasstmt> ::= alias <alias> { ; <alias> } do [ <stmts> ] end
<proccall> ::= <ID> \( <expr> {, <expr> } \)
<clearstmt> ::= clear <designator>
<errorstmt> ::= error <string>
<assertstmt> ::= assert <expr> [ <string> ]
<putstmt> ::= put ( <expr> | <string> )
<returnstmt> ::= return [ <expr> ]
-/
inductive Statement
  | assignment : Designator → Expr → Statement
  | ifstmt : Expr → List Statement → Option (Expr × List Statement) → List Statement → Statement
  | switchstmt : Expr → List (List Expr × List Statement) → List Statement → Statement
  | forstmt : Quantifier → List Statement → Statement
  | whilestmt : Expr → List Statement → Statement
  | aliasstmt : List Alias → List Statement → Statement
  | proccall : ID → List Expr → Statement
  | clearstmt : Designator → Statement
  | errorstmt : String → Statement
  | assertstmt : Expr → String → Statement
  | putstmtexp : Expr → Statement
  | putstmtstr : String → Statement
  | returnstmt : Option Expr → Statement

-- <alias> ::= <ID> : <expr>
inductive Alias
  | mk : ID → Expr → Alias

/-
<rules> ::= <rule> {; <rule> } [;]

<rule> ::= <simplerule>
         | <startstate>
         | <invariant>
         | <ruleset>
         | <aliasrule>
<simplerule> ::= rule [<string>]
                 [ <expr> ==> ]
                 [ { <decl> } begin ]
                 [ stmts ]
                 end
<startstate> ::= startstate [ <string> ]
                 [ { <decl> } begin ]
                 [ <stmts> ]
<invariant> ::= invariant [ <string> ] <expr>
<ruleset> ::= ruleset <quantifier>
             {; <quantifier> } do [<rules>] end
<aliasrule> ::= alias <alias> {; <alias> } do [<rules>] end
-/
inductive Rule
  | simplerule : Option String → Option Expr → List Decl → List Statement → Rule
  | startstate : Option String → List Decl → List Statement → Rule
  | invariant : Option String → Expr → Rule
  | ruleset : List Quantifier → List Rule → Rule
  | aliasrule : List Alias → List Rule → Rule

end -- mutual

structure Program where
  decls : List Decl
  procdecls  : List ProcDecl
  rules   : List Rule

mutual
private partial def typeExprToString : TypeExpr → String
  | .previouslyDefined id => s!"{id}"
  | .integerSubrange startexp endexp => exprToString startexp ++ " .. " ++ exprToString endexp
  | .enum ids => "enum {" ++ (", ".intercalate ids) ++ "}"
  | .record decls => "record" ++ (", ".intercalate $ decls.map declToString) ++ "end"
  | .array ty₁ ty₂ => "array [" ++ typeExprToString ty₁ ++ "] of " ++ typeExprToString ty₂

private partial def declToString : Decl → String
  | .const id expr => s!"{id} : " ++ exprToString expr
  | .type  id typeexpr => s!"{id} : " ++ typeExprToString typeexpr
  | .var  ids typeexpr => (",".intercalate ids) ++ ": " ++ typeExprToString typeexpr


private partial def exprToString : Expr → String
  | .designator des => designatorToString des
  | .integerConst i => i.repr
  | .call id actuals => s!"{id}(" ++ ", ".intercalate (actuals.map exprToString) ++ ")"
  | .universal quant doexp => "forall " ++ quantifierToString quant ++ " do " ++ exprToString doexp ++ " endforall"
  | .existential quant doexp => "exists " ++ quantifierToString quant ++ " do " ++ exprToString doexp ++ " endexists"
  | .binop op lhs rhs => "(" ++ exprToString lhs ++ s!" {op} " ++ exprToString rhs ++ ")"
  | .negation exp => "!(" ++ exprToString exp ++ ")"
  | .conditional cond thenexp elseexp => exprToString cond ++ " ? " ++ exprToString thenexp ++ " : " ++ exprToString elseexp

private partial def formalToString : Formal → String
  | .mk var ids type => (if var then "var " else "") ++ ", ".intercalate ids ++ " : " ++ typeExprToString type

private partial def procDeclToString : ProcDecl → String
  | .procedure id formals decls statements => s!"procedure {id} (" ++ "; ".intercalate (formals.map formalToString) ++ ");\n"
    ++ "\n".intercalate (decls.map declToString) ++ "\n begin \n" ++ ";\n".intercalate (statements.map statementToString)
    ++ "\n end;"
  | .function id formals type decls statements => s!"function {id} (" ++ ";\n".intercalate (formals.map formalToString) ++ ")"
    ++ " : " ++ typeExprToString type ++ ";\n" ++ "\n".intercalate (decls.map declToString) ++ "\n begin \n"
    ++ ";\n".intercalate (statements.map statementToString) ++ "\n end;"

private partial def designatorToString : Designator → String
  | .mk id idorexps =>
    let idOrExpToString : Sum ID Expr → String
      | .inl id => s!".{id}"
      | .inr expr => "[ " ++ exprToString expr ++ " ]"
    id ++ String.join (idorexps.map idOrExpToString)

private partial def quantifierToString : Quantifier → String
  | .simple id type => s!"{id} : {typeExprToString type}"
  | .assign id exp toexp byexpOp =>
    let expS := exprToString exp
    let toexpS := exprToString toexp
    let byexpS := match byexpOp with
      | none => ""
      | some byexp => "by " ++ exprToString byexp
    s!"{id} := {expS} to {toexpS} {byexpS}"

private partial def statementToString : Statement → String
  | .assignment des expr => (designatorToString des) ++ " := " ++ (exprToString expr)
  | .ifstmt cond thenstmts elifop elsestmts =>
    let thenS := ";\n".intercalate $ thenstmts.map statementToString
    let elseS := if elsestmts.length == 0 then ""
     else "else " ++ ";\n".intercalate (elsestmts.map statementToString)
    let elifS := match elifop with
      | none => ""
      | some (elifexp, elifstmts) => s!" elsif {exprToString elifexp} then "
        ++ ";\n".intercalate (elifstmts.map statementToString)
    s!"if {exprToString cond} then " ++ thenS ++ elifS ++ elseS
  | .switchstmt exp cases elsestmts =>
    let casesS := "\n".intercalate $ cases.map
      λ (exps,stmts) => ",".intercalate (exps.map exprToString) ++ " : "
        ++ ";\n".intercalate (stmts.map statementToString)
    let elseS := if elsestmts.length == 0 then ""
        else "else " ++ ";\n".intercalate (elsestmts.map statementToString)
    s!"switch {exprToString exp}{casesS}{elseS} endswitch"
  | .forstmt quant stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    s!"for {quantifierToString quant} do {stmtsS} endfor"
  | .whilestmt cond stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    s!"while {exprToString cond} do {stmtsS} endwhile"
  | .aliasstmt aliases stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    "alias " ++ ("; ".intercalate $ aliases.map aliasToString) ++ s!" do {stmtsS} end"
  | .proccall id args => s!"{id} (" ++ (", ".intercalate $ args.map exprToString) ++ ")"
  | .clearstmt des => s!"clear {designatorToString des}"
  | .errorstmt msg => s!"error {msg}"
  | .assertstmt exp msg => s!"assert {exprToString exp} {msg}"
  | .putstmtexp exp => s!"put {exprToString exp}"
  | .putstmtstr str => s!"put {str}"
  | .returnstmt opExp => "return " ++ (match opExp with | none => "" | some exp => exprToString exp)

private partial def aliasToString : Alias → String
  | .mk id exp => s!"{id} : {exprToString exp}"

private partial def ruleToString : Rule → String
  | .simplerule opName opExp decls stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    let declsS := String.intercalate ";\n" $ decls.map declToString
    let nameS := match opName with
      | none => ""
      | some name => name
    let expS := match opExp with
      | none => ""
      | some exp => exprToString exp ++ " ==>\n"
    s!"rule {nameS}{expS}{declsS}\n begin {stmtsS} end"
  | .startstate opName decls stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    let declsS := String.intercalate ";\n" $ decls.map declToString
    let nameS := match opName with
      | none => ""
      | some name => name
    s!"startstate {nameS}{declsS}\n begin {stmtsS} end"
  | .invariant opName exp =>
    let nameS := match opName with
      | none => ""
      | some name => name
    s!"invariant {nameS} {exprToString exp}"
  | .ruleset quants rules =>
    let quantsS := "; ".intercalate (quants.map quantifierToString)
    let rulesS := String.intercalate ";\n" $ rules.map ruleToString
    s!"ruleset {quantsS} do {rulesS} end"
  | .aliasrule aliases rules =>
    let aliasesS := "; ".intercalate (aliases.map aliasToString)
    let rulesS := String.intercalate ";\n" $ rules.map ruleToString
    s!"alias {aliasesS} do {rulesS} end"
end


def Formal.toString : Formal → String := formalToString
instance : ToString Formal where toString := Formal.toString

def ProcDecl.toString : ProcDecl → String := procDeclToString
instance : ToString ProcDecl where toString := ProcDecl.toString

def Designator.toString : Designator → String := designatorToString
instance : ToString Designator where toString := Designator.toString

def Quantifier.toString : Quantifier → String := quantifierToString
instance : ToString Quantifier where toString := Quantifier.toString

def Statement.toString : Statement → String := statementToString
instance : ToString Statement where toString := Statement.toString

def Alias.toString : Alias → String := aliasToString
instance : ToString Alias where toString := Alias.toString

def Rule.toString : Rule → String := ruleToString
instance : ToString Rule where toString := Rule.toString

def Expr.toString : Expr → String := exprToString
instance : ToString Expr where toString := Expr.toString

def TypeExpr.toString : TypeExpr → String := typeExprToString
instance : ToString TypeExpr where toString := TypeExpr.toString

def Decl.toString : Decl → String := declToString
instance : ToString Decl where toString := Decl.toString

def Program.toString : Program → String
  | prog =>
    let decls := String.intercalate ";\n" $ prog.decls.map Decl.toString
    let procdecls := String.intercalate ";\n" $ prog.procdecls.map ProcDecl.toString
    let rules := String.intercalate ";\n" $ prog.rules.map Rule.toString
  s!"{decls} \n {procdecls} \n {rules}"
instance : ToString Program where toString := Program.toString

def Designator.concat : Designator → (ID ⊕ Expr) → Designator
  | .mk id rest, new => Designator.mk id (new::rest)

declare_syntax_cat formal
declare_syntax_cat proc_decl
declare_syntax_cat designator
declare_syntax_cat quantifier
declare_syntax_cat statement
declare_syntax_cat statements
declare_syntax_cat mur_alias
declare_syntax_cat mur_rule
declare_syntax_cat expr
declare_syntax_cat type_expr
declare_syntax_cat decl
declare_syntax_cat var_decl
declare_syntax_cat const_decl
declare_syntax_cat type_decl
declare_syntax_cat program
declare_syntax_cat paramident
declare_syntax_cat paramstr

syntax (name := paramident1) ident : paramident
syntax (name := paramident2) "£" ident : paramident
syntax  str : paramstr
syntax  "£" ident : paramstr

syntax ("var")? paramident (paramident),* ":" type_expr : formal
syntax "procedure" paramident "(" sepBy(formal,";") ")" ";" (decl* "begin")* statement* "end" ";" : proc_decl
syntax "function" paramident "(" sepBy(formal,";") ")" ":" type_expr ";" (decl* "begin")* statement* "end" ";" : proc_decl
-- TODO: this needs space for the ".", should fix it
syntax paramident : designator
syntax designator "." paramident : designator
syntax designator "[" expr "]" : designator
syntax (name := simplequantifier) paramident ":" type_expr : quantifier
syntax (name := quantifierassign) paramident ":=" expr "to" expr ("by" expr)? : quantifier
syntax designator ":=" expr : statement
syntax "if" expr "then" statement* ("elsif" expr "then" statement*)? ("else" statement*)? "endif" : statement
syntax "switch" expr ("case" expr,+ ":" statement*)* ("else" statement*)? "endswitch" : statement
syntax "for" quantifier "do" statement* "endfor" : statement
syntax "while" expr "do" statement* "end" : statement
syntax "alias" sepBy(mur_alias,";") "do" statement* "end" : statement
syntax paramident "(" expr,+ ")" : statement
syntax "clear" designator : statement
syntax "error" str : statement
syntax "assert" expr (str)? : statement
syntax "put" (expr <|> str) : statement
syntax "return" (expr)? : statement
syntax statement : statements
syntax statement ";" statements : statements
syntax paramident ":" expr : mur_alias
syntax (name := simplerule) "rule" (paramstr)? (expr "==>")? (decl* "begin")? statements "end" : mur_rule
-- commenting this out with the above removes the errors on individual statements, which makes no sense to me
-- syntax  "rule" (str)? (expr "==>")? (decl* "begin")? statement* "end" : mur_rule
syntax  "startstate" (str)? (decl "begin")? statement* "end" : mur_rule
syntax "invariant" (str)? expr : mur_rule
syntax (name := rulesetsyn) "ruleset" sepBy1(quantifier,";") "do" sepBy(mur_rule,";",";",allowTrailingSep) "endruleset" : mur_rule
syntax "alias" sepBy1(mur_alias,";") "do" sepBy(mur_rule,";") "end" : mur_rule
syntax "(" expr ")" : expr
syntax designator : expr
syntax num : expr
syntax paramident "(" expr,* ")" : expr -- still don't know what "actuals" are
syntax "forall" quantifier "do" expr "endforall" : expr
syntax "exists" quantifier "do" expr "endexists" : expr
syntax expr ("+" <|> "-" <|> "*" <|> "/" <|> "%" <|> "|" <|> "&" <|>
             "->" <|> "<" <|> "<=" <|> ">" <|> ">=" <|> "=" <|> "!=") expr : expr
syntax "!" expr : expr
syntax expr "?" expr ":" expr : expr
syntax paramident : type_expr
syntax expr ".." expr : type_expr
syntax "enum" "{" paramident,+ "}" : type_expr
syntax "record" decl* "end" : type_expr
syntax "array" "[" type_expr "]" "of" type_expr : type_expr
syntax (name := vardecl) paramident,+ ":" type_expr : var_decl
syntax paramident ":" expr : const_decl
syntax paramident ":" type_expr : type_decl
syntax "const" sepBy(const_decl,";",";",allowTrailingSep) : decl
syntax "type" sepBy(type_decl,";",";",allowTrailingSep) : decl
syntax "var" sepBy(var_decl,";",";",allowTrailingSep) : decl
syntax decl decl decl : program


syntax "[murϕ|" formal "]" : term
syntax "[murϕ|" proc_decl "]" : term
syntax "[murϕ|" designator "]" : term
syntax "[murϕ|" quantifier "]" : term
syntax "[murϕ|" statement "]" : term
syntax "[murϕ|" mur_alias "]" : term
syntax "[murϕ|" mur_rule "]" : term
syntax "[murϕ|" expr "]" : term
syntax "[murϕ|" type_expr "]" : term
syntax "[murϕ|" decl "]" : term
syntax "[murϕ|" program "]" : term

@[macro paramident1]
def expandParamIdent : Macro
  |  `(paramident| $x:ident) => `($(Lean.quote x.getId.toString))
  |  `(paramident| £ $x:ident ) => `($x)
  | _ => Lean.Macro.throwUnsupported

-- This feels very hacky! But it won't work with <|>. TODO: I should produce an MWE
@[macro paramident2]
def expandParamIdent2 : Macro := expandParamIdent

macro_rules
  |  `(paramstr| $x:str) => `($x)
  |  `(paramstr| £ $x:ident ) => `($x)

@[macro vardecl]
def expandVarDecl : Macro
  | `(var_decl| $[$ids:paramident],* : $t:type_expr ) => do
    let idsListSyn : List Syntax := ids.toList.map λ stx => stx.raw
    let idsList' <- idsListSyn.mapM expandParamIdent
    let idsList : List Term := idsList'.map λ stx => ⟨stx⟩
    let texp : Term ← Lean.expandMacros t |>.map λ stx => ⟨stx⟩
    `(Decl.var $(Lean.quote idsList) $texp)
  | _ => Lean.Macro.throwUnsupported

/-
syntax (name := quantifier1) paramident ":" type_expr : quantifier
syntax (name := quantifier2) paramident ":=" expr "to" expr ("by" expr)? : quantifier
-/
open TSyntax.Compat
def liftListSyntax : List Syntax → MacroM Syntax
  | [] => `([])
  | syn::syns => do
    let syns' <- liftListSyntax syns
    `($syn ::$syns')

@[macro simplequantifier]
def expandSimpleQuantifier : Lean.Macro
  | `(quantifier| $x:paramident : $t) => do `(Quantifier.simple $(← expandParamIdent x) $t)
  | _ => Lean.Macro.throwUnsupported

@[macro quantifierassign]
def expandQuantifierAssign : Lean.Macro
  | `(quantifier| $x:paramident := $e₁ to $e₂ $[by $e₃]?) => do
    let x' <- expandParamIdent x
    -- TODO: deal with e₃
    `(Quantifier.assign $x' $e₁ $e₂ none)
  | _ => Lean.Macro.throwUnsupported

def expandQuantifier := expandSimpleQuantifier >=> expandQuantifierAssign

macro_rules
  | `(decl| var $[$vardecls];*) => do
    let arraySyn <- vardecls.mapM expandVarDecl
    let listSyn <- liftListSyntax arraySyn.toList
    return listSyn

macro_rules
  | `(type_expr| $x:paramident) => do `(TypeExpr.previouslyDefined $(← expandParamIdent x))

@[macro simplerule]
def expandSimpleRule : Lean.Macro
  | `(mur_rule| rule $(x)? $e ==> $(ds)* begin $stmts end) => do
    let xSyn <- match x with
      | none => `(none)
      | some x' =>
        let expanded <- expandMacros x'
        `(some $expanded)
    let dsSynList <-  ds.toList.map (λ x => x.raw) |>.mapM expandMacros
    let dsSyn <- liftListSyntax dsSynList
    `(Rule.simplerule $xSyn $e (List.join $dsSyn ) $stmts)
  | _ => Lean.Macro.throwUnsupported

@[macro rulesetsyn]
def expandRuleset : Lean.Macro
  | `(mur_rule| ruleset $[$quantifiers];* do $[$rules];* endruleset) => do
    let qs' <- quantifiers.mapM expandMacros
    let qs <- liftListSyntax qs'.toList
    let rs' <- rules.toList.map (λ x => x.raw) |>.mapM expandMacros
    let rs <- liftListSyntax rs'
    `(Rule.ruleset $qs $rs)
  | _ => do
    Lean.Macro.throwUnsupported


macro_rules
  | `(statements| $stmt:statement) => `( [ $stmt ])
  | `(statements| $stmt:statement ; $stmts:statements) => `($stmt :: $stmts)

macro_rules
  | `(expr| $x = $y) => `(Expr.binop "=" $x $y)
  | `(expr| $x:designator ) => `(Expr.designator $x)

macro_rules
  | `(designator| $x:paramident ) => do `(Designator.mk $(← expandParamIdent x) [])
  | `(designator| $d:designator [$e:expr] ) => `(Designator.concat $d $ Sum.inr $e)
  | `(designator| $d:designator . $x:paramident ) => do `(Designator.concat $d $ Sum.inl $(← expandParamIdent x))

macro_rules
  | `(statement| $x:designator := $y ) => `(Statement.assignment $x $y)

macro_rules
  | `([murϕ| $x:formal     ]) => `(formal| $x)
  | `([murϕ| $x:proc_decl  ]) => `(proc_decl| $x)
  | `([murϕ| $x:quantifier ]) => `(quantifier| $x)
  | `([murϕ| $x:statement  ]) => `(statement| $x)
  | `([murϕ| $x:mur_alias  ]) => `(mur_alias| $x)
  | `([murϕ| $x:mur_rule   ]) => `(mur_rule| $x)
  | `([murϕ| $x:expr       ]) => `(expr| $x)
  | `([murϕ| $x:type_expr  ]) => `(type_expr| $x)
  | `([murϕ| $x:decl       ]) => `(decl| $x)
  | `([murϕ| $x:program    ]) => `(program| $x)
 -- | `([murϕ| $x:designator ]) => `(designator| $x)

def foo := "bar"
#eval [murϕ| var foo : baz]
#eval [murϕ| var £foo : baz]

#check [murϕ| ld_entry .phys_addr := ld_entry .virt_addr]
#check [murϕ| ld_entry .phys_addr := ld_entry .virt_addr]
#check [murϕ| next_state .core_[j] .lsq_ .lq_ .ld_entries[i] := ld_entry]
#check [murϕ| ruleset j : cores_t do endruleset]
#check [murϕ|
ruleset j : cores_t do
rule "await_translation TO await_fwd_check"
Sta.core_[j].lsq_.lq_.ld_entries[i].ld_state = await_translation
==>
-- decls
var next_state : STATE;
var ld_entry : LD_ENTRY_VALUES;
begin
next_state := Sta;
ld_entry := Sta.core_[j].lsq_.lq_.ld_entries[i];

--# "translate" the address
--#NOTE TODO: access tlb? not quite necessary for litmus tests
ld_entry.phys_addr := ld_entry.virt_addr;

ld_entry.ld_state := await_fwd_check;

next_state.core_[j].lsq_.lq_.ld_entries[i] := ld_entry;

Sta := next_state
end
endruleset

]
end Murϕ
