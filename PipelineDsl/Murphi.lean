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
| mk : ID → List (Sum ID Expr) → Designator

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
  | returnstmt : Expr → Statement

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
  | .call id actuals => "{id}(" ++ ", ".intercalate (actuals.map exprToString) ++ ")"
  | .universal quant doexp => "forall " ++ quantifierToString quant ++ " do " ++ exprToString doexp ++ " endforall"
  | .existential quant doexp => "exists " ++ quantifierToString quant ++ " do " ++ exprToString doexp ++ " endexists"
  | .binop op lhs rhs => exprToString lhs ++ s!" {op} " ++ exprToString rhs
  | .negation exp => "!" ++ exprToString exp
  | .conditional cond thenexp elseexp => exprToString cond ++ " ? " ++ exprToString thenexp ++ " : " ++ exprToString elseexp

private partial def formalToString : Formal → String
  | .mk var ids type => (if var then "var " else "") ++ ", ".intercalate ids ++ " : " ++ typeExprToString type

private partial def procDeclToString : ProcDecl → String
  | .procedure id formals decls statements => "procedure {id} (" ++ ";\n".intercalate (formals.map formalToString) ++ ");\n"
    ++ "\n".intercalate (decls.map declToString) ++ "\n begin \n" ++ ";\n".intercalate (statements.map statementToString)
    ++ "\n end;"
  | .function id formals type decls statements => "function {id} (" ++ ";\n".intercalate (formals.map formalToString) ++ ");\n"
    ++ " : " ++ typeExprToString type ++ "\n".intercalate (decls.map declToString) ++ "\n begin \n"
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
  | .proccall id args => s!"id (" ++ (", ".intercalate $ args.map exprToString) ++ ")"
  | .clearstmt des => s!"clear {designatorToString des}"
  | .errorstmt msg => s!"error {msg}"
  | .assertstmt exp msg => s!"assert {exprToString exp} {msg}"
  | .putstmtexp exp => s!"put {exprToString exp}"
  | .putstmtstr str => s!"put {str}"
  | .returnstmt exp => "return " ++ exprToString exp

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
    s!"rule {nameS}{expS}{declsS} begin {stmtsS} end"
  | .startstate opName decls stmts =>
    let stmtsS := ";\n".intercalate (stmts.map statementToString)
    let declsS := String.intercalate ";\n" $ decls.map declToString
    let nameS := match opName with
      | none => ""
      | some name => name
    s!"startstate {nameS}{declsS} begin {stmtsS} end"
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
