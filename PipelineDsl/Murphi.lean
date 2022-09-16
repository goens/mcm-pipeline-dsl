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
  deriving Inhabited

/-
<formal> ::=	[var] <ID> { , <ID> } : <typeExpr>
-/
inductive Formal
  | mk : Bool → List ID → TypeExpr → Formal
  deriving Inhabited

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
  deriving Inhabited

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
  deriving Inhabited

-- <designator> :=	<ID> { . <ID> | \[ <expr> \] }
inductive Designator
| mk : ID → List (ID ⊕ Expr) → Designator
deriving Inhabited

/-
<quantifier> ::= <ID> : <typeExpr>
  | <ID> := <expr> to <expr> [ by <expr> ]
-/

inductive Quantifier
  | simple : ID → TypeExpr → Quantifier
  | assign : ID → Expr → Expr → Option Expr → Quantifier
  deriving Inhabited

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
  | ifstmt : Expr → List Statement → List (Expr × List Statement) → List Statement → Statement
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
  | undefine : ID → Statement
  deriving Inhabited

-- <alias> ::= <ID> : <expr>
inductive Alias
  | mk : ID → Expr → Alias
  deriving Inhabited

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
  deriving Inhabited

end -- mutual

structure Program where
  constdecls : List Decl
  typedecls : List Decl
  vardecls : List Decl
  procdecls  : List ProcDecl
  rules   : List Rule

def indent : Nat → String
  | Nat.zero => ""
  | Nat.succ n => "  " ++ indent n

mutual
private partial def typeExprToString : TypeExpr → String
  | .previouslyDefined id => s!"{id}"
  | .integerSubrange startexp endexp => exprToString startexp ++ " .. " ++ exprToString endexp
  | .enum ids => "enum {" ++ (", ".intercalate ids) ++ "}"
  | .record decls => "record\n" ++
    (String.join (( decls.map declToString ).map fun str => (String.join ["  ", str, ";\n"] ) )) -- (";\n".intercalate $ decls.map declToString)
    ++ "end"
  | .array ty₁ ty₂ => "array [" ++ typeExprToString ty₁ ++ "] of " ++ typeExprToString ty₂

private partial def declToString : Decl → String
  | .const id expr => s!"{id} : " ++ exprToString expr
  | .type  id typeexpr => s!"{id} : " ++ typeExprToString typeexpr
  | .var  ids typeexpr => (",".intercalate ids) ++ " : " ++ typeExprToString typeexpr


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

private partial def procDeclToString (inputProcDecl : ProcDecl) : String :=
  let indentationLevel : Nat := 1
  let recCall := statementToString (indentationLevel := indentationLevel + 1)
  match inputProcDecl with
  | .procedure id formals decls statements => s!"procedure {id} (" ++ "; ".intercalate (formals.map formalToString) ++ ");\n"
    ++ "\n".intercalate (decls.map declToString) ++ "begin\n" ++ ";\n".intercalate (statements.map recCall)
    ++ ";\n end;"
  | .function id formals type decls statements => s!"function {id} (" ++ ";\n".intercalate (formals.map formalToString) ++ ")"
    ++ " : " ++ typeExprToString type ++ ";\n" ++
    -- Function Variables, Syntax: var <var_name> : <var_type> ;
    String.join (( decls.map declToString ).map fun str => String.join ["  var ", str, ";\n"] ) -- "\n".intercalate (decls.map declToString)
    ++ "\nbegin\n"
    ++ String.join (( statements.map recCall ).map fun str => String.join [(indent indentationLevel), str, ";\n"] )
    -- ";\n".intercalate (statements.map statementToString)
    ++ "end"

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

private partial def statementToString ( indentationLevel := 0) (inputStatement : Statement)  : String :=
  let end_indents : String := indent (indentationLevel - 1)
  let recCall := statementToString (indentationLevel := indentationLevel + 1)
  match inputStatement with
  | .assignment des expr => (designatorToString des) ++ " := " ++ (exprToString expr)
  | .ifstmt cond thenstmts eliflist elsestmts =>
    let thenS := String.join (( thenstmts.map recCall ).map fun str => String.join [(indent indentationLevel), str, ";\n"] ) --";\n".intercalate $ thenstmts.map recCall
    let elseS :=
     if elsestmts.length == 0 then ""
     else end_indents ++ "else\n" ++
     String.join (( elsestmts.map recCall ).map fun str => String.join [(indent indentationLevel), str, ";\n"] ) -- ";\n".intercalate (elsestmts.map recCall)
    let elifS := String.intercalate " " $ eliflist.map λ (elifexp, elifstmts) =>
      s!"{end_indents}elsif {exprToString elifexp} then\n"
      ++ String.join (( elifstmts.map recCall ).map fun str => String.join [(indent indentationLevel), str, ";\n"] ) -- ";\n".intercalate (elifstmts.map recCall)
    s!"if {exprToString cond} then\n" ++ thenS ++ elifS ++ elseS ++ end_indents ++ "end"
  | .switchstmt exp cases elsestmts =>
    let casesS := "\n".intercalate $ cases.map
      λ (exps,stmts) => ",".intercalate (exps.map exprToString) ++ " : "
        ++ ";\n".intercalate (stmts.map recCall)
    let elseS := if elsestmts.length == 0 then ""
        else "else " ++ ";\n".intercalate (elsestmts.map recCall)
    s!"switch {exprToString exp}{casesS}{elseS}" ++ end_indents ++ "endswitch"
  | .forstmt quant stmts =>
    let stmtsS := (String.join (( stmts.map recCall ).map (fun str => String.join [(indent indentationLevel), str, ";\n"]) )) -- ";\n".intercalate (stmts.map recCall)
    s!"for {quantifierToString quant} do\n{stmtsS}" ++ end_indents ++ "endfor"
  | .whilestmt cond stmts =>
    let stmtsS := (String.join (( stmts.map recCall ).map (fun str => String.join [(indent indentationLevel), str, ";\n"]) )) -- ";\n".intercalate (stmts.map recCall)
    s!"while {exprToString cond} do\n{stmtsS}" ++ end_indents ++ "end"
  | .aliasstmt aliases stmts =>
    let stmtsS := (String.join (( stmts.map recCall ).map (fun str => String.join [(indent indentationLevel), str, ";\n"]) )) -- ";\n".intercalate (stmts.map recCall)
    "alias " ++ ("; ".intercalate $ aliases.map aliasToString) ++ s!" do {stmtsS}" ++ end_indents ++ "end"
  | .proccall id args => s!"{id} (" ++ (", ".intercalate $ args.map exprToString) ++ ")"
  | .clearstmt des => s!"clear {designatorToString des}"
  | .errorstmt msg => s!"error \"{msg}\""
  | .assertstmt exp msg => s!"assert {exprToString exp} \"{msg}\""
  | .putstmtexp exp => s!"put {exprToString exp}"
  | .putstmtstr str => s!"put {str}"
  | .returnstmt opExp => "return " ++ (match opExp with | none => "" | some exp => exprToString exp)
  | .undefine id => s!"undefine {id}"

private partial def aliasToString : Alias → String
  | .mk id exp => s!"{id} : {exprToString exp}"

private partial def ruleToString ( indentationLevel := 0) (inputRule : Rule) : String :=
  let recCall := statementToString (indentationLevel := indentationLevel + 1)
  match inputRule with
  | .simplerule opName opExp decls stmts =>
    let stmtsS := String.join (( stmts.map recCall ).map fun str => String.join [(indent ( indentationLevel + 1 )), str, ";\n"] ) -- ";\n".intercalate (stmts.map statementToString)
    let declsS := (String.join (( decls.map declToString ).map (fun str => String.join ["  var ", str, ";\n"]) )) -- String.intercalate ";\n" $ decls.map declToString
    let nameS := match opName with
      | none => ""
      | some name => name
    let expS := match opExp with
      | none => ""
      | some exp => exprToString exp ++ "\n==>\n"
      s!"rule \"{nameS}\" \n{expS} \n{declsS}\nbegin\n{stmtsS}\nend"
  | .startstate opName decls stmts =>
    let stmtsS := (String.join (( stmts.map statementToString ).map (fun str => String.join ["  ", str, ";\n"]) )) -- ";\n".intercalate (stmts.map statementToString)
    let declsS := (String.join (( decls.map declToString ).map (fun str => String.join ["  var ", str, ";\n"]) )) -- String.intercalate ";\n" $ decls.map declToString
    let nameS := match opName with
      | none => ""
      | some name => name
    s!"startstate \"{nameS}\"{declsS}\nbegin\n{stmtsS} end"
  | .invariant opName exp =>
    let nameS := match opName with
      | none => ""
      | some name => name
    s!"invariant \"{nameS}\"\n{exprToString exp}"
  | .ruleset quants rules =>
    let quantsS := "; ".intercalate (quants.map quantifierToString)
    let rulesS := (String.join (( rules.map ruleToString ).map (fun str => String.join ["  ", str, ";\n"]) )) -- String.intercalate ";\n" $ rules.map ruleToString
    s!"\nruleset {quantsS} do \n{rulesS}\nend"
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
    let constdecls := String.join (( prog.constdecls.map Decl.toString ).map fun str => str.append ";\n") -- String.intercalate ";\n" $ prog.constdecls.map Decl.toString
    let vardecls := String.join (( prog.vardecls.map Decl.toString ).map fun str => str.append ";\n") -- String.intercalate ";\n" $ prog.vardecls.map Decl.toString
    let typedecls := String.join (( prog.typedecls.map Decl.toString ).map fun str => str.append ";\n") -- String.intercalate ";\n" $ prog.typedecls.map Decl.toString
    let decls := s!"const\n{constdecls}\ntype\n{typedecls}\nvar\n{vardecls}\n"
    let procdecls := String.join (( prog.procdecls.map ProcDecl.toString ).map fun str => str.append ";\n\n") -- String.intercalate ";\n" $ prog.procdecls.map ProcDecl.toString
    let rules := String.join (( prog.rules.map Rule.toString ).map fun str => str.append ";\n\n") -- String.intercalate ";\n" $ prog.rules.map Rule.toString
  s!"{decls}\n{procdecls}\n{rules}"
instance : ToString Program where toString := Program.toString

def Designator.cons : Designator → (ID ⊕ Expr) → Designator
  | .mk id rest, new => Designator.mk id (rest ++ [new])

def Expr.appendCallArg : Expr → Expr → Expr
  | .call id args, newArg => .call id (args ++ [newArg])
  | expr, _ => expr

mutual

partial def Expr.beq : Expr → Expr → Bool
  | .designator d₁, .designator d₂ => d₁.beq d₂
  | .integerConst n₁, .integerConst n₂ => n₁ == n₂
  | .call id₁ exprs₁, .call id₂ exprs₂ =>
      id₁ == id₂ && exprs₁.length == exprs₂.length &&
      (exprs₁.zip exprs₂).foldl (init := true)
        λ res (e₁, e₂) => res && e₁.beq e₂
  | .universal q₁ e₁, .universal q₂ e₂ => q₁.beq q₂ && e₁.beq e₂
  | .existential q₁ e₁, .existential q₂ e₂ => q₁.beq q₂ && e₁.beq e₂
  | .binop op₁ e₁₁ e₁₂, .binop op₂ e₂₁ e₂₂ => op₁ == op₂ && e₁₁.beq e₂₁ && e₁₂.beq e₂₂
  | .negation e₁, .negation e₂ => e₁.beq e₂
  | .conditional c₁ t₁ e₁, .conditional c₂ t₂ e₂ => c₁.beq c₂ && t₁.beq t₂ && e₁.beq e₂
  | _, _ => false

partial def Quantifier.beq : Quantifier → Quantifier → Bool
  | .simple id₁ te₁, .simple id₂ te₂ => id₁ == id₂ && te₁.beq te₂
  | .assign id₁ e₁₁ e₁₂ oe₁, .assign id₂ e₂₁ e₂₂ oe₂ =>
    id₁ == id₂ && e₁₁.beq e₂₁ && e₁₂.beq e₂₂ &&
    match oe₁, oe₂ with
      | none, none => true
      | some exp₁, some exp₂ => Expr.beq exp₁ exp₂
      | _, _ => false
  | _, _ => false

partial def Designator.beq : Designator → Designator → Bool
  | .mk id₁ rest₁, .mk id₂ rest₂ => and (id₁ == id₂) $
    rest₁.zip rest₂ |>.map (λ (sum₁,sum₂) => match sum₁, sum₂ with
    | .inl id'₁, .inl id'₂ => id'₁ == id'₂
    | .inr exp₁, .inr exp₂ => Expr.beq exp₁ exp₂
    | _, _ => false)
      |>.all id

partial def TypeExpr.beq : TypeExpr → TypeExpr → Bool
  | .previouslyDefined id₁, .previouslyDefined id₂ => id₁ == id₂
  | .integerSubrange e₁ te₁, .integerSubrange e₂ te₂ => e₁.beq e₂ && te₁.beq te₂
  | .enum ids₁, .enum ids₂ => ids₁ == ids₂
  | .record decls₁, .record decls₂ => decls₁.zip decls₂ |>.all λ (d₁, d₂) => d₁.beq d₂
  | .array te₁₁ te₁₂, .array te₂₁ te₂₂ => te₁₁.beq te₂₁ && te₁₂.beq te₂₂
  | _, _ => false

partial def Decl.beq : Decl → Decl → Bool
  | .const id₁ e₁, .const id₂ e₂ => id₁ == id₂ && e₁.beq e₂
  | .type id₁ te₁, .type id₂ te₂ => id₁ == id₂ && te₁.beq te₂
  | .var  ids₁ te₁, .var ids₂ te₂ => ids₁ == ids₂ && te₁.beq te₂
  | _, _ => false

end

instance : BEq Designator where beq := Designator.beq
instance : BEq Decl where beq := Decl.beq
instance : BEq Expr where beq := Expr.beq
instance : BEq Quantifier where beq := Quantifier.beq
instance : BEq TypeExpr where beq := TypeExpr.beq

declare_syntax_cat formal
declare_syntax_cat proc_decl
declare_syntax_cat designator
declare_syntax_cat quantifier
declare_syntax_cat mur_statement
declare_syntax_cat statements
declare_syntax_cat mur_alias
declare_syntax_cat mur_rule
declare_syntax_cat mur_expr
declare_syntax_cat type_expr
declare_syntax_cat decl
declare_syntax_cat var_decls
declare_syntax_cat const_decls
declare_syntax_cat type_decls
declare_syntax_cat var_decl
declare_syntax_cat const_decl
declare_syntax_cat type_decl
declare_syntax_cat program
declare_syntax_cat paramident
declare_syntax_cat paramstr
declare_syntax_cat justparam

syntax (name := paramident1) ident : paramident
syntax (name := paramident2) "£" ident : paramident
syntax (name := paramident3) "£(" ident ")" : paramident

syntax (name := paramstr1) str : paramstr
syntax (name := paramstr2) "£" ident : paramstr

syntax (name := justparam1) "£" ident : justparam
syntax (name := justparam2) "£(" ident ")" : justparam

syntax "var" paramident,+ ":" type_expr : formal
syntax  paramident,+ ":" type_expr : formal
syntax "procedure" paramident "(" sepBy(formal,";") ")" ";" (var_decls* "begin")* mur_statement* ("end" <|> "endprocedure") : proc_decl
syntax "function" paramident "(" sepBy(formal,";",";",allowTrailingSep) ")" ":" type_expr ";" (var_decls* "begin")? (statements)? ("end" <|> "endfunction") : proc_decl
-- TODO: this needs space for the ".", should fix it
syntax paramident : designator
syntax designator "." paramident : designator
syntax designator "[" mur_expr "]" : designator
syntax (name := simplequantifier) paramident ":" type_expr : quantifier
syntax (name := quantifierassign) paramident ":=" mur_expr "to" mur_expr ("by" mur_expr)? : quantifier
syntax designator ":=" mur_expr : mur_statement
syntax "if" mur_expr "then" statements
       ("elsif" mur_expr "then" statements)*
       ("else" sepBy(mur_statement,";",";",allowTrailingSep))? ("endif" <|> "end") : mur_statement
syntax "switch" mur_expr ("case" mur_expr,+ ":" mur_statement*)* ("else" mur_statement*)? ("end" <|> "endswitch") : mur_statement
syntax "for" quantifier "do" sepBy(mur_statement,";","; ",allowTrailingSep) ("end" <|> "endfor") : mur_statement
syntax "while" mur_expr "do" sepBy(mur_statement,";",";",allowTrailingSep) ("end" <|> "endwhile") : mur_statement
syntax "alias" sepBy(mur_alias,";",";",allowTrailingSep) "do" statements ("end" <|> "endalias") : mur_statement
syntax paramident "(" mur_expr,+ ")" : mur_statement
syntax "clear" designator : mur_statement
syntax "error" str : mur_statement
syntax "assert" mur_expr (str)? : mur_statement
syntax "put" (mur_expr <|> str) : mur_statement
syntax "return" (mur_expr)? : mur_statement
syntax "undefine" ident : mur_statement
syntax justparam : statements
syntax mur_statement ";" : statements
syntax justparam ";" : statements
syntax mur_statement ";" statements : statements
syntax justparam ";" statements : statements
syntax paramident ":" mur_expr : mur_alias
syntax "rule" (paramstr)? (mur_expr "==>")? (var_decls* "begin")? statements ("end" <|> "endrule") : mur_rule
-- commenting this out with the above removes the errors on individual statements, which makes no sense to me
-- syntax  "rule" (str)? (expr "==>")? (decl* "begin")? statement* "end" : mur_rule
syntax  "startstate" (str)? (var_decls* "begin")? statements ("end" <|> "endstartstate") : mur_rule
syntax "invariant" (str)? mur_expr : mur_rule
syntax "ruleset" sepBy1(quantifier,";") "do" sepBy(mur_rule,";",";",allowTrailingSep) ("end" <|> "endruleset") : mur_rule -- TODO: see if we need to add (";")?
syntax "alias" sepBy1(mur_alias,";") "do" sepBy(mur_rule,";") ("end" <|> "endalias"): mur_rule
syntax justparam : mur_expr
syntax "(" mur_expr ")" : mur_expr
syntax designator : mur_expr
syntax num : mur_expr
syntax paramident "(" mur_expr,* ")" : mur_expr -- still don't know what "actuals" are
syntax "forall" quantifier "do" mur_expr ("end" <|> "endforall") : mur_expr
syntax "exists" quantifier "do" mur_expr ("end" <|> "endexists") : mur_expr
syntax mur_expr "+" mur_expr : mur_expr
syntax mur_expr "-" mur_expr : mur_expr
syntax mur_expr "*" mur_expr : mur_expr
syntax mur_expr "/" mur_expr : mur_expr
syntax mur_expr "%" mur_expr : mur_expr
syntax mur_expr "|" mur_expr : mur_expr
syntax mur_expr "&" mur_expr : mur_expr
syntax mur_expr "->" mur_expr : mur_expr
syntax mur_expr "<" mur_expr : mur_expr
syntax mur_expr "<=" mur_expr : mur_expr
syntax mur_expr ">" mur_expr : mur_expr
syntax mur_expr ">=" mur_expr : mur_expr
syntax mur_expr "=" mur_expr : mur_expr
syntax mur_expr "!=" mur_expr : mur_expr
syntax "!" mur_expr : mur_expr
syntax mur_expr "?" mur_expr ":" mur_expr : mur_expr
syntax paramident : type_expr
syntax mur_expr ".." mur_expr : type_expr
syntax "enum" "{" paramident,+ "}" : type_expr
syntax "record" sepBy(var_decl,";",";",allowTrailingSep) ("endrecord" <|> "end") : type_expr
syntax "array" "[" type_expr "]" "of" type_expr : type_expr
syntax (name := vardecl) paramident,+ ":" type_expr : var_decl
syntax (name := constdecl) paramident ":" mur_expr : const_decl
syntax (name := typedecl) paramident ":" type_expr : type_decl
syntax "const" sepBy(const_decl,";",";",allowTrailingSep) : const_decls
syntax "type" sepBy(type_decl,";",";",allowTrailingSep) : type_decls
syntax "var" sepBy(var_decl,";",";",allowTrailingSep) : var_decls
syntax (name := constdeclp) justparam : const_decl
syntax (name := vardeclp) justparam : var_decl
syntax (name := typedeclp) justparam : type_decl
syntax justparam : decl
syntax justparam : proc_decl
syntax justparam : mur_rule
syntax const_decls type_decls var_decls sepBy(proc_decl,";",";",allowTrailingSep) sepBy(mur_rule,";",";",allowTrailingSep) : program

syntax "[murϕ|" proc_decl "]" : term
syntax "[murϕ|" designator "]" : term
syntax "[murϕ|" quantifier "]" : term
syntax "[murϕ|" mur_statement "]" : term
syntax "[murϕ|" statements "]" : term
syntax "[murϕ|" mur_alias "]" : term
syntax "[murϕ|" mur_rule "]" : term
syntax "[murϕ|" mur_expr "]" : term
syntax "[murϕ|" type_expr "]" : term
syntax "[murϕ|" var_decls "]" : term
syntax "[murϕ|" const_decls "]" : term
syntax "[murϕ|" type_decls "]" : term
syntax "[murϕ|" decl "]" : term
syntax "[murϕ_program|" program "]" : term
syntax "[murϕ_formal|" formal "]" : term
syntax "[murϕ_proc_decl|" proc_decl "]" : term
syntax "[murϕ_designator|" designator "]" : term
syntax "[murϕ_quantifier|" quantifier "]" : term
syntax "[murϕ_statement|" mur_statement "]" : term
syntax "[murϕ_statements|" statements "]" : term
syntax "[murϕ_alias|" mur_alias "]" : term
syntax "[murϕ_rule|" mur_rule "]" : term
syntax "[murϕ_expr|" mur_expr "]" : term
syntax "[murϕ_type_expr|" type_expr "]" : term
syntax "[murϕ_decl|" decl "]" : term
syntax "[murϕ_var_decl|" var_decl "]" : term
syntax "[murϕ_var_decls|" var_decls "]" : term
syntax "[murϕ_type_decls|" type_decls "]" : term
syntax "[murϕ_const_decls|" const_decls "]" : term

macro_rules
  | `([murϕ| $x:proc_decl  ]) => `(proc_decl| $x)
  | `([murϕ| $x:quantifier ]) => `(quantifier| $x)
  | `([murϕ| $x:statements ]) => `(statements| $x)
  | `([murϕ| $x:mur_statement  ]) => `(mur_statement| $x)
  | `([murϕ| $x:mur_alias  ]) => `(mur_alias| $x)
  | `([murϕ| $x:mur_rule   ]) => `(mur_rule| $x)
  | `([murϕ| $x:mur_expr       ]) => `(mur_expr| $x)
  | `([murϕ| $x:type_expr  ]) => `(type_expr| $x)
  | `([murϕ| $x:decl       ]) => `(decl| $x)
  | `([murϕ| $x:var_decls]) => `(var_decls| $x)
  | `([murϕ| $x:const_decls]) => `(const_decls| $x)
  | `([murϕ| $x:type_decls]) => `(type_decls| $x)
 -- | `([murϕ| $x:designator]) => `(designator| $x)
  | `([murϕ_formal| $x:formal]) => `(formal| $x)
  | `([murϕ_proc_decl| $x:proc_decl]) => `(proc_decl| $x)
  | `([murϕ_quantifier| $x:quantifier]) => `(quantifier| $x)
  | `([murϕ_statement| $x:mur_statement]) => `(mur_statement| $x)
  | `([murϕ_statements| $x:statements]) => `(statements| $x)
  | `([murϕ_alias| $x:mur_alias]) => `(mur_alias| $x)
  | `([murϕ_rule| $x:mur_rule]) => `(mur_rule| $x)
  | `([murϕ_expr| $x:mur_expr]) => `(mur_expr| $x)
  | `([murϕ_type_expr| $x:type_expr]) => `(type_expr| $x)
  | `([murϕ_decl| $x:decl]) => `(decl| $x)
  | `([murϕ_var_decl| $x:var_decl]) => `(var_decl| $x)
  | `([murϕ_designator| $x:designator]) => `(designator| $x)
  | `([murϕ_program| $x:program    ]) => `(program| $x)
  | `([murϕ_var_decls| $x:var_decls]) => `(var_decls| $x)
  | `([murϕ_const_decls| $x:const_decls]) => `(const_decls| $x)
  | `([murϕ_type_decls| $x:type_decls]) => `(type_decls| $x)


abbrev TMacro α := TSyntax α → MacroM Term

private def foldlSyntaxArrayJoinAux
  {α : SyntaxNodeKinds}
  (expandFun : TSyntax α → MacroM Term)
  (rest : Term)
  (val : TSyntax α) : MacroM Term := do
  `($rest ++ $(← expandFun val))

def foldlSyntaxArrayJoin {α : SyntaxNodeKinds} :
TSyntaxArray α → (TSyntax α → MacroM Term) → MacroM Term
  | es, expandFun => do
    let folded : Term ← es.foldlM (foldlSyntaxArrayJoinAux expandFun) (← `([]))
    return Lean.quote folded

def mapSyntaxArray {α : SyntaxNodeKinds} :
TSyntaxArray α → (TSyntax α → MacroM Term) → MacroM Term
  | es, expandFun => do
    let esArr : Array Term ← es.mapM expandFun
    return Lean.quote esArr.toList

-- HACK!
-- def expandTMacros {α} : TSyntax α → MacroM Term
--   | t => do return ⟨← Lean.expandMacros t⟩

def expandJustParam : TMacro `justparam
|  `(justparam| £ $x:ident ) => `($x)
|  `(justparam| £($x:ident) ) => `($x)
| _ => Lean.Macro.throwUnsupported

@[macro paramident1]
def expandJustParamMacro1 : Macro
  | `(justparam| $p) => expandJustParam p

@[macro paramident2]
def expandJustParamMacro2 : Macro
  | `(justparam| $p) => expandJustParam p

def expandParamIdent : TMacro `paramident
  |  `(paramident| $x:ident) => `($(Lean.quote x.getId.toString))
  |  `(paramident| £ $x:ident ) => `($x)
  |  `(paramident| £($x:ident) ) => `($x)
  | _ => Lean.Macro.throwUnsupported

-- This feels very hacky! But it won't work with <|>. TODO: I should produce an MWE
@[macro paramident1]
def expandParamIdentMacro1 : Macro
  | `(paramident| $p) => expandParamIdent p

@[macro paramident2]
def expandParamIdentMacro2 : Macro
  | `(paramident| $p) => expandParamIdent p

@[macro paramident3]
def expandParamIdentMacro3 : Macro
  | `(paramident| $p) => expandParamIdent p

def expandParamStr : TMacro `paramstr
  |  `(paramstr| $x:str) => `($x)
  |  `(paramstr| £ $x:ident ) => `($x)
  | _ => Lean.Macro.throwUnsupported

@[macro paramstr1]
def expandParamStrMacro1 : Macro
  | `(paramstr| $p) => expandParamStr p

@[macro paramstr2]
def expandParamStrMacro2 : Macro
  | `(paramstr| $p) => expandParamStr p

def expandVarDecl : TMacro `var_decl
  | `(var_decl| $[$ids:paramident],* : $t:type_expr ) => do
    let idsList : Term ← mapSyntaxArray ids expandParamIdent
    `([Decl.var $idsList [murϕ_type_expr| $t]])
  | `(var_decl| $p:justparam) => do `($(← expandJustParam p))
  | _ => Lean.Macro.throwUnsupported

-- Hack to lift expandVarDecl to the untyped macro world
@[macro vardecl]
def expandVarDeclMacro : Macro
 | `(var_decl| $v) => expandVarDecl v

@[macro vardeclp]
def expandVarDeclPMacro : Macro
 | `(var_decl| $v) => expandVarDecl v

-- syntax paramident ":" expr : const_decl
def expandTypeDecl : TMacro `type_decl
  | `(type_decl| $id:paramident : $t:type_expr ) => do
    `([Decl.type $(← expandParamIdent id) [murϕ_type_expr| $t]])
  | `(type_decl| $p:justparam) => do `($(← expandJustParam p))
  | _ => Lean.Macro.throwUnsupported


@[macro typedeclp]
def expandTypeDeclPMacro : Macro
 | `(type_decl| $d:type_decl) => expandTypeDecl d

@[macro typedecl]
def expandTypeDeclMacro : Macro
 | `(type_decl| $d:type_decl) => expandTypeDecl d

def expandConstDecl : TMacro `const_decl
  | `(const_decl| $id:paramident : $e:mur_expr ) => do
    `([Decl.const $(← expandParamIdent id) [murϕ_expr| $e]])
  | `(const_decl| $p:justparam) => do `($(← expandJustParam p))
  | _ => Lean.Macro.throwUnsupported

@[macro constdecl]
def expandConstDeclMacro : Macro
  | `(const_decl| $d) => expandConstDecl d

@[macro constdeclp]
def expandConstDeclPMacro : Macro
  | `(const_decl| $d) => expandConstDecl d

def expandSimpleQuantifier : TMacro `quantifier
  | `(quantifier| $x:paramident : $t) => do
   `(Quantifier.simple $(← expandParamIdent x) [murϕ_type_expr| $t])
  | _ => Lean.Macro.throwUnsupported

@[macro simplequantifier]
def  expandSimpleQuantifierMacro : Lean.Macro
  | `(quantifier| $v) => expandSimpleQuantifier v

@[macro quantifierassign]
def expandQuantifierAssign : Lean.Macro
  | `(quantifier| $x:paramident := $e₁ to $e₂ $[by $e₃]?) => do
    let x' <- expandParamIdent x
    -- TODO: deal with e₃
    `(Quantifier.assign $x' [murϕ_expr| $e₁] [murϕ_expr| $e₂] none)
  | _ => Lean.Macro.throwUnsupported

--def expandQuantifier := expandSimpleQuantifier >=> expandQuantifierAssign

macro_rules
  | `(var_decls| var $[$vardecls];*) => do
   foldlSyntaxArrayJoin vardecls expandVarDecl
  | `(type_decls| type $[$typedecls];*) => do
   foldlSyntaxArrayJoin typedecls expandTypeDecl
  | `(const_decls| const $[$constdecls];*) => do
   foldlSyntaxArrayJoin constdecls expandConstDecl
  | `(decl| $p:justparam) => do `($(← expandJustParam p))

macro_rules
  | `(type_expr| $x:paramident) => do `(TypeExpr.previouslyDefined $(← expandParamIdent x))
  | `(type_expr| $x:mur_expr .. $y) => do `(TypeExpr.integerSubrange [murϕ_expr| $x] [murϕ_expr| $y])
  | `(type_expr| enum { $[$ids],*} ) => do `(TypeExpr.enum $(← mapSyntaxArray ids expandParamIdent))
  | `(type_expr| record $[$decls];* end ) => do `(TypeExpr.record $(← foldlSyntaxArrayJoin decls λ d => `([murϕ_var_decl| $d]) ) )
  | `(type_expr| array[$t₁] of $t₂) => do `(TypeExpr.array [murϕ_type_expr| $t₁] [murϕ_type_expr| $t₂])


macro_rules
  | `(mur_rule| rule $(x)? $e ==> $(vds)* begin $stmts end) => do
    let xSyn <- match x with
      | none => `(none)
      | some x' =>
        let expanded <- expandParamStr x'
        `(some $expanded)
    let dsSyn ← mapSyntaxArray vds λ d => `([murϕ_var_decls| $d])
    `([Rule.simplerule $xSyn [murϕ_expr| $e] (List.join $dsSyn) [murϕ_statements| $stmts]])
  | `(mur_rule| ruleset $[$quantifiers];* do $[$rules];* endruleset ) => do
    let qs <- mapSyntaxArray quantifiers λ q => `([murϕ_quantifier| $q])
    let rs <- foldlSyntaxArrayJoin rules λ r => `([murϕ_rule| $r])
    `([Rule.ruleset $qs $rs])
  | `(mur_rule| invariant $[$s]? $e) => match s with
    | none => `(Rule.invariant none [murϕ_expr| $e])
    | some str => `([Rule.invariant (some $str) [murϕ_expr| $e]])
  | `(mur_rule| startstate $[$name]? $[$decls]* begin $stmts end) => do
    let dsSyn ← mapSyntaxArray decls λ d => `([murϕ_var_decls| $d])
    let nameSyn ← match name with
      | none => `(none)
      | some str => `(some $str)
     `([Rule.startstate $nameSyn $dsSyn [murϕ_statements| $stmts]])
  | `(mur_rule| startstate $[$name:str]? $stmts:statements end) => do
    let nameSyn ← match name with
      | none => `(none)
      | some str => `(some $str)
    `([Rule.startstate $nameSyn [] [murϕ_statements| $stmts]])
  | `(mur_rule| $p:justparam) => do `($(← expandJustParam p))

syntax  "startstate" (str)? (decl "begin")? statements ("end" <|> "endstartstate") : mur_rule

macro_rules
  | `(statements| $stmt:justparam ;) => do `( [ $(← expandJustParam stmt) ])
  | `(statements| $stmts:justparam ) => do `( $(← expandJustParam stmts))
  | `(statements| $stmt:mur_statement ;) => `( [ [murϕ_statement| $stmt] ])
  | `(statements| $stmt:justparam ; $stmts:statements) => do `( $(← expandJustParam stmt) ++ [murϕ_statements| $stmts])
  | `(statements| $stmt:mur_statement ; $stmts:statements) => `([murϕ_statement| $stmt] :: [murϕ_statements| $stmts])

macro_rules
  | `(mur_expr| $p:justparam) => do do `( $(← expandJustParam p))
  | `(mur_expr| $x + $y) => `(Murϕ.Expr.binop "+" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x - $y) => `(Murϕ.Expr.binop "-" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x * $y) => `(Murϕ.Expr.binop "*" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x / $y) => `(Murϕ.Expr.binop "/" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x % $y) => `(Murϕ.Expr.binop "%" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x | $y) => `(Murϕ.Expr.binop "|" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x & $y) => `(Murϕ.Expr.binop "&" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x -> $y) => `(Murϕ.Expr.binop "->" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x < $y) => `(Murϕ.Expr.binop "<" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x <= $y) => `(Murϕ.Expr.binop "<=" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x > $y) => `(Murϕ.Expr.binop ">" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x >= $y) => `(Murϕ.Expr.binop ">=" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x = $y) => `(Murϕ.Expr.binop "=" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| $x != $y) => `(Murϕ.Expr.binop "!=" [murϕ_expr| $x] [murϕ_expr| $y])
  | `(mur_expr| !$x) => `(Murϕ.Expr.negation [murϕ_expr| $x])
  | `(mur_expr| ($e:mur_expr)) => `([murϕ_expr| $e])
  | `(mur_expr| $x:designator ) => `(Murϕ.Expr.designator [murϕ_designator| $x])
  | `(mur_expr| $x:num ) => `(Murϕ.Expr.integerConst $x)
  | `(mur_expr| $x:paramident($es:mur_expr,*) ) => do
    let args <- mapSyntaxArray es.getElems λ e => `([murϕ_expr|$e])
    `(Murϕ.Expr.call $(← expandParamIdent x) $args)
--  syntax paramident "(" expr,* ")" : expr -- still don't know what "actuals" are

macro_rules
  | `(designator| $x:paramident ) => do `(Designator.mk $(← expandParamIdent x) [])
  | `(designator| $d:designator [$e:mur_expr] ) => `(Designator.cons [murϕ_designator| $d] $ Sum.inr [murϕ_expr| $e])
  | `(designator| $d:designator . $x:paramident ) => do `(Designator.cons [murϕ_designator| $d] $ Sum.inl $(← expandParamIdent x))

macro_rules
  | `(mur_statement| $x:designator := $y ) => `(Statement.assignment [murϕ_designator| $x] [murϕ_expr| $y])
  | `(mur_statement| for $q do $[$stmts];* endfor) => do
  let stmtsSyntax ← mapSyntaxArray stmts λ s => `([murϕ_statement| $s])
  `(Statement.forstmt [murϕ_quantifier| $q] $stmtsSyntax)
  | `(mur_statement| while $expr do $[$stmts];* end) => do
  let stmtsSyntax ← mapSyntaxArray stmts λ s => `([murϕ_statement| $s])
  `(Statement.whilestmt [murϕ_expr| $expr] $stmtsSyntax)
  | `(mur_statement| if $e then $thens:statements $[elsif $es then $elsifs:statements]* $[else $[$opElses];*]? endif) => do
  let thensSyntax ← `([murϕ_statements| $thens])
  let elseifsSyntax ← es.toList.zip elsifs.toList |>.mapM λ (exp, stmts) => `(([murϕ_expr| $exp],[murϕ_statements| $stmts]))
  let elses := match opElses with
    | none => #[]
    | some es => es
  let elsesSyntax ← mapSyntaxArray elses λ s => `([murϕ_statement| $s])
  `(Statement.ifstmt [murϕ_expr| $e] $thensSyntax $(Lean.quote elseifsSyntax) $elsesSyntax)
  | `(mur_statement| assert $e $[$s]?) =>
  let strSyn := match s with
    | some strSyn => strSyn
    | none => Lean.quote ""
  `(Statement.assertstmt [murϕ_expr| $e] $strSyn)
  | `(mur_statement| error $msg) => `(Statement.errorstmt $msg)
  | `(mur_statement| return $[$exp]?) => match exp with | none => `(Statement.returnstmt none) | some e => `(Statement.returnstmt (some [murϕ_expr| $e]))
  | `(mur_statement| undefine $id ) => `(Statement.undefine $(quote id.getId.toString))
  | `(mur_statement| alias $[$aliases];* do $stmts end) => do
    let aliasesSyn ← mapSyntaxArray aliases λ a => `([murϕ_alias| $a ])
    `(Statement.aliasstmt $aliasesSyn [murϕ_statements| $stmts])

macro_rules
  | `(formal| var $[$pis],* : $te) => do
    let ids ← mapSyntaxArray pis expandParamIdent
    `(Formal.mk true $(ids) [murϕ_type_expr| $te])
  | `(formal| $[$pis],* : $te) => do
    let ids ← mapSyntaxArray pis expandParamIdent
    `(Formal.mk false $(ids) [murϕ_type_expr| $te])

macro_rules
  | `(proc_decl| function $pi ($[$formals:formal];*) : $te; $[$decls:var_decls]* begin $[$opStmts:statements]? end) => do
    let formalsSyn ← mapSyntaxArray formals λ f => `([murϕ_formal| $f])
    let declsArray : Array Term ← decls.mapM λ d => `([murϕ_var_decls| $d])
    let declsSyn : Term ← `(List.join $(Lean.quote declsArray.toList))
    let stmts ← match opStmts with | none => `([]) | some ss => `([murϕ_statements| $ss])
    `([ProcDecl.function $(← expandParamIdent pi) $formalsSyn [murϕ_type_expr| $te] $declsSyn $stmts])
  | `(proc_decl| function $pi ($[$formals:formal];*) : $te;  $[$opStmts:statements]? end) => do
    let formalsSyn ← mapSyntaxArray formals λ f => `([murϕ_formal| $f])
    let stmts ← match opStmts with | none => `([]) | some ss => `([murϕ_statements| $ss])
    `([ProcDecl.function $(← expandParamIdent pi) $formalsSyn [murϕ_type_expr| $te] [] $stmts])
  | `(proc_decl| $p:justparam) => do `($(← expandJustParam p))

macro_rules
  | `(mur_alias|  $pi:paramident : $expr ) => do `(Alias.mk $(← expandParamIdent pi) [murϕ_expr| $expr])

macro_rules
  | `(program|  $cdecls:const_decls $tdecls:type_decls $vdecls:var_decls $[$procdecls];* $[$rules];*) => do
    let procdeclsSyn ← foldlSyntaxArrayJoin procdecls λ d => `([murϕ_proc_decl| $d])
    let rulesSyn ← foldlSyntaxArrayJoin rules λ r => `([murϕ_rule| $r])
    `({ vardecls := [murϕ_var_decls| $vdecls], typedecls := [murϕ_type_decls| $tdecls],
        constdecls := [murϕ_const_decls| $cdecls],
        procdecls := $procdeclsSyn, rules := $rulesSyn : Program})

def foo := "bar"
#eval [murϕ| var foo : baz]
#eval [murϕ| var £foo : baz]

def test := "hi"
-- #eval [murϕ| (£test)_13 ]
-- #check [murϕ_statements|
-- sq := Sta.core_[j].lsq_.sq_;
-- lq := Sta.core_[j].lsq_.lq_;
-- ]

#check [murϕ| ld_entry .phys_addr := ld_entry .virt_addr]
#check [murϕ| ld_entry .phys_addr := ld_entry .virt_addr]
#check [murϕ| £foo .phys_addr := ld_entry .virt_addr]
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

Sta := next_state;
end;
endruleset

]

def somestmts := [murϕ| a := b; b := c; ]
def somestmts' := [murϕ| £somestmts; b := c; ]
def onestmt := [murϕ| b := c ]

#check [murϕ| if (true) then
  £somestmts
  endif]
#check [murϕ_statement|
  alias mem:init_state.mem_ do
    for i : addr_idx_t do
      mem.arr[i] := 0;
    end;
    --#mem.msg 
  endalias
]

#check [murϕ_statement| undefine init_state]
#check [murϕ_statements|
  undefine init_state;
  alias mem:init_state.mem_ do
    for i : addr_idx_t do
      mem.arr[i] := 0;
    end;
    --#mem.msg 
  endalias;
]


end Murϕ
