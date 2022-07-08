import PipelineDsl.Murphi

open Murϕ

def inttype := TypeExpr.previouslyDefined "Int"
def testdeclvar := Decl.type "x" inttype
def testdeclconst := Decl.const "y"  (Expr.integerConst 0)
def simpledes := λ s => Expr.designator $ Designator.mk s []
def testfun := ProcDecl.function "foo" [Formal.mk true ["x","y"] inttype] inttype []
              [Statement.returnstmt $ Expr.binop "+" (simpledes "x") (simpledes "y")]
def simpleif := Statement.ifstmt (Expr.negation (Expr.binop "==" (simpledes "x") (simpledes "y"))) [Statement.assignment (Designator.mk "x" []) (Expr.binop "+" (simpledes "x") (Expr.integerConst 1))] none []
def testrule := Rule.startstate none [] [simpleif]
def testprog : Program := {decls := [testdeclvar, testdeclconst], procdecls := [testfun], rules := [testrule]}

#eval testprog.toString
