

inductive structure_description
| thing : Nat → structure_description

inductive AST
-- | struct_descript : structure_description → AST
| struct_descript : structure_description → AST
| none -- is this right? can I say none here?

-- ================ inner example (structure_description)

def get_inner (sth : Option structure_description) : Option Nat :=
match sth with
| structure_description.thing x => x
| none => none

def ex1 : structure_description := structure_description.thing 4

#check AST
#eval get_inner ex1

-- ================== outter example (AST example)

def get_inner_ast (sth : AST) : Option structure_description :=
match sth with
| AST.struct_descript stuff => stuff
| AST.none => none

def ex2 : AST := AST.struct_descript ( structure_description.thing 3 )
#check get_inner_ast ex2

#check get_inner_ast ex2
#eval get_inner ( get_inner_ast ex2 )
-- .structure_description.thing 4



inductive list_test
| somthing : List Nat → list_test

def ex4 : list_test := list_test.somthing []

def get_list (x : list_test) : List Nat :=
match x with
| list_test.somthing y => --dbg_trace y
y

def get_list_item (x: List Nat) : Nat :=
match x with
| h :: t => h
| [] => 0

#check ex4
#eval get_list ex4
#eval get_list_item (get_list ex4)

inductive factor
| simple : Nat → factor
| unary : String → factor → factor

inductive expr
| expr_factor : factor → expr
| term : factor → List (String × factor) → expr

def ex5 : expr := expr.expr_factor ( factor.simple 7 )
def ex6 : factor := factor.simple 9
def ex7 : factor := factor.unary "EY YO!!!"  ex6

def get_inner_fact (sth : factor) : Nat :=
match sth with
| factor.simple y => y
-- | factor.unary ls => get_inner_fact ls.2
| factor.unary st sth => get_inner_fact sth --get_inner_fact ls.2

def get_inner_expr (x : expr) : Nat :=
match x with
| expr.expr_factor n => get_inner_fact n
| expr.term m (l :: ls) => get_inner_fact m
| expr.term m [] => get_inner_fact m

#check ex5
#eval get_inner_expr ex5


-- ========================================================

inductive one_or_more_term
| right_recursion : term → term_tail → one_or_more_term

inductive term_tail
| recursion : term → term_tail
| zero : ε → term_tail

inductive term
| term : Nat → term

inductive ε
| nothing : String → ε

-- ======= ONE TERM ======
def ex8 : term := term.term 42
def ex9 : one_or_more_term := one_or_more_term.right_recursion ex8 ""
def ex10 : one_or_more_term := one_or_more_term.right_recursion 69 "heh"
def ex11 : term_tail := term_tail.recursion ( one_or_more_term.right_recursion 42 "lol !" )
-- ======= TWO TERMS? ======
def ex12 : one_or_more_term := one_or_more_term.right_recursion 42 term_tail

#check ex12

-- =======================================================

inductive item
| something : item → item → item
| int : Nat → item
| nothing : item

-- ==== ONE TERM ====
def ex13 : item := item.int 1
def ex14 : item := item.something ex13 item.nothing
-- ==== TWO TERMS ====
def ex15 : item := item.int 2
def ex16 : item := item.something ex15 ex14 