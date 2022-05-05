

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
| list_test.somthing y => dbg_trace y
y

def get_list_item (x: List Nat) : Nat :=
match x with
| h :: t => h
| [] => 

#check ex4
#eval get_list ex4