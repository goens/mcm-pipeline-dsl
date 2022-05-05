

inductive structure_description
| thing : Nat → structure_description

inductive AST
-- | struct_descript : structure_description → AST
| struct_descript : structure_description → AST
-- | none -- is this right? can I say none here?

-- ================ inner example (structure_description)

def get_inner (sth : structure_description) : Nat :=
match sth with
| structure_description.thing x => x

def ex1 : structure_description := structure_description.thing 4

#check AST
#eval get_inner ex1

-- ================== outter example (AST example)

def get_inner_ast (sth : AST) : structure_description :=
match sth with
| AST.struct_descript stuff => stuff

def ex2 : AST := AST.struct_descript ( structure_description.thing 3 )
#check get_inner_ast ex2

#check get_inner_ast ex2
#eval get_inner ( get_inner_ast ex2 )
-- .structure_description.thing 4

