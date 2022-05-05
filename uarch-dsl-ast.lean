

inductive AST
| structure_description
| none -- is this right? can I say none here?

inductive structure_description
| structure_specification
| structure_state
| structure_transition

inductive structure_specification
| statements --statements? Declarations?
/- how to define it as multiple statements -/

#check structure_specification

inductive statements
| statements : List statement → statements
| none

inductive statement
| complete_statement : statement_core → String → statement

inductive statement_core
-- explicitly separate "declaration" and "value_declaration"
-- To explicitly only allow declaration in the state list
-- alternatively, allow for assignment / default state assignment?
 -- declare a variable
 | declaration : type_def → var → statement_core
 -- declare a variable with a value
 | value_declaration : type_def → var → String → expr → statement_core
 -- assign a var an expr
 | variable_assignment : var → String → expr → statement_core
 -- if statement, if else statement
 | conditional : 
 | try_catch -- function call?
 | await -- await?
 | transition -- transition to an explicit state

inductive conditional
| if_statement

inductive if_statement
| -- if (expr) statement else statement

inductive uninit_declaration -- assign the declaration name
| -- declarations with or without var assignment