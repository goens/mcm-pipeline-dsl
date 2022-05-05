
-- TODO: Remember to flip the order of the type definitions!
-- since lean isn't smart enough to match the lazy
-- type definitions with the later defined definitions..

inductive AST
| structure_descriptions : List structure_description → AST
-- don't need to model none, if we use a list
-- | none -- must handle none in every function later..

inductive structure_description
-- constructors have the same signature, but will use different keywords
| structure_specification : String → String → statements → String → structure_description
| structure_state : String → String → statements → String → structure_description
| structure_init_state : String → String → statements → String → structure_description
| structure_transition : String → String → statements → String → structure_description

-- inductive structure_specification
-- | statements --statements? Declarations?
/- how to define it as multiple statements -/

#check structure_description.structure_specification

-- statements, zero or more
inductive statements
-- Don't know if I want to allow including these arbitrary scopes..
-- | scoped_statements : String /- { -/ → List statement → String /- } -/ →
--                       statements
| statements        : List statement → statements
-- Don't need none if we use list
-- | none

-- do we enforce semi-colons at the end?
-- i'm just leaving it optional for now
inductive statement
| complete_statement_with_delimiter : statement_core → String → statement
| complete_statement                : statement_core → statement

inductive statement_core
-- explicitly separate "declaration" and "value_declaration"
-- To explicitly only allow declaration in the state list
-- alternatively, allow for assignment / default state assignment? Not for now.
 -- declare a variable
 | declaration : type_def → var → statement_core
 -- declare a variable with a value
 | value_declaration : type_def → var → String → expr → statement_core
 -- assign a var an expr
 | variable_assignment : var → String → expr → statement_core
 -- if statement, if else statement
 | conditional_stmt : conditional → statement_core
 -- function call?
 | try_catch : String /- try -/ → String /- { -/ → statements → String /- } -/ →
               catch_blocks → statement_core
-- TODO: await, and transition
 | await -- await?
 | transition -- transition to an explicit state

-- one or more catch blocks
-- Is there a nicer way to do 1 or more?
 inductive catch_blocks
 | catch_block : String /- catch -/ →
                 String /- { -/ → statements → String /- } -/ →
                 List catch_block → catch_blocks

inductive conditional
-- if with else
| if_else_statement : String /- if -/ → String /- ( -/ → expr → String /- ) -/ →
                      String /- { -/ → statements → String /- } -/ →
                      String /- else -/ →
                      String /- { -/ → statements → String /- } -/ → conditional
-- if without else
| if_statement      : String /- if -/ → String /- ( -/ → expr → String /- ) -/ →
                      String /- { -/ → statements → String /- } -/ → conditional

-- TODO: Still need to define Exprs