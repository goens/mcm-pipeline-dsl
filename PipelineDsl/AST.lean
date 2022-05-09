
inductive AST
| structure_descriptions : List structure_description → AST

inductive descriptions
-- constructors have the same signature, but will use different keywords
| structure_specification :
String /- structure -/ → structure_name →
String /- { -/ → List statement_ → String /- } -/ →
descriptions
| structure_state :
String /- state -/ → structure_name →
String /- { -/ → List statement_ → String /- } -/ →
descriptions
| structure_init_state :
String /- init_state -/ → structure_name →
String /- { -/ → List statement_ → String /- } -/ →
descriptions
| structure_transition :
String /- transition -/ → structure_name →
String /- { -/ → List statement_ → String /- } -/ →
descriptions
-- Function definition
| function_definition :
typed_function → String /- ( -/ → arg_list → String /- ) -/ →
String /- { -/ → List statement_ → String /- } -/ →
descriptions

inductive arg_list
| args : expr → List (String /- , -/ × expr) → arg_list
| none : arg_list

inductive typed_function
| type_and_name : type_def → function_name → typed_function

inductive type_def
| type_id : identifier → type_def

inductive structure_name
| structure_id : identifier → structure_name

inductive function_name
| function_id : identifier → function_name

-- leaving semicolons optional for now
inductive statement_ -- lean 4 asks to specify universe without the _
| statement_with_delimiter : statement_core → String → statement_
| statement_no_delim       : statement_core → statement_
| block : String /- { -/ → List statement → String /- } -/ → statement_

inductive statement_core
| labelled_statement : label → statement_core
-- declare a variable
| declaration : type_def → var → statement_core
-- declare a variable with a value
| value_declaration : type_def → var → String /- = -/ → expr → statement_core
-- assign a var an expr
| variable_assignment : var → String /- = -/ → expr → statement_core
-- if statement, if else statement
| conditional_stmt : conditional → statement_core
-- function call?
| try_catch :
String /- try -/ → String /- { -/ → List statement_ → String /- } -/ →
catch_blocks → statement_core
-- await?
| await :
String /- await -/ →
String /- { -/ → List statement_ → String /- } -/ →
statement_core
| when :
String /- when -/ → qualified_function →
String /- { -/ → List statement_ → String /- } -/ →
statement_core
-- transition to an explicit state
| transition :
String /- transition -/ → String /- { -/ → List statement_ → String /- } -/ →
statement_core
-- should just be a function call
| stray_expr : expr → statement_core

-- explicit label type..
inductive label
| label_keyword : identifier → label

inductive qualified_function
| structure_interface : structure_name → String /- . -/ → function_name →
                        qualified_function


-- one or more catch blocks
inductive catch_blocks
| catch_block :
String /- catch -/ → String /- ( -/ → qualified_function → String /- ) -/ →
String /- { -/ → List statement_ → String /- } -/ →
catch_blocks → catch_blocks
| nothing : catch_blocks -- one or more, this is the "none"

inductive conditional
-- if with else
| if_else_statement :
String /- if -/ → String /- ( -/ → expr → String /- ) -/ →
statement_ →
String /- else -/ →
statement_ →
conditional
-- if without else
| if_statement :
String /- if -/ → String /- ( -/ → expr → String /- ) -/ →
String /- { -/ → statement_ → String /- } -/ →
conditional

-- TODO: Test this (expr) in a sandbox
inductive expr
| add : expr → String → term → expr
| sub : expr → String → term → expr
| greater_than : expr → String → term → expr
| less_than    : expr → String → term → expr
| equal        : expr → String → term → expr
| not_equal : expr → String → term → expr
| some_term : term → expr
-- | term : factor → expr → expr
-- | nothing : expr

inductive term
| mult : term → String → factor → term
| div  : term → String → factor → term
| some_factor : factor → term
-- | lsh  : expr → String → term → expr
-- | rsh  : expr → String → term → expr

inductive factor
| negation: String → factor → factor
| parentheses: String → expr → String → factor
| variable_ : var → factor -- variable is a lean keyword...
| constant_ : const → factor -- constant is a lean keyword...
-- function call
| function_call :
function_name →
String /- ( -/ → expr → List (String × expr) → String /- ) -/ → factor

inductive identifier
| name : String → identifier -- non-empty string

inductive var
| var_id : identifier → var -- non-empty string

inductive const
| literal : Nat → const -- might require String for the text?

--- ==== some tests... =====

def ex0000 : identifier := identifier.name "hullo"
def ex0001 : var := var.var_id ex0000
def ex0002 : factor := factor.variable_ ex0001
def ex0003 : term := term.some_factor ex0002

-- === expression
def ex0004 : expr := expr.some_term ex0003

-- === statement_core
def ex0005 : statement_core := statement_core.stray_expr ex0004

-- === statement no delim
def ex0006 : statement_ := statement_.statement_no_delim ex0005

-- === statement with delimiter
def ex0007 : statement_ := statement_.statement_with_delimiter ex0005 ";"

-- === Conditional
def ex0008 : conditional := conditional.if_else_statement
"if" "(" ex0004 ")" ex0007 "else" ex0007

-- === await
def ex0009 : statement_core := statement_core.await
"await" "{" [ ex0007 ] "}"

-- === await in statement
def ex0010 : statement_ := statement_.statement_no_delim ex0009

-- === descriptions
def ex0011 : descriptions := descriptions.structure_specification
"structure" "example_structure" "{" [ ex0010 ] "}"

-- === AST with 1 description!
def ex0012 : AST := AST.structure_descriptions [ ex0011 ]
