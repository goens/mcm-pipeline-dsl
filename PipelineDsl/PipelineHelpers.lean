import PipelineDsl.AST

open Pipeline

abbrev when_stmt := Statement.when -- when <msg>() from <ctrler> { <stmts> }
abbrev qualified_var := Term.qualified_var -- i.e. "entry.instruction.seq_num", LQ.tail_search, etc.
abbrev equal := Expr.equal -- equal
abbrev sub := Expr.sub -- subtract
abbrev function_call := Term.function_call -- function_call
abbrev some_term := Expr.some_term -- Expr.some_term
abbrev less_than := Expr.less_than -- less_than
abbrev await := Statement.await -- await

abbrev str_lit := Const.str_lit
abbrev term_expr := Term.expr
abbrev binand := Expr.binand

abbrev SearchStatement := Statement

abbrev conditional_stmt := Statement.conditional_stmt
abbrev if_else_statement := Conditional.if_else_statement
abbrev reset := Statement.reset

abbrev not_equal := Expr.not_equal
abbrev num_lit := Const.num_lit
abbrev var_term := Term.var
abbrev or_expr := Expr.binor
abbrev expr_term := Term.expr
abbrev bool_decl := Statement.variable_declaration
abbrev variable_assignment := Statement.variable_assignment
abbrev transition := Statement.transition
abbrev complete := Statement.complete
abbrev state := Description.state
abbrev stray_expr := Statement.stray_expr
abbrev variable_declaration := Statement.variable_declaration

abbrev value_decl := Statement.value_declaration

abbrev if_statement := Conditional.if_statement
  
def Pipeline.Statement.to_block : Statement → Statement
| stmt => match stmt with
  | .block _ => stmt
  | _ => Statement.block [stmt]

-- NOTE: "to_block_if_not"
def List.to_block (stmts : List Pipeline.Statement) : Pipeline.Statement :=
  match stmts with
  | [stmt] => match stmt with
    | .block _ => stmt
    | _ => Pipeline.Statement.block stmts
  | _ => Pipeline.Statement.block stmts

def Prod.to_typed_identifier ( tuple : Prod String String) : TypedIdentifier :=
  TypedIdentifier.mk (tuple.1 : Identifier) (tuple.2 : Identifier)

def Pipeline.Description.state_name : Pipeline.Description → Except String Identifier
| .state ident /- stmt -/ _ => pure ident
| _ => throw "Error: Description is not a state"

def Pipeline.Description.stmt : Pipeline.Description → Except String Pipeline.Statement
| .state /- ident -/ _ stmt => pure stmt
| _ => throw "Error: Description is not a state"

def Pipeline.Statement.stmt_block : Pipeline.Statement → Except String (List Pipeline.Statement)
| .block stmts => pure stmts
| _ => throw "Statement is not a block"
