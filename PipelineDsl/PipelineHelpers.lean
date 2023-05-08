import PipelineDsl.AST

abbrev when_stmt := Pipeline.Statement.when -- when <msg>() from <ctrler> { <stmts> }
abbrev qualified_var := Pipeline.Term.qualified_var -- i.e. "entry.instruction.seq_num", LQ.tail_search, etc.
abbrev equal := Pipeline.Expr.equal -- equal
abbrev sub := Pipeline.Expr.sub -- subtract
abbrev function_call := Pipeline.Term.function_call -- function_call
abbrev some_term := Pipeline.Expr.some_term -- Expr.some_term
abbrev less_than := Pipeline.Expr.less_than -- less_than
abbrev await := Pipeline.Statement.await -- await

abbrev str_lit := Pipeline.Const.str_lit
abbrev term_expr := Pipeline.Term.expr
abbrev binand := Pipeline.Expr.binand

abbrev SearchStatement := Pipeline.Statement

abbrev conditional_stmt := Pipeline.Statement.conditional_stmt
abbrev if_else_statement := Pipeline.Conditional.if_else_statement
abbrev reset := Pipeline.Statement.reset

abbrev not_equal := Pipeline.Expr.not_equal
abbrev num_lit := Pipeline.Const.num_lit
abbrev var_term := Pipeline.Term.var
abbrev or_expr := Pipeline.Expr.binor
abbrev expr_term := Pipeline.Term.expr
abbrev bool_decl := Pipeline.Statement.variable_declaration
abbrev variable_assignment := Pipeline.Statement.variable_assignment
abbrev transition := Pipeline.Statement.transition
abbrev complete := Pipeline.Statement.complete
abbrev state := Pipeline.Description.state
abbrev stray_expr := Pipeline.Statement.stray_expr
abbrev variable_declaration := Pipeline.Statement.variable_declaration

abbrev value_decl := Pipeline.Statement.value_declaration