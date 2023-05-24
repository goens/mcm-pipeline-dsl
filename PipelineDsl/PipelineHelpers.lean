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

def List.to_qual_name (idents : List Identifier) : Pipeline.QualifiedName :=
  Pipeline.QualifiedName.mk idents

def var_expr (var_name : Identifier) : Pipeline.Expr :=
 Pipeline.Expr.some_term $ Pipeline.Term.var var_name

-- def var_term (var_name : Identifier) : Pipeline.Term := Pipeline.Term.var var_name

def qual_var_expr (var_name : List Identifier) : Pipeline.Expr :=
 Pipeline.Expr.some_term $ Pipeline.Term.qualified_var var_name.to_qual_name

def qual_var_term (var_name : List Identifier) : Pipeline.Term :=
 Pipeline.Term.qualified_var var_name.to_qual_name

def var_asn_var (var1 : List String) (var2 : String) : Statement :=
  variable_assignment var1.to_qual_name <| var_expr var2

def List.to_dsl_var_expr : List Identifier → Pipeline.Expr
| idents =>
  Pipeline.Expr.some_term (Pipeline.Term.qualified_var idents.to_qual_name)

-- function that checks if an expr is in a list of exprs
-- and finds an expr with the matching index in another list
-- then returns that expr
def Pipeline.Expr.match_expr_from_to_list_upon_match
(expr : Expr)
(src_exprs : List Expr)
(dest_exprs : List Expr)
: Except String (Option Expr) := do
  let src_dest_translation := ← 
    match src_exprs.length == dest_exprs.length with
    | true => do pure $ src_exprs.zip dest_exprs
    | false => do
      throw s!"Error: src_exprs and dest_exprs are not the same length. src_exprs: ({src_exprs}), dest_exprs: ({dest_exprs})"
  
  let dest_translations :=
    src_dest_translation.filter (·.1 == expr)
  
  match dest_translations with
  | [dest_expr] => do pure $ some dest_expr.2
  | [] => do pure none
  | _ => do throw s!"Error: more than one dest_expr found for src_expr: ({expr}). Matching dest_exprs: ({dest_translations}). src_exprs: ({src_exprs}), dest_exprs: ({dest_exprs})"

def Pipeline.Statement.get_when_stmt_src_args
(when_stmt : Statement)
: Except String (List Identifier)
:= do
  match when_stmt with
  | .when _ args _ => return args
  | _ =>
    let msg : String := "Error: function expected a when stmt\n"++
      s!"Instead got this stmt: ({when_stmt})"
    throw msg

-- #check "a.b".splitOn "."
-- #eval "a.b".splitOn "."
-- #eval "ab".splitOn "."
def Identifier.to_expr (ident : Identifier) : Except String Expr := do
  let qualified_ident? := ident.splitOn "."
  match qualified_ident? with
  | [just_ident] => do pure $ var_expr just_ident
  | _ :: _ => do pure $ qual_var_expr qualified_ident?
  | _ => throw s!"Error: Identifier.to_expr: identifier is empty: ({ident})"

def List.to_exprs (idents : List Identifier) : Except String (List Expr) := do
  idents.mapM (·.to_expr)

def Pipeline.Statement.get_when_stmt_src_arg_exprs
(when_stmt : Statement)
: Except String (List Expr)
:= do
  let ident_args : List Identifier ← when_stmt.get_when_stmt_src_args
  ident_args.to_exprs

def Pipeline.Expr.term
(expr : Expr)
: Except String Term := do
  match expr with
  | .some_term term => do pure term
  | _ => do throw s!"Error: Pipeline.Expr.term: expr is not a term: ({expr})"

def Pipeline.Term.map_var_term_to_dest_var_term
(term : Term)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Term := do
  let expr := some_term term
  let expr? := ← expr.match_expr_from_to_list_upon_match src_vars dest_vars
  let expr' := match expr? with
    | some translated_expr => translated_expr
    | none => expr
  
  expr'.term

partial def Pipeline.Expr.map_var_expr_to_dest_var
(expr : Expr)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Expr := do
  match expr with 
  | .some_term /- term -/ _ => do
    let mapped? := ← expr.match_expr_from_to_list_upon_match src_vars dest_vars
    match mapped? with
    | some expr' => pure expr'
    | none => pure expr
  | .list exprs => do
    let list := exprs.mapM (·.map_var_expr_to_dest_var src_vars dest_vars)
    bind list (pure $ Expr.list ·)
  | .not_equal term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.not_equal term1' term2'
  | .equal term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.equal term1' term2'
  | .geq term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.geq term1' term2'
  | .leq term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.leq term1' term2'
  | .less_than term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.less_than term1' term2'
  | .greater_than term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.greater_than term1' term2'
  | .rightshift term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.rightshift term1' term2'
  | .leftshift term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.leftshift term1' term2'
  | .binxor term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.binxor term1' term2'
  | .binor term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.binor term1' term2'
  | .binand term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.binand term1' term2'
  | .div term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.div term1' term2'
  | .mul term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.mul term1' term2'
  | .sub term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.sub term1' term2'
  | .add term1 term2 => do
    let term1' ← term1.map_var_term_to_dest_var_term src_vars dest_vars
    let term2' ← term2.map_var_term_to_dest_var_term src_vars dest_vars
    pure $ Expr.add term1' term2'

def Pipeline.Term.map_function_call_src_to_dest_vars
(term : Term)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Term := do
  match term with
  | .function_call qual_name args => do
    let args' ← args.mapM (·.map_var_expr_to_dest_var src_vars dest_vars)
    pure $ Term.function_call qual_name args'
  | _ => do throw s!"Error: Pipeline.Term.map_function_call_src_to_dest_vars: term is not a function call: ({term})"

def Pipeline.Expr.map_stray_expr_src_vars_to_dest_vars
(expr : Expr)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Expr := do
  match expr with
  | .some_term term => do
    let term' := ← term.map_function_call_src_to_dest_vars src_vars dest_vars
    pure $ Expr.some_term term'
  | _ => do throw s!"Error: Pipeline.Expr.map_stray_expr_src_vars_to_dest_vars: expr is not a term: ({expr})"

mutual
partial def Pipeline.Conditional.map_rhs_vars_src_to_dest
(cond : Conditional)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Conditional := do
  match cond with
  | .if_else_statement expr stmt1 stmt2 => do
    let expr' := ← expr.map_var_expr_to_dest_var src_vars dest_vars
    let stmt1' := ← stmt1.map_rhs_vars_src_to_dest src_vars dest_vars
    let stmt2' := ← stmt2.map_rhs_vars_src_to_dest src_vars dest_vars
    pure $ Conditional.if_else_statement expr' stmt1' stmt2'
  | .if_statement expr stmt => do
    let expr' := ← expr.map_var_expr_to_dest_var src_vars dest_vars
    let stmt' := ← stmt.map_rhs_vars_src_to_dest src_vars dest_vars
    pure $ Conditional.if_statement expr' stmt'

partial def Pipeline.Statement.map_rhs_vars_src_to_dest
(stmt : Statement)
(src_vars : List Expr)
(dest_vars : List Expr)
: Except String Statement := do
  match stmt with
  | .variable_assignment qual_name expr => do
    let expr' := ← expr.map_var_expr_to_dest_var src_vars dest_vars
    -- dbg_trace s!"(dbg_trace: {Statement.variable_assignment qual_name expr'})"
    pure $ Statement.variable_assignment qual_name expr'
  | .stall _ => do throw s!"stall not supported, for expr translation"
  | .return_stmt _ => do throw s!"return_stmt not supported, for expr translation"
  | .block stmts => do
    let stmts' := ← stmts.mapM (·.map_rhs_vars_src_to_dest src_vars dest_vars)
    pure $ Statement.block stmts'
  | .stray_expr expr => do
    let expr' := ← expr.map_var_expr_to_dest_var src_vars dest_vars
    pure $ Statement.stray_expr expr'
  | .complete (String.mk _)
  | .reset (String.mk _)
  | .transition (String.mk _) => do
    pure stmt
  | .when qual_name arg_idents stmt' => do
    let stmt'' := ← stmt'.map_rhs_vars_src_to_dest src_vars dest_vars
    pure $ Statement.when qual_name arg_idents stmt''
  | .await (some term) stmts => do
    let term' ← term.map_var_term_to_dest_var_term src_vars dest_vars
    let stmts' ← stmts.mapM (·.map_rhs_vars_src_to_dest src_vars dest_vars)
    pure $ Statement.await (some term') stmts'
  | .await none _ => do
    pure stmt
  | .listen_handle _ _ => do
    pure stmt
  | .conditional_stmt cond => do
    let cond' ← cond.map_rhs_vars_src_to_dest src_vars dest_vars
    pure $ Statement.conditional_stmt $ cond'
  | .value_declaration typed_ident expr => do
    let expr' := ← expr.map_var_expr_to_dest_var src_vars dest_vars
    pure $ Statement.value_declaration typed_ident expr'
  | .variable_declaration _ => do
    pure stmt
  | .labelled_statement label stmt' => do
    let stmt'' := ← stmt'.map_rhs_vars_src_to_dest src_vars dest_vars
    pure $ Statement.labelled_statement label stmt''
end

def Pipeline.TypedIdentifier.type_ident : TypedIdentifier → (TIden × Identifier)
| .mk type' identifier => (type', identifier)

def Pipeline.TypedIdentifier.is_ident_ordering : TypedIdentifier → Bool
| typed_ident =>
  let (type', ident) := typed_ident.type_ident
  if (type' == "element_ordering") && (ident == "ordering") then
    true
  else
    false

def Pipeline.Expr.var_ident : Pipeline.Expr → Except String Identifier
| expr => do
  match expr with
  | .some_term term =>
    match term with
    | .var ident' => pure ident'
    | _ => throw "Expr.some_term Term is not 'var' (i.e. a var)"
  | _ => throw "Expr is not 'some_term' (i.e. a var)"

def filter_lst_of_stmts_for_ordering_asn
(lst_stmts : List Pipeline.Statement)
:=
  List.filter (
    λ stmt => 
      match stmt with
      -- | Statement.variable_assignment qual_name expr =>
      --   match qual_name with
      --   | QualifiedName.mk lst_idents' =>
      --     if (lst_idents'.contains "element_ordering")
      --       then true
      --       else false
      | Statement.value_declaration typed_ident expr =>
        match typed_ident with
        | TypedIdentifier.mk tident ident =>
          if (
            or
            (tident == "element_ordering")
            (ident == "ordering")
          )
          then true
          else false
      | _ => false
        
  )
  lst_stmts
    