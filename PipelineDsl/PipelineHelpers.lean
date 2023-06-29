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
abbrev term_expr := Expr.some_term
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

abbrev handle := HandleBlock.mk
abbrev listen := Statement.listen_handle

abbrev value_decl := Statement.value_declaration

abbrev if_statement := Conditional.if_statement

abbrev labelled_stmt := Pipeline.Statement.labelled_statement

def Pipeline.Statement.get_await_when_blocks
(stmt : Statement)
: Except String (List Statement) :=
  match stmt with
  | .await _ list_stmt =>
    pure list_stmt
  | _ => throw s!"Error: Expected input stmt to be an Await, instead got: ({stmt})"

def List.first!
(list : List a)
: Except String a :=
  match list with
  | [one] => pure one
  | h :: _ => pure h
  | [] => throw s!"Error: Expected a non-empty list!"

def Pipeline.QualifiedName.idents
(qual_name : QualifiedName)
: List Identifier :=
match qual_name with
| .mk idents => idents

def Pipeline.Expr.var's_identifier
(expr : Expr)
: Except String Identifier :=
  match expr with
  | .some_term term =>
    match term with
    | .var ident => pure ident
    | .qualified_var qual_name =>
      qual_name.idents.first!
    | _ => throw s!"Error: Expected term to be a var or qualified var?"
  | _ => throw s!"Error: Expected expr to be some_term, and that term to be a var or qualified var?"

def Pipeline.Statement.append_stmts_to_block
(stmt_blk : Statement)
(stmt : List Statement)
: Except String Statement :=
  match stmt_blk with
  | .block stmts =>
  pure $ Statement.block (stmts ++ stmt)
  | _ => throw s!"Error: Expected stmt to be a block? Stmt_blk: ({stmt_blk})"

def Pipeline.Statement.append_to_block
(stmt_blk stmt : Statement)
: Except String Statement :=
  match stmt_blk with
  | .block stmts =>
    pure $ Statement.block (stmts ++ [stmt])
  | _ => throw s!"Error: Expected stmt to be a block? Stmt_blk: ({stmt_blk})"

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

def Pipeline.Description.body_stmts : Pipeline.Description → Except String (List Statement)
| .state ident stmt =>
  match stmt with
  | .block stmts =>
    match stmts with
    | h :: _ =>
      match h with
      | .listen_handle stmt /- handle_blks -/ _ =>
        stmt.stmt_block
      | _ => pure stmts
    | [] => throw s!"Error: Description.stmt_body Stmts Block in State is empty? State: ({ident})"
  | _ => throw s!"Error: Description.stmt_body State stmt isn't a block? Stmt: ({stmt})"
| _ => throw "Error: Description is not a state"

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

open Pipeline in
def List.to_dsl_var_expr : List Identifier → Except String Expr
| idents =>
  match idents with
  | [one] => pure $ var_expr one
  | _::_ => pure $ qual_var_expr idents
  | [] => throw s!"Error: passed empty list of Idenifiers to convert to DSL Expr."

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

def Pipeline.Statement.is_ident_in_when_args
(when_stmt : Statement)
(arg_ident : Identifier)
: Except String Bool
:= do
  match when_stmt with
  | .when _ args _ => return args.contains arg_ident
  | _ =>
    let msg : String := "Error: function expected a when stmt\n"++
    s!"Instead got this stmt: ({when_stmt})"
    throw msg

def Pipeline.Statement.is_send_load_api
(statement : Statement)
: Bool :=
  match statement with
  | .stray_expr expr =>
    match expr with
    | .some_term term =>
      match term with
      | .function_call qual_name /- args -/ _ =>
        qual_name.idents == ["memory_interface", "send_load_request"]
      | _ => false
    | _ => false
  | _ => false

open Pipeline in
def List.remove_post_send_load_stmts
(stmts : List Statement)
: /- Except String -/ (List Statement) :=
  match stmts with
  | h :: t =>
    if h.is_send_load_api then
      [h]
    else
      [h] ++ (t.remove_post_send_load_stmts)
  | [] => []

def Pipeline.TypedIdentifier.type_ident : TypedIdentifier → (TIden × Identifier)
| .mk type' identifier => (type', identifier)

def Pipeline.Statement.is_stmt_var_assign_to_var
(stmt : Statement)
(var_name : Identifier)
(only_consider_var_assign : Bool)
: Bool :=
  match stmt with
  | .variable_assignment qual_name /- expr -/ _ =>
    let qual_idents := qual_name.idents
    qual_idents == [var_name]
  | .value_declaration typed_ident /- expr -/ _ =>
    if only_consider_var_assign then
      false
    else
      let (_, value_var_name) := typed_ident.type_ident
      value_var_name == var_name
  | _ => false

open Pipeline in
def List.first_var_assign_to_var
(stmts : List Statement)
(var_name : Identifier)
(only_consider_var_assign : Bool)
: Option Statement :=
  match stmts with
  | h :: t =>
    let h_is_the_var_assign : Bool := h.is_stmt_var_assign_to_var var_name only_consider_var_assign
    if h_is_the_var_assign then
      some h
    else
      t.first_var_assign_to_var var_name only_consider_var_assign
  | [] => none

def Identifier.to_term (ident : Identifier) : Term :=
  var_term ident
def List.to_term (idents : List Identifier) : Term :=
  qual_var_term idents

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

open Pipeline in
def AssignSrcToDestVar
(dest_var : Identifier)
(src_var : Expr)
: Statement :=
  variable_assignment [dest_var].to_qual_name src_var

open Pipeline in
def AssignSrcToDestVarPair
: Identifier × Expr → Statement
| (dest_var, src_var) => AssignSrcToDestVar dest_var src_var

open Pipeline in
def AssignSrcToDestVars
(dest_vars : List Identifier)
(src_vars : List Expr)
: Except String (List Statement) :=
  let same_length := dest_vars.length == src_vars.length
  match same_length with
  | true =>
    let var_pairs := dest_vars.zip src_vars
    pure $ var_pairs.map AssignSrcToDestVarPair
  | false =>
    throw s!"Error: AssignSrcToDestVars: Message Argument dest_vars and src_vars are not the same length. dest_vars: ({dest_vars}), src_vars: ({src_vars})"

def Pipeline.Statement.is_inst_source_stmt
(stmt : Statement)
: Bool :=
  match stmt with
  | .labelled_statement label _ =>
    match label with
    | .inst_source => true
    | .result_write
    | .commit => false
  | _ => false

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

def Pipeline.Statement.is_result_write_from_stmts : Pipeline.Statement → Bool
| stmt =>
  match stmt with
  | .labelled_statement label /-stmt-/ _ =>
    match label with
    | .result_write => true
    | .commit
    | .inst_source => false
  | _ => false

def Pipeline.Statement.result_write_from_effects? : Pipeline.Statement → Option Pipeline.Statement
| stmt =>
  match stmt with
  | .labelled_statement label /-stmt-/ _ =>
    match label with
    | .result_write => some stmt
    | .commit
    | .inst_source => none
  | _ => none

def Pipeline.TypedIdentifier.var_name
(t_ident : TypedIdentifier)
: Identifier :=
  let (/-type-/_, ident) := t_ident.type_ident
  ident

def Pipeline.Description.add_stmt_to_ctrler
(ctrler_description : Description)
(stmt : Statement)
: Except String Description := do
  match ctrler_description with
  | .controller ident stmt_blk => do
    let appended ← stmt_blk.append_to_block stmt
    pure $ Description.controller ident appended
  | _ =>
    throw s!"Error: Description wasn't controller type?"

def Pipeline.Description.add_stmt_to_entry
(ctrler_description : Description)
(stmt : Statement)
: Except String Description := do
  match ctrler_description with
  | .entry ident stmt_blk => do
    let appended ← stmt_blk.append_to_block stmt
    pure $ Description.entry ident appended
  | _ =>
    throw s!"Error: Description wasn't entry type?"

partial def Pipeline.Statement.is_contains_transition
(stmt : Statement)
: Bool :=
  match stmt with
  | .complete /- state_name -/ _
  | .transition /- state_name -/ _
  | .reset /- state_name -/ _
    => true
  | .block stmts =>
    stmts.any (·.is_contains_transition)
  | .conditional_stmt cond =>
    match cond with
    | .if_else_statement /- cond_expr -/ _ stmt1 stmt2 =>
      stmt1.is_contains_transition || stmt2.is_contains_transition
    | .if_statement /- cond_expr -/ _ stmt1 =>
      stmt1.is_contains_transition
  | .listen_handle stmt1 handle_blks =>
    let handle_stmts := handle_blks.any (
      match · with
      | .mk _ _ stmt => stmt.is_contains_transition
      )
    stmt1.is_contains_transition || handle_stmts
  | .await _ stmts =>
    stmts.any (·.is_contains_transition)
  | .when _ _ stmt =>
    stmt.is_contains_transition
  | .labelled_statement _ _
  | .variable_declaration _
  | .value_declaration _ _
  | .variable_assignment _ _
  | .stray_expr _
  | .return_stmt _
  | .stall _
    => false

partial def Pipeline.Statement.get_all_child_stmts
(stmt : Statement)
: List Statement :=
  match stmt with
  | .complete /- state_name -/ _
  | .transition /- state_name -/ _
  | .reset /- state_name -/ _
  | .labelled_statement _ _
  | .variable_declaration _
  | .value_declaration _ _
  | .variable_assignment _ _
  | .stray_expr _
  | .return_stmt _
  | .stall _
    => [stmt]
  | .block stmts =>
    List.join $ stmts.map (·.get_all_child_stmts)
  | .conditional_stmt cond =>
    match cond with
    | .if_else_statement /- cond_expr -/ _ stmt1 stmt2 =>
      stmt1.get_all_child_stmts ++ stmt2.get_all_child_stmts
    | .if_statement /- cond_expr -/ _ stmt1 =>
      stmt1.get_all_child_stmts
  | .listen_handle stmt1 handle_blks =>
    let handle_stmts :=
      List.join $ handle_blks.map (
        match · with
        | .mk _ _ stmt => stmt.get_all_child_stmts
      )
    stmt1.get_all_child_stmts ++ handle_stmts
  | .await _ stmts =>
    List.join $ stmts.map (·.get_all_child_stmts)
  | .when _ _ stmt =>
    stmt.get_all_child_stmts

