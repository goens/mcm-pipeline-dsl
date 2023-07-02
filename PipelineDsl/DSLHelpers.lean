
import PipelineDsl.AST
import PipelineDsl.PipelineHelpers
-- import PipelineDsl.ControllerHelpers
import PipelineDsl.InstructionHelpers

abbrev StateName := String
abbrev MsgName := String
abbrev CtrlerName := String

def reg_file : String := "reg_file"
def write := "write"
def read := "read"
def DSLKeyword.state : Identifier := "state"

namespace DSL
namespace type
  def inst := "inst"
end type
end DSL

def instruction := "instruction"
def op := "op"
def ld := "ld"

-- def inst := "inst"
def seq_num := "seq_num"

def inst_seq_num_expr := [instruction, seq_num].to_dsl_var_expr
def violating_seq_num := "violating_seq_num"
def squash := "squash"

-- TODO NOTE: create a namespace and list of these API names somewhere....
def remove_head := "remove_head"
def remove := "remove"

def remove_key := "remove_key"

def insert := "insert"
def insert_tail := "insert_tail"

def insert_key := "insert_key"

def search := "search"
def tail_search := "tail_search"
def search_all : MsgName := "search_all"

def search_fail := "search_fail"
def search_success := "search_success"

def entry := "entry"
def min := "min"
-- def API_msg_names : List MsgName := [load_completed, store_completed, remove_head]

def memory_interface : CtrlerName := "memory_interface"
def load_completed : MsgName := "load_completed"
def load_perform : MsgName := "send_load_request"
def store_completed : MsgName := "store_completed"
def store_perform : MsgName := "send_store_request"

-- NOTE: the "name" of the load value from the load response api
def load_value : String := "load_value"
def API_msg_names : List MsgName := [remove_head, remove, insert, insert_tail, squash]
def API_dest_ctrlers_msg_names : List (CtrlerName × MsgName) := [
  (memory_interface, load_perform),
  (memory_interface, store_perform),
  (memory_interface, load_completed),
  (memory_interface, store_completed),
  (reg_file, remove_head),
  (reg_file, write)
]

abbrev VarName := String
abbrev BoolDecl := Pipeline.Statement
abbrev BoolSetIfStmt := Pipeline.Statement

abbrev key := "key"
abbrev num_entries := "num_entries"

def ordering := "ordering"
def element_ordering := "element_ordering"

def address := "address"
def invalidation := "invalidation"
def invalidation_ack := "invalidation_ack"

abbrev VarType := String

def init_state := "init_state"

def dsl_bool := "bool"

def u32 := "u32"

open Pipeline

def num_lit_expr (n : Nat) : Expr :=
  Expr.some_term (Term.const (Const.num_lit n))

def default_value_expr (var_type : VarType) : Except String Expr := do
  if var_type == seq_num ||
     var_type == address ||
     var_type == u32
    then do
    pure $ num_lit_expr 0
  else if var_type == instruction then do
    pure $ var_expr "instruction_inhabited"
  else if var_type == dsl_bool then do
    pure $ var_expr "false"
  else do
    throw s!"Unexpected var type: ({var_type})"

def List.QualVar (idents : List Identifier) : Term :=
  qualified_var idents.to_qual_name

def List.EntryQualVar (idents : List Identifier) : Term :=
  let entry_list := [entry].append idents
  qualified_var entry_list.to_qual_name

-- abbrev ASTComparison := Term → Term → Expr

def /-ASTComparison.-/
  EntryVarCompare (entry_var : List Identifier) (check_constructor : Term → Term → Expr) (check_var : List Identifier) : Expr :=
  check_constructor entry_var.EntryQualVar check_var.QualVar

def EntryVarOp (entry_var : List Identifier) (check_constructor : Term → Term → Expr) (check_var : List Identifier) : Expr :=
  check_constructor entry_var.EntryQualVar check_var.QualVar
def minOp (expr : Expr) : Expr :=
  Expr.some_term $ Term.function_call [(min : Identifier)].to_qual_name [expr]

def minEntryVarOp (entry_var : List Identifier) (check_constructor : Term → Term → Expr) (check_var : List Identifier) : Expr :=
  minOp <| EntryVarOp entry_var check_constructor check_var

def CtrlerName.TableUnorderedSearchAll
(dest_ctrler : CtrlerName)
(table_key : VarName)
(search_key : VarName)
(search_success_stmts : List Statement)
(search_failure_stmts : List Statement)
-- (src_ctrler : CtrlerName)
: Statement :=
  let if_valid_expr := EntryVarCompare ["valid"] equal ["true"]
  let key_match_expr := EntryVarCompare [ table_key ] equal [ search_key ]
  let valid_and_key_match := binand (expr_term if_valid_expr) (expr_term key_match_expr)
  -- let min_sub := minEntryVarOp [ table_key ] sub [ search_key ]
  let search_call := function_call [dest_ctrler, search_all].to_qual_name [valid_and_key_match]

  let when_search_success := when_stmt [dest_ctrler, search_success].to_qual_name [] search_success_stmts.to_block
  let when_search_failure := when_stmt [dest_ctrler, search_fail ].to_qual_name [] search_failure_stmts.to_block

  let await_search := await (some search_call) [when_search_success, when_search_failure]
  await_search

def minEntry (entry_var : List Identifier) : Expr :=
  minOp <| term_expr entry_var.EntryQualVar

def CtrlerName.TableUnorderedSearch
(dest_ctrler : CtrlerName)
(table_key : VarName)
(search_key : VarName)
(search_success_stmts : List Statement)
(search_failure_stmts : List Statement)
-- (src_ctrler : CtrlerName)
(min_expr : Expr)
: Statement :=
  let if_valid_expr := EntryVarCompare ["valid"] equal ["true"]
  let key_match_expr := EntryVarCompare [ table_key ] equal [ search_key ]
  let valid_and_key_match := binand (expr_term if_valid_expr) (expr_term key_match_expr)
  let min_cond := minOp min_expr
  let search_call := function_call [dest_ctrler, search].to_qual_name [valid_and_key_match, min_cond]

  let when_search_success := when_stmt [dest_ctrler, search_success].to_qual_name [] search_success_stmts.to_block
  let when_search_failure := when_stmt [dest_ctrler, search_fail ].to_qual_name [] search_failure_stmts.to_block

  let await_search := await (some search_call) [when_search_success, when_search_failure]
  await_search

def List.TermVar (idents : List Identifier) : Term :=
  match idents with
  | [one] => var_term one
  | _ => qual_var_term idents
  -- | [] => throw s!"Error: List of Idents -> Var function: Provided an empty list of idents"

def VarCompare (var1 : List Identifier) (check_constructor : Term → Term → Expr) (var2 : List Identifier) : Expr :=
  check_constructor var1.TermVar var2.TermVar

-- NOTE: Better to explicitly error with a msg at specific points, to get a "stack trace" where I care
def Except.throw_exception_nesting_msg (e : Except String α) (msg : String) : Except String α := do
  match e with
  | .ok a => pure a
  | .error err_msg => throw s!"{msg} --\n-- Msg: ({err_msg})"

def InstType.completion_msg_name : InstType → Except String MsgName
| .memory_access access =>
  match access with
  | .load => pure load_completed
  | .ldar => pure load_completed
  | .store => pure store_completed
  | .stlr => pure store_completed
| .memory_ordering ordering =>
  match ordering with
  | .mfence => throw "Error: mFence has no awaited completion message"
  | .dmb_sy => throw "Error: dmb_sy has no awaited completion message"
  | .dmb_ld => throw "Error: dmb_ld has no awaited completion message"
  | .dmb_st => throw "Error: dmb_st has no awaited completion message"

def InstType.perform_msg_name : InstType → Except String MsgName
| .memory_access access =>
  match access with
  | .load => pure load_perform
  | .ldar => pure load_perform
  | .store => pure store_perform
  | .stlr => pure store_perform
| .memory_ordering ordering =>
  match ordering with
  | .mfence => throw "Error: mFence has no perform message"
  | .dmb_sy => throw "Error: dmb_sy has no perform message"
  | .dmb_ld => throw "Error: dmb_ld has no perform message"
  | .dmb_st => throw "Error: dmb_st has no perform message"

def Pipeline.Term.is_type_perform_msg
(term : Term)
(inst_type : InstType)
: Except String Bool := do
  match term with
  | Term.function_call qual_name /- exprs -/ _ => do
    match qual_name with
    | .mk [dest_ctrler, msg_name] => do
      if dest_ctrler == memory_interface && msg_name == (← inst_type.perform_msg_name) then
        pure true
      else
        pure false
    | .mk _ => do pure false
  | _ => do pure false

open Pipeline in
partial def List.inject_stmts_at_perform
(stmts : List Statement)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(stmt_to_insert_after? : Option Statement)
(when_stmt_has_this_arg? : Option Identifier)
-- returns: (the stmts we kinda re-build with, the commit stmts)
: Except String (Bool × List Statement /- × List Statement-/) := do
  -- try tail recursion
  match stmts with
  | h :: t =>
    let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?

    match h with
    -- The case of interest...
    | .stray_expr expr => do
      match expr with
      | .some_term term => do
        if ← term.is_type_perform_msg inst_type then do
        -- add stmts
          pure $ (true, stmts_to_inject ++ [h] ++ tail_re_build_stmts)
        else do
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | _ =>
        pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
    | .labelled_statement label stmt =>
      let ( found_in_stmt, updated_stmts ) ← [stmt].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_in_stmt then
        pure $ (found_in_stmt, [Statement.labelled_statement label updated_stmts.to_block ] ++ t)
      else
        pure $ (found_in_tail, [Statement.labelled_statement label h ] ++ tail_re_build_stmts)
    | .block stmts' =>
      let (found_it, re_build_stmts)  ← stmts'.inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [re_build_stmts.to_block] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    -- Recursive cases, stmts that may contain stmts, then collect stmts, before recursing on t
    | .when qual_name idents stmt =>
      let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.when qual_name idents re_build_stmts.to_block] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .await term? stmts' =>
      let (found_it, re_build_stmts) ← stmts'.inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.await term? re_build_stmts] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .listen_handle stmt handle_blks =>
      let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.listen_handle re_build_stmts.to_block handle_blks] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .conditional_stmt cond =>
      match cond with
      | .if_statement expr stmt =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [Pipeline.Statement.conditional_stmt $ Pipeline.Conditional.if_statement expr re_build_stmts.to_block] ++ t)
        else
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .if_else_statement expr stmt stmt' =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        let (found_it', re_build_stmts') ← [stmt'].inject_stmts_at_perform inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        let re_build_if_stmt := conditional_stmt
          $ if_else_statement expr
            (if found_it then re_build_stmts.to_block else stmt)
            (if found_it' then re_build_stmts'.to_block else stmt')
        if found_it then
          pure $ (found_it || found_it', [re_build_if_stmt] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
    -- Non recursive cases, collect stmt, simply recurse on t
    | .stall _ => throw "Error while injecting stmts to replace commit stmts: Stall stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .return_stmt _ => throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .complete _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .reset (String.mk _) =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .transition (String.mk _) =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .variable_assignment _ _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .value_declaration _ _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .variable_declaration _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
  | [] => pure (false, [])

open Pipeline in
partial def List.inject_stmts_after_stmt_at_ctrler_state
(stmts : List Statement)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(stmt_to_search? : Option Statement)
(when_stmt_has_this_arg? : Option Identifier)
-- returns: (the stmts we kinda re-build with, the commit stmts)
: Except String (Bool × List Statement /- × List Statement-/) := do
  let stmt_to_search ← match stmt_to_search? with
    | some a_stmt => pure a_stmt
    | none => throw s!"Error: Need a stmt to search for this function!"
  -- try tail recursion
  match stmts with
  | h :: t =>
    if h == stmt_to_search then
      pure $ (true, [h] ++ stmts_to_inject ++ t)
    else do
      let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?

      match h with
      | .labelled_statement label stmt =>
        let (found_it, stmts_searched) ← [stmt].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
        let stmt_reconstructed := Statement.labelled_statement label <| stmts_searched.to_block
        if found_it then
          pure $ (found_it, [stmt_reconstructed] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .block stmts' =>
        let (found_it, re_build_stmts) ← stmts'.inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
        if found_it then
          pure $ (found_it, [ re_build_stmts.to_block ] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      -- Recursive cases, stmts that may contain stmts, then collect stmts, before recursing on t
      | .when qual_name idents stmt =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
        if found_it then
          pure $ (found_it, [ Pipeline.Statement.when qual_name idents re_build_stmts.to_block ] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .await term? stmts' =>
        let (found_it, re_build_stmts) ← stmts'.inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
        if found_it then
          pure $ (found_it, [Pipeline.Statement.await term? re_build_stmts] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .listen_handle stmt handle_blks =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
        -- NOTE: This ignores the handle blocks. May want to consider them in the future?
        if found_it then
          pure $ (found_it, [Pipeline.Statement.listen_handle re_build_stmts.to_block handle_blks] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .conditional_stmt cond =>
        match cond with
        | .if_statement expr stmt =>
          let (found_it, re_build_stmts) ← [stmt].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
          if found_it then
            pure $ (found_it, [Pipeline.Statement.conditional_stmt $ Pipeline.Conditional.if_statement expr re_build_stmts.to_block] ++ t)
          else
            pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
        | .if_else_statement expr stmt stmt' =>
          let (found_it, re_build_stmts) ← [stmt].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
          let (found_it', re_build_stmts') ← [stmt'].inject_stmts_after_stmt_at_ctrler_state inst_type stmts_to_inject stmt_to_search when_stmt_has_this_arg?
          let re_build_if_stmt := conditional_stmt
            $ if_else_statement expr
              (if found_it then re_build_stmts.to_block else stmt)
              (if found_it' then re_build_stmts'.to_block else stmt')
          if found_it then
            pure $ (found_it || found_it', [re_build_if_stmt] ++ t)
          else
            pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .stall _ => throw "Error while injecting stmts to replace commit stmts: Stall stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
      | .return_stmt _ => throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
      | .complete _ =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .reset (String.mk _) =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .transition (String.mk _) =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .variable_assignment _ _ =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .value_declaration _ _ =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .variable_declaration _ =>
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .stray_expr /- expr -/ _=> do
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | [] => pure (false, [])

open Pipeline in
partial def List.inject_stmts_at_body
(stmts : List Pipeline.Statement)
(inst_type : InstType)
(stmts_to_inject : List Pipeline.Statement)
(stmt_to_insert_after? : Option Statement)
(when_stmt_has_this_arg? : Option Identifier)
-- returns: (the stmts we kinda re-build with, the commit stmts)
: Except String (Bool × List Pipeline.Statement /- × List Pipeline.Statement-/) := do
  -- try tail recursion
  match stmts with
  | h :: t =>
    let (/- found_in_tail -/ _, tail_re_build_stmts) ← t.inject_stmts_at_body inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?

    let (found_in_h, h_re_built) := ←
      match h with
      | .listen_handle stmt handle_blks => do
        let (found_it, updated_stmt) ← List.inject_stmts_at_body [stmt] inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure $ (
          found_it,
          Statement.listen_handle
            updated_stmt.to_block
            handle_blks
          )
      | .block blk_stmts =>
        pure (true, Statement.block $ stmts_to_inject ++ blk_stmts)
      | _ => throw s!"Error: When injecting stmts at stmt body, stmt isn't a listen_handle or block to add stmts into? Stmts: ({stmts}) Head: ({h})"

    pure $ (found_in_h, [h_re_built] ++ tail_re_build_stmts)

  | [] => pure (false, [])

-- TODO NOTE: later, try to make this more generic
open Pipeline in
partial def List.inject_stmts_at_commit
(stmts : List Pipeline.Statement)
(inst_type : InstType)
(stmts_to_inject : List Pipeline.Statement)
-- returns: (the stmts we kinda re-build with, the commit stmts)
(stmt_to_insert_after? : Option Statement)
(when_stmt_has_this_arg? : Option Identifier)
: Except String (Bool × List Pipeline.Statement /- × List Pipeline.Statement-/) := do
  -- try tail recursion
  match stmts with
  | h :: t =>
    let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?

    match h with
    -- The case of interest...
    | .labelled_statement label stmt =>
      match label with
      | .commit =>
        let (found_it, inject_stmts) ← [stmt].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        let rebuilt_stmt := Statement.labelled_statement label $ (
            stmts_to_inject ++
            inject_stmts).to_block
        if found_it then
          pure $ (true, [rebuilt_stmt] ++ t)
        else
          pure $ (true, [h] ++ tail_re_build_stmts)
      | _ =>
        pure $ (false, [h] ++ tail_re_build_stmts)
    | .block stmts' =>
      let (found_it, re_build_stmts) ← stmts'.inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [re_build_stmts.to_block] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    -- Recursive cases, stmts that may contain stmts, then collect stmts, before recursing on t
    | .when qual_name idents stmt =>
      let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.when qual_name idents re_build_stmts.to_block] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .await term? stmts' =>
      let (found_it, re_build_stmts) ← stmts'.inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.await term? re_build_stmts] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .listen_handle stmt handle_blks =>
      let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
      if found_it then
        pure (found_it, [Pipeline.Statement.listen_handle re_build_stmts.to_block handle_blks] ++ t)
      else
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .conditional_stmt cond =>
      match cond with
      | .if_statement expr stmt =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [Pipeline.Statement.conditional_stmt $ Pipeline.Conditional.if_statement expr re_build_stmts.to_block] ++ t)
        else
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .if_else_statement expr stmt stmt' =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        let (found_it', re_build_stmts') ← [stmt'].inject_stmts_at_commit inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        let re_build_if_stmt := conditional_stmt
          $ if_else_statement expr
            (if found_it then re_build_stmts.to_block else stmt)
            (if found_it' then re_build_stmts'.to_block else stmt')
        if found_it then
          pure $ (found_it || found_it', [re_build_if_stmt] ++ t)
        else
          pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
    | .stray_expr /- expr -/ _ => do
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    -- Non recursive cases, collect stmt, simply recurse on t
    | .stall _ => throw "Error while injecting stmts to replace commit stmts: Stall stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .return_stmt _ => throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
      -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
    | .complete _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .reset (String.mk _) =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .transition (String.mk _) =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .variable_assignment _ _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .value_declaration _ _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
    | .variable_declaration _ =>
      pure (found_in_tail, [h] ++ tail_re_build_stmts)
  | [] => pure (false, [])

def Pipeline.Statement.is_a_when_stmt_that_has_this_arg_then_insert_stmts
(stmt : Statement)
(arg : Identifier)
(stmts_to_inject : List Statement)
: Except String (Option Statement) := do
  match stmt with
  | .when qual_name idents stmt => do
    if idents.contains arg then do
      let appended_stmts ← stmt.append_stmts_to_block stmts_to_inject
      pure $ some (Statement.when qual_name idents appended_stmts)
    else
      pure none
  | _ => pure none

-- AZ TODO: Use this to generalize the inject functions!
-- open Pipeline in
-- partial def List.inject_stmts_at_ctrler_state_with_function
-- (stmts : List Pipeline.Statement)
-- (inst_type : InstType)
-- (stmts_to_inject : List Pipeline.Statement)
-- -- returns: (the stmts we kinda re-build with, the commit stmts)
-- (stmt_to_insert_after? : Option Statement)
-- (when_stmt_has_this_arg? : Option Identifier)
-- (update_statement_function : Statement -> ....?)
-- : Except String (Bool × List Pipeline.Statement /- × List Pipeline.Statement-/) := do

open Pipeline in
partial def List.inject_stmts_in_when_matching_arg_at_ctrler_state
(stmts : List Pipeline.Statement)
(inst_type : InstType)
(stmts_to_inject : List Pipeline.Statement)
-- returns: (the stmts we kinda re-build with, the commit stmts)
(stmt_to_insert_after? : Option Statement)
(when_stmt_has_this_arg? : Option Identifier)
: Except String (Bool × List Pipeline.Statement /- × List Pipeline.Statement-/) := do

  let arg_to_check ← match when_stmt_has_this_arg? with
    | some arg => pure arg
    | none => throw s!"Error: don't have a provided identifier to check?"

  match stmts with
  | h::t =>
    let appended_to_when? ← h.is_a_when_stmt_that_has_this_arg_then_insert_stmts arg_to_check stmts_to_inject
    -- if it's some, then return (true, appended ++ t)
    -- if it's none, then go into the cases, that recursively call this function..
    match appended_to_when? with
    | some h_injected => pure (true, [h_injected] ++ t)
    | none =>
      -- match h, if it has stmts in it to recurse down
      -- else append tail...
      match h with
      | .block stmts' =>
        let (found_it, re_build_stmts) ← stmts'.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [re_build_stmts.to_block] ++ t)
        else
          let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      -- Recursive cases, stmts that may contain stmts, then collect stmts, before recursing on t
      | .when qual_name idents stmt =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [Pipeline.Statement.when qual_name idents re_build_stmts.to_block] ++ t)
        else
          let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .await term? stmts' =>
        let (found_it, re_build_stmts) ← stmts'.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [Pipeline.Statement.await term? re_build_stmts] ++ t)
        else
          let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .listen_handle stmt handle_blks =>
        let (found_it, re_build_stmts) ← [stmt].inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        if found_it then
          pure (found_it, [Pipeline.Statement.listen_handle re_build_stmts.to_block handle_blks] ++ t)
        else
          let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .conditional_stmt cond =>
        match cond with
        | .if_statement expr stmt =>
          let (found_it, re_build_stmts) ← [stmt].inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          if found_it then
            pure (found_it, [Pipeline.Statement.conditional_stmt $ Pipeline.Conditional.if_statement expr re_build_stmts.to_block] ++ t)
          else
            let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
            pure (found_in_tail, [h] ++ tail_re_build_stmts)
        | .if_else_statement expr stmt stmt' =>
          let (found_it, re_build_stmts) ← [stmt].inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          let (found_it', re_build_stmts') ← [stmt'].inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
          let re_build_if_stmt := conditional_stmt
            $ if_else_statement expr
              (if found_it then re_build_stmts.to_block else stmt)
              (if found_it' then re_build_stmts'.to_block else stmt')
          if found_it then
            pure $ (found_it || found_it', [re_build_if_stmt] ++ t)
          else
            let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
            pure $ (found_in_tail, [h] ++ tail_re_build_stmts)
      | .labelled_statement /- label -/ _ /- stmt -/ _ =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .stray_expr /- expr -/ _ => do
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      -- Non recursive cases, collect stmt, simply recurse on t
      | .stall _ => throw "Error while injecting stmts to replace commit stmts: Stall stmts not supported"
        -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
      | .return_stmt _ => throw "Error while injecting stmts to replace commit stmts: Return stmts not supported"
        -- pure ([h] ++ tail_re_build_stmts, tail_commit_stmts)
      | .complete _ =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .reset (String.mk _) =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .transition (String.mk _) =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .variable_assignment _ _ =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .value_declaration _ _ =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
      | .variable_declaration _ =>
        let (found_in_tail, tail_re_build_stmts) ← t.inject_stmts_in_when_matching_arg_at_ctrler_state inst_type stmts_to_inject stmt_to_insert_after? when_stmt_has_this_arg?
        pure (found_in_tail, [h] ++ tail_re_build_stmts)
  | [] => pure (false, [])

abbrev InjectStmtsFunction :=
  -- stmts to search through and to inject into
  List Pipeline.Statement
    → InstType -- instruction type used by some inject fns to check if the controller flow path is of a specific instruction type
    → List Pipeline.Statement -- statements to inject
    → Option Statement -- use for inject stmts fn that injects statements after a provided/specified statement
    → Option Identifier -- use for inject stmts fn that checks if a when has the this identifier in it's args.
    → Except String (Bool × List Statement) -- return found or not & the updated statements. TODO: Use an Option type instead of bool..

def Pipeline.Description.inject_stmts_at_stmt
(state : Description)
(inst_type : InstType)
(stmts_to_inject : List Statement)
(var_stmt? : Option Statement)
(arg_in_when_stmt? : Option Identifier)
(InjectStmtsAt : InjectStmtsFunction)
-- returns: (the stmts we kinda re-build with, the commit stmts)
-- Return
-- 1. the state with commit smts replaced with injected stmts
-- 2. the stmt at commit and stmts after
: Except String Description := do
  -- open up state, look through stmts
  match state with
  | .state state_name stmt => do
    let (/- success -/ _, updated_stmt_with_injected) : Bool × List Statement := ← InjectStmtsAt [stmt] inst_type stmts_to_inject var_stmt? arg_in_when_stmt?
    let updated_state := Pipeline.Description.state state_name updated_stmt_with_injected.to_block

    pure updated_state
  | _ => throw "Error: (inject stmts at perform) Expected input Pipeline.Description to be a state. Instead got ({state})"

def Pipeline.TypedIdentifier.is_inst_type
(t_ident : TypedIdentifier)
: Bool :=
  match t_ident with
  | .mk type_ident /- var name -/ _ =>
    if type_ident == DSL.type.inst then
      true
    else
      false

