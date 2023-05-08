
import PipelineDsl.AST

abbrev StateName := String
abbrev MsgName := String
abbrev CtrlerName := String

def reg_file : String := "reg_file"
def write := "write"
def read := "read"

def instruction := "instruction"
def op := "op"
def ld := "ld"

-- def inst := "inst"
def seq_num := "seq_num"

def List.to_qual_name (idents : List Identifier) : Pipeline.QualifiedName :=
  Pipeline.QualifiedName.mk idents

def List.to_dsl_var_expr : List Identifier → Pipeline.Expr
| idents =>
  Pipeline.Expr.some_term (Pipeline.Term.qualified_var idents.to_qual_name)

def inst_seq_num_expr := [instruction, seq_num].to_dsl_var_expr
def violating_seq_num := "violating_seq_num"
def squash := "squash"

-- TODO NOTE: create a namespace and list of these API names somewhere....
def remove_head := "remove_head"
def remove := "remove"
def insert := "insert"
def insert_tail := "insert_tail"
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

open Pipeline in
def num_lit_expr (n : Nat) : Expr :=
  Expr.some_term (Term.const (Const.num_lit n))

def ordering := "ordering"
def element_ordering := "element_ordering"

def address := "address"
def invalidation := "invalidation"
def invalidation_ack := "invalidation_ack"

abbrev VarType := String

def init_state := "init_state"

def bool' := "bool"

def u32 := "u32"

def var_expr (var_name : VarName) : Pipeline.Expr :=
 Pipeline.Expr.some_term $ Pipeline.Term.var var_name

open Pipeline in
def default_value_expr (var_type : VarType) : Except String Expr := do
  if var_type == seq_num ||
     var_type == address ||
     var_type == u32
    then do
    pure $ num_lit_expr 0
  else if var_type == instruction then do
    pure $ var_expr "instruction_inhabited"
  else if var_type == bool' then do
    pure $ var_expr "false"
  else do
    throw s!"Unexpected var type: ({var_type})"
