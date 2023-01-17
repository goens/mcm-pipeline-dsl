
import PipelineDsl.AnalysisHelpers
import PipelineDsl.AST

-- cdfg 'data structure'
-- use for IR?

-- Build the CDFG
-- describe the CDFG "nodes" : state

namespace CDFG

abbrev VarName := String

inductive InEqRel
| LT : InEqRel /- Less than -/
| LEq : InEqRel /- Less than or equal -/
| GT : InEqRel
| GEq : InEqRel
| Eq : InEqRel
def InEqRel.toString : InEqRel → String
| .LT => "<"
| .LEq => "<="  
| .GT => "<"
| .GEq => "<="  
| .Eq => "=="
instance : ToString InEqRel where toString := InEqRel.toString

def NatToString (nat : Nat) : String := toString nat

inductive Inequality
| ValueIneq : VarName → InEqRel → Nat → Inequality
| VarIneq : VarName → InEqRel → VarName → Inequality
def Inequality.toString : Inequality → String
| ValueIneq var_name in_eq nat => var_name ++ in_eq.toString ++ (NatToString nat)
| VarIneq var_name1 in_eq var_name2 => var_name1 ++ in_eq.toString ++ var_name2
instance : ToString Inequality where toString := Inequality.toString

inductive BoolValue
| True : BoolValue
| False : BoolValue
| TrueOrFalse : BoolValue
def BoolValue.toString : BoolValue → String
| .True => "True"
| .False => "False"
| .TrueOrFalse => "TrueOrFalse"
instance : ToString BoolValue where toString := BoolValue.toString

abbrev InequalitiesOrBool := Sum (List Inequality) BoolValue
inductive ConstraintInfo
| mk : VarName → InequalitiesOrBool → ConstraintInfo

-- NOTE: Simple thing for now.
-- Better to encode relevant positions..
inductive FIFOPosition
| Inactive : FIFOPosition
| NotHead : FIFOPosition
| Head : FIFOPosition
| HeadOrNotHead : FIFOPosition -- probably some nicer way to write this

inductive UnorderedEntry
| Inactive : UnorderedEntry
| Active : UnorderedEntry

-- Queue info
inductive QueueInfo
| FIFOQueue : FIFOPosition → QueueInfo
| UnorderedQueue : UnorderedEntry → QueueInfo
| None : QueueInfo

--TODO: Define Predicate
-- Should consist of any conditions
--> boolean >, <, >=, <=, !=, ==
--  && ||
--> ∃ some other inst?
-- inductive APICondition
-- | Search : APICondition

abbrev AwaitTermStmt := Pipeline.Statement
abbrev CondExpr := Pipeline.Expr
inductive Condition
| DSLExpr : CondExpr → Condition
| APICondition : AwaitTermStmt → Condition

abbrev Predicate := List Condition
inductive TransitionType
| Transition : Predicate → StateName → TransitionType
| Reset : Predicate → StateName → TransitionType
| Completion : Predicate → StateName → TransitionType

abbrev MessageName := String
abbrev CtrlerName := String
inductive Message
| mk : MessageName → CtrlerName → Message

/-
1. describe the nodes' contents
- transitions / resets / completions
--> Maybe this isn't necessary; Can write a fn to extract from stmts
- message sends
--> Also can just be gotten from a func
- variables
--> also can be gotten from a func?
--> No; because I want to specifically note
--> constraints?
--> Or can i compute the constraints instead?
- Transitions & their predicate guard
- state name
--> store this..
--> maybe I can create a state struct
--> to make things simpler, instead of
--> finding it each time in a search..
- Any queue (FIFO) info of this entry
--> This has a type; set based on
--> 
- ctrler this belongs to
--> get from a func?
-/
abbrev StateName := String
-- abbrev CtrlerName := String
abbrev Stmts := List Pipeline.Statement
abbrev VarDef := Pipeline.TypedIdentifier
abbrev VarList := List VarDef
abbrev Messages := List Message
abbrev Transitions := List TransitionType
inductive CDFGNode
| mk: StateName → CtrlerName → VarList → Stmts → Transitions → Messages → QueueInfo → CDFGNode

-- def CDFGNode.
