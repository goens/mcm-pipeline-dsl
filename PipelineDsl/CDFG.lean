
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
deriving Inhabited
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
deriving Inhabited
def Inequality.toString : Inequality → String
| ValueIneq var_name in_eq nat => var_name ++ in_eq.toString ++ (NatToString nat)
| VarIneq var_name1 in_eq var_name2 => var_name1 ++ in_eq.toString ++ var_name2
instance : ToString Inequality where toString := Inequality.toString

inductive BoolValue
| True : BoolValue
| False : BoolValue
| TrueOrFalse : BoolValue
deriving Inhabited
def BoolValue.toString : BoolValue → String
| .True => "True"
| .False => "False"
| .TrueOrFalse => "TrueOrFalse"
instance : ToString BoolValue where toString := BoolValue.toString

abbrev InequalitiesOrBool := Sum (List Inequality) BoolValue
inductive ConstraintInfo
| mk : VarName → InequalitiesOrBool → ConstraintInfo
deriving Inhabited
def ineq_or_bool_toString (ineq_or_bool : InequalitiesOrBool) : String :=
  toString ineq_or_bool
def ConstraintInfo.toString : ConstraintInfo → String
| .mk var_name inequalities_or_bool => var_name ++ " : " ++ (ineq_or_bool_toString inequalities_or_bool)
instance : ToString ConstraintInfo where toString := ConstraintInfo.toString

-- NOTE: Simple thing for now.
-- Better to encode relevant positions..
inductive FIFOPosition
| Inactive : FIFOPosition
| NotHead : FIFOPosition
| Head : FIFOPosition
| HeadOrNotHead : FIFOPosition -- probably some nicer way to write this
deriving Inhabited
def FIFOPosition.toString : FIFOPosition → String
| .Inactive => "Inactive"
| .NotHead => "NotHead"
| .Head => "Head"
| .HeadOrNotHead => "HeadOrNotHead"
instance : ToString FIFOPosition where toString := FIFOPosition.toString

inductive UnorderedEntry
| Inactive : UnorderedEntry
| Active : UnorderedEntry
deriving Inhabited
def UnorderedEntry.toString : UnorderedEntry → String
| .Inactive => "Inactive"
| .Active => "Active"
instance : ToString UnorderedEntry where toString := UnorderedEntry.toString

-- Queue info
inductive QueueInfo
| FIFOQueue : FIFOPosition → QueueInfo
| UnorderedQueue : UnorderedEntry → QueueInfo
| None : QueueInfo
deriving Inhabited
def QueueInfo.toString : QueueInfo → String
| .FIFOQueue pos => s!"FIFOQueue: ({pos})"
| .UnorderedQueue entry => s!"UnorderedQueue: ({entry})"
| .None => "None"
instance : ToString QueueInfo where toString := QueueInfo.toString

--TODO: Define Predicate
-- Should consist of any conditions
--> boolean >, <, >=, <=, !=, ==
--  && ||
--> ∃ some other inst?
-- inductive APICondition
-- | Search : APICondition

abbrev AwaitStmt := Pipeline.Statement
abbrev CondExpr := Pipeline.Expr
inductive Condition
| DSLExpr : CondExpr → Condition
| APICondition : AwaitStmt → Condition
| AwaitCondition : AwaitStmt → Condition
| HandleCondition : Pipeline.HandleBlock → Condition
deriving Inhabited
def Condition.toString : Condition → String
| .DSLExpr cond_expr => s!"DSLExpr: ({cond_expr})"
| .APICondition await_stmt => s!"APICondition: ({await_stmt})"
| .AwaitCondition await_stmt => s!"AwaitCondition: ({await_stmt})"
| .HandleCondition handle_blk => s!"HandleCondition: ({handle_blk})"
instance : ToString Condition where toString := Condition.toString

-- abbrev MessageName := String
abbrev CtrlerName := String
inductive Message
| mk : Pipeline.Term → Message
deriving Inhabited
def Message.toString : Message → String
| .mk term => s!"Message Term: ({term})"
instance : ToString Message where toString := Message.toString

abbrev Messages := List Message
abbrev Effects := List Pipeline.Statement
abbrev Stmts := List Pipeline.Statement
abbrev StateName := String
abbrev Predicate := List Condition
inductive TransitionType
| Transition : TransitionType
| Reset : TransitionType
| Completion : TransitionType
deriving Inhabited
def TransitionType.toString : TransitionType → String
| .Transition => "Transition"
| .Reset => "Reset"
| .Completion => "Completion"
instance : ToString TransitionType where toString := TransitionType.toString
structure IncompleteTransition where
predicate : Predicate
orig_state : StateName
messages : Messages
effects : Effects
stmts : Stmts
queue_info : QueueInfo
constraint_info : List ConstraintInfo -- Would come from state updates?
deriving Inhabited
structure Transition where
predicate : Predicate
orig_state : StateName
dest_state : StateName
messages : Messages
effects : Effects
stmts : Stmts
trans_type : TransitionType
queue_info : QueueInfo
constraint_info : List ConstraintInfo -- Would come from state updates?
deriving Inhabited

def predicateToString (predicate : Predicate) : String :=
", ".intercalate (predicate.map (λ pred => toString pred))

def messagesToString (messages : Messages) : String :=
", ".intercalate (messages.map (λ message => toString message))

def effectsToString (effects : Effects) : String :=
", ".intercalate (effects.map (λ effect => toString effect))

def stmtsToString (effects : Effects) : String :=
", ".intercalate (effects.map (λ effect => toString effect))

def constraintInfoToString (constraint_info_list : List ConstraintInfo) : String :=
", ".intercalate (constraint_info_list.map (λ constraint => toString constraint))

def Transition.toString (transition : Transition) : String :=
let predicate : String := (predicateToString transition.predicate)
let messages : String := (messagesToString transition.messages)
let effects : String := (effectsToString transition.effects)
let stmts : String := (stmtsToString transition.stmts)
let constraint : String := (constraintInfoToString transition.constraint_info)
-- let transition_type : String := t
let str : String :=
"== Transition ==\n"++
s!"Predicate: {predicate}" ++ "\n" ++
s!"Src State: ("++ transition.orig_state ++ ")" ++ "\n" ++
s!"Dest State: ("++ transition.dest_state ++ ")" ++ "\n" ++
s!"Messages: ({messages})" ++ "\n" ++
s!"Effects: ({effects})" ++ "\n" ++
s!"Stmts: ({stmts})" ++ "\n" ++
s!"Transition Type: ({transition.trans_type})" ++ "\n" ++
s!"Queue Info: ({transition.queue_info})" ++ "\n" ++
s!"Constraint Info: ({constraint})" ++ "\n" ++
"== End Transition ==\n"
str
instance : ToString Transition where toString := Transition.toString

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
-- abbrev StateName := String
-- abbrev CtrlerName := String
-- abbrev Stmts := List Pipeline.Statement
abbrev VarDef := Pipeline.TypedIdentifier
abbrev VarList := List VarDef
-- abbrev Messages := List Message
abbrev IncompleteTransitions := List IncompleteTransition
abbrev Transitions := List Transition

/-
  AG: why call this CDFGNode and not Node? If you hover it you will see its fully
  quallified name is CDFG.CDFGNode, which seems a bit redundant. On the other hand
  I would probably put this in the Pipeline namespace (as in Pipeline.CDFG.Node)
-/
def TidentoString (tident : Pipeline.TypedIdentifier) : String := toString tident

structure Node where
current_state : StateName
ctrler_name : CtrlerName
vars : VarList
transitions : Transitions
deriving Inhabited
def Node.toString (cdfg_node: Node) : String :=
let vars : String := ", ".intercalate (List.map TidentoString cdfg_node.vars)
let str : String :=
"<< CDFG Node >>\n" ++
"Current State: ("++cdfg_node.current_state++")" ++ "\n" ++
s!"Ctrler Name: ({cdfg_node.ctrler_name})" ++ "\n" ++
s!"Vars: ({vars})" ++  "\n" ++
s!"Transitions: ({cdfg_node.transitions})"  ++ "\n" ++
"<< End CDFG Node >>\n"
str
instance : ToString Node where toString := Node.toString

/- As-is, nodes (vertices) and transitions (edges) are not technically related as data structures,
  i.e., an edge does not have pointers to its destination, just the name. For now we can take this to
  be a list and assume there has to be exactly one node with the name of the destination. We should
  improve this design later though to ensure this is the case.
 -/
structure Graph where
  nodes : List Node

abbrev GraphElement := Node ⊕ Transition

-- traversal of graph
private partial def graphMapAux (gr : Graph) (f : GraphElement → α) (unvisited visited : List GraphElement) (partialRes : List α) : List α :=
  match unvisited with
    | [] => partialRes
    | elem::rest =>
      match elem with
        | .inl node =>
           let newVisited := elem::visited
           let newPartialRes := f elem::partialRes
           let newUnvisited := rest ++ (node.transitions.map .inr)
           graphMapAux gr f newUnvisited newVisited newPartialRes
        | .inr transition =>
            let newVisited := elem::visited
            let newPartialRes := f elem::partialRes
            let dest? := gr.nodes.find? (λ node => node.current_state == transition.dest_state)
            let newUnvisited :=  match dest? with
              | none => rest
              | some dest => (.inl dest)::rest
            graphMapAux gr f newUnvisited newVisited newPartialRes

def Graph.map (gr : Graph) (f : GraphElement → α) (headIdx := 0) : List α :=
  match gr.nodes[headIdx]? with
    | none => []
    | some head => graphMapAux gr f [.inl head] [] []

-- def Node.
