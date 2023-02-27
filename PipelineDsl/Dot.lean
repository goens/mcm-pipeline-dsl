import PipelineDsl.CDFG

namespace Dot

inductive Att :=
| Box : Att
| Ellipse : Att
| NoneShape : Att
| Color : String → Att -- probably better to have a generic color? for now should be enough
| Label (s : String) : Att
| NullAtt : Att deriving BEq, DecidableEq

instance : Inhabited Att where default := Att.NullAtt

def att2Str (att : Att) : String :=
match att with
| Att.Box => "shape = \"box\" "
| Att.Ellipse => "shape = \"ellipse\" "
| Att.NoneShape => "shape = \"none\" "
| Att.Color c => "color = " ++ c
| Att.Label s  => "label = <" ++ s ++ ">"
| Att.NullAtt => ""

def att2HtmlStr : Att → String
| Att.Color c => "bgcolor=\"" ++ c ++ "\""
| _ => ""

instance : ToString Att := ⟨att2Str⟩

def attrs2String (attrs : List Att) : String :=
match attrs with
 | [] => ""
 | _ => "[" ++ String.join (attrs.map λ a => att2Str a ++ " ") ++ "]"

def makeEdge (attrs : List Att) (nodeFrom nodeTo : String) :=
  nodeFrom ++ "->" ++ nodeTo ++ attrs2String attrs ++ ";\n"

def makeNode (attrs : List Att) (nodeName : String) : String :=
  nodeName ++ " " ++ (attrs2String (Att.Ellipse::attrs)) ++ ";\n"

private def mkDotHtmlTable (name : String) (entries : List (String × Att)) :=
   let pre := "<table>"
   let namerow := "<tr><td colspan=\"" ++ (toString entries.length)
   ++ "\"> " ++ name ++ " </td></tr>"
   let post := "</table>"
   let toCol := λ (e,att) => "<td " ++ (att2HtmlStr att) ++ " port = \""
   ++ e ++ "\"> " ++ e ++ "</td>"
   pre ++ namerow ++ "<tr>" ++ String.join (entries.map toCol) ++ "</tr>" ++ post


open CDFG

abbrev AttFun := GraphElement → List Att

def dotPreamble : String :=
"digraph PipelineDslGenerated"

def elem2Dot :  AttFun → GraphElement → String
  | attFun, .inl node =>
      makeNode (attFun $ .inl node) (node.ctrler_name ++ "." ++ node.current_state)
  | attFun, .inr transition =>
      makeEdge (attFun $ .inr transition) transition.orig_state transition.dest_state

def graph2Dot (attFun :  GraphElement → List Att)
(g : Graph) : String  :=
  let elemAtts := g.map (elem2Dot attFun)
  dotPreamble ++ "{\n" ++ "\n".intercalate elemAtts ++ "\n}"

def trivialAttrs : GraphElement → List Att
 | _ => []

/-

Copied from UarchLang, not needed for now but leaving here in case we want to color some paths later:

open UarchGraph in
def colorNode (color : String) (node : NodeNameFull) :
UarchGraphNode → List Att
| UarchGraphNode.mkFun name _ => if node.1 == name then [(Att.Color color)] else []
| UarchGraphNode.mkState name => if node.1 == name then [(Att.Color color)] else []
| UarchGraphNode.mkStateArray name entries =>
  if node.1 == name then
    let entryFun := λ e => if e == node.2 then Att.Color color else Att.NullAtt
    entries.map entryFun
  else List.replicate (entries.length) Att.NullAtt

open UarchGraph in
def colorPath (path : List UarchGraph.NodeNameFull) (color : String) :
UarchGraphNode → List Att :=
  λ n => match n with
    | UarchGraphNode.mkStateArray _ _ =>
      AttListListJoin (path.map (λ nd => colorNode color nd n))
    | _ => List.join (path.map (λ nd => colorNode color nd n))

-/
