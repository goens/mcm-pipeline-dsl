import Init.Data.String
import PipelineDsl.Parser

def RemoveCommentsLineCharList : List Char → List Char
| c::cs => if c == '#' then [] else c::(RemoveCommentsLineCharList cs)
| [] => []

def RemoveCommentsLine : String → String :=
λ s => { data := RemoveCommentsLineCharList s.data }

def AddSpacesPoints (s : String ) : String := s.replace "." " . "

def Preprocess (lines : Array String ) : Array String :=
  -- is there a function composition syntax?
  lines.map (λ l => AddSpacesPoints $ RemoveCommentsLine l)

def toSyntax : String → Lean.MacroM Lean.Syntax :=
  λ file  => `([file| $(Lean.quote file)])
