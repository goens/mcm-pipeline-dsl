import Init.Data.String

def removeCommentsLineCharList : List Char → List Char
| c::cs => if c == '#' then [] else c::(removeCommentsLineCharList cs)
| [] => []

def removeCommentsLine : String → String :=
λ s => { data := removeCommentsLineCharList s.data }

def addSpacesPoints (s : String ) : String := s.replace "." " . "

def preprocess (lines : Array String ) : Array String :=
  -- is there a function composition syntax?
  lines.map (λ l => addSpacesPoints $ removeCommentsLine l)
