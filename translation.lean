
import PipelineDsl

-- start at the top of the AST
-- (1) Collect the state_queues or "controllers"
-- (2) Match a state_queue/controller to an entry description
--     Tells us what variables the entry has
-- (3) Match state_queue/controller to transitions
-- (4) break up transitions at await

-- TODO: Write Objects to represent Murphi code as well
-- (5) Convert entries into records
-- (6) Convert controllers into Murphi records
--     consisting of an array of entries
-- (7) Convert transitions into Rules
--     

-- pick structure_descriptions, to get controllers

open Pipeline.AST in
def ast0000 ( input : Pipeline.AST) :=
  match input with
  | structure_descriptions lst => List.join (lst.map ast0001_descript)

open Pipeline.Description in
def ast0001_descript (descript : Pipeline.Description) :=
  match descript with
  | control_flow a b => a
  | entry a b => a
  | controller a => a
  | function_definition a b c => a.ident
  -- | controller a b => a
  --| entry a b => a
  --| transition a b => a b
  -- | function_definition a b c => a

-- def ex0 : Pipeline.AST := Pipeline.AST (Pipeline.AST.structure_descriptions)