
import PipelineDsl.ApplyTransformations

def transforms := [
  (Transformation.LR,
   MCMOrdering.ternary_ordering
     (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )
]
