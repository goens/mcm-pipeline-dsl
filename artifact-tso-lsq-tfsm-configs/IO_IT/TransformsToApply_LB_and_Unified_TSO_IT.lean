
import PipelineDsl.ApplyTransformations

def transforms := [
  (Transformation.IT, -- ordering below not used.
   MCMOrdering.ternary_ordering
     (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )
]
