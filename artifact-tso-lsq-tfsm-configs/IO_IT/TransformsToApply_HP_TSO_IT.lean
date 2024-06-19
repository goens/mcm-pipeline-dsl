
import PipelineDsl.ApplyTransformations

def transforms := [
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ store ] [ store, mfence ] Addresses.any) ),
  (Transformation.IT, -- ordering below not used.
   MCMOrdering.ternary_ordering
     (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )
]
