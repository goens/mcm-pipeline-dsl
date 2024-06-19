
import PipelineDsl.ApplyTransformations

def transforms := [
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ ldar ] [ load ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ ldar ] [ ldar ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_sy ] [ load ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_sy ] [ ldar ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_ld ] [ load ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_ld ] [ ldar ] Addresses.any) )
  -- (Transformation.IT, -- ordering below not used.
  --  MCMOrdering.ternary_ordering
  --    (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )
]
