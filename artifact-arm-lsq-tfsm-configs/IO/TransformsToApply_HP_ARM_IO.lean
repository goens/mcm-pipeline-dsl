
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
     (BinaryOrdering.mk [ store, stlr ] [ stlr ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ store, stlr ] [ dmb_st ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.ternary_ordering
     (TernaryOrdering.mk [ store', stlr' ] dmb_sy' [ load' ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.ternary_ordering
     (TernaryOrdering.mk [ store', stlr' ] dmb_sy' [ ldar' ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_ld ] [ ldar ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ dmb_ld ] [ load ] Addresses.any) )
]
