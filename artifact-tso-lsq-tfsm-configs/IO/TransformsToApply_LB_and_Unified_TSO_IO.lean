
import PipelineDsl.ApplyTransformations

def transforms := [
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ load ] [ load ] Addresses.any) ),
  (Transformation.IO,
   MCMOrdering.binary_ordering
     (BinaryOrdering.mk [ mfence ] [ load ] Addresses.any) )

  --  (Transformation.IO,
  --   MCMOrdering.binary_ordering
  --     (BinaryOrdering.mk [ store, stlr ] [ stlr ] Addresses.any) ),
  --  (Transformation.IO,
  --   MCMOrdering.binary_ordering
  --     (BinaryOrdering.mk [ store, stlr ] [ dmb_st ] Addresses.any) ),
  --  (Transformation.IO,
  --   MCMOrdering.binary_ordering
  --     (BinaryOrdering.mk [ store, stlr ] [ dmb_sy ] Addresses.any) ),
  --  (Transformation.IO,
  --   MCMOrdering.binary_ordering
  --     (BinaryOrdering.mk [ store, stlr ] [ dmb_sy ] Addresses.any) ),

  --(Transformation.IO,
  -- MCMOrdering.binary_ordering
  --   (BinaryOrdering.mk [ dmb_ld ] [ ldar ] Addresses.any) ),
  --(Transformation.IO,
  -- MCMOrdering.binary_ordering
  --   (BinaryOrdering.mk [ dmb_ld ] [ load ] Addresses.any) )
  -- (Transformation.LR,
  --  MCMOrdering.ternary_ordering
  --    (TernaryOrdering.mk [ load' ] dmb_sy' [ load' ] Addresses.any) )

  -- (Transformation.LR,
  --  MCMOrdering.ternary_ordering
  --    (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )

  --  (Transformation.IT, -- ordering below not used.
  --   MCMOrdering.ternary_ordering
  --     (TernaryOrdering.mk [ load' ] mfence' [ load' ] Addresses.any) )
]
