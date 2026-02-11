---
date: 2025-10-13
summary: Set the edge order

---
 
# obj_SetEdgeOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetEdgeOrder(obj, edgeOrder, tEdge, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrder(:)
!! cell orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
!! size of cellOrder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetEdgeOrder
END INTERFACE
```
 
