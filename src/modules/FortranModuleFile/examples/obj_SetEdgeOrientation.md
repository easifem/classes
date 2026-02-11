---
date: 2025-10-13
summary:  Set the edge orientation

---
 
# obj_SetEdgeOrientation
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetEdgeOrientation(obj, edgeOrient, tEdge, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: edgeOrient(:)
!! edge orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tEdge
!! size of edgeOrder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetEdgeOrientation
END INTERFACE
```
 
