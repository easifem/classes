---
date: 2025-10-13
summary: Set the cell orientation

---
 
# obj_SetCellOrientation
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetCellOrientation(obj, cellOrient, tCell, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: cellOrient(:)
!! cell orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tCell
!! size of cellOrder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetCellOrientation
END INTERFACE
```
 
