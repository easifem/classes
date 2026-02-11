---
date: 2025-10-13
summary: Set the face order

---
 
# obj_SetFaceOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetFaceOrder(obj, faceOrder, tFace, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrder(:, :)
!! cell orient, necessary for Hierarchical interpolation
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
!! size of cellOrder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetFaceOrder
END INTERFACE
```
 
