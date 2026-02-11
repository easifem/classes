---
date: 2025-10-17
summary: Get face order

---
 
# obj_GetFaceOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_GetFaceOrder(obj, ans, nrow, ncol)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B), INTENT(INOUT) :: ans(:, :)

INTEGER(I4B), INTENT(OUT) :: nrow, ncol

END SUBROUTINE obj_GetFaceOrder
END INTERFACE
```
 
