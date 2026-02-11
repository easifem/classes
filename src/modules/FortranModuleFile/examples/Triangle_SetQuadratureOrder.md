---
date: 2025-07-17
summary: Set the order of accuracy for Triangle elements

---
 
# Triangle_SetQuadratureOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Triangle_SetQuadratureOrder(obj, order)
CLASS(BasisOpt_), INTENT(inout) :: obj

INTEGER(I4B), INTENT(IN) :: order

END SUBROUTINE Triangle_SetQuadratureOrder
END INTERFACE
```
 
