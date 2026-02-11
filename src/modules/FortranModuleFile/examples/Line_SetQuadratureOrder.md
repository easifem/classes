---
date: 2025-07-17
summary: Set the order of accuracy for line elements

---
 
# Line_SetQuadratureOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Line_SetQuadratureOrder(obj, order)
CLASS(BasisOpt_), INTENT(inout) :: obj

INTEGER(I4B), INTENT(IN) :: order

END SUBROUTINE Line_SetQuadratureOrder
END INTERFACE
```
 
