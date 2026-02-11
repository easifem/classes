---
date: 2025-07-17
summary: Set the order of accuracy for Quadrangle elements

---
 
# Quadrangle_SetQuadratureOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Quadrangle_SetQuadratureOrder(obj, order, order1, order2)
CLASS(BasisOpt_), INTENT(inout) :: obj

INTEGER(I4B), OPTIONAL, INTENT(IN) :: order(:)

INTEGER(I4B), OPTIONAL, INTENT(IN) :: order1

INTEGER(I4B), OPTIONAL, INTENT(IN) :: order2

END SUBROUTINE Quadrangle_SetQuadratureOrder
END INTERFACE
```
 
