---
date: 2025-07-17
summary: Set the quadrature type on line element

---
 
# Line_SetQuadratureType
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Line_SetQuadratureType(obj, quadratureType)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), INTENT(IN) :: quadratureType

END SUBROUTINE Line_SetQuadratureType
END INTERFACE
```
 
