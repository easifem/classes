---
date: 2025-07-17
summary: Set the quadrature type on Triangle element

---
 
# Triangle_SetQuadratureType
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Triangle_SetQuadratureType(obj, quadratureType)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), INTENT(IN) :: quadratureType

END SUBROUTINE Triangle_SetQuadratureType
END INTERFACE
```
 
