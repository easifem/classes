---
date: 2025-07-17
summary: Set the quadrature type on Quadrangle element

---
 
# Quadrangle_SetQuadratureType
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Quadrangle_SetQuadratureType( &
obj, quadratureType, quadratureType1, quadratureType2)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType(:)

INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType1

INTEGER(I4B), OPTIONAL, INTENT(IN) :: quadratureType2

END SUBROUTINE Quadrangle_SetQuadratureType
END INTERFACE
```
 
