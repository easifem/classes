---
date: 2025-07-15
summary: Get the quadrature points on line

---
 
# Line_GetQuadraturePoints
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Line_GetQuadraturePoints(obj, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
!! Quadrature points
END SUBROUTINE Line_GetQuadraturePoints
END INTERFACE
```
 
