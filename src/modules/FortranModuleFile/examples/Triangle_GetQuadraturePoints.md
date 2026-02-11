---
date: 2025-07-15
summary: Get the quadrature points on Triangle

---
 
# Triangle_GetQuadraturePoints
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Triangle_GetQuadraturePoints(obj, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
!! Quadrature points
END SUBROUTINE Triangle_GetQuadraturePoints
END INTERFACE
```
 
