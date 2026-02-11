---
date: 2025-07-15
summary: Get the quadrature points on Quadrangle

---
 
# Quadrangle_GetQuadraturePoints
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Quadrangle_GetQuadraturePoints(obj, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(QuadraturePoint_), INTENT(INOUT) :: quad
!! Quadrature points
END SUBROUTINE Quadrangle_GetQuadraturePoints
END INTERFACE
```
 
