---
date:  2023-09-05
summary: Get quadrature points on a local face of element

---
 
# Triangle_GetFacetQuadraturePoints
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE Triangle_GetFacetQuadraturePoints(obj, quad, facetQuad, &
localFaceNumber)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
!! Quadrature points
INTEGER(I4B), INTENT(IN) :: localFaceNumber

END SUBROUTINE Triangle_GetFacetQuadraturePoints
END INTERFACE
```
 
