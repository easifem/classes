---
date: 2025-07-09
summary:  Get local facet element shape data

---
 
# LineH1LagFE_GetLocalFacetElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE LineH1LagFE_GetLocalFacetElemShapeData( &
obj, elemsd, facetElemsd, quad, facetQuad, localFaceNumber)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! finite element
TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
!! element shape data on cell
TYPE(QuadraturePoint_), INTENT(IN) :: quad, facetQuad
!! Quadrature points on each facet element
INTEGER(I4B), INTENT(IN) :: localFaceNumber
!! local face number
END SUBROUTINE LineH1LagFE_GetLocalFacetElemShapeData
END INTERFACE
```
 
