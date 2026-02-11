---
date:  2023-08-15
summary:  Get Global element shape data shape data in cell and facet

---
 
# TriangleH1HieFE_GetGlobalFacetElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE TriangleH1HieFE_GetGlobalFacetElemShapeData( &
obj, elemsd, facetElemsd, localFaceNumber, geoElemsd, geoFacetElemsd, xij)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd, facetElemsd
!! element shape data in cell and facet
TYPE(ElemShapedata_), INTENT(INOUT) :: geoElemsd, geoFacetElemsd
!! element shape data for geometry in cell and facet
REAL(DFP), INTENT(IN) :: xij(:, :)
!! nodal coordinates of cell element
!! The number of rows in xij should be same as the spatial dimension
!! The number of columns should be same as the number of nodes
!! present in the reference element in geoElemsd.
INTEGER(I4B), INTENT(IN) :: localFaceNumber

END SUBROUTINE TriangleH1HieFE_GetGlobalFacetElemShapeData
END INTERFACE
```
 
