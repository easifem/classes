---
date: 2025-10-13
summary:  Get global element shape data  for TriangleH1HierarchicalFE

---
 
# TriangleH1HieFE_GetGlobalElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE TriangleH1HieFE_GetGlobalElemShapeData(obj, xij, elemsd, &
geoelemsd)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! Abstract finite element
REAL(DFP), INTENT(IN) :: xij(:, :)
!! nodal coordinates of element
!! The number of rows in xij should be same as the spatial dimension
!! The number of columns should be same as the number of nodes
!! present in the reference element in geoElemsd.
TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd
!! shape function data
TYPE(ElemShapeData_), INTENT(INOUT) :: geoelemsd
!! shape function data for geometry which contains local shape function
!! data. If not present then the local shape function in elemsd
!! will be used for geometry. This means we are dealing with
!! isoparametric shape functions.
END SUBROUTINE TriangleH1HieFE_GetGlobalElemShapeData
END INTERFACE
```
 
