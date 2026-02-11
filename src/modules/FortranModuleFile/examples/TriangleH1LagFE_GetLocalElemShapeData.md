---
date: 2025-10-15
summary: Get local element shape data for Triangle H1 lagrange FE

---
 
# TriangleH1LagFE_GetLocalElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd

TYPE(QuadraturePoint_), INTENT(IN) :: quad

END SUBROUTINE TriangleH1LagFE_GetLocalElemShapeData
END INTERFACE
```
 
