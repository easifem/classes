---
date: 2025-10-15
summary:  Get local element shape data for line H1 Hierange FE

---
 
# LineH1HieFE_GetLocalElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE LineH1HieFE_GetLocalElemShapeData(obj, elemsd, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd

TYPE(QuadraturePoint_), INTENT(INOUT) :: quad

END SUBROUTINE LineH1HieFE_GetLocalElemShapeData
END INTERFACE
```
 
