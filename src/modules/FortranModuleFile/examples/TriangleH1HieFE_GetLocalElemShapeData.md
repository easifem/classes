 
# TriangleH1HieFE_GetLocalElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE TriangleH1HieFE_GetLocalElemShapeData(obj, elemsd, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd

TYPE(QuadraturePoint_), INTENT(IN) :: quad

END SUBROUTINE TriangleH1HieFE_GetLocalElemShapeData
END INTERFACE
```
 
