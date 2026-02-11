 
# QuadrangleH1LagFE_GetLocalElemShapeData
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData(obj, elemsd, quad)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

TYPE(ElemShapedata_), INTENT(INOUT) :: elemsd

TYPE(QuadraturePoint_), INTENT(IN) :: quad

END SUBROUTINE QuadrangleH1LagFE_GetLocalElemShapeData
END INTERFACE
```
 
