SUBMODULE( Element_Class ) ShapeData
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 H1Lagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE get_elemsd_H1_Lagrange
  CALL initiate( Obj = ElemSD, Quad = Quad, &
    & RefElem = Obj%RefElem, &
    & ContinuityType= typeH1, &
    & InterpolType = TypeLagrangeInterpolation )
  CALL setValue( Obj = ElemSD, Val = XiJ, N =ElemSD%N, dNdXi=ElemSD%dNdXi )
END PROCEDURE get_elemsd_H1_Lagrange
END SUBMODULE ShapeData