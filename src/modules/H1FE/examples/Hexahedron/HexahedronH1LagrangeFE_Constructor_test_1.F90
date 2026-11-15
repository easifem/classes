!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise HexahedronH1LagrangeFE_Constructor

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE HexahedronH1LagrangeFE_Class, ONLY: FE_ => HexahedronH1LagrangeFE_
USE HexahedronH1LagrangeFE_Class, ONLY: &
  NewFE => HexahedronH1LagrangeFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE(order=2_I4B, nsd=3_I4B, &
             ipType=TypeInterpolationOpt%Equidistance, &
             quadratureOrder=[6_I4B])
CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
CALL OK(ptr%isInit, "Target is initialized")
CALL OK(ptr%opt%GetTotalDOF() .EQ. 9_I4B, &
        "Constructor establishes quadratic DOFs")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
