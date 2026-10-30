!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TetrahedronH1LagrangeFE_GetGlobalElemShapeData

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(ElemShapeData_) :: shapes, geometry
REAL(DFP) :: xij(3, 8)

! This operation is currently a WIP placeholder.
xij = 0.0_DFP
CALL obj%GetGlobalElemShapeData( &
  elemsd=shapes, geoelemsd=geometry, xij=xij)
CALL OK(shapes%nips .GT. 0_I4B, "Mapped shapes populated")
CALL obj%DEALLOCATE()
END PROGRAM main
