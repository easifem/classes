!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Read factory defaults

PROGRAM main
USE BasisOpt_Class, ONLY: BasisOpt_
USE BasisOpt_Class, ONLY: TypeBasisOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj

obj = TypeBasisOpt
CALL OK(obj%GetNSD() .EQ. 0_I4B, "Default spatial dimension")
CALL OK(obj%GetTotalDOF() .EQ. 0_I4B, "Default DOF count")
CALL OK(obj%GetBaseInterpolation() .EQ. "LAGR", "Default family")
CALL OK(obj%GetBaseContinuity() .EQ. "H1", "Default continuity")
CALL obj%DEALLOCATE()
END PROGRAM main
