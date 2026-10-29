!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ ImportFromToml operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
INTEGER :: unitno
INTEGER(I4B) :: nsd

OPEN (NEWUNIT=unitno, FILE="abstractfe_example.toml", STATUS="replace", &
      ACTION="write")
WRITE (unitno, '(A)') '[finite_element]'
WRITE (unitno, '(A)') 'baseContinuity = "H1"'
WRITE (unitno, '(A)') 'baseInterpolation = "Lagrange"'
CLOSE (unitno)
CALL obj%ImportFromToml(tomlName="finite_element", &
                        filename="abstractfe_example.toml", &
                        elemType=TypeElemNameOpt%Triangle, &
                        nsd=2_I4B)
CALL obj%GetParam(nsd=nsd)
CALL OK(obj%isInit .AND. nsd .EQ. 2_I4B, "Named TOML table initialization")
CALL obj%DEALLOCATE()
OPEN (NEWUNIT=unitno, FILE="abstractfe_example.toml", STATUS="old", &
      ACTION="read")
CLOSE (unitno, STATUS="delete")
END PROGRAM main
