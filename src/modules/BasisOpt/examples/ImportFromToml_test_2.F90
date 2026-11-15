!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise ImportFromToml

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER :: unitno

OPEN (NEWUNIT=unitno, FILE="basisopt_example.toml", &
      STATUS="replace", ACTION="write")
WRITE (unitno, '(A)') '[basis]'
WRITE (unitno, '(A)') 'baseContinuity = "H1"'
WRITE (unitno, '(A)') 'baseInterpolation = "Lagrange"'
CLOSE (unitno)
CALL obj%ImportFromToml(tomlName="basis", &
                        filename="basisopt_example.toml", &
                        elemType=TypeElemNameOpt%Triangle, nsd=2_I4B)
CALL OK(obj%GetTopologyType() .EQ. TypeElemNameOpt%Triangle, "TOML topology")
CALL OK(obj%GetBaseContinuity() .EQ. "H1", "TOML continuity")
CALL obj%DEALLOCATE()
OPEN (NEWUNIT=unitno, FILE="basisopt_example.toml", &
      STATUS="old", ACTION="read")
CLOSE (unitno, STATUS="delete")
END PROGRAM main
