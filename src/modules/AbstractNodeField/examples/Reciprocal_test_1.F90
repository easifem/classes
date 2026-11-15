!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Reciprocal of a node field
!
! Reciprocal replaces each value of obj by its reciprocal,
! obj(i) = 1 / obj(i). The values must not be zero.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: x
  REAL(DFP) :: src(6), found(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

  CALL x%Reciprocal()
  CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP / src) .LT. tol), "Reciprocal")

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
