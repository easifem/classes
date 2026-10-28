!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Dot product of two node fields
!
! DOT_PRODUCT returns the sum of obj(i) * obj2(i). For
! x = [1, 2, 3, 4, 5, 6] and y = 2 the dot product is 42.

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
  TYPE(ScalarField_) :: x, y
  REAL(DFP) :: src(6)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL y%Initiate(NAME="y", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
  CALL y%SetAll(VALUE=2.0_DFP)

  CALL OK(ABS(x%DOT_PRODUCT(y) - 42.0_DFP) .LT. tol, "DOT_PRODUCT")
  CALL OK(ABS(x%DOT_PRODUCT(x) - 91.0_DFP) .LT. tol, &
          "DOT_PRODUCT: x . x")

  CALL y%DEALLOCATE()
  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
