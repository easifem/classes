!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Pointer to the values of a node field
!
! GetPointer returns a pointer to the Fortran vector which stores the
! values of obj (NATIVE_SERIAL engine only). Changing the values
! through the pointer changes obj.

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
  TYPE(ScalarField_), TARGET :: x
  REAL(DFP), POINTER :: ptr(:)
  REAL(DFP) :: src(6), val

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

  ptr => x%GetPointer()
  CALL OK(ASSOCIATED(ptr), "GetPointer: associated")
  CALL OK(SIZE(ptr) .EQ. 6, "GetPointer: size")
  CALL OK(ALL(ABS(ptr - src) .LT. tol), "GetPointer: values")

  ptr(2) = -2.0_DFP
  CALL x%GetSingle(indx=2, VALUE=val)
  CALL OK(ABS(val + 2.0_DFP) .LT. tol, "GetPointer: change values")
  NULLIFY(ptr)

  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
