!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: AXPY of node fields
!
! AXPY adds scaled node fields to obj: obj = obj + scale * x,
! obj = obj + a1 * x1 + a2 * x2, and
! obj = obj + a1 * x1 + a2 * x2 + a3 * x3. The fields are ScalarField_
! with six DOFs and the same FEDOF.

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
  TYPE(ScalarField_) :: x, y, z, w
  REAL(DFP) :: src(6), found(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL y%Initiate(NAME="y", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)
  CALL z%Initiate(NAME="z", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)
  CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
  CALL y%SetAll(VALUE=1.0_DFP)
  CALL z%SetAll(VALUE=10.0_DFP)

  ! w = 0 + 2 * x
  CALL w%SetAll(VALUE=0.0_DFP)
  CALL w%AXPY(x=x, scale=2.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")

  ! w = 2 x + x - y
  CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - (3.0_DFP * src - 1.0_DFP)) .LT. tol), &
          "AXPY: w + a1 * x1 + a2 * x2")

  ! w = 3 x - y + y + 0.1 z - x
  CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
  CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - (2.0_DFP * src + 1.0_DFP)) .LT. tol), &
          "AXPY: w + a1 * x1 + a2 * x2 + a3 * x3")

  CALL w%DEALLOCATE()
  CALL z%DEALLOCATE()
  CALL y%DEALLOCATE()
  CALL x%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
