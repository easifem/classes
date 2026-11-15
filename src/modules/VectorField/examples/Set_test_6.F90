!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Copy the values between vector fields
!
! Set(VALUE=u) copies the values of the vector field u into obj, and
! Get(VALUE=w) copies the values of obj into the vector field w. All
! fields must be initiated with the same FEDOF and spaceCompo.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u, v, w
  REAL(DFP) :: raw(12), want(12)
  INTEGER(I4B) :: tsize, ii

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                  fedof=fedof, geofedof=geofedof)
  CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[2], &
                  fedof=fedof, geofedof=geofedof)

  want = [(REAL(ii, DFP), ii=1, 12)]
  CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=want)
  CALL v%Set(VALUE=0.0_DFP)
  CALL w%Set(VALUE=0.0_DFP)

  CALL v%Set(VALUE=u)
  CALL v%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw - want) .LT. tol), "Set(VALUE=u): v = u")

  CALL v%Get(VALUE=w)
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw - want) .LT. tol), "Get(VALUE=w): w = v")

  CALL w%DEALLOCATE()
  CALL v%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
