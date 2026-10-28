!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set a scalar field from another scalar field
!
! The assignment v = u copies the values of u into v (both fields have
! the same FEDOF). Set(VALUE=u, scale, addContribution=.TRUE.) adds
! scale * u to v.

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

  CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u, v
  REAL(DFP) :: found(6), want(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)

  want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL u%Set(VALUE=want)
  CALL v%Set(VALUE=0.0_DFP)

  v = u
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "v = u")

  CALL v%Set(VALUE=u, scale=2.0_DFP, addContribution=math%yes)
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 3.0_DFP * want) .LT. tol), "v = v + 2 * u")

  CALL v%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
