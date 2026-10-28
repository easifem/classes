!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set all nodes to one vector
!
! Set(VALUE(:)) with SIZE(VALUE) = spaceCompo sets the vector of all
! nodes to VALUE.

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
  TYPE(VectorField_) :: u
  REAL(DFP) :: vec(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  CALL u%Set(VALUE=[1.0_DFP, 2.0_DFP])
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - 1.0_DFP) .LT. tol), "component 1")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(vec - 2.0_DFP) .LT. tol), "component 2")

  CALL u%Set(VALUE=[1.0_DFP, 1.0_DFP], scale=-1.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=vec, tsize=tsize, globalNode=5)
  CALL OK(ALL(ABS(vec(1:2) - [0.0_DFP, 1.0_DFP]) .LT. tol), &
          "add -1 * value")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
