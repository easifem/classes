!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get the values of one space component
!
! Set(VALUE, spaceCompo) sets a component of all nodes to one value,
! Set(VALUE(:), spaceCompo) sets a component of all nodes from a
! vector (one value for each node), and Set(VALUE(:), globalNode(:),
! islocal, spaceCompo) sets a component of the selected nodes.
! Get(VALUE(:), tsize, globalNode(:), islocal, spaceCompo) returns a
! component of the selected nodes.

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
  REAL(DFP) :: vec(6), src(6)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]

  CALL u%Set(VALUE=-1.0_DFP, spaceCompo=2)
  CALL u%Set(VALUE=src, spaceCompo=1)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - src) .LT. tol), "Set component from vector")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(vec + 1.0_DFP) .LT. tol), "Set component to value")

  CALL u%Set(VALUE=[10.0_DFP, 20.0_DFP], globalNode=[2, 5], &
             islocal=math%yes, spaceCompo=2)
  CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[5, 2, 1], &
             islocal=math%yes, spaceCompo=2)
  CALL OK(tsize .EQ. 3, "Get selected component: tsize")
  CALL OK(ALL(ABS(vec(1:3) - [20.0_DFP, 10.0_DFP, -1.0_DFP]) .LT. tol), &
          "Set selected component")

  CALL u%Set(VALUE=src, spaceCompo=1, scale=2.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - 3.0_DFP * src) .LT. tol), &
          "Set component: add 2 * vector")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
