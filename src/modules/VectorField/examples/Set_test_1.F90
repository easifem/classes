!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get the vector of a node and single values
!
! The values are stored in DOF format: component 1 of all nodes, then
! component 2 of all nodes, so the value of component c of node i is
! entry (c - 1) * 6 + i.
!
! Set(VALUE) with a scalar VALUE sets all entries. Set(globalNode,
! islocal, VALUE(:)) sets the vector of one node, and with
! globalNode(:) the same vector at several nodes.
! Set(VALUE, globalNode, islocal, spaceCompo) sets one component of
! one node. Get(VALUE(:), tsize, globalNode) returns the vector of a
! node, Get(VALUE(:), tsize, spaceCompo) all values of a component, and
! Get(VALUE, globalNode, islocal, spaceCompo) one value.

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
  REAL(DFP) :: raw(12), vec(6), val
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  ! all entries to one value
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw) .LT. tol), "Set all to a value")

  ! vector of one node
  CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 2.0_DFP])
  CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ABS(raw(3) - 1.0_DFP) .LT. tol .AND. &
          ABS(raw(9) - 2.0_DFP) .LT. tol, "Set node: DOF storage")
  CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
  CALL OK(tsize .EQ. 2, "Get node: tsize")
  CALL OK(ALL(ABS(vec(1:2) - [1.0_DFP, 2.0_DFP]) .LT. tol), "Get node")

  CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 1.0_DFP], &
             scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
  CALL OK(ALL(ABS(vec(1:2) - [3.0_DFP, 4.0_DFP]) .LT. tol), &
          "Set node: add 2 * value")

  ! one component of one node
  CALL u%Set(VALUE=7.0_DFP, globalNode=4, islocal=math%yes, spaceCompo=2)
  CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes, spaceCompo=2)
  CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set/Get one value")

  ! the same vector at several nodes
  CALL u%Set(globalNode=[1, 2], islocal=math%yes, &
             VALUE=[5.0_DFP, 6.0_DFP])
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(tsize .EQ. 6, "Get component: tsize")
  CALL OK(ALL(ABS(vec - [5.0_DFP, 5.0_DFP, 3.0_DFP, 0.0_DFP, 0.0_DFP, &
                         0.0_DFP]) .LT. tol), "Get component 1")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(vec - [6.0_DFP, 6.0_DFP, 4.0_DFP, 7.0_DFP, 0.0_DFP, &
                         0.0_DFP]) .LT. tol), "Get component 2")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
