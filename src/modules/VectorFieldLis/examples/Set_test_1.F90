!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set and get values of a LIS vector field
!
! VectorFieldLis_ uses the Set and Get methods of VectorField_. The
! values are stored in DOF format: component 1 of all nodes, then
! component 2 of all nodes, so the value of component c of node i is
! entry (c - 1) * 6 + i.
!
! Set(VALUE) sets all entries, Set(globalNode, islocal, VALUE(:)) the
! vector of one node, and Set(VALUE, globalNode, islocal, spaceCompo)
! one component of one node. Get(VALUE(:), tsize, globalNode) returns
! the vector of a node and Get(VALUE(:), tsize, spaceCompo) all values
! of a component.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u
REAL(DFP) :: raw(12), vec(6), val
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

! Test 1: all entries to one value
CALL u%Set(VALUE=0.0_DFP)
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, tsize=tsize)
CALL OK(tsize .EQ. 12, "GetMultiple: tsize")
CALL OK(ALL(ABS(raw) .LT. tol), "Set all to a value")

! Test 2: vector of one node, stored in DOF format
CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 2.0_DFP])
CALL u%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, tsize=tsize)
CALL OK(ABS(raw(3) - 1.0_DFP) .LT. tol .AND. &
        ABS(raw(9) - 2.0_DFP) .LT. tol, "Set node: DOF storage")
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
CALL OK(tsize .EQ. 2, "Get node: tsize")
CALL OK(ALL(ABS(vec(1:2) - [1.0_DFP, 2.0_DFP]) .LT. tol), "Get node")

! Test 3: add a scaled vector to one node
CALL u%Set(globalNode=3, islocal=math%yes, VALUE=[1.0_DFP, 1.0_DFP], &
           scale=2.0_DFP, addContribution=math%yes)
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=3)
CALL OK(ALL(ABS(vec(1:2) - [3.0_DFP, 4.0_DFP]) .LT. tol), &
        "Set node: add 2 * value")

! Test 4: one component of one node
CALL u%Set(VALUE=7.0_DFP, globalNode=4, islocal=math%yes, spaceCompo=2)
CALL u%Get(VALUE=val, globalNode=4, islocal=math%yes, spaceCompo=2)
CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set/Get one value")

! Test 5: all values of a component
CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
CALL OK(tsize .EQ. 6, "Get component: tsize")
CALL OK(ALL(ABS(vec - [0.0_DFP, 0.0_DFP, 4.0_DFP, 7.0_DFP, 0.0_DFP, &
                       0.0_DFP]) .LT. tol), "Get component 2")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
