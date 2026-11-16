!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set values of a space-time block node field
!
! The block field u has two physical variables on six nodes: U (two
! space and two time components, four DOFs) and T (one space and three
! time components, three DOFs), so it has 42 values. The local DOF of
! (spaceCompo s, timeCompo t) is s + (t - 1) * tSpaceCompo. In DOF
! format the DOFs of U are entries 1-24 and those of T entries 25-42;
! DOF k of a variable at node i is entry offset + (k - 1) * 6 + i.
!
! The methods with timeCompo(:) or spaceCompo(:) take VALUE in node
! order: all components of the first node, then of the second node.
!- Set(globalNode, VALUE, ivar, spaceCompo, timeCompo(:)) and
!  Set(globalNode, VALUE, ivar, spaceCompo(:), timeCompo) set several
!  components of one node to one value.
!- Set(globalNode(:), islocal, VALUE(:), ivar, spaceCompo, timeCompo(:))
!  and Set(globalNode(:), islocal, VALUE(:), ivar, spaceCompo(:),
!  timeCompo) set several components at several nodes; with a scalar
!  VALUE all of them get the same value.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeField_) :: u
REAL(DFP) :: raw(42), want(42)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[2, 3], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["U", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=42, &
  fedof=fedofs, geofedof=geofedofs)

CALL OK(u%SIZE() .EQ. 42, "Initiate: Size")

u = 0.0_DFP
want = 0.0_DFP

! T at node 2, time components 2, 3 (obj_Set15)
CALL u%Set(globalNode=2, VALUE=7.0_DFP, ivar=2, spaceCompo=1, &
           timeCompo=[2, 3])
want([32, 38]) = 7.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set15: T at node 2")

! U at node 4, space components 1, 2 of time component 2 (obj_Set16)
CALL u%Set(globalNode=4, VALUE=8.0_DFP, ivar=1, spaceCompo=[1, 2], &
           timeCompo=2)
want([16, 22]) = 8.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set16: U at node 4")

! U_y at nodes 1, 2, time components 1, 2 (obj_Set10)
CALL u%Set(globalNode=[1, 2], islocal=math%yes, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], ivar=1, &
           spaceCompo=2, timeCompo=[1, 2])
want([7, 19, 8, 20]) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set10: U_y at nodes 1, 2")

! T at nodes 3, 4, time components 1, 3 to one value (obj_Set11)
CALL u%Set(globalNode=[3, 4], islocal=math%yes, VALUE=5.0_DFP, ivar=2, &
           spaceCompo=1, timeCompo=[1, 3])
want([27, 28, 39, 40]) = 5.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set11: T at nodes 3, 4")

! U at nodes 5, 6, space components 1, 2 of time 2 (obj_Set12)
CALL u%Set(globalNode=[5, 6], islocal=math%yes, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], ivar=1, &
           spaceCompo=[1, 2], timeCompo=2)
want([17, 23, 18, 24]) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set12: U at nodes 5, 6")

! U at node 3, space components 1, 2 of time 1 to one value (obj_Set13)
CALL u%Set(globalNode=[3], islocal=math%yes, VALUE=6.0_DFP, ivar=1, &
           spaceCompo=[1, 2], timeCompo=1)
want([3, 9]) = 6.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set13: U at node 3")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
