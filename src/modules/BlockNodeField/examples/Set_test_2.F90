!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get values of a block node field by ivar and idof
!
! The block field u has two physical variables on six nodes: V (two
! space components) and P (one component). The values are stored in
! DOF format: V_x of all nodes, V_y of all nodes, then P of all nodes,
! so idof of ivar at node i is entry offset + (idof - 1) * 6 + i, where
! offset is 0 for V and 12 for P.
!
! Set(globalNode, islocal, VALUE, ivar, idof) sets one entry,
! Set(globalNode(:), islocal, VALUE, ivar, idof) sets several entries to
! one value, and Set(globalNode(:), islocal, VALUE(:), ivar, idof) sets
! one value for each node. Get(VALUE, globalNode, islocal, ivar, idof)
! returns one entry and Get(VALUE(:), tsize, globalNode(:), islocal,
! ivar, idof) several entries.

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
REAL(DFP) :: raw(18), want(18), vec(3), val
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
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

u = 0.0_DFP
want = 0.0_DFP

! one entry (obj_Set3): V_y of node 3, P of node 5
CALL u%Set(globalNode=3, islocal=math%yes, VALUE=4.0_DFP, ivar=1, &
           idof=2)
CALL u%Set(globalNode=5, islocal=math%yes, VALUE=6.0_DFP, ivar=2, &
           idof=1)
want(9) = 4.0_DFP
want(17) = 6.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set3: one entry")

! one entry (obj_Get1)
CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, ivar=1, idof=2)
CALL OK(ABS(val - 4.0_DFP) .LT. tol, "Get1: V_y of node 3")
CALL u%Get(VALUE=val, globalNode=5, islocal=math%yes, ivar=2, idof=1)
CALL OK(ABS(val - 6.0_DFP) .LT. tol, "Get1: P of node 5")

! add to one entry: 4 + 2 * 1
CALL u%Set(globalNode=3, islocal=math%yes, VALUE=1.0_DFP, ivar=1, &
           idof=2, scale=2.0_DFP, addContribution=math%yes)
want(9) = 6.0_DFP
CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, ivar=1, idof=2)
CALL OK(ABS(val - 6.0_DFP) .LT. tol, "Set3: add to one entry")

! several entries to one value (obj_Set6): P of nodes 1, 2
CALL u%Set(globalNode=[1, 2], islocal=math%yes, VALUE=7.0_DFP, ivar=2, &
           idof=1)
want(13:14) = 7.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set6: several entries, scalar")

! one value for each node (obj_Set7): V_x of nodes 4, 5, 6
CALL u%Set(globalNode=[4, 5, 6], islocal=math%yes, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], ivar=1, idof=1)
want(4:6) = [1.0_DFP, 2.0_DFP, 3.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set7: several entries, vector")

! several entries (obj_Get3)
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[4, 5, 6], &
           islocal=math%yes, ivar=1, idof=1)
CALL OK(tsize .EQ. 3, "Get3: tsize")
CALL OK(ALL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
        "Get3: V_x of nodes 4, 5, 6")
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[1, 2, 5], &
           islocal=math%yes, ivar=2, idof=1)
CALL OK(ALL(ABS(vec - [7.0_DFP, 7.0_DFP, 6.0_DFP]) .LT. tol), &
        "Get3: P of nodes 1, 2, 5")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
