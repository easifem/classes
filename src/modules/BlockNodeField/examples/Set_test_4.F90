!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get values of a block node field by space and time
!   components
!
! The block field u has two physical variables on six nodes: V (two
! space components, entries 1-12) and P (one component, entries 13-18),
! stored in DOF format.
!
! Set(globalNode(:), islocal, VALUE(:), ivar, spaceCompo, timeCompo)
! sets one component at several nodes, one value per node; with a
! scalar VALUE all of them get the same value.
! Set(globalNode, VALUE, ivar, spaceCompo, timeCompo) sets one entry.
! Get(VALUE(:), tsize, globalNode(:), islocal, ivar, spaceCompo,
! timeCompo) returns one component at several nodes.

PROGRAM main
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldInitiate
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
REAL(DFP) :: raw(18), want(18), vec(3)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

CALL AbstractNodeFieldInitiate( &
  obj=u, name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

u = 0.0_DFP
want = 0.0_DFP

! V_y at nodes 1, 2, 3 from a vector (obj_Set8)
CALL u%Set(globalNode=[1, 2, 3], islocal=math%yes, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], ivar=1, spaceCompo=2, &
           timeCompo=1)
want(7:9) = [1.0_DFP, 2.0_DFP, 3.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set8: V_y at nodes 1, 2, 3")

! P at nodes 4, 5, 6 to one value (obj_Set9)
CALL u%Set(globalNode=[4, 5, 6], islocal=math%yes, VALUE=4.0_DFP, &
           ivar=2, spaceCompo=1, timeCompo=1)
want(16:18) = 4.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set9: P at nodes 4, 5, 6")

! one component at several nodes (obj_Get6)
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[1, 2, 3], &
           islocal=math%yes, ivar=1, spaceCompo=2, timeCompo=1)
CALL OK(tsize .EQ. 3, "Get6: tsize")
CALL OK(ALL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
        "Get6: V_y at nodes 1, 2, 3")
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[4, 5, 6], &
           islocal=math%yes, ivar=2, spaceCompo=1, timeCompo=1)
CALL OK(ALL(ABS(vec - 4.0_DFP) .LT. tol), "Get6: P at nodes 4, 5, 6")

! one entry (obj_Set14): V_x of node 6
CALL u%Set(globalNode=6, VALUE=5.0_DFP, ivar=1, spaceCompo=1, &
           timeCompo=1)
want(6) = 5.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set14: V_x of node 6")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
