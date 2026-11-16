!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Apply space-time Dirichlet boundary conditions to a block
!   node field
!
! The block field u has U (two space and two time components, entries
! 1-24) and T (one space and two time components, entries 25-36) on six
! nodes, stored in DOF format. The local DOF of (spaceCompo s,
! timeCompo t) is s + (t - 1) * tSpaceCompo, and DOF k of a variable at
! node i is entry offset + (k - 1) * 6 + i (offset 0 for U, 24 for T).
!
! The field is initiated with a TimeFEDOF (order 1, two time DOFs). The
! list dbc (see BlockNodeField_test.toml) has two BCs: bottom_y
! (component 2 = 7 on nodes 1, 5, 2) and left_x (component 1 = 3 on
! nodes 4, 1). The values are constant, so every time component gets
! the same value.
!
!- ApplyDirichletBC(dbc, ivar, times(:)) applies one DirichletBC_,
!- ApplyDirichletBC(dbc(:), ivar, times(:)) applies a vector of
!  DirichletBCPointer_.
!
! times(:) are the times of the time nodes. All other entries are
! unchanged.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE DirichletBC_Class, ONLY: DirichletBCDeallocate
USE DirichletBC_Class, ONLY: DirichletBCImportFromToml
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(TimeFEDOFPointer_) :: timefedofs(1)
TYPE(BlockNodeField_) :: u
TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
REAL(DFP) :: raw(36), want(36)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
CALL timefedof%ImportFromToml(tomlName="time", filename=tomlFileName)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof
timefedofs(1)%ptr => timefedof

CALL OK(timefedof%GetTotalDOF() .EQ. 2, "TimeFEDOF: two time DOFs")

CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[2, 2], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["U", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=36, &
  fedof=fedofs, geofedof=geofedofs, timefedof=timefedofs)

CALL DirichletBCImportFromToml(obj=dbc, dom=dom, tomlName="dbc", &
                               filename=tomlFileName)

! one DirichletBC_ (obj_ApplyDirichletBC3): U_y = 7 on nodes 1, 5, 2
! at both time components (local DOFs 2 and 4)
u = 0.0_DFP
want = 0.0_DFP
CALL u%ApplyDirichletBC(dbc=dbc(1)%ptr, ivar=1, times=times)
want([7, 11, 8]) = 7.0_DFP
want([19, 23, 20]) = 7.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "single dbc: ivar = 1")

! one DirichletBC_ on T: T = 3 on nodes 4, 1 at both time components
CALL u%ApplyDirichletBC(dbc=dbc(2)%ptr, ivar=2, times=times)
want([28, 25]) = 3.0_DFP
want([34, 31]) = 3.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "single dbc: ivar = 2")

! vector of DirichletBCPointer_ (obj_ApplyDirichletBC4) on U:
! U_y = 7 on nodes 1, 5, 2 and U_x = 3 on nodes 4, 1
u = -1.0_DFP
want = -1.0_DFP
CALL u%ApplyDirichletBC(dbc=dbc, ivar=1, times=times)
want([7, 11, 8]) = 7.0_DFP
want([19, 23, 20]) = 7.0_DFP
want([4, 1]) = 3.0_DFP
want([16, 13]) = 3.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "dbc vector: ivar = 1")

CALL DirichletBCDeallocate(dbc)
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
