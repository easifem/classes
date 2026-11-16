!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Apply Dirichlet boundary conditions to a block node field
!
! The block field u has V (two space components, entries 1-12) and P
! (one component, entries 13-18) on six nodes, stored in DOF format.
! The list dbc (see BlockNodeField_test.toml) has two BCs:
! bottom_y (component 2 = 7 on nodes 1, 5, 2) and left_x (component 1
! = 3 on nodes 4, 1).
!
!- ApplyDirichletBC(dbc, ivar) applies one DirichletBC_ to ivar,
!- ApplyDirichletBC(dbc(:), ivar) applies a vector of
!  DirichletBCPointer_ to ivar.
!
! The DOF number of a BC is the space component of ivar; all other
! entries are unchanged.

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
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeField_) :: u
TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
REAL(DFP) :: raw(18), want(18)
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

CALL DirichletBCImportFromToml(obj=dbc, dom=dom, tomlName="dbc", &
                               filename=tomlFileName)

! one DirichletBC_ (obj_ApplyDirichletBC1): V_y = 7 on nodes 1, 5, 2
u = 0.0_DFP
want = 0.0_DFP
CALL u%ApplyDirichletBC(dbc=dbc(1)%ptr, ivar=1, times=0.0_DFP)
want([7, 11, 8]) = 7.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "single dbc: ivar = 1")

! one DirichletBC_ on P: P = 3 on nodes 4, 1
CALL u%ApplyDirichletBC(dbc=dbc(2)%ptr, ivar=2)
want([16, 13]) = 3.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "single dbc: ivar = 2")

! vector of DirichletBCPointer_ (obj_ApplyDirichletBC2) on V
u = -1.0_DFP
want = -1.0_DFP
CALL u%ApplyDirichletBC(dbc=dbc, ivar=1, times=0.0_DFP)
want([7, 11, 8]) = 7.0_DFP
want([4, 1]) = 3.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "dbc vector: ivar = 1")

CALL DirichletBCDeallocate(dbc)
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
