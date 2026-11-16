!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate, copy and deallocate a block node field
!
! Initiate(name, engine, ..., fedof(:), geofedof(:)) initiates u with
! V (two space components) and P (one component) on six nodes, so u
! has 18 values. Initiate(obj2=u) makes v a copy of u (layout and
! values). Both methods deallocate an initiated field first, so they
! can be called again with a different layout: here u gets U (two
! space, two time components) and T (one space, two time components),
! 36 values. DEALLOCATE removes all data.

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
TYPE(BlockNodeField_) :: u, v
REAL(DFP) :: src(18), found(18), val
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

! Initiate from arguments (obj_Initiate2)
CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

CALL OK(u%IsInitiated(), "Initiate2: IsInitiated")
CALL OK(u%SIZE() .EQ. 18, "Initiate2: Size")
CALL OK(u%GetTotalPhysicalVars() .EQ. 2, "Initiate2: physical vars")

src = [(REAL(ii, DFP), ii=1, 18)]
CALL u%Set(VALUE=src)

! Initiate by copying (obj_Initiate1)
CALL v%Initiate(obj2=u)
CALL OK(v%IsInitiated(), "Initiate1: IsInitiated")
CALL OK(v%SIZE() .EQ. 18, "Initiate1: Size")
CALL OK(v%GetTotalPhysicalVars() .EQ. 2, "Initiate1: physical vars")
CALL v%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Initiate1: values")

! the DOF layout is copied: P of node 5 is entry 17
CALL v%Get(VALUE=val, globalNode=5, islocal=math%yes, ivar=2, idof=1)
CALL OK(ABS(val - 17.0_DFP) .LT. tol, "Initiate1: DOF layout")

! the values are copied, not shared
v = 0.0_DFP
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Initiate1: u unchanged")

! Initiate an initiated field again with another layout (obj_Initiate2)
CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[2, 2], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["U", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=36, &
  fedof=fedofs, geofedof=geofedofs)
CALL OK(u%SIZE() .EQ. 36, "Initiate2 again: Size")

! copy into an initiated field (obj_Initiate1)
CALL v%Initiate(obj2=u)
CALL OK(v%SIZE() .EQ. 36, "Initiate1 again: Size")

! DEALLOCATE
CALL v%DEALLOCATE()
CALL OK(.NOT. v%IsInitiated(), "DEALLOCATE: not initiated")
CALL OK(v%SIZE() .EQ. 0, "DEALLOCATE: Size = 0")
CALL OK(u%SIZE() .EQ. 36, "DEALLOCATE: u unchanged")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
