!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate, copy and deallocate a LIS block node field
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
!
! Initiate(name, engine, ..., fedof(:), geofedof(:)) initiates u with
! V (two space components) and P (one component) on six nodes, so u
! has 18 values. The engine is set to LIS_OMP and a LIS vector is
! created. Initiate(obj2=u) makes v a copy of u with its own LIS
! vector. Initiate deallocates an initiated field first, so it can be
! called again with another layout. DEALLOCATE destroys the LIS vector.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeFieldLis_Class, ONLY: BlockNodeFieldLis_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DOF_FMT
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeFieldLis_test.toml"
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeFieldLis_) :: u, v

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
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
CALL OK(u%GetEngineName() .EQ. "LIS_OMP", "Initiate2: engine LIS_OMP")
CALL OK(u%lis_ptr .NE. 0, "Initiate2: LIS vector created")

! Initiate by copying (obj_Initiate1)
CALL v%Initiate(obj2=u)
CALL OK(v%IsInitiated(), "Initiate1: IsInitiated")
CALL OK(v%SIZE() .EQ. 18, "Initiate1: Size")
CALL OK(v%GetEngineName() .EQ. "LIS_OMP", "Initiate1: engine LIS_OMP")
CALL OK(v%lis_ptr .NE. 0, "Initiate1: LIS vector created")
CALL OK(v%lis_ptr .NE. u%lis_ptr, "Initiate1: own LIS vector")

! Initiate an initiated field again with another layout
CALL u%Initiate( &
  name="u", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[2, 2], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["U", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=36, &
  fedof=fedofs, geofedof=geofedofs)
CALL OK(u%SIZE() .EQ. 36, "Initiate2 again: Size")
CALL OK(u%lis_ptr .NE. 0, "Initiate2 again: LIS vector created")

! DEALLOCATE destroys the LIS vector
CALL v%DEALLOCATE()
CALL OK(.NOT. v%IsInitiated(), "DEALLOCATE: not initiated")
CALL OK(v%lis_ptr .EQ. 0, "DEALLOCATE: lis_ptr = 0")
CALL OK(u%SIZE() .EQ. 36, "DEALLOCATE: u unchanged")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
