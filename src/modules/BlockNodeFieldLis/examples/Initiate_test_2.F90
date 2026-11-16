!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Copy the values when a LIS block node field is copied
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! The fields have V (two space components) and P (one component) on
! six nodes, 18 values in DOF format: V_x of nodes 1-6 (entries 1-6),
! V_y (7-12), then P (13-18).
!
! Initiate(obj2) copies the values into the new LIS vector: from the
! LIS vector of a LIS block field (u -> v), or from the native vector of
! a native BlockNodeField_ (n -> w). The copy has its own LIS vector.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE BlockNodeFieldLis_Class, ONLY: BlockNodeFieldLis_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeFieldLis_) :: u, v, w
TYPE(BlockNodeField_) :: n
REAL(DFP) :: found(18), src(18)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

CALL u%Initiate( &
  name="u", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)
CALL n%Initiate( &
  name="n", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

src = [(REAL(ii, DFP), ii=1, 18)]
CALL u%Set(VALUE=src)
CALL n%Set(VALUE=10.0_DFP * src)

! copy of a LIS block field
CALL v%Initiate(obj2=u)
CALL v%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "LIS -> LIS: values")
v = 0.0_DFP
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "LIS -> LIS: u unchanged")

! copy of a native block field
CALL w%Initiate(obj2=n)
CALL OK(w%GetEngineName() .EQ. "LIS_OMP", "native -> LIS: engine")
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 10.0_DFP * src) .LT. tol), &
        "native -> LIS: values")

CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL n%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
