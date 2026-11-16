!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set a block node field from a block field of another engine
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! u is a LIS block field and n a native BlockNodeField_ with the same
! layout: V (two space components) and P (one component) on six nodes,
! 18 values.
!
! Set(VALUE=field) of BlockNodeField_ copies the values of the other
! field, and with addContribution adds scale times them. It works
! between engines: LIS <- native and native <- LIS.

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
TYPE(BlockNodeFieldLis_) :: u
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

! LIS <- native: copy, then add 2 * n
CALL n%Set(VALUE=src)
u = 0.0_DFP
CALL u%Set(VALUE=n)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "LIS <- native: copy")
CALL u%Set(VALUE=n, scale=2.0_DFP, addContribution=math%yes)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP * src) .LT. tol), "LIS <- native: add")

! scale is not used when the values are copied
CALL u%Set(VALUE=n, scale=5.0_DFP)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "LIS <- native: copy, no scale")

! native <- LIS: copy, then add -1 * u
CALL u%Set(VALUE=10.0_DFP * src)
n = 0.0_DFP
CALL n%Set(VALUE=u)
CALL n%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 10.0_DFP * src) .LT. tol), "native <- LIS: copy")
CALL n%Set(VALUE=u, scale=-1.0_DFP, addContribution=math%yes)
CALL n%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found) .LT. tol), "native <- LIS: add")

CALL n%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
