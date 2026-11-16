!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: AXPY, SCAL, COPY, Swap and Set from a block field (LIS)
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! All fields are LIS block fields with V (two space components) and P
! (one component) on six nodes, 18 values. The BLAS methods call the
! LIS vector routines, so all field arguments must use the LIS engine.
!
! Set(VALUE=x) of BlockNodeField_ copies x (COPY), and with
! addContribution adds scale * x (AXPY).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
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
TYPE(BlockNodeFieldLis_) :: w, x, y, z
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

CALL w%Initiate( &
  name="w", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)
CALL x%Initiate( &
  name="x", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)
CALL y%Initiate( &
  name="y", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)
CALL z%Initiate( &
  name="z", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

src = [(REAL(ii, DFP), ii=1, 18)]
CALL x%Set(VALUE=src)
y = 1.0_DFP
z = 10.0_DFP

! AXPY with one, two and three fields
w = 0.0_DFP
CALL w%AXPY(x=x, scale=2.0_DFP)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")

CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - (3.0_DFP * src - 1.0_DFP)) .LT. tol), &
        "AXPY: w + a1 * x1 + a2 * x2")

CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - (2.0_DFP * src + 1.0_DFP)) .LT. tol), &
        "AXPY: w + a1 * x1 + a2 * x2 + a3 * x3")

! COPY and SCAL
CALL w%COPY(obj2=x)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: w = x")
CALL w%SCAL(scale=-2.0_DFP)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found + 2.0_DFP * src) .LT. tol), "SCAL")

! Swap
CALL w%COPY(obj2=x)
CALL w%Swap(obj2=y)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Swap: w = old y")
CALL y%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Swap: y = old w")

! Set from another block field (obj_Set17 of BlockNodeField_)
w = 0.0_DFP
CALL w%Set(VALUE=x)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Set(VALUE=x): copy")
CALL w%Set(VALUE=x, scale=2.0_DFP, addContribution=math%yes)
CALL w%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP * src) .LT. tol), "Set(VALUE=x): add")

CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL w%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
