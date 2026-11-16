!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Norms, DOT_PRODUCT, PMUL and Reciprocal (LIS)
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! All fields are LIS block fields with V (two space components) and P
! (one component) on six nodes, 18 values. The BLAS methods call the
! LIS vector routines, so all field arguments must use the LIS engine.
!
! x has the values 1, -2, 3, ..., -18 (src(i) = (-1)**(i+1) * i), and y
! has 1 in entries 1-9 and 2 in entries 10-18.
!
!- Norm1 = 1 + 2 + ... + 18 = 171,
!- Norm2 = SQRT(1 + 4 + ... + 324) = SQRT(2109),
!- Normi = 18,
!- DOT_PRODUCT(x, y) = 5 + 2 * (-14) = -23.

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
TYPE(BlockNodeFieldLis_) :: x, y, z
REAL(DFP) :: found(18), src(18), ysrc(18)
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

src = [((-1)**(ii + 1) * REAL(ii, DFP), ii=1, 18)]
ysrc(1:9) = 1.0_DFP
ysrc(10:18) = 2.0_DFP
CALL x%Set(VALUE=src)
CALL y%Set(VALUE=ysrc)

! norms
CALL OK(ABS(x%Norm1() - 171.0_DFP) .LT. tol, "Norm1")
CALL OK(ABS(x%Norm2() - SQRT(2109.0_DFP)) .LT. tol, "Norm2")
CALL OK(ABS(x%Normi() - 18.0_DFP) .LT. tol, "Normi")

! dot product
CALL OK(ABS(x%DOT_PRODUCT(obj2=y) + 23.0_DFP) .LT. tol, "DOT_PRODUCT")

! PMUL, z = x * y entry by entry
CALL z%PMUL(obj1=x, obj2=y)
CALL z%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src * ysrc) .LT. tol), "PMUL")

! Reciprocal, z = 1 / z entry by entry
CALL z%Reciprocal()
CALL z%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 1.0_DFP / (src * ysrc)) .LT. tol), "Reciprocal")

CALL z%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
