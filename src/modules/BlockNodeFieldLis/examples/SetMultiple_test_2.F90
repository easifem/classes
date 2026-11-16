!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get ranges of a LIS block node field from fields
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! The fields have V (two space components) and P (one component) on
! six nodes, 18 values in DOF format: V_x of nodes 1-6 (entries 1-6),
! V_y (7-12), then P (13-18).
!
! x and y are LIS block fields, n is a native BlockNodeField_ with the
! same layout. SetMultiple(istart, iend, stride, VALUE=field,
! istart_value, iend_value, stride_value) sets (or adds) scale times a
! range of another field: native or LIS. GetMultiple with scale and
! addContribution sets (or adds) scale times a range of the field to a
! range of a vector.

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
TYPE(BlockNodeFieldLis_) :: x, y
TYPE(BlockNodeField_) :: n
REAL(DFP) :: found(18), want(18), src(18)
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
CALL n%Initiate( &
  name="n", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

src = [(REAL(ii, DFP), ii=1, 18)]
CALL y%SetMultiple(istart=1, iend=18, stride=1, VALUE=src)
CALL n%SetMultiple(istart=1, iend=18, stride=1, VALUE=10.0_DFP * src)

! GetMultiple4, add: found(2:6:2) = found(2:6:2) + 2 * y(1:5:2)
found = 1.0_DFP
CALL y%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
                   istart_value=2, iend_value=6, stride_value=2, &
                   scale=2.0_DFP, addContribution=math%yes)
want = 1.0_DFP
want([2, 4, 6]) = [3.0_DFP, 7.0_DFP, 11.0_DFP]
CALL OK(ALL(ABS(found - want) .LT. tol), "GetMultiple: add")

! GetMultiple4, set: found(4:6) = y(16:18)
found = 7.0_DFP
CALL y%GetMultiple(istart=16, iend=18, stride=1, VALUE=found, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   scale=1.0_DFP)
want = 7.0_DFP
want(4:6) = [16.0_DFP, 17.0_DFP, 18.0_DFP]
CALL OK(ALL(ABS(found - want) .LT. tol), "GetMultiple: set")

! SetMultiple5, LIS <- native: x(13:17:2) = 0.5 * n(1:3)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=13, iend=17, stride=2, VALUE=n, &
                   istart_value=1, iend_value=3, stride_value=1, &
                   scale=0.5_DFP)
want = 7.0_DFP
want([13, 15, 17]) = [5.0_DFP, 10.0_DFP, 15.0_DFP]
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "LIS <- native")

! SetMultiple5, LIS <- LIS, copy: x(1:3) = y(16:18)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=16, iend_value=18, stride_value=1)
want = 7.0_DFP
want(1:3) = [16.0_DFP, 17.0_DFP, 18.0_DFP]
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "LIS <- LIS: copy")

! SetMultiple5, LIS <- LIS, add: x(4:6) = x(4:6) + 2 * y(1:5:2)
CALL x%SetAll(VALUE=1.0_DFP)
CALL x%SetMultiple(istart=4, iend=6, stride=1, VALUE=y, &
                   istart_value=1, iend_value=5, stride_value=2, &
                   scale=2.0_DFP, addContribution=math%yes)
want = 1.0_DFP
want(4:6) = [3.0_DFP, 7.0_DFP, 11.0_DFP]
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "LIS <- LIS: add")

! native <- LIS: n(1:3) = n(1:3) + y(4:6)
CALL n%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   addContribution=math%yes)
want = 10.0_DFP * src
want(1:3) = [14.0_DFP, 25.0_DFP, 36.0_DFP]
CALL n%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "native <- LIS")

CALL n%DEALLOCATE()
CALL y%DEALLOCATE()
CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
