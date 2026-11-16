!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get entries of a LIS block node field by index
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! The fields have V (two space components) and P (one component) on
! six nodes, 18 values in DOF format: V_x of nodes 1-6 (entries 1-6),
! V_y (7-12), then P (13-18).
!
! SetMultiple and GetMultiple work on the indices of the LIS vector:
!
!- indx(:) with VALUE(:): entries indx(i) = scale * VALUE(i),
!- istart, iend, stride with VALUE(:): one value for each index,
!- istart, iend, stride with a scalar VALUE: one value for all,
!- istart, iend, stride with a range (istart_value, iend_value,
!  stride_value) of VALUE.
!
! SetSingle and GetSingle set and get one entry. With addContribution
! the values are added.

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
TYPE(BlockNodeFieldLis_) :: x
REAL(DFP) :: found(18), want(18), src(18), val
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

src = [(REAL(ii, DFP), ii=1, 18)]

! SetSingle and GetSingle
CALL x%SetAll(VALUE=0.0_DFP)
CALL x%SetSingle(indx=13, VALUE=2.0_DFP, scale=4.0_DFP)
CALL x%GetSingle(indx=13, VALUE=val)
CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale * value")
CALL x%SetSingle(indx=13, VALUE=1.0_DFP, addContribution=math%yes)
CALL x%GetSingle(indx=13, VALUE=val)
CALL OK(ABS(val - 9.0_DFP) .LT. tol, "SetSingle: add")

! indices (SetMultiple1, GetMultiple1)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(indx=[2, 14], VALUE=[10.0_DFP, 20.0_DFP], &
                   scale=2.0_DFP)
CALL x%GetMultiple(indx=[14, 2], VALUE=found, tsize=tsize)
CALL OK(tsize .EQ. 2, "GetMultiple indx: tsize")
CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
        "SetMultiple indx: scale * value")
CALL x%SetMultiple(indx=[2, 14], VALUE=[1.0_DFP, 1.0_DFP], &
                   scale=2.0_DFP, addContribution=math%yes)
CALL x%GetMultiple(indx=[2, 14], VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found(1:2) - [22.0_DFP, 42.0_DFP]) .LT. tol), &
        "SetMultiple indx: add 2 * value")

! range, vector value (SetMultiple2, GetMultiple2): x(13:17:2) = 3 * v
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=13, iend=17, stride=2, &
                   VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], scale=3.0_DFP)
want = 7.0_DFP
want([13, 15, 17]) = [3.0_DFP, 6.0_DFP, 9.0_DFP]
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(tsize .EQ. 18, "GetMultiple range: tsize")
CALL OK(ALL(ABS(found - want) .LT. tol), "SetMultiple range")

! range, one value (SetMultiple4): x(1:18:6) = 0.5 * 8
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=1, iend=18, stride=6, VALUE=8.0_DFP, &
                   scale=0.5_DFP)
want = 7.0_DFP
want([1, 7, 13]) = 4.0_DFP
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "SetMultiple range: one value")

! range of value (SetMultiple3): x(2:6:2) = -1 * src(16:18)
CALL x%SetAll(VALUE=7.0_DFP)
CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
                   istart_value=16, iend_value=18, stride_value=1, &
                   scale=-1.0_DFP)
want = 7.0_DFP
want([2, 4, 6]) = [-16.0_DFP, -17.0_DFP, -18.0_DFP]
CALL x%GetMultiple(istart=1, iend=18, stride=1, VALUE=found, &
                   tsize=tsize)
CALL OK(ALL(ABS(found - want) .LT. tol), "SetMultiple range of value")

! range into a range of value (GetMultiple3): found(4:6) = x(1:3)
found = 0.0_DFP
CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
                   istart_value=4, iend_value=6, stride_value=1, &
                   tsize=tsize)
CALL OK(tsize .EQ. 3, "GetMultiple range of value: tsize")
CALL OK(ALL(ABS(found(4:6) - [7.0_DFP, -16.0_DFP, 7.0_DFP]) .LT. tol) &
        .AND. ALL(ABS(found(1:3)) .LT. tol), &
        "GetMultiple range of value")

CALL x%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
