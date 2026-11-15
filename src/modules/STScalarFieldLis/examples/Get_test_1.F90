!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Get the values of a LIS space-time scalar field
!
! u is a STScalarFieldLis_ (six space DOFs, timeCompo = 2) initiated
! with engine = "NATIVE_SERIAL": Initiate sets the engine to LIS_OMP,
! so Get2 and Get3 of STScalarField_ read the LIS vector by GetMultiple
! (in OpenMP loops), not the stale realVec. Entry (a - 1) * 6 + i, the
! value of node i and time component a, is set to k = (a - 1) * 6 + i.
!
!- Get(VALUE(:, :), nrow, ncol, storageFMT): all values (Get2),
!- Get(VALUE(:, :), nrow, ncol, globalNode(:), islocal, storageFMT):
!  the selected nodes (Get3),
!- Get(VALUE(:), tsize, globalNode, islocal): one node (Get1).

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarFieldLis_) :: w, u
REAL(DFP) :: src(12), found(12), v2(6, 6)
INTEGER(I4B) :: ii, nrow, ncol, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL w%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL u%Initiate(NAME="u", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof, timefedof=timefedof)
src = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)

! Test 1: the engine is LIS_OMP, whatever engine is given
CALL OK(u%GetEngineName() .EQ. "LIS_OMP", "Initiate: engine LIS_OMP")
CALL OK(u%lis_ptr .NE. 0, "Initiate: LIS vector created")

! Test 2: Get2, all values in NODES and DOF format
BLOCK
  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 6, "Get2, NODES: shape")
  CALL OK(ALL(ABS(v2(1, 1:6) - src(1:6)) .LT. tol) .AND. &
          ALL(ABS(v2(2, 1:6) - src(7:12)) .LT. tol), "Get2, NODES")

  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 2, "Get2, DOF: shape")
  CALL OK(ALL(ABS(v2(1:6, 1) - src(1:6)) .LT. tol) .AND. &
          ALL(ABS(v2(1:6, 2) - src(7:12)) .LT. tol), "Get2, DOF")
END BLOCK

! Test 3: Get3, selected nodes in NODES and DOF format
BLOCK
  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, globalNode=[2, 5], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "Get3, NODES: shape")
  CALL OK(ALL(ABS(v2(1:2, 1) - [2.0_DFP, 8.0_DFP]) .LT. tol) .AND. &
          ALL(ABS(v2(1:2, 2) - [5.0_DFP, 11.0_DFP]) .LT. tol), &
          "Get3, NODES")

  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, globalNode=[2, 5], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "Get3, DOF: shape")
  CALL OK(ALL(ABS(v2(1:2, 1) - [2.0_DFP, 5.0_DFP]) .LT. tol) .AND. &
          ALL(ABS(v2(1:2, 2) - [8.0_DFP, 11.0_DFP]) .LT. tol), &
          "Get3, DOF")
END BLOCK

! Test 4: Get1, all time components of one node
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=4, islocal=math%yes)
  CALL OK(tsize .EQ. 2 .AND. &
          ALL(ABS(found(1:2) - [4.0_DFP, 10.0_DFP]) .LT. tol), "Get1")
END BLOCK

CALL u%DEALLOCATE()
CALL w%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
