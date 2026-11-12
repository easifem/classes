!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Get the values of a space-time scalar field
!
! The field u has six space DOFs and timeCompo = 2. The values are
! stored in the DOF format: first all space DOFs of time component 1,
! then all of time component 2, so the value of node i and time
! component a is entry (a - 1) * 6 + i. Here entry k is set to k.
!
!- Get(VALUE(:), tsize, globalNode): all time components of one node,
!- Get(VALUE(:), tsize, timeCompo): all nodes of one time component,
!- Get(VALUE(:, :), nrow, ncol, storageFMT): all values; NODES format
!  gives VALUE(a, i), DOF format gives VALUE(i, a),
!- Get(VALUE(:, :), nrow, ncol, globalNode(:), islocal, storageFMT):
!  the same for the selected nodes,
!- Get(VALUE(:), globalNode(:), tsize, islocal, timeCompo): the
!  selected nodes of one time component,
!- Get(VALUE, globalNode, islocal, timeCompo): one value.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
REAL(DFP) :: src(12), found(12), val, v2(6, 6)
INTEGER(I4B) :: ii, tsize, nrow, ncol

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

src = [(REAL(ii, DFP), ii=1, 12)]
CALL u%SetMultiple(istart=1, iend=12, stride=1, VALUE=src)

! Test 1: all time components of one node
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=4)
  CALL OK(tsize .EQ. 2, "node: tsize")
  CALL OK(ALL(ABS(found(1:2) - [4.0_DFP, 10.0_DFP]) .LT. tol), &
          "node: values")
END BLOCK

! Test 2: all nodes of one time component
BLOCK
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(tsize .EQ. 6, "timeCompo: tsize")
  CALL OK(ALL(ABS(found(1:6) - src(7:12)) .LT. tol), &
          "timeCompo: values")
END BLOCK

! Test 3: all values in NODES and DOF format
BLOCK
  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 6, "all, NODES: shape")
  CALL OK(ALL(ABS(v2(1, 1:6) - src(1:6)) .LT. tol) .AND. &
          ALL(ABS(v2(2, 1:6) - src(7:12)) .LT. tol), "all, NODES")

  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 2, "all, DOF: shape")
  CALL OK(ALL(ABS(v2(1:6, 1) - src(1:6)) .LT. tol) .AND. &
          ALL(ABS(v2(1:6, 2) - src(7:12)) .LT. tol), "all, DOF")
END BLOCK

! Test 4: selected nodes in NODES and DOF format
BLOCK
  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, globalNode=[2, 5], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "selected, NODES: shape")
  CALL OK(ALL(ABS(v2(1:2, 1) - [2.0_DFP, 8.0_DFP]) .LT. tol) .AND. &
          ALL(ABS(v2(1:2, 2) - [5.0_DFP, 11.0_DFP]) .LT. tol), &
          "selected, NODES")

  v2 = 0.0_DFP
  CALL u%Get(VALUE=v2, nrow=nrow, ncol=ncol, globalNode=[2, 5], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "selected, DOF: shape")
  CALL OK(ALL(ABS(v2(1:2, 1) - [2.0_DFP, 5.0_DFP]) .LT. tol) .AND. &
          ALL(ABS(v2(1:2, 2) - [8.0_DFP, 11.0_DFP]) .LT. tol), &
          "selected, DOF")
END BLOCK

! Test 5: selected nodes of one time component
BLOCK
  CALL u%Get(VALUE=found, globalNode=[3, 6], tsize=tsize, &
             islocal=math%yes, timeCompo=2)
  CALL OK(tsize .EQ. 2, "selected, timeCompo: tsize")
  CALL OK(ALL(ABS(found(1:2) - [9.0_DFP, 12.0_DFP]) .LT. tol), &
          "selected, timeCompo")
END BLOCK

! Test 6: one value
BLOCK
  CALL u%Get(VALUE=val, globalNode=6, islocal=math%yes, timeCompo=1)
  CALL OK(ABS(val - 6.0_DFP) .LT. tol, "single value")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
