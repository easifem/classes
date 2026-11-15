!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a space-time field from a rank-2 array
!
!- Set(VALUE(:, :), storageFMT) sets all entries. With the DOF format
!  VALUE(i, a) is the value of space DOF i at time component a; with
!  the NODES format it is VALUE(a, i).
!- Set(globalNode(:), islocal, VALUE(:, :), storageFMT) sets the
!  selected nodes; with the NODES format VALUE(a, j) is the value of
!  node globalNode(j), with the DOF format VALUE(j, a).

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
REAL(DFP) :: vdof(6, 2), vnodes(2, 6), found(6, 6)
INTEGER(I4B) :: ii, nrow, ncol

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! value of node i at time component a is 10 * a + i
DO ii = 1, 6
  vdof(ii, 1:2) = [10.0_DFP, 20.0_DFP] + REAL(ii, DFP)
END DO
vnodes = TRANSPOSE(vdof)

! Test 1: all entries, DOF format
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=vdof, storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(ALL(ABS(found(1:6, 1:2) - vdof) .LT. tol), "all, DOF")
END BLOCK

! Test 2: all entries, NODES format
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=vnodes, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(ALL(ABS(found(1:6, 1:2) - vdof) .LT. tol), "all, NODES")

  CALL u%Set(VALUE=vnodes, storageFMT=TypeFieldOpt%storageFormatNodes, &
             scale=-1.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(ALL(ABS(found(1:6, 1:2)) .LT. tol), "all, NODES: add -1")
END BLOCK

! Test 3: selected nodes, NODES format
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(globalNode=[2, 4], islocal=math%yes, &
             VALUE=vnodes(1:2, [2, 4]), &
             storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[2, 4], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(ALL(ABS(found(1:2, 1:2) - vnodes(1:2, [2, 4])) .LT. tol), &
          "selected, NODES")
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[1, 3], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(ALL(ABS(found(1:2, 1:2)) .LT. tol), "other nodes unchanged")
END BLOCK

! Test 4: selected nodes, DOF format
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(globalNode=[5, 6], islocal=math%yes, &
             VALUE=vdof([5, 6], 1:2), &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL u%Get(VALUE=found, nrow=nrow, ncol=ncol, globalNode=[5, 6], &
             islocal=math%yes, storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(ALL(ABS(found(1:2, 1:2) - vdof([5, 6], 1:2)) .LT. tol), &
          "selected, DOF")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
