!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Set and get the values in a rank 2 array
!
! The values can be given and returned in a rank 2 array in two
! formats: TypeFieldOpt%storageFormatNodes, VALUE(spaceCompo, nodes),
! and TypeFieldOpt%storageFormatDOF, VALUE(nodes, spaceCompo).
! Set(VALUE(:,:), storageFMT) and Get(VALUE(:,:), nrow, ncol,
! storageFMT) use all nodes; Set(globalNode(:), islocal, VALUE(:,:),
! storageFMT) and Get(VALUE(:,:), nrow, ncol, storageFMT, globalNode,
! islocal) the selected nodes.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE FieldOpt_Class, ONLY: TypeFieldOpt
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u
  REAL(DFP) :: xn(2, 6), xd(6, 2), yn(2, 2), yd(2, 2)
  INTEGER(I4B) :: nrow, ncol, ii

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  DO ii = 1, 6
    xn(:, ii) = [REAL(ii, DFP), 10.0_DFP * ii]
  END DO

  ! all nodes, nodes format
  CALL u%Set(VALUE=xn, storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL u%Get(VALUE=xd, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 2, "Get DOF format: nrow, ncol")
  CALL OK(ALL(ABS(xd - TRANSPOSE(xn)) .LT. tol), &
          "Set nodes format, Get DOF format")

  ! all nodes, DOF format
  CALL u%Set(VALUE=2.0_DFP * xd, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL u%Get(VALUE=xn, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 6, "Get nodes format: nrow, ncol")
  CALL OK(ALL(ABS(xn - 2.0_DFP * TRANSPOSE(xd)) .LT. tol), &
          "Set DOF format, Get nodes format")

  ! selected nodes, nodes format
  yn = RESHAPE([-1.0_DFP, -2.0_DFP, -3.0_DFP, -4.0_DFP], [2, 2])
  CALL u%Set(globalNode=[1, 6], islocal=math%yes, VALUE=yn, &
             storageFMT=TypeFieldOpt%storageFormatNodes)
  CALL u%Get(VALUE=yd, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatDOF, &
             globalNode=[1, 6], islocal=math%yes)
  CALL OK(ALL(ABS(yd - TRANSPOSE(yn)) .LT. tol), &
          "Set selected, nodes format")

  ! selected nodes, DOF format
  CALL u%Set(globalNode=[1, 6], islocal=math%yes, VALUE=-yd, &
             storageFMT=TypeFieldOpt%storageFormatDOF)
  CALL u%Get(VALUE=yn, nrow=nrow, ncol=ncol, &
             storageFMT=TypeFieldOpt%storageFormatNodes, &
             globalNode=[6, 1], islocal=math%yes)
  CALL OK(ALL(ABS(yn - RESHAPE([3.0_DFP, 4.0_DFP, 1.0_DFP, 2.0_DFP], &
                              [2, 2])) .LT. tol), &
          "Set selected, DOF format")

  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
