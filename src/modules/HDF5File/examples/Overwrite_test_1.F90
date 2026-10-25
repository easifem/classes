!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Overwrite a dataset with data of the same or another shape
!
! In mode OVERWRITE a dataset can be written again. Data of the same
! shape replaces the old values. Data of another shape (here a smaller
! array) must also replace the dataset, so Read returns the new shape.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Overwrite_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: v1(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=[1.0_DFP, 2.0_DFP, 3.0_DFP])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="OVERWRITE")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=[4.0_DFP, 5.0_DFP, 6.0_DFP])
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL OK(SIZE(v1) .EQ. 3, "same shape: size")
  CALL OK(ALL(ABS(v1 - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "same shape: values")

  CALL obj%WRITE(dsetname="/v", vals=[7.0_DFP, 8.0_DFP])
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL OK(SIZE(v1) .EQ. 2, "smaller shape: size")
  CALL OK(ALL(ABS(v1 - [7.0_DFP, 8.0_DFP]) .LT. tol), &
          "smaller shape: values")
  CALL obj%DEALLOCATE()
END PROGRAM main
