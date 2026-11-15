!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read reallocates arrays to the dataset shape
!
! If vals is allocated with a different shape, it is reallocated to
! the shape of the dataset.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: v(4), m(2, 3)
  REAL(DFP), ALLOCATABLE :: v1(:), m1(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
  CALL RANDOM_NUMBER(m)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  ALLOCATE(v1(10), m1(2, 5))
  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL obj%READ(dsetname="/m", vals=m1)
  CALL obj%DEALLOCATE()

  CALL OK(SIZE(v1) .EQ. 4, "rank 1 reallocated")
  CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1 values")
  CALL OK(ALL(SHAPE(m1) .EQ. [2, 3]), "rank 2 reallocated")
  CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2 values")
END PROGRAM main
