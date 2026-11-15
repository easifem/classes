!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read Real64 data
!
! Missing groups in dsetname are created by Write. Read allocates
! the array to the shape of the dataset.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: s, v(5), m(3, 4), t(2, 3, 4)
  REAL(DFP) :: s1
  REAL(DFP), ALLOCATABLE :: v1(:), m1(:, :), t1(:, :, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = 2.0_DFP
  CALL RANDOM_NUMBER(v)
  CALL RANDOM_NUMBER(m)
  CALL RANDOM_NUMBER(t)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/rank0/s", vals=s)
  CALL obj%WRITE(dsetname="/rank1/v", vals=v)
  CALL obj%WRITE(dsetname="/rank2/m", vals=m)
  CALL obj%WRITE(dsetname="/rank3/t", vals=t)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/rank0/s", vals=s1)
  CALL obj%READ(dsetname="/rank1/v", vals=v1)
  CALL obj%READ(dsetname="/rank2/m", vals=m1)
  CALL obj%READ(dsetname="/rank3/t", vals=t1)
  CALL obj%DEALLOCATE()

  CALL OK(ABS(s1 - s) .LT. tol, "rank 0")
  CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1")
  CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2")
  CALL OK(ALL(ABS(t1 - t) .LT. tol), "rank 3")
END PROGRAM main
