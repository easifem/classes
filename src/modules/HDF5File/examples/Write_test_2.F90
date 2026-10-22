!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read Real32 data

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: REAL32
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_2.h5"
  REAL(REAL32), PARAMETER :: tol = 1.0E-6_REAL32
  TYPE(HDF5File_) :: obj
  REAL(REAL32) :: s, v(5), m(3, 4)
  REAL(REAL32) :: s1
  REAL(REAL32), ALLOCATABLE :: v1(:), m1(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = 2.0_REAL32
  CALL RANDOM_NUMBER(v)
  CALL RANDOM_NUMBER(m)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/s", vals=s)
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/s", vals=s1)
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL obj%READ(dsetname="/m", vals=m1)
  CALL obj%DEALLOCATE()

  CALL OK(ABS(s1 - s) .LT. tol, "rank 0")
  CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1")
  CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2")
END PROGRAM main
