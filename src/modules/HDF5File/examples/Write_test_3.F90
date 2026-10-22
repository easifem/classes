!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read integer data

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_3.h5"
  TYPE(HDF5File_) :: obj
  INTEGER(I4B) :: s, v(5), m(2, 3)
  INTEGER(I4B) :: s1
  INTEGER(I4B), ALLOCATABLE :: v1(:), m1(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = 7
  v = [1, 2, 3, 4, 5]
  m = RESHAPE([1, 2, 3, 4, 5, 6], [2, 3])

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

  CALL OK(s1 .EQ. s, "rank 0")
  CALL OK(ALL(v1 .EQ. v), "rank 1")
  CALL OK(ALL(m1 .EQ. m), "rank 2")
END PROGRAM main
