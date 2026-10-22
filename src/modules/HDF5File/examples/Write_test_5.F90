!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read String data
!
! An array of strings is stored with the length of the longest
! string.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_5.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: s, v(2), m(2, 2)
  TYPE(String) :: s1
  TYPE(String), ALLOCATABLE :: v1(:), m1(:, :)
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = "name"
  v(1) = "hello"
  v(2) = "world"
  m(1, 1) = "a"
  m(2, 1) = "b"
  m(1, 2) = "c"
  m(2, 2) = "d"

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

  CALL OK(s1%chars() .EQ. "name", "scalar")
  isok = (v1(1)%chars() .EQ. "hello") .AND. (v1(2)%chars() .EQ. "world")
  CALL OK(isok, "rank 1")
  isok = (m1(1, 1)%chars() .EQ. "a") .AND. (m1(2, 1)%chars() .EQ. "b") &
    .AND. (m1(1, 2)%chars() .EQ. "c") &
    .AND. (m1(2, 2)%chars() .EQ. "d")
  CALL OK(isok, "rank 2")
END PROGRAM main
