!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read empty String data
!
! An empty String is a valid value. Write stores it, and Read returns an
! empty String. This also holds for an array whose entries are all
! empty.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_8.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: s, v(3), s1
  TYPE(String), ALLOCATABLE :: v1(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  s = ""
  v(1) = ""
  v(2) = ""
  v(3) = ""

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/s", vals=s)
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL OK(obj%PathExists("/s"), "scalar: dataset exists")
  CALL OK(obj%PathExists("/v"), "rank 1: dataset exists")
  CALL obj%READ(dsetname="/s", vals=s1)
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL obj%DEALLOCATE()

  CALL OK(s1%LEN_TRIM() .EQ. 0, "scalar: empty")
  CALL OK(SIZE(v1) .EQ. 3, "rank 1: size")
  CALL OK(ALL(v1(:)%LEN_TRIM() .EQ. 0), "rank 1: empty")
END PROGRAM main
