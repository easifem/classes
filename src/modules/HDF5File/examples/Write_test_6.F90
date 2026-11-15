!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write and read a character variable
!
! A character variable is stored as an array of single characters.
! It can be read in a character variable or in a String.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_6.h5"
  TYPE(HDF5File_) :: obj
  CHARACTER(LEN=11) :: cval
  TYPE(String) :: strval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/greeting", vals="hello world")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/greeting", vals=cval)
  CALL obj%READ(dsetname="/greeting", vals=strval)
  CALL obj%DEALLOCATE()

  CALL OK(cval .EQ. "hello world", "read in character")
  CALL OK(strval%chars() .EQ. "hello world", "read in String")
END PROGRAM main
