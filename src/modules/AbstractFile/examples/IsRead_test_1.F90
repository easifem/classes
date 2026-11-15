!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Check the read and write access of a file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! The action argument of Initiate sets the read and write access, which
! are returned by IsRead and IsWrite.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./IsRead_test_1.txt"
  TYPE(FortranFile_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, ACTION="READWRITE")
  CALL OK(obj%IsRead(), "IsRead: READWRITE")
  CALL OK(obj%IsWrite(), "IsWrite: READWRITE")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, ACTION="WRITE")
  CALL OK(.NOT. obj%IsRead(), "IsRead: WRITE")
  CALL OK(obj%IsWrite(), "IsWrite: WRITE")
  CALL obj%DEALLOCATE()
END PROGRAM main
