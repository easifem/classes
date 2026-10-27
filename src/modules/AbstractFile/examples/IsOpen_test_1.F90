!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Check if a file is open
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! IsOpen is true after Open, and false after Close or Delete. Delete
! of FortranFile_ removes the file only if it is open.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./IsOpen_test_1.txt"
  TYPE(FortranFile_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)
  CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Initiate")

  CALL obj%OPEN()
  CALL OK(obj%IsOpen(), "IsOpen: after Open")

  CALL obj%CLOSE()
  CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Close")

  CALL obj%OPEN()
  CALL obj%Delete()
  CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Delete")

  CALL obj%DEALLOCATE()
END PROGRAM main
