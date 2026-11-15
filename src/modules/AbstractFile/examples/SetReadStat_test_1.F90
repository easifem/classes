!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Set the read and write status of a closed file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! SetReadStat and SetWriteStat change the access flags while the file
! is not open. On an open file the calls are ignored.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./SetReadStat_test_1.txt"
  TYPE(FortranFile_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, ACTION="WRITE")
  CALL OK(.NOT. obj%IsRead(), "IsRead: WRITE")

  CALL obj%SetReadStat(math%yes)
  CALL OK(obj%IsRead(), "SetReadStat: true")

  CALL obj%SetWriteStat(math%no)
  CALL OK(.NOT. obj%IsWrite(), "SetWriteStat: false")

  CALL obj%DEALLOCATE()
END PROGRAM main
