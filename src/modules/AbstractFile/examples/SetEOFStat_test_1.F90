!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Set and get the end of file status
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! SetEOFStat changes the end of file status only if the file is open.
! On a closed file the call is ignored (a debug message is raised).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./SetEOFStat_test_1.txt"
  TYPE(FortranFile_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)
  CALL OK(.NOT. obj%IsEOF(), "IsEOF: default")

  CALL obj%SetEOFStat(math%yes)
  CALL OK(.NOT. obj%IsEOF(), "SetEOFStat: ignored on closed file")

  CALL obj%OPEN()
  CALL obj%SetEOFStat(math%yes)
  CALL OK(obj%IsEOF(), "SetEOFStat: true")
  CALL obj%SetEOFStat(math%no)
  CALL OK(.NOT. obj%IsEOF(), "SetEOFStat: false")

  CALL obj%Delete()
  CALL obj%DEALLOCATE()
END PROGRAM main
