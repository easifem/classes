!> author: Vikas Sharma, Ph. D.
! date: 2026-10-27
! summary: Set the open status of a file
!
! AbstractFile_ is abstract, so FortranFile_ (which extends it) is used.
! SetOpenStat only changes the flag returned by IsOpen; it does not
! open or close the file. It is normally called by the Open and Close
! methods of the extended types.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FortranFile_Class, ONLY: FortranFile_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./SetOpenStat_test_1.txt"
  TYPE(FortranFile_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename)

  CALL obj%SetOpenStat(math%yes)
  CALL OK(obj%IsOpen(), "SetOpenStat: true")

  CALL obj%SetOpenStat(math%no)
  CALL OK(.NOT. obj%IsOpen(), "SetOpenStat: false")

  CALL obj%DEALLOCATE()
END PROGRAM main
