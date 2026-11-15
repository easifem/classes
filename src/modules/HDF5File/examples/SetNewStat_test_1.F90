!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the new status of a HDF5 file
!
! The new status decides if Open creates the file. Open sets it to
! false after creating the file.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "SetNewStat_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%SetNewStat(.FALSE.)
  CALL OK(.NOT. obj%IsNew(), "SetNewStat false")
  CALL obj%SetNewStat(.TRUE.)
  CALL OK(obj%IsNew(), "SetNewStat true")
  CALL obj%OPEN()
  CALL OK(obj%IsOpen(), "Open creates the file")
  CALL obj%DEALLOCATE()
END PROGRAM main
