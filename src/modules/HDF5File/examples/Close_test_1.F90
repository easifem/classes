!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Close a HDF5 file
!
! Close closes the file but keeps the object initiated, so the file
! can be opened again. Calling Close on a closed file does nothing.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Close_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%CLOSE()
  CALL OK(.NOT. obj%IsOpen(), "Close")
  CALL OK(obj%IsInitiated(), "still initiated after Close")

  CALL obj%CLOSE()
  CALL OK(.NOT. obj%IsOpen(), "Close a closed file")
  CALL obj%DEALLOCATE()
END PROGRAM main
