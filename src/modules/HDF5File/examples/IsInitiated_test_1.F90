!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Check if a HDF5 file object is initiated

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "IsInitiated_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL OK(.NOT. obj%IsInitiated(), "not initiated")
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL OK(obj%IsInitiated(), "IsInitiated")
  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
END PROGRAM main
