!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Check if a HDF5 file is new
!
! A file initiated with mode NEW is new until Open creates it.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "IsNew_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL OK(obj%IsNew(), "IsNew after Initiate")
  CALL obj%OPEN()
  CALL OK(.NOT. obj%IsNew(), "IsNew after Open")
  CALL obj%DEALLOCATE()
END PROGRAM main
