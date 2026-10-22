!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a HDF5 file in mode NEW
!
! Mode NEW gives read and write access. The file is created (or
! truncated) when Open is called; after that IsNew returns false.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Initiate_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL OK(obj%IsInitiated(), "IsInitiated")
  CALL OK(obj%IsNew(), "IsNew")
  CALL OK(obj%IsRead(), "IsRead")
  CALL OK(obj%IsWrite(), "IsWrite")
  CALL OK(.NOT. obj%IsOpen(), "not open before Open")

  CALL obj%OPEN()
  CALL OK(obj%IsOpen(), "IsOpen")
  CALL OK(.NOT. obj%IsNew(), "not new after Open")

  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
END PROGRAM main
