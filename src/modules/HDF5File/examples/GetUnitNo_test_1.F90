!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the unit number of a HDF5 file
!
! A free unit number is assigned in Initiate. It is only used to
! delete the file. It is -1 when the object is not initiated.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "GetUnitNo_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL OK(obj%GetUnitNo() .EQ. -1, "GetUnitNo before Initiate")
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL OK(obj%GetUnitNo() .GT. 0, "GetUnitNo after Initiate")
  CALL obj%DEALLOCATE()
  CALL OK(obj%GetUnitNo() .EQ. -1, "GetUnitNo after Deallocate")
END PROGRAM main
