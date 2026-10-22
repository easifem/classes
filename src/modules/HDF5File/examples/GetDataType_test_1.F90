!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the data type of a dataset
!
! GetDataType returns 'Rea' for real, 'Int' for integer and 'STR' for
! string datasets.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "GetDataType_test_1.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: strval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  strval = "hello"
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/real", vals=1.0_DFP)
  CALL obj%WRITE(dsetname="/int", vals=1_I4B)
  CALL obj%WRITE(dsetname="/str", vals=strval)

  CALL OK(obj%GetDataType("/real") .EQ. "Rea", "real")
  CALL OK(obj%GetDataType("/int") .EQ. "Int", "integer")
  CALL OK(obj%GetDataType("/str") .EQ. "STR", "string")

  CALL obj%DEALLOCATE()
END PROGRAM main
