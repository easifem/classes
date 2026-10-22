!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write attributes to a dataset
!
! An attribute can be a String, a character, an integer or a real.
! The object (dataset or group) must exist.

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

  CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  TYPE(String) :: strval, found
  INTEGER(I4B) :: ival
  REAL(DFP) :: rval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  strval = "meter"
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/length", vals=2.0_DFP)

  CALL obj%WriteAttribute(obj_name="/length", attr_name="unit", &
    attr_val=strval)
  CALL obj%WriteAttribute(obj_name="/length", attr_name="note", &
    attr_val="measured")
  CALL obj%WriteAttribute(obj_name="/length", attr_name="count", &
    attr_val=3_I4B)
  CALL obj%WriteAttribute(obj_name="/length", attr_name="scale", &
    attr_val=0.5_DFP)

  CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
    attr_val=found)
  CALL OK(found%chars() .EQ. "meter", "String attribute")
  CALL obj%ReadAttribute(obj_name="/length", attr_name="note", &
    attr_val=found)
  CALL OK(found%chars() .EQ. "measured", "character attribute")
  CALL obj%ReadAttribute(obj_name="/length", attr_name="count", &
    attr_val=ival)
  CALL OK(ival .EQ. 3, "integer attribute")
  CALL obj%ReadAttribute(obj_name="/length", attr_name="scale", &
    attr_val=rval)
  CALL OK(ABS(rval - 0.5_DFP) .LT. tol, "real attribute")

  CALL obj%DEALLOCATE()
END PROGRAM main
