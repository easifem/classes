!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Overwrite a String attribute with a longer value
!
! In mode OVERWRITE an attribute can be written again. A longer String
! must replace the old value; it must not be cut to the old length.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_2.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: s1

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/x", vals=1.0_DFP)
  CALL obj%WriteAttribute(obj_name="/x", attr_name="unit", &
                          attr_val="m")
  CALL obj%WriteAttribute(obj_name="/x", attr_name="unit", &
                          attr_val="meter")
  CALL obj%ReadAttribute(obj_name="/x", attr_name="unit", attr_val=s1)
  CALL obj%DEALLOCATE()

  CALL OK(s1%chars() .EQ. "meter", "longer value replaces the old one")
END PROGRAM main
