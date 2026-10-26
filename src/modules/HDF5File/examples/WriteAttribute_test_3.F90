!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Replace attributes and write an empty String attribute
!
! When overwriting is allowed, an attribute is replaced by a value of
! another length or another type. An empty String attribute is read back
! as an empty String.

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

  CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_3.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(HDF5File_) :: obj
  TYPE(String) :: s1, empty
  REAL(DFP) :: rval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/x", vals=1.0_DFP)

  ! shorter value
  CALL obj%WriteAttribute(obj_name="/x", attr_name="unit", &
                          attr_val="meter")
  CALL obj%WriteAttribute(obj_name="/x", attr_name="unit", attr_val="m")
  CALL obj%ReadAttribute(obj_name="/x", attr_name="unit", attr_val=s1)
  CALL OK(s1%chars() .EQ. "m" .AND. s1%LEN() .EQ. 1, &
          "shorter value replaces the old one")

  ! another type
  CALL obj%WriteAttribute(obj_name="/x", attr_name="scale", attr_val=3)
  CALL obj%WriteAttribute(obj_name="/x", attr_name="scale", &
                          attr_val=2.5_DFP)
  CALL obj%ReadAttribute(obj_name="/x", attr_name="scale", attr_val=rval)
  CALL OK(ABS(rval - 2.5_DFP) .LT. tol, "integer replaced by real")

  ! empty String
  empty = ""
  CALL obj%WriteAttribute(obj_name="/x", attr_name="note", &
                          attr_val=empty)
  CALL obj%ReadAttribute(obj_name="/x", attr_name="note", attr_val=s1)
  CALL OK(s1%LEN() .EQ. 0, "empty String attribute")

  CALL obj%DEALLOCATE()
END PROGRAM main
