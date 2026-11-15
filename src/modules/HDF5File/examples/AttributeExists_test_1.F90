!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Check if an attribute exists
!
! AttributeExists returns false, without an error, when the object or the
! attribute does not exist.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "AttributeExists_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/mesh/x", vals=1.0_DFP)
  CALL obj%WriteAttribute(obj_name="/mesh/x", attr_name="unit", &
                          attr_val="m")
  CALL obj%WriteAttribute(obj_name="/mesh", attr_name="nsd", attr_val=2)

  CALL OK(obj%AttributeExists("/mesh/x", "unit"), "attribute of a dataset")
  CALL OK(obj%AttributeExists("/mesh", "nsd"), "attribute of a group")
  CALL OK(.NOT. obj%AttributeExists("/mesh/x", "scale"), &
          "missing attribute")
  CALL OK(.NOT. obj%AttributeExists("/missing", "unit"), "missing object")
  CALL OK(.NOT. obj%AttributeExists("/", "unit"), "root group")
  CALL obj%DEALLOCATE()
END PROGRAM main
