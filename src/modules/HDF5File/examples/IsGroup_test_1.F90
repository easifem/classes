!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Check if a path is a group

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "IsGroup_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%Mkdir("/grp")
  CALL obj%WRITE(dsetname="/grp/data", vals=1.0_DFP)
  CALL OK(obj%IsGroup("/grp"), "group")
  CALL OK(.NOT. obj%IsGroup("/grp/data"), "dataset is not a group")
  CALL OK(.NOT. obj%IsGroup("/missing"), "missing path")

  CALL obj%DEALLOCATE()
END PROGRAM main
