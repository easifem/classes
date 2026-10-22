!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Check if a path exists in a HDF5 file
!
! The path can be a group or a dataset.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "PathExists_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%WRITE(dsetname="/grp1/grp2/data", vals=1.0_DFP)
  CALL OK(obj%PathExists("/grp1"), "group")
  CALL OK(obj%PathExists("/grp1/grp2/data"), "dataset")
  CALL OK(.NOT. obj%PathExists("/grp1/missing"), "missing")
  CALL OK(.NOT. obj%PathExists("/missing/data"), "missing parent")

  CALL obj%DEALLOCATE()
END PROGRAM main
