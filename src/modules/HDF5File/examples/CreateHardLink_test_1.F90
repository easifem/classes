!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Create a hard link to a dataset
!
! The link is a second name for the same dataset.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "CreateHardLink_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: aval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  CALL obj%WRITE(dsetname="/data", vals=5.0_DFP)
  CALL obj%CreateHardLink(source_path="/data", link_path="/link")
  CALL OK(obj%PathExists("/link"), "CreateHardLink")
  CALL obj%READ(dsetname="/link", vals=aval)
  CALL OK(ABS(aval - 5.0_DFP) .LT. tol, "read through the link")

  CALL obj%DEALLOCATE()
END PROGRAM main
