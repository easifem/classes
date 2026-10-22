!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Allow overwriting datasets in a file
!
! In mode WRITE an existing dataset can not be written again.
! SetOverwriteStat(.TRUE.) allows it. The new data must have the same
! type and shape as the old data.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "SetOverwriteStat_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: found(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/a", vals=[1.0_DFP, 2.0_DFP])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="WRITE")
  CALL obj%SetOverwriteStat(.TRUE.)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/a", vals=[3.0_DFP, 4.0_DFP])
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/a", vals=found)
  CALL OK(ALL(ABS(found - [3.0_DFP, 4.0_DFP]) .LT. tol), &
    "SetOverwriteStat")
  CALL obj%DEALLOCATE()
END PROGRAM main
