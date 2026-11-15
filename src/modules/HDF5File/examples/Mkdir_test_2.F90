!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Create an existing group in mode WRITE
!
! Creating a group that exists does nothing, in every mode. In mode
! WRITE (no overwriting) this is not an error, since nothing is
! overwritten.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Mkdir_test_2.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%Mkdir("/a/b")
  CALL obj%WRITE(dsetname="/a/b/x", vals=1.0_DFP)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="WRITE")
  CALL obj%OPEN()
  CALL obj%Mkdir("/a/b")
  CALL OK(obj%IsGroup("/a/b"), "existing group in mode WRITE")
  CALL obj%Mkdir("/a/c")
  CALL OK(obj%IsGroup("/a/c"), "new group in mode WRITE")
  CALL OK(obj%PathExists("/a/b/x"), "data of the group is kept")
  CALL obj%DEALLOCATE()
END PROGRAM main
