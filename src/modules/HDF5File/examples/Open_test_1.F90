!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Open a HDF5 file
!
! Open creates the file in mode NEW. After Close the file can be
! opened again; it is not truncated the second time.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Open_test_1.h5"
  TYPE(HDF5File_) :: obj

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL OK(obj%IsOpen(), "Open")
  CALL obj%WRITE(dsetname="/x", vals=1.0_DFP)
  CALL obj%CLOSE()

  CALL obj%OPEN()
  CALL OK(obj%IsOpen(), "Open again")
  CALL OK(obj%PathExists("/x"), "data is kept after Open again")
  CALL obj%DEALLOCATE()
END PROGRAM main
