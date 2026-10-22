!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Delete a HDF5 file from disk
!
! Delete removes the file from disk. The object stays initiated, so
! Deallocate is still called at the end.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Delete_test_1.h5"
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%CLOSE()
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(isok, "file exists")

  CALL obj%Delete()
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(.NOT. isok, "Delete")
  CALL obj%DEALLOCATE()
END PROGRAM main
