!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Deallocate a HDF5 file object
!
! Deallocate closes the file and resets the object. With
! delete=.TRUE. the file is also removed from disk.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Deallocate_test_1.h5"
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
  CALL OK(.NOT. obj%IsOpen(), "file is closed")
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(isok, "file is kept")

  ! the file is still open: Deallocate closes it before deleting it
  CALL obj%Initiate(filename=filename, mode="WRITE")
  CALL obj%OPEN()
  CALL obj%DEALLOCATE(delete=.TRUE.)
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(.NOT. isok, "Deallocate with delete")
END PROGRAM main
