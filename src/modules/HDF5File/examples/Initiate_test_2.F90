!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate an existing HDF5 file
!
! Modes READ, WRITE and OVERWRITE need an existing file.
! READ gives read access, WRITE gives write access, and OVERWRITE
! gives read and write access and allows overwriting datasets.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Initiate_test_2.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: aval

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! create the file
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/a", vals=1.0_DFP)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL OK(obj%IsRead() .AND. (.NOT. obj%IsWrite()), "mode READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/a", vals=aval)
  CALL OK(ABS(aval - 1.0_DFP) .LT. tol, "read in mode READ")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="WRITE")
  CALL OK(obj%IsWrite() .AND. (.NOT. obj%IsRead()), "mode WRITE")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/b", vals=2.0_DFP)
  CALL OK(obj%PathExists("/b"), "write in mode WRITE")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="OVERWRITE")
  CALL OK(obj%IsWrite() .AND. obj%IsRead(), "mode OVERWRITE")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/a", vals=3.0_DFP)
  CALL obj%READ(dsetname="/a", vals=aval)
  CALL OK(ABS(aval - 3.0_DFP) .LT. tol, "overwrite in mode OVERWRITE")
  CALL obj%DEALLOCATE()
END PROGRAM main
