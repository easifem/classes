!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read Real32 and integer data without allocation

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: REAL32
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read__test_2.h5"
  REAL(REAL32), PARAMETER :: tol = 1.0E-6_REAL32
  TYPE(HDF5File_) :: obj
  REAL(REAL32) :: rv(4), rbuf(10)
  INTEGER(I4B) :: iv(2, 3), ibuf(4, 4)
  INTEGER(I4B) :: tsize, nrow, ncol

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL RANDOM_NUMBER(rv)
  iv = RESHAPE([1, 2, 3, 4, 5, 6], [2, 3])

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/rv", vals=rv)
  CALL obj%WRITE(dsetname="/iv", vals=iv)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ_(dsetname="/rv", vals=rbuf, tsize=tsize)
  CALL obj%READ_(dsetname="/iv", vals=ibuf, nrow=nrow, ncol=ncol)
  CALL obj%DEALLOCATE()

  CALL OK(tsize .EQ. 4, "Real32 tsize")
  CALL OK(ALL(ABS(rbuf(1:tsize) - rv) .LT. tol), "Real32 values")
  CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 3), "integer nrow, ncol")
  CALL OK(ALL(ibuf(1:nrow, 1:ncol) .EQ. iv), "integer values")
END PROGRAM main
