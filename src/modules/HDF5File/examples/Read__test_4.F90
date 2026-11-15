!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read String data without allocation

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE String_Class, ONLY: String
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read__test_4.h5"
  TYPE(HDF5File_) :: obj
  TYPE(String) :: v(2), m(2, 2)
  TYPE(String) :: vbuf(5), mbuf(3, 3)
  INTEGER(I4B) :: tsize, nrow, ncol
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  v(1) = "hello"
  v(2) = "world"
  m(1, 1) = "a"
  m(2, 1) = "b"
  m(1, 2) = "c"
  m(2, 2) = "d"

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
  CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
  CALL obj%DEALLOCATE()

  CALL OK(tsize .EQ. 2, "rank 1 tsize")
  isok = (vbuf(1)%chars() .EQ. "hello") .AND. &
    (vbuf(2)%chars() .EQ. "world")
  CALL OK(isok, "rank 1 values")
  CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 2), "rank 2 nrow, ncol")
  isok = (mbuf(1, 1)%chars() .EQ. "a") .AND. &
    (mbuf(2, 2)%chars() .EQ. "d")
  CALL OK(isok, "rank 2 values")
END PROGRAM main
