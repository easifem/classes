!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read Real64 data without allocation
!
! Read_ does not allocate vals. vals must be large enough; the extent
! of the data is returned, and the rest of vals is not changed.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read__test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: v(3), m(2, 3), t(2, 2, 2)
  REAL(DFP) :: vbuf(10), mbuf(5, 5), tbuf(3, 3, 3)
  INTEGER(I4B) :: tsize, nrow, ncol, dim1, dim2, dim3

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL RANDOM_NUMBER(v)
  CALL RANDOM_NUMBER(m)
  CALL RANDOM_NUMBER(t)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%WRITE(dsetname="/t", vals=t)
  CALL obj%DEALLOCATE()

  vbuf = -1.0_DFP
  mbuf = -1.0_DFP
  tbuf = -1.0_DFP
  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
  CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
  CALL obj%READ_(dsetname="/t", vals=tbuf, dim1=dim1, dim2=dim2, &
    dim3=dim3)
  CALL obj%DEALLOCATE()

  CALL OK(tsize .EQ. 3, "rank 1 tsize")
  CALL OK(ALL(ABS(vbuf(1:tsize) - v) .LT. tol), "rank 1 values")
  CALL OK(ALL(vbuf(tsize + 1:) .EQ. -1.0_DFP), "rank 1 rest")

  CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 3), "rank 2 nrow, ncol")
  CALL OK(ALL(ABS(mbuf(1:nrow, 1:ncol) - m) .LT. tol), "rank 2 values")
  CALL OK(ALL(mbuf(nrow + 1:, :) .EQ. -1.0_DFP), "rank 2 rest")

  CALL OK(ALL([dim1, dim2, dim3] .EQ. 2), "rank 3 dim1, dim2, dim3")
  CALL OK(ALL(ABS(tbuf(1:dim1, 1:dim2, 1:dim3) - t) .LT. tol), &
    "rank 3 values")
END PROGRAM main
