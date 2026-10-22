!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read logical data without allocation

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Read__test_3.h5"
  TYPE(HDF5File_) :: obj
  LOGICAL(LGT) :: v(3), m(2, 2), t(2, 1, 2)
  LOGICAL(LGT) :: vbuf(5), mbuf(3, 3), tbuf(2, 2, 2)
  INTEGER(I4B) :: tsize, nrow, ncol, dim1, dim2, dim3

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  v = [.TRUE., .FALSE., .TRUE.]
  m = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])
  t = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 1, 2])

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%WRITE(dsetname="/t", vals=t)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
  CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
  CALL obj%READ_(dsetname="/t", vals=tbuf, dim1=dim1, dim2=dim2, &
    dim3=dim3)
  CALL obj%DEALLOCATE()

  CALL OK(tsize .EQ. 3, "rank 1 tsize")
  CALL OK(ALL(vbuf(1:tsize) .EQV. v), "rank 1 values")
  CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 2), "rank 2 nrow, ncol")
  CALL OK(ALL(mbuf(1:nrow, 1:ncol) .EQV. m), "rank 2 values")
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 1, 2]), "rank 3 extents")
  CALL OK(ALL(tbuf(1:dim1, 1:dim2, 1:dim3) .EQV. t), "rank 3 values")
END PROGRAM main
