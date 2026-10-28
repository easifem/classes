!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Read slabs into a buffer without allocation
!
! ReadSlab_ reads icount entries along the last dimension, starting at
! the zero based index istart, into a buffer given by the caller. The
! buffer is not allocated or reallocated; it should be large enough.
! tsize (rank-1) or nrow, ncol (rank-2) give the size of the data read.
! A rank-2 slab is placed in vals(1:nrow, 1:ncol), so the buffer can be
! larger than the slab in both dimensions. A rank-1 slab can be read
! into a contiguous section of an array, for example the first row of
! a (1, n) array.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "ReadSlab__test_1.h5"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(HDF5File_) :: obj
REAL(DFP) :: m(3, 4), rbuf(10), rbuf2(5, 6), row(1, 4)
INTEGER(I4B) :: ibuf(10), ibuf2(4, 3), tsize, nrow, ncol
INTEGER(INT8) :: tbuf(5)

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP, &
             7.0_DFP, 8.0_DFP, 9.0_DFP, 10.0_DFP, 11.0_DFP, 12.0_DFP], &
            [3, 4])

CALL obj%Initiate(filename=filename, mode="NEW")
CALL obj%OPEN()
CALL obj%Append("/u", [0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP, 0.5_DFP])
CALL obj%Append("/m", m)
CALL obj%Append("/ids", [10_I4B, 20_I4B, 30_I4B, 40_I4B, 50_I4B])
CALL obj%Append("/cells", RESHAPE([1_I4B, 2_I4B, 3_I4B, &
                                   2_I4B, 3_I4B, 4_I4B], [3, 2]))
CALL obj%Append("/types", [5_INT8, 5_INT8, 9_INT8])
CALL obj%DEALLOCATE()

CALL obj%Initiate(filename=filename, mode="READ")
CALL obj%OPEN()

! rank-1 real into a larger buffer
rbuf = -1.0_DFP
CALL obj%ReadSlab_("/u", 1, 3, rbuf, tsize)
CALL OK(tsize .EQ. 3, "real rank-1: tsize")
CALL OK(ALL(ABS(rbuf(1:3) - [0.2_DFP, 0.3_DFP, 0.4_DFP]) .LT. tol), &
        "real rank-1: values")
CALL OK(ALL(rbuf(4:) .EQ. -1.0_DFP), "real rank-1: rest unchanged")

! rank-1 real into the first row of a (1, n) array
CALL obj%ReadSlab_("/u", 0, 4, row(1, :), tsize)
CALL OK(tsize .EQ. 4 .AND. ALL(ABS(row(1, :) - [0.1_DFP, 0.2_DFP, &
                                                0.3_DFP, 0.4_DFP]) .LT. tol), "real rank-1: row of an array")

! rank-2 real into a larger buffer: vals(1:nrow, 1:ncol)
rbuf2 = -1.0_DFP
CALL obj%ReadSlab_("/m", 1, 2, rbuf2, nrow, ncol)
CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 2, "real rank-2: nrow, ncol")
CALL OK(ALL(ABS(rbuf2(1:3, 1:2) - m(:, 2:3)) .LT. tol), &
        "real rank-2: values")
CALL OK(ALL(rbuf2(4:, :) .EQ. -1.0_DFP) .AND. &
        ALL(rbuf2(:, 3:) .EQ. -1.0_DFP), "real rank-2: rest unchanged")

! rank-1 and rank-2 integer
CALL obj%ReadSlab_("/ids", 2, 3, ibuf, tsize)
CALL OK(tsize .EQ. 3 .AND. ALL(ibuf(1:3) .EQ. [30, 40, 50]), &
        "integer rank-1")
ibuf2 = 0
CALL obj%ReadSlab_("/cells", 1, 1, ibuf2, nrow, ncol)
CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 1 .AND. &
        ALL(ibuf2(1:3, 1) .EQ. [2, 3, 4]), "integer rank-2")

! unsigned 8-bit integers
CALL obj%ReadSlab_("/types", 0, 3, tbuf, tsize)
CALL OK(tsize .EQ. 3 .AND. ALL(tbuf(1:3) .EQ. [5_INT8, 5_INT8, 9_INT8]), &
        "8-bit integers")

CALL obj%DEALLOCATE()
END PROGRAM main
