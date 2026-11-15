!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Compress rank-1 and rank-2 datasets
!
! With compression the datasets with at least 64 KiB of data are
! chunked and compressed. This must hold for a rank-1 array and for a rank-2
! array of the same size (400 x 400 Real64 is 1.28 MB). The data is
! read back unchanged, and the compressed file is smaller than the file
! without compression.

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

  CHARACTER(*), PARAMETER :: file1 = "Compression_test_1_zlib.h5"
  CHARACTER(*), PARAMETER :: file2 = "Compression_test_1_raw.h5"
  INTEGER(I4B), PARAMETER :: n = 400
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: v(:), m(:, :), v1(:), m1(:, :)
  INTEGER(I4B) :: i, j, size1, size2

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(v(n * n), m(n, n))
  DO j = 1, n
    DO i = 1, n
      m(i, j) = REAL(MOD(i + j, 7), DFP)
    END DO
  END DO
  v = RESHAPE(m, [n * n])

  CALL obj%Initiate(filename=file1, mode="NEW", zlibOpt=4)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL OK(obj%IsCompressed(path="/v"), "rank 1 is compressed")
  CALL OK(obj%IsCompressed(path="/m"), "rank 2 is compressed")
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=file2, mode="NEW", zlibOpt=-1)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/v", vals=v)
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=file1, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/v", vals=v1)
  CALL obj%READ(dsetname="/m", vals=m1)
  CALL obj%DEALLOCATE()
  CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1: values")
  CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2: values")

  INQUIRE(FILE=file1, SIZE=size1)
  INQUIRE(FILE=file2, SIZE=size2)
  WRITE(*, '(A,I0,A,I0)') "# file size zlib: ", size1, &
    ", no compression: ", size2
  CALL OK(2 * size1 .LT. size2, "compressed file is smaller")
END PROGRAM main
