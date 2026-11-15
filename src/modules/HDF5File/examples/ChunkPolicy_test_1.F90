!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Set the layout and compression of datasets with a chunk policy
!
! The default policy compresses datasets with at least 64 KiB of data,
! in uniform chunks of at most 1 MiB of whole columns. chunkMethod=2 uses
! the largest chunk that fits, deflateLevel=-1 turns the compression off,
! and targetBytes changes the chunk size.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5, ONLY: HSIZE_T
  USE HDF5File_Class, ONLY: HDF5ChunkPolicy_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "ChunkPolicy_test_1.h5"
  INTEGER(I4B), PARAMETER :: n = 1000
  TYPE(HDF5File_) :: obj
  TYPE(HDF5ChunkPolicy_) :: policy
  REAL(DFP), ALLOCATABLE :: m(:, :), small(:)
  INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ALLOCATE(m(n, n), small(10))
  m = 1.0_DFP
  small = 2.0_DFP

  ! default policy: deflate level 1, chunks of about 1 MiB
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%WRITE(dsetname="/small", vals=small)
  CALL OK(obj%IsCompressed(path="/m"), "default: m is compressed")
  CALL OK(.NOT. obj%IsCompressed(path="/small"), &
          "default: small is not compressed")
  CALL obj%GetChunkSize(path="/m", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 125_HSIZE_T]), &
          "default: uniform chunks of whole columns")
  CALL obj%DEALLOCATE()

  ! largest chunk that fits targetBytes
  policy%chunkMethod = 2
  CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%GetChunkSize(path="/m", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 131_HSIZE_T]), &
          "chunkMethod=2: largest chunk")
  CALL obj%DEALLOCATE()
  policy%chunkMethod = 1

  ! no compression
  policy%deflateLevel = -1
  CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL OK(.NOT. obj%IsCompressed(), "deflateLevel=-1: no compression")
  CALL OK(.NOT. obj%IsCompressed(path="/m"), &
          "deflateLevel=-1: m is not compressed")
  CALL obj%DEALLOCATE()

  ! smaller chunks
  policy%deflateLevel = 4
  policy%targetBytes = 80000
  CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%GetChunkSize(path="/m", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 10_HSIZE_T]), &
          "targetBytes=80000: chunk 1000 x 10")
  CALL obj%DEALLOCATE()
END PROGRAM main
