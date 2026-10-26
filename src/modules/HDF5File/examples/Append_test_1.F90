!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Append to extendable datasets
!
! Append creates an extendable dataset on the first call and extends it
! along the last dimension. The chunk shape follows the chunk policy: a
! chunk holds stepsPerChunk appends and at least minBytes of data.

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

  CHARACTER(*), PARAMETER :: filename = "Append_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(HDF5File_) :: obj
  TYPE(HDF5ChunkPolicy_) :: policy
  REAL(DFP), ALLOCATABLE :: vals(:), xij(:, :), field(:, :)
  INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
  INTEGER(I4B) :: offset, istep

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()

  ! rank-1: one value per call, then two values
  CALL obj%Append("/steps/values", [0.0_DFP], offset)
  CALL OK(offset .EQ. 0, "Append: offset of first call")
  CALL obj%Append("/steps/values", [0.1_DFP, 0.2_DFP], offset)
  CALL OK(offset .EQ. 1, "Append: offset of second call")
  CALL OK(obj%GetExtent("/steps/values") .EQ. 3, "Append: extent")
  CALL obj%ReadSlab("/steps/values", 1, 2, vals)
  CALL OK(ALL(ABS(vals - [0.1_DFP, 0.2_DFP]) .LT. tol), "ReadSlab rank-1")

  ! the chunk has at least minBytes (64 KiB): 8192 Real64 values
  CALL obj%GetChunkSize(path="/steps/values", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [8192_HSIZE_T]), "chunk of rank-1 appends")
  CALL OK(obj%IsCompressed(path="/steps/values"), "compressed")

  ! rank-2: points (3, nPoints), appended along nPoints
  CALL obj%Append("/points", RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, &
                                      1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2]))
  CALL obj%Append("/points", RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP], &
                                    [3, 1]), offset)
  CALL OK(offset .EQ. 2, "Append rank-2: offset")
  CALL OK(obj%GetExtent("/points") .EQ. 3, "Append rank-2: extent")
  CALL obj%ReadSlab("/points", 2, 1, xij)
  CALL OK(ALL(SHAPE(xij) .EQ. [3, 1]), "ReadSlab rank-2: shape")
  CALL OK(ABS(xij(2, 1) - 1.0_DFP) .LT. tol, "ReadSlab rank-2: value")
  CALL obj%GetChunkSize(path="/points", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [3_HSIZE_T, 2731_HSIZE_T]), "chunk of points")
  CALL obj%DEALLOCATE()

  ! a field of 3000 values per time step, 4 steps per chunk
  policy%stepsPerChunk = 4
  CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
  CALL obj%OPEN()
  ALLOCATE(field(3000, 1))
  DO istep = 1, 5
    field = REAL(istep, DFP)
    CALL obj%Append("/field", field)
  END DO
  CALL OK(obj%GetExtent("/field") .EQ. 5, "time steps: extent")
  CALL obj%GetChunkSize(path="/field", cdims=cdims)
  CALL OK(ALL(cdims .EQ. [3000_HSIZE_T, 4_HSIZE_T]), &
          "stepsPerChunk=4: chunk of 4 steps")
  CALL obj%ReadSlab("/field", 4, 1, xij)
  CALL OK(ALL(ABS(xij - 5.0_DFP) .LT. tol), "time steps: last step")
  CALL obj%DEALLOCATE()
END PROGRAM main
