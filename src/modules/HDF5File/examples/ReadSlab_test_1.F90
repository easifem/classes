!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read slabs of datasets
!
! ReadSlab reads icount entries along the last dimension, starting at the
! zero based index istart; the other dimensions are read whole. It works
! for extendable datasets (Append) and for datasets written by Write.

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

  CHARACTER(*), PARAMETER :: filename = "ReadSlab_test_1.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(HDF5File_) :: obj
  INTEGER(I4B), ALLOCATABLE :: ivec(:), conn(:, :)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  REAL(DFP) :: m(3, 4)
  REAL(DFP), ALLOCATABLE :: mslab(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP, &
               7.0_DFP, 8.0_DFP, 9.0_DFP, 10.0_DFP, 11.0_DFP, 12.0_DFP], &
             [3, 4])

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%Append("/ids", [10_I4B, 20_I4B, 30_I4B])
  CALL obj%Append("/ids", [40_I4B, 50_I4B])
  CALL obj%Append("/cells", RESHAPE([1_I4B, 2_I4B, 3_I4B, &
                                     2_I4B, 3_I4B, 4_I4B], [3, 2]))
  CALL obj%Append("/types", [5_INT8, 5_INT8, 9_INT8])
  CALL obj%WRITE(dsetname="/m", vals=m)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()

  ! rank-1 integer; ivec is allocated with another size first
  ALLOCATE(ivec(10))
  CALL obj%ReadSlab("/ids", 2, 3, ivec)
  CALL OK(SIZE(ivec) .EQ. 3, "integer rank-1: size")
  CALL OK(ALL(ivec .EQ. [30, 40, 50]), "integer rank-1: values")

  ! rank-2 integer: the first dimension is read whole
  CALL obj%ReadSlab("/cells", 1, 1, conn)
  CALL OK(ALL(SHAPE(conn) .EQ. [3, 1]), "integer rank-2: shape")
  CALL OK(ALL(conn(:, 1) .EQ. [2, 3, 4]), "integer rank-2: values")

  ! unsigned 8-bit integers
  CALL obj%ReadSlab("/types", 0, 3, types)
  CALL OK(ALL(types .EQ. [5_INT8, 5_INT8, 9_INT8]), "8-bit integers")

  ! a slab of a dataset written by Write (not extendable)
  CALL obj%ReadSlab("/m", 1, 2, mslab)
  CALL OK(ALL(SHAPE(mslab) .EQ. [3, 2]), "slab of Write data: shape")
  CALL OK(ALL(ABS(mslab - m(:, 2:3)) .LT. tol), "slab of Write data")
  CALL obj%DEALLOCATE()
END PROGRAM main
