!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write and read high-rank Real64, Real32 and integer data
!
! Real64 data of rank 4 to 7, and Real32 and integer data of rank 3 are
! written and read back. Read allocates the arrays to the shape of the
! dataset. Read_ (integer, rank 3) returns the extent of the data.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: REAL32
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "Write_test_7.h5"
  REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: a4(2, 3, 2, 2), a5(2, 2, 2, 2, 3)
  REAL(DFP) :: a6(2, 2, 2, 2, 2, 2), a7(2, 2, 2, 2, 2, 2, 2)
  REAL(DFP), ALLOCATABLE :: b4(:, :, :, :), b5(:, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: b6(:, :, :, :, :, :)
  REAL(DFP), ALLOCATABLE :: b7(:, :, :, :, :, :, :)
  REAL(REAL32) :: r3(2, 3, 4)
  REAL(REAL32), ALLOCATABLE :: s3(:, :, :)
  INTEGER(I4B) :: i3(3, 2, 2), j3(4, 4, 4), d1, d2, d3

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL RANDOM_NUMBER(a4)
  CALL RANDOM_NUMBER(a5)
  CALL RANDOM_NUMBER(a6)
  CALL RANDOM_NUMBER(a7)
  CALL RANDOM_NUMBER(r3)
  i3 = RESHAPE([(d1, d1=1, SIZE(i3))], SHAPE(i3))

  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/d/a4", vals=a4)
  CALL obj%WRITE(dsetname="/d/a5", vals=a5)
  CALL obj%WRITE(dsetname="/d/a6", vals=a6)
  CALL obj%WRITE(dsetname="/d/a7", vals=a7)
  CALL obj%WRITE(dsetname="/s/r3", vals=r3)
  CALL obj%WRITE(dsetname="/n/i3", vals=i3)
  CALL obj%DEALLOCATE()

  CALL obj%Initiate(filename=filename, mode="READ")
  CALL obj%OPEN()
  CALL obj%READ(dsetname="/d/a4", vals=b4)
  CALL obj%READ(dsetname="/d/a5", vals=b5)
  CALL obj%READ(dsetname="/d/a6", vals=b6)
  CALL obj%READ(dsetname="/d/a7", vals=b7)
  CALL obj%READ(dsetname="/s/r3", vals=s3)
  j3 = 0
  CALL obj%READ_(dsetname="/n/i3", vals=j3, dim1=d1, dim2=d2, dim3=d3)
  CALL obj%DEALLOCATE()

  CALL OK(ALL(ABS(b4 - a4) .LT. tol), "Real64 rank 4")
  CALL OK(ALL(ABS(b5 - a5) .LT. tol), "Real64 rank 5")
  CALL OK(ALL(ABS(b6 - a6) .LT. tol), "Real64 rank 6")
  CALL OK(ALL(ABS(b7 - a7) .LT. tol), "Real64 rank 7")
  CALL OK(ALL(ABS(s3 - r3) .LT. 1.0E-6), "Real32 rank 3")
  CALL OK(d1 .EQ. 3 .AND. d2 .EQ. 2 .AND. d3 .EQ. 2, &
          "integer rank 3: extent")
  CALL OK(ALL(j3(1:d1, 1:d2, 1:d3) .EQ. i3), "integer rank 3: values")
END PROGRAM main
