!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the shape of a dataset
!
! The shape of a scalar dataset has size zero.

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

  CHARACTER(*), PARAMETER :: filename = "GetDataShape_test_1.h5"
  TYPE(HDF5File_) :: obj
  REAL(DFP) :: mat(3, 4)
  INTEGER(I4B), ALLOCATABLE :: dshape(:)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  mat = 1.0_DFP
  CALL obj%Initiate(filename=filename, mode="NEW")
  CALL obj%OPEN()
  CALL obj%WRITE(dsetname="/mat", vals=mat)
  CALL obj%WRITE(dsetname="/scalar", vals=1.0_DFP)

  dshape = obj%GetDataShape("/mat")
  CALL OK(ALL(dshape .EQ. [3, 4]), "GetDataShape of a matrix")
  dshape = obj%GetDataShape("/scalar")
  CALL OK(SIZE(dshape) .EQ. 0, "GetDataShape of a scalar")

  CALL obj%DEALLOCATE()
END PROGRAM main
