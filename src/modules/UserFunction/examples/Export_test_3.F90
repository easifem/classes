PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(UserFunction_) :: obj
  TYPE(HDF5File_) :: hdf5file
! #UserFunction/Initiate
  CALL obj%Initiate(returnType=MATRIX, argType=CONSTANT)
! #UserFunction/Set
  CALL obj%Set(matrixValue=RESHAPE([11.0_DFP, 21.0_DFP, 12.0_DFP, 22.0_DFP], &
                                  [2, 2]))
! #HDF5File/Initiate
  CALL hdf5file%Initiate(filename="./test_7.h5", MODE="NEW")
! #HDF5File/Open
  CALL hdf5file%OPEN()
! #UserFunction/Export
  CALL obj%Export(hdf5file, "")
  CALL hdf5file%DEALLOCATE()
  CALL obj%DEALLOCATE()
END PROGRAM main
