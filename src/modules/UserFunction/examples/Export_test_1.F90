PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(UserFunction_) :: obj
  TYPE(HDF5File_) :: hdf5file
! #UserFunction/Initiate
  CALL obj%Initiate(returnType=SCALAR, argType=CONSTANT)
! #UserFunction/Set
  CALL obj%Set(scalarValue=20.0_DFP)
! #HDF5File/Initiate
  CALL hdf5file%Initiate(filename="./test_5.h5", MODE="NEW")
! #HDF5File/Open
  CALL hdf5file%OPEN()
! #UserFunction/Export
  CALL obj%Export(hdf5file, "")
  CALL hdf5file%DEALLOCATE()
  CALL obj%DEALLOCATE()
END PROGRAM main
