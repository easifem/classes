PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(UserFunction_) :: obj
  TYPE(HDF5File_) :: hdf5file
! #HDF5File/Initiate
  CALL hdf5file%Initiate(filename="./test_5.h5", MODE="READ")
! #HDF5File/Open
  CALL hdf5file%OPEN()
! #UserFunction/Import
  CALL obj%IMPORT(hdf5file, "")
  CALL obj%Display("")
  CALL hdf5file%DEALLOCATE()
  CALL obj%DEALLOCATE()
END PROGRAM main
