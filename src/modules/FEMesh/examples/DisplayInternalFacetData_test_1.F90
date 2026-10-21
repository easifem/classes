!> author: Vikas Sharma, Ph. D.
! summary: DisplayInternalFacetData_test_1

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = &
  & "../../Mesh/examples/meshdata/small_mesh.h5"
CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=2)
CALL obj%InitiateFacetElements()
CALL obj%DisplayInternalFacetData(msg="internal facet data of "//filename)
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
