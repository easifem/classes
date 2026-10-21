!> author: Vikas Sharma, Ph. D.
! date:  2025-06-03
! summary:  Test for InitiateFacetElements method of FEMesh class
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Display_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile

CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh_two_region.h5"
CHARACTER(*), PARAMETER :: testname="InitiateFacetElements_test_2"

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=2)

CALL obj%InitiateFacetElements()

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CALL Display("Finish test: " // testname)

END PROGRAM main
