!> author: Vikas Sharma, Ph. D.
! date: 2025-06-02
! summary:  Test method for DisplayFacetData
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = &
                               "./meshdata/small_tri3_mesh.h5"

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=2)

CALL obj%InitiateFacetElements()

CALL obj%DisplayFacetData(msg="facet data of "//filename)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
