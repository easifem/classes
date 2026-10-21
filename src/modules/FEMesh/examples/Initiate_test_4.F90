!> author: Vikas Sharma, Ph. D.
! summary: In this example, we will read a 3D mesh of hexahedral elements with
!          two regions.

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Test_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile

CHARACTER(LEN=*), PARAMETER :: filename = &
                    "./meshdata/very_small_hexa8_mesh_two_region.h5"
INTEGER(I4B), PARAMETER :: nsd = 3

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%DisplayMeshInfo("Mesh information: ")

CALL OK(.TRUE., "Initiate_4")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
