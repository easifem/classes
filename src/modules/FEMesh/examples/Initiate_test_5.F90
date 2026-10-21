!> author: Vikas Sharma, Ph. D.
! date: 2025-05-30
! summary:  Read a 3D mesh with two regions from an HDF5 file.
!           We specify the entities to be read from the file.
!
! Read a 3D mesh with two regions from an HDF5 file. We specify the entities
! to be read from the file.

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
INTEGER(I4B), PARAMETER :: entities(2) = [1, 2]

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=nsd, entities=entities)
CALL obj%DisplayMeshInfo("Mesh information: ")

! CALL obj%Display("")
CALL ok(.TRUE., "Initiate_5")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
