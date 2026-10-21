!> author: Vikas Sharma, Ph. D.
! date: 2025-05-30
! summary:  Read a 2D quadrangle mesh with two regions from an HDF5 file
!
!# Introduction
!
! This program reads a 2D quadrangle mesh with two regions from an HDF5 file.
! It will read all the entities of a given dimension (nsd) from the file.
!
! Read a 2D mesh of quadrangle elements with two regions.

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Test_Method

IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile

CHARACTER(LEN=*), PARAMETER :: filename = &
                       "./meshdata/very_small_quad4_mesh_two_region.h5"
INTEGER(I4B), PARAMETER :: nsd = 2

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

! Read all the entities of dim = nsd
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%DisplayMeshInfo("Mesh information: ")

CALL OK(.TRUE., "Initiate")

! CALL obj%Display("")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
