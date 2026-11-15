!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Testing High order two dimensional mesh

PROGRAM main
USE FEMesh_Class, only: FEMesh_
USE HDF5File_Class, only: HDF5File_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/small_tri6_mesh.h5"

INTEGER(I4B), PARAMETER :: nsd = 2

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%Display("FEMesh")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main


