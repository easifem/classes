!> author: Vikas Sharma, Ph. D.
! date:  2025-05-30
! summary: Benchmark the initiate methods
!
! Benchmarking for initiating the mesh.

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(*), PARAMETER :: filename = "./meshdata/big_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
CHARACTER(*), PARAMETER :: testname = "Benchmark Initiate Method"

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)

CALL meshfile%Initiate(FileName=filename, MODE="READ")

!Open the mesh file
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)

!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)

!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)

CALL ok(.TRUE., testname)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
