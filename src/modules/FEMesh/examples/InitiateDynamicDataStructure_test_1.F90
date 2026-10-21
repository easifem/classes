!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:         Testing InitiateDynamicDataStructure
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE GlobalData

IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)

CALL meshfile%Initiate(FileName=filename, MODE="READ")
!Open the mesh file
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)
!Initiate an instance of `FEMesh_`
CALL obj%Initiate(hdf5=meshfile, dim=2)
! Initiate dynamic data structure
CALL obj%InitiateDynamicDataStructure()
!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
