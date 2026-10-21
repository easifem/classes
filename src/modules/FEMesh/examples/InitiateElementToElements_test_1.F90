!> author: Vikas Sharma, Ph. D.
! date: 2025-06-03
! summary:  This program tests the InitiateElementToElements method of the FEMesh class.
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

TYPE(FEMesh_) :: obj

TYPE(HDF5File_) :: meshfile
CHARACTER(*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"

CALL meshfile%Initiate(FileName=filename, MODE="READ")
!Open the mesh file

CALL meshfile%OPEN()

CALL obj%SetShowTime(.TRUE.)

!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=2)

!Initiate an edge connectivity
CALL obj%InitiateEdgeConnectivity()

! Initiate Element to Element data
CALL obj%InitiateElementToElements()

!Display the content of mesh.
CALL obj%DisplayElementData("ElementData:")

! Display the mesh information
CALL obj%DisplayMeshInfo(filename)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
