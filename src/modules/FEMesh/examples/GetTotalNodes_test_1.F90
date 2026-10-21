!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Test for `GetTotalNodes` method of `FEMesh_` class.
!

PROGRAM main
USE FEMesh_Class, ONLY: FEMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE GlobalData, ONLY: I4B, LGT, Quadrangle
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display
USE Test_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B) :: found(8), want(8)
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: testname = "GetTotalNodes"

! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")

! Open the mesh file
CALL meshfile%OPEN()

! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%DisplayMeshInfo("Mesh Info")

CALL meshfile%DEALLOCATE()

found(1) = obj%GetTotalNodes()
found(2) = obj%GetTotalNodes(meshid=1)

want(1) = 9
want(2) = 9

isok = found(1) .EQ. want(1)
CALL ok(isok, testname)

isok = found(2) .EQ. want(2)
CALL ok(isok, testname)

CALL obj%DEALLOCATE()
END PROGRAM main

! Mesh Info
! ==============================
! total nodes: 9
! total elements: 4
! tEdges: 0
! tFaces: 12
! ==============================
