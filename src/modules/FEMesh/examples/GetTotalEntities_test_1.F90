!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Test the `GetTotalEntities` method of `FEMesh_` class.

PROGRAM main
USE FEMesh_Class, ONLY: FEMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE GlobalData, ONLY: I4B, LGT
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display
USE Test_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B) :: ans(4)
LOGICAL(LGT) :: isok

! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")
! Open the mesh file
CALL meshfile%OPEN()
! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL meshfile%DEALLOCATE()

ans = obj%GetTotalEntities()
CALL Display("Total entities in mesh:")

isok = ans(1) .EQ. obj%GetTotalNodes()
CALL OK(isok, "Total nodes: ")

isok = ans(2) .EQ. obj%GetTotalEdges()
CALL OK(isok, "Total edges: ")

isok = ans(3) .EQ. obj%GetTotalFaces()
CALL OK(isok, "Total faces: ")

isok = ans(4) .EQ. obj%GetTotalCells()
CALL OK(isok, "Total cells: ")

! CALL Display(ans(1), "Total nodes:")
! CALL Display(ans(2), "Total edges:")
! CALL Display(ans(3), "Total faces:")
! CALL Display(ans(4), "Total cells:")

ans = obj%GetTotalEntities(globalElement=1, islocal=.TRUE.)
CALL Display("Total entities in local element 1:")

isok = ans(1) .EQ. 3
CALL OK(isok, "Total nodes: ")

isok = ans(2) .EQ. 0
CALL OK(isok, "Total edges: ")

isok = ans(3) .EQ. 3
CALL OK(isok, "Total faces: ")

isok = ans(4) .EQ. 1
CALL OK(isok, "Total cells: ")

! CALL Display(ans(1), "Total nodes:")
! CALL Display(ans(2), "Total edges:")
! CALL Display(ans(3), "Total faces:")
! CALL Display(ans(4), "Total cells:")

! Display the content of mesh.
CALL obj%DEALLOCATE()
END PROGRAM main

