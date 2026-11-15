!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Boundary and internal nodes
!
! The nodes on the facets of boundary elements which are on the
! boundary of the mesh are boundary nodes. Node 9 is the only
! internal node.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nodes(:)
  INTEGER(I4B) :: buf(1)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%IsBoundaryNode(1_I4B), "node 1")
  CALL OK(.NOT. obj%IsBoundaryNode(9_I4B), "node 9")
  CALL OK(obj%GetTotalBoundaryNodes() .EQ. 8_I4B, &
    "GetTotalBoundaryNodes")
  CALL OK(obj%GetTotalInternalNodes() .EQ. 1_I4B, &
    "GetTotalInternalNodes")
  nodes = obj%GetBoundaryNptrs()
  CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8]), "GetBoundaryNptrs")
  nodes = obj%GetInternalNptrs()
  CALL OK(ALL(nodes .EQ. [9]), "GetInternalNptrs")
  CALL obj%GetInternalNptrs_(nptrs=buf)
  CALL OK(buf(1) .EQ. 9_I4B, "GetInternalNptrs_")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
