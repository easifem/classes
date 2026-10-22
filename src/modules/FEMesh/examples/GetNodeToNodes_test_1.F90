!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the nodes connected to a node
!
! GetNodeToNodes returns the global node numbers connected to a node
! through the elements around it. With includeSelf the node itself
! is the first entry. For a list of nodes the result is sorted and
! has no duplicates. Call InitiateNodeToNodes first.
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
  USE SortUtility, ONLY: Sort
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: nodes(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateNodeToNodes()

  nodes = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.FALSE.)
  CALL OK(SIZE(nodes) .EQ. 3_I4B, "node 1: size")
  CALL OK(ALL([5, 7, 9] .EQ. Sort(nodes)), "node 1")

  nodes = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.TRUE.)
  CALL OK(nodes(1) .EQ. 1_I4B, "includeSelf: first entry")
  CALL OK(SIZE(nodes) .EQ. 4_I4B, "includeSelf: size")

  nodes = obj%GetNodeToNodes(globalNode=[1_I4B, 2_I4B], &
    includeSelf=.FALSE.)
  CALL OK(ALL(nodes .EQ. [5, 7, 8, 9]), "nodes 1 and 2")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
