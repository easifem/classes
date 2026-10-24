!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get node to nodes of a list of nodes with extra nodes
!
! After InitiateExtraNodeToNodes, GetNodeToNodes also returns the
! extra nodes: the nodes of the neighbour elements (elements sharing
! a facet) of the elements around the node. In
! very_small_quad4_mesh.h5 node 1 is in element 13, whose neighbours
! are 14 and 15, so its extra nodes are 2, 3, 6 and 8. Node 5 is in
! elements 13 and 15; their other neighbours are 14 and 16, so its
! extra nodes are 3, 4 and 6. For a list with one node the result is
! the sorted result of the single node version.
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
  INTEGER(I4B), ALLOCATABLE :: one(:), list(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateExtraNodeToNodes()

  one = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.FALSE.)
  CALL OK(ALL(Sort(one) .EQ. [2, 3, 5, 6, 7, 8, 9]), "node 1")

  one = obj%GetNodeToNodes(globalNode=5_I4B, includeSelf=.FALSE.)
  CALL OK(ALL(Sort(one) .EQ. [1, 2, 3, 4, 6, 7, 8, 9]), &
          "node 5: extra nodes of both elements")

  list = obj%GetNodeToNodes(globalNode=[5_I4B], includeSelf=.FALSE.)
  CALL OK(SIZE(list) .EQ. SIZE(one), "list: same size")
  CALL OK(ALL(list .EQ. Sort(one)), "list: same nodes")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
