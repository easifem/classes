!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate node to nodes data
!
! InitiateNodeToNodes stores, for every node, the nodes of the
! elements around it (without the node itself). GetNodeToNodes
! needs this data, it does not initiate it. GetMaxNodeToNodes
! returns the largest number of nodes connected to a node.
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateNodeToNodes()
  CALL OK(obj%IsNodeToNodes(), "IsNodeToNodes")
  CALL OK(obj%IsNodeToElements(), "node to elements is initiated too")
  nodes = obj%GetNodeToNodes(globalNode=9_I4B, includeSelf=.FALSE.)
  CALL OK(SIZE(nodes) .EQ. 8_I4B, "center node has 8 neighbours")
  CALL OK(obj%GetMaxNodeToNodes() .EQ. 8_I4B, "GetMaxNodeToNodes")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
