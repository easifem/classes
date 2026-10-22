!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the elements connected to a node
!
! GetNodeToElements returns the global element numbers around a
! node. The node to elements data is initiated on the first call.
! For a list of nodes the result is sorted and has no duplicates.
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
  INTEGER(I4B), ALLOCATABLE :: elems(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  ! corner node 1 belongs to element 13 only
  elems = obj%GetNodeToElements(globalNode=1_I4B)
  CALL OK(SIZE(elems) .EQ. 1_I4B, "node 1: size")
  CALL OK(elems(1) .EQ. 13_I4B, "node 1: element")
  CALL OK(obj%IsNodeToElements(), "IsNodeToElements")

  ! edge node 5 belongs to elements 13 and 15
  elems = obj%GetNodeToElements(globalNode=5_I4B)
  CALL OK(ALL(elems .EQ. [13_I4B, 15_I4B]), "node 5")

  ! center node 9 belongs to all elements
  elems = obj%GetNodeToElements(globalNode=9_I4B)
  CALL OK(ALL(elems .EQ. [13_I4B, 14_I4B, 15_I4B, 16_I4B]), "node 9")

  ! several nodes: union of elements, sorted
  elems = obj%GetNodeToElements(globalNode=[4_I4B, 1_I4B])
  CALL OK(ALL(elems .EQ. [13_I4B, 16_I4B]), "nodes 4 and 1")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
