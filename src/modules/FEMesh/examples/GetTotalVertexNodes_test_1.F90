!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the total number of vertex nodes
!
! For a linear mesh the vertex nodes are all nodes. GetTotalVertexNodes
! works for the whole mesh, a mesh id, a list of elements, or one
! element.
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

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetTotalVertexNodes() .EQ. 9_I4B, "whole mesh")
  CALL OK(obj%GetTotalVertexNodes(meshid=1_I4B) .EQ. 9_I4B, "meshid")
  CALL OK(obj%GetTotalVertexNodes(globalElement=[13_I4B, 14_I4B]) &
    .EQ. 6_I4B, "elements 13 and 14")
  CALL OK(obj%GetTotalVertexNodes(globalElement=13_I4B, &
    islocal=.FALSE.) .EQ. 4_I4B, &
    "element 13")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
