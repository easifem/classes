!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get node coordinates
!
! GetNodeCoord reads the coordinates of all nodes from the mesh
! file, or returns the coordinates of all nodes, of an element, of
! a list of nodes, or of one node. Coordinates are in xiJ format
! (one column per node, three rows). Columns are in local node
! order.
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
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: xx(3, 20), x(3)
  INTEGER(I4B) :: nrow, ncol, tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
  CALL OK(ALL(SHAPE(xij) .EQ. [3, 9]), "from the mesh file")

  CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 9_I4B, "all nodes")
  CALL OK(ALL(ABS(xx(1:2, 9) - 0.5_DFP) .LT. tol), "node 9")

  CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
    globalElement=16_I4B)
  CALL OK(ncol .EQ. 4_I4B, "element 16: ncol")
  CALL OK(ALL(ABS(xx(1:2, 3) - 1.0_DFP) .LT. tol), "element 16: node 4")

  CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
    globalNode=[4_I4B, 1_I4B])
  CALL OK(ALL(ABS(xx(1:2, 2)) .LT. tol), "nodes 4 and 1")

  CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=6_I4B)
  CALL OK(ALL(ABS(x(1:2) - [0.5_DFP, 1.0_DFP]) .LT. tol), "node 6")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
