!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the node nearest to a point
!
! GetNearestNode returns the global node number and the coordinates
! of the node nearest to the query point qv. The kdtree of the mesh
! is initiated on the first call. With nn, the nn nearest nodes are
! returned (in no fixed order).
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
  REAL(DFP) :: x(3), xn(3, 2)
  INTEGER(I4B) :: node, nodes(2)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  ! nearest node to (0.9, 0.9) is the corner node 4 at (1, 1)
  CALL obj%GetNearestNode(qv=[0.9_DFP, 0.9_DFP], x=x, globalNode=node)
  CALL OK(node .EQ. 4_I4B, "nearest node to (0.9, 0.9)")
  CALL OK(ALL(ABS(x(1:2) - [1.0_DFP, 1.0_DFP]) .LT. tol), "coordinates")

  ! nearest node to (0.45, 0.55) is the center node 9
  CALL obj%GetNearestNode(qv=[0.45_DFP, 0.55_DFP], x=x, globalNode=node)
  CALL OK(node .EQ. 9_I4B, "nearest node to (0.45, 0.55)")

  ! two nearest nodes to (0.1, 0.0): node 1 and node 5
  CALL obj%GetNearestNode(qv=[0.1_DFP, 0.0_DFP], x=xn, &
    globalNode=nodes, nn=2_I4B)
  CALL OK(ANY(nodes .EQ. 1_I4B) .AND. ANY(nodes .EQ. 5_I4B), &
    "two nearest nodes")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
