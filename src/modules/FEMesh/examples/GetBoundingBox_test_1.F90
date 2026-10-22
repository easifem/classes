!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the bounding box of a mesh
!
! GetBoundingBox returns the smallest box which contains all nodes
! of the mesh. With nodes, the box of the given coordinates (xiJ
! format, columns are global nodes) is returned.
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
  USE BaseType, ONLY: BoundingBox_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BoundingBox_Method, ONLY: OPERATOR(.Xmax.)
  USE BoundingBox_Method, ONLY: OPERATOR(.Xmin.)
  USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
  USE BoundingBox_Method, ONLY: OPERATOR(.Ymin.)
  USE BoundingBox_Method, ONLY: OPERATOR(.Zmax.)
  USE BoundingBox_Method, ONLY: OPERATOR(.Zmin.)
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: DFP
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  TYPE(BoundingBox_) :: box
  REAL(DFP), ALLOCATABLE :: xij(:, :)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  box = obj%GetBoundingBox()
  CALL OK(ABS(.Xmin.box) .LT. tol, "Xmin")
  CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax")
  CALL OK(ABS(.Ymin.box) .LT. tol, "Ymin")
  CALL OK(ABS((.Ymax.box) - 1.0_DFP) .LT. tol, "Ymax")
  CALL OK(ABS(.Zmin.box) .LT. tol, "Zmin")
  CALL OK(ABS(.Zmax.box) .LT. tol, "Zmax")

  CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
  box = obj%GetBoundingBox(nodes=xij)
  CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "box of nodes")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
