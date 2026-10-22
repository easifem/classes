!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the bounding box of the domain
!
! Without dim the box of all nodes of the domain is returned; with
! dim the box of the nodes of the mesh of dimension dim.
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
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
  USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  TYPE(BoundingBox_) :: box

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  box = obj%GetBoundingBox()
  CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax")
  CALL OK(ABS((.Ymax.box) - 1.0_DFP) .LT. tol, "Ymax")
  box = obj%GetBoundingBox(DIM=1_I4B)
  CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax of curve mesh")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
