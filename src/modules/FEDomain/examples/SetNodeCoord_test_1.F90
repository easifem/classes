!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the node coordinates of the domain
!
! SetNodeCoord sets the node coordinates (obj = nodeCoord), or with
! addContribution adds them (obj = obj + scale * nodeCoord). The
! shape of nodeCoord must be the shape of the node coordinates.
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
  USE BaseType, ONLY: math => TypeMathOpt
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
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP) :: x(3)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%GetNodeCoord(nodeCoord=xij)
  CALL obj%SetNodeCoord(nodeCoord=xij, scale=1.0_DFP, &
    addContribution=.TRUE.)
  CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=4_I4B)
  CALL OK(ALL(ABS(x(1:2) - 2.0_DFP) .LT. tol), "add")
  CALL obj%SetNodeCoord(nodeCoord=xij)
  CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=4_I4B)
  CALL OK(ALL(ABS(x(1:2) - 1.0_DFP) .LT. tol), "set")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
