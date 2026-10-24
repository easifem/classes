!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get node coordinates of the domain
!
! GetNodeCoord returns the coordinates of all nodes (nsd rows), of
! the nodes of a mesh (three rows), of a list of nodes, or of one
! node (nsd rows). GetNodeCoordPointer returns a pointer to the
! node coordinates of the domain.
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
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  REAL(DFP), POINTER :: ptr(:, :)
  REAL(DFP) :: xx(3, 10), x(3)
  REAL(DFP) :: buf(3, 20)
  INTEGER(I4B) :: nrow, ncol, tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%GetNodeCoord(nodeCoord=xij)
  CALL OK(SIZE(xij, 2) .EQ. 9_I4B, "all nodes")
  CALL OK(ALL(ABS(xij(1:2, 9) - 0.5_DFP) .LT. tol), "node 9")
  CALL obj%GetNodeCoord(nodeCoord=xij, DIM=1_I4B)
  CALL OK(SIZE(xij, 2) .EQ. 8_I4B, "nodes of the curve mesh")
  CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
                        globalNode=[4_I4B, 9_I4B])
  CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 2_I4B, "nrow and ncol")
  CALL OK(ALL(ABS(xx(1:2, 1) - 1.0_DFP) .LT. tol), "node 4")
  CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=6_I4B)
  CALL OK(ALL(ABS(x(1:2) - [0.5_DFP, 1.0_DFP]) .LT. tol), "node 6")
  CALL obj%GetNodeCoord(nodeCoord=buf, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 9_I4B, &
          "all nodes without allocation: nrow and ncol")
  CALL OK(ALL(ABS(buf(1:2, 9) - 0.5_DFP) .LT. tol), &
          "all nodes without allocation: node 9")
  buf = 0.0_DFP
  CALL obj%GetParam(nodeCoord=buf)
  CALL OK(ALL(ABS(buf(1:2, 9) - 0.5_DFP) .LT. tol), "GetParam")
  ptr => obj%GetNodeCoordPointer()
  CALL OK(SIZE(ptr, 2) .EQ. 9_I4B, "GetNodeCoordPointer")
  ptr => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
