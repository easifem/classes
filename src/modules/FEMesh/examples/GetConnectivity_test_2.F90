!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get all connectivity data of an element
!
! With opt the connectivity can be the vertices (V), edges (E),
! faces (F), cell (C), or all of them (A). Edges are numbered by
! InitiateEdgeConnectivity, faces by Initiate.
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
  INTEGER(I4B), ALLOCATABLE :: con(:)
  INTEGER(I4B) :: buf(20), cellCon(2), faceCon(10), edgeCon(10)
  INTEGER(I4B) :: nodeCon(10), tsize, tcell, tface, tedge, tnode

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%InitiateEdgeConnectivity()
  con = obj%GetConnectivity(globalElement=13_I4B, opt="E")
  CALL OK(SIZE(con) .EQ. 4_I4B, "opt=E")
  con = obj%GetConnectivity(globalElement=13_I4B, opt="F")
  CALL OK(SIZE(con) .EQ. 4_I4B, "opt=F")
  con = obj%GetConnectivity(globalElement=13_I4B, opt="C")
  CALL OK(ALL(con .EQ. [13]), "opt=C")
  con = obj%GetConnectivity(globalElement=13_I4B, opt="A")
  CALL OK(SIZE(con) .EQ. 13_I4B, "opt=A")
  CALL OK(ALL(con(1:4) .EQ. [1, 5, 9, 7]) .AND. con(13) .EQ. 13_I4B, &
    "opt=A: vertices first, cell last")

  CALL obj%GetConnectivity_(globalElement=13_I4B, ans=buf, &
    tsize=tsize, opt="V")
  CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7]), "GetConnectivity_")

  CALL obj%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
    edgeCon=edgeCon, nodeCon=nodeCon, &
    tCellCon=tcell, tFaceCon=tface, &
    tEdgeCon=tedge, tNodeCon=tnode, &
    globalElement=13_I4B)
  CALL OK(tcell .EQ. 1_I4B .AND. tface .EQ. 4_I4B .AND. &
    tedge .EQ. 4_I4B .AND. tnode .EQ. 4_I4B, "sizes")
  CALL OK(cellCon(1) .EQ. 13_I4B, "cellCon")
  CALL OK(ALL(nodeCon(1:4) .EQ. [1, 5, 9, 7]), "nodeCon")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
