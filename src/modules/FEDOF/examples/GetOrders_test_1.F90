!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the cell, face and edge orders of an element
!
! GetOrders returns the order of the cell, of each face and of each edge of
! an element, and their orientation flags. With H1 hierarchical basis of
! homogeneous order 2 on a 2D mesh each element has one cell of order 2 and
! four faces (the edges of the quadrangle) of order 2; the three rows of
! faceOrder hold the same order.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  INTEGER(I4B), PARAMETER :: order = 2
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  INTEGER(I4B) :: cellOrder(3), faceOrder(3, 10), edgeOrder(20), &
    cellOrient(3), faceOrient(3, 10), edgeOrient(20), &
    tCellOrder, tFaceOrder, tEdgeOrder, tCellOrient, &
    tFaceOrient(2), tEdgeOrient, iel
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")
  meshptr => dom%GetMeshPointer()

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=order, dom=dom)

  isok = .TRUE.
  DO iel = 1, meshptr%GetTotalElements()
    CALL obj%GetOrders(cellOrder=cellOrder, faceOrder=faceOrder, &
                       edgeOrder=edgeOrder, cellOrient=cellOrient, &
                       faceOrient=faceOrient, edgeOrient=edgeOrient, &
                       tCellOrder=tCellOrder, tFaceOrder=tFaceOrder, &
                       tEdgeOrder=tEdgeOrder, tCellOrient=tCellOrient, &
                       tFaceOrient=tFaceOrient, tEdgeOrient=tEdgeOrient, &
                       globalElement=iel, islocal=math%yes)

    isok = isok .AND. (tCellOrder .EQ. 1) .AND. (cellOrder(1) .EQ. order)
    isok = isok .AND. (tFaceOrder .EQ. 4) &
      .AND. ALL(faceOrder(1:3, 1:tFaceOrder) .EQ. order)
  END DO
  CALL OK(isok, "GetOrders: cell and face orders of all elements")

  meshptr => NULL()
  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
