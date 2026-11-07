!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing GetOrders in broken spaces
!
! A broken space stores zero face and edge orders, because faces and edges
! own no DOFs. The element still has a complete local basis, so GetOrders
! must return the cell order for every local face and edge. The triangle
! mesh tests the faces and the hexahedron mesh also tests the edges.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, nfile = 2
CHARACTER(30), PARAMETER :: filenames(nfile) = [CHARACTER(30) :: &
  "./meshdata/small_tri3_mesh.h5", "./meshdata/small_hexa8_mesh.h5"]
INTEGER(I4B), PARAMETER :: wantFaces(nfile) = [3, 6]
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: cellOrder(3), faceOrder(3, 6), edgeOrder(12), &
                cellOrient(3), faceOrient(3, 6), edgeOrient(12), &
                tCellOrder, tFaceOrder, tEdgeOrder, tCellOrient, &
                tFaceOrient(2), tEdgeOrient, iel, ifile
LOGICAL(LGT) :: isCell, isFace, isEdge

CALL e%SetQuietMode(math%yes)

DO ifile = 1, nfile
  CALL meshfile%Initiate(TRIM(filenames(ifile)), mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                    isBrokenSpace=math%yes, order=order, dom=dom)

  isCell = math%yes
  isFace = math%yes
  isEdge = math%yes

  DO iel = 1, obj%GetTotalCells()
    CALL obj%GetOrders(cellOrder=cellOrder, faceOrder=faceOrder, &
                       edgeOrder=edgeOrder, cellOrient=cellOrient, &
                       faceOrient=faceOrient, edgeOrient=edgeOrient, &
                       tCellOrder=tCellOrder, tFaceOrder=tFaceOrder, &
                       tEdgeOrder=tEdgeOrder, tCellOrient=tCellOrient, &
                       tFaceOrient=tFaceOrient, tEdgeOrient=tEdgeOrient, &
                       globalElement=iel, islocal=math%yes)

    isCell = isCell .AND. (tCellOrder .EQ. 1) .AND. &
             (cellOrder(1) .EQ. order)
    isFace = isFace .AND. (tFaceOrder .EQ. wantFaces(ifile)) .AND. &
             ALL(faceOrder(1:3, 1:tFaceOrder) .EQ. order)
    isEdge = isEdge .AND. ALL(edgeOrder(1:tEdgeOrder) .EQ. order)
  END DO

  CALL OK(isCell, TRIM(filenames(ifile))//": cell order")
  CALL OK(isFace, TRIM(filenames(ifile))//": faces have the cell order")
  CALL OK(isEdge, TRIM(filenames(ifile))//": edges have the cell order")

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END DO

END PROGRAM main
