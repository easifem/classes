!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check H1 orders with local and global element IDs
!
! Uses two H1 quadrangles with sparse node and element identifiers.
! The adjacent cells share their complete conforming trace.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MSHFile_Class, ONLY: MSHFile_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FEDOF_) :: obj
TYPE(FEDomain_), TARGET :: dom
TYPE(HDF5File_) :: meshfile
TYPE(MSHFile_) :: SOURCE
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CHARACTER(12), PARAMETER :: families(2) = &
                            [CHARACTER(12) :: "Lagrange", "Hierarchical"]
INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
INTEGER(I4B) :: order, family, iel
INTEGER :: unitno
INTEGER(I4B) :: cellOrder(3), faceOrder(3, 6), edgeOrder(12)
INTEGER(I4B) :: cellOrient(3), faceOrient(3, 6), edgeOrient(12), sizes(7)
INTEGER(I4B) :: cellOrderLocal(3), faceOrderLocal(3, 6), edgeOrderLocal(12)
INTEGER(I4B) :: cellOrientLocal(3), faceOrientLocal(3, 6)
INTEGER(I4B) :: edgeOrientLocal(12), localSizes(7)

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
                     STATUS="OLD", ACTION="READ")
CALL SOURCE%OPEN()
CALL SOURCE%READ()
CALL meshfile%Initiate(filename="H1Orders_test_1.h5", mode="NEW")
CALL meshfile%SetOverwriteStat(.TRUE.)
CALL meshfile%OPEN()
CALL SOURCE%Export(hdf5=meshfile, group="")
CALL SOURCE%DEALLOCATE()
CALL dom%Initiate(meshfile, "")
mesh => dom%GetMeshPointer()
CALL OK(mesh%GetTotalElements() .EQ. 2_I4B, "Fixture has two cells")
CALL OK(mesh%GetTotalVertexNodes() .EQ. 6_I4B, "Fixture has six vertices")
CALL OK(mesh%GetMaxNodeNumber() .GT. 6_I4B, "Mesh vertex IDs have gaps")

DO family = 1, SIZE(families)
  DO order = 1, 3
    CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
                      baseInterpolation=TRIM(families(family)), &
                      ipType=TypeInterpolationOpt%Equidistance)
    DO iel = 1, SIZE(elements)
      CALL obj%GetOrders( &
        cellOrder=cellOrderLocal, faceOrder=faceOrderLocal, &
        edgeOrder=edgeOrderLocal, cellOrient=cellOrientLocal, &
        faceOrient=faceOrientLocal, edgeOrient=edgeOrientLocal, &
        tCellOrder=localSizes(1), tFaceOrder=localSizes(2), &
        tEdgeOrder=localSizes(3), &
        tCellOrient=localSizes(4), tFaceOrient=localSizes(5:6), &
        tEdgeOrient=localSizes(7), &
        globalElement=iel, islocal=.TRUE.)
      CALL obj%GetOrders( &
        cellOrder=cellOrder, faceOrder=faceOrder, &
        edgeOrder=edgeOrder, cellOrient=cellOrient, &
        faceOrient=faceOrient, edgeOrient=edgeOrient, &
        tCellOrder=sizes(1), tFaceOrder=sizes(2), tEdgeOrder=sizes(3), &
        tCellOrient=sizes(4), tFaceOrient=sizes(5:6), &
        tEdgeOrient=sizes(7), &
        globalElement=elements(iel), islocal=.FALSE.)
      CALL OK(ALL(sizes .EQ. localSizes), "Local/global output sizes")
      CALL OK(ALL(cellOrder(1:sizes(1)) .EQ. &
                  cellOrderLocal(1:localSizes(1))), "Local/global cell order")
      CALL OK(ALL(faceOrder(:, 1:sizes(2)) .EQ. &
                  faceOrderLocal(:, 1:localSizes(2))), &
                  "Local/global facet orders")
      CALL OK(ALL(edgeOrder(1:sizes(3)) .EQ. &
                  edgeOrderLocal(1:localSizes(3))), &
                  "Local/global edge orders")
      CALL OK(ALL(cellOrient(1:sizes(4)) .EQ. &
                  cellOrientLocal(1:localSizes(4))), &
                  "Local/global cell orientation")
      CALL OK(ALL(faceOrient(1:sizes(5), 1:sizes(6)) .EQ. &
                  faceOrientLocal(1:localSizes(5), 1:localSizes(6))), &
              "Local/global facet orientations")
      CALL OK(ALL(edgeOrient(1:sizes(7)) .EQ. &
                  edgeOrientLocal(1:localSizes(7))), &
                  "Local/global edge orientations")
    END DO
  END DO
END DO

CALL obj%DEALLOCATE()
mesh => NULL()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
OPEN (NEWUNIT=unitno, FILE="H1Orders_test_1.h5", &
      STATUS="old", ACTION="read")
CLOSE (unitno, STATUS="delete")
END PROGRAM main
