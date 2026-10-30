!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check H1 facet DOF IDs with sparse node IDs
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
INTEGER(I4B) :: actual(16), want(16), nodes(8), nactual, nnode, nface
INTEGER(I4B) :: iface, left(16), right(16), nleft, nright, ii, SHARED

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
                     STATUS="OLD", ACTION="READ")
CALL SOURCE%OPEN()
CALL SOURCE%READ()
CALL meshfile%Initiate(filename="H1FacetConnectivity_test_1.h5", mode="NEW")
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
      DO iface = 1, 4
        CALL mesh%GetFacetConnectivity_(ans=nodes, tsize=nnode, &
                                        globalElement=elements(iel), &
                                        iface=iface)
        want(1:nnode) = mesh%GetLocalNodeNumber(globalNode=nodes(1:nnode))
        CALL obj%GetFaceDOF(globalElement=elements(iel), &
                            localFaceNumber=iface, ans=want(nnode + 1:), &
                            tsize=nface)
        CALL obj%GetFacetConnectivity_(globalElement=elements(iel), &
                                       localFaceNumber=iface, ans=actual, &
                                       tsize=nactual)
        CALL OK(nactual .EQ. order + 1_I4B, "H1 facet connectivity size")
        CALL OK(nactual .EQ. nnode + nface, "Facet block sizes agree")
        IF (nactual .EQ. nnode + nface) CALL OK( &
          ALL(actual(1:nactual) .EQ. want(1:nactual)), "Facet DOF IDs")
      END DO
    END DO
    CALL obj%GetFacetConnectivity_(globalElement=elements(1), &
                                   localFaceNumber=2_I4B, ans=left, &
                                   tsize=nleft)
    CALL obj%GetFacetConnectivity_(globalElement=elements(2), &
                                   localFaceNumber=4_I4B, ans=right, &
                                   tsize=nright)
    SHARED = 0
    DO ii = 1, nleft
      IF (ANY(right(1:nright) .EQ. left(ii))) SHARED = SHARED + 1
    END DO
    CALL OK(SHARED .EQ. order + 1_I4B, "Both cells share the same facet DOFs")
  END DO
END DO

CALL obj%DEALLOCATE()
mesh => NULL()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
OPEN (NEWUNIT=unitno, FILE="H1FacetConnectivity_test_1.h5", &
      STATUS="old", ACTION="read")
CLOSE (unitno, STATUS="delete")
END PROGRAM main
