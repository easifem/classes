!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Verify uniform H1 entity numbering on adjacent 2D cells
!
! Run from this examples directory. Uses the checked-in Gmsh fixtures.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MSHFile_Class, ONLY: MSHFile_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FEDOF_) :: obj
TYPE(FEDomain_), TARGET :: dom
TYPE(HDF5File_) :: meshfile
TYPE(MSHFile_) :: SOURCE
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CHARACTER(32), PARAMETER :: fixtures(2) = &
                            [CHARACTER(32) :: "h1_two_triangles.msh.txt", &
                            "h1_two_quads.msh.txt"]
CHARACTER(12), PARAMETER :: families(2) = &
                            [CHARACTER(12) :: "Lagrange", "Hierarchical"]
INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
INTEGER(I4B), PARAMETER :: orders(5) = [1, 2, 3, 4, 1]
INTEGER(I4B) :: shape, family, ip, p, nv, nf, ni, ndof, localSize
INTEGER(I4B) :: iel, iface, ii, jj, n, nnode, nface, ncell, offset
INTEGER(I4B) :: nodes(4), faces(4), found(64), want(64), part(16)
INTEGER(I4B), ALLOCATABLE :: usage(:), con(:)
INTEGER :: unitno

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
DO shape = 1, SIZE(fixtures)
  CALL SOURCE%Initiate(filename="./fixtures/"//TRIM(fixtures(shape)), &
                       STATUS="OLD", ACTION="READ")
  CALL SOURCE%OPEN()
  CALL SOURCE%READ()
  CALL meshfile%Initiate(filename="H1Numbering_test_1.h5", mode="NEW")
  CALL meshfile%SetOverwriteStat(.TRUE.)
  CALL meshfile%OPEN()
  CALL SOURCE%Export(hdf5=meshfile, group="")
  CALL SOURCE%DEALLOCATE()
  CALL dom%Initiate(meshfile, "")
  mesh => dom%GetMeshPointer()
  nv = mesh%GetTotalVertexNodes()
  nf = mesh%GetTotalFaces()
  CALL OK(mesh%GetTotalElements() .EQ. 2, "Two adjacent cells")
  CALL OK(nv .EQ. 2 * shape + 2, "Fixture vertex count")
  CALL OK(nf .EQ. 2 * shape + 3, "Fixture facet count")

  DO family = 1, SIZE(families)
    DO ip = 1, SIZE(orders)
      p = orders(ip)
      IF (shape .EQ. 1) THEN
        ni = (p - 1) * (p - 2) / 2
        localSize = (p + 1) * (p + 2) / 2
      ELSE
        ni = (p - 1)**2
        localSize = (p + 1)**2
      END IF
      ndof = nv + nf * (p - 1) + 2 * ni
      CALL obj%Initiate(order=p, dom=dom, baseContinuity="H1", &
                        baseInterpolation=TRIM(families(family)), &
                        ipType=TypeInterpolationOpt%Equidistance)
      CALL OK(obj%GetTotalDOF() .EQ. ndof, "Global H1 DOF count")
      CALL OK(obj%GetTotalVertexDOF() .EQ. nv, "One DOF per vertex")
      ALLOCATE (usage(ndof))
      usage = 0

      ! In 2D, line interiors are stored in the face block.
      DO iface = 1, nf
        CALL obj%GetFaceDOF(globalFace=iface, ans=part, tsize=n)
        CALL OK(n .EQ. p - 1, "Facet-interior count")
        offset = nv + (iface - 1) * (p - 1)
        CALL OK(ALL(part(1:n) .EQ. [(offset + ii, ii=1, n)]), &
                "Facet IDs follow entity-number order")
      END DO

      DO iel = 1, SIZE(elements)
        CALL mesh%GetConnectivity_(globalElement=elements(iel), &
                                   opt="V", ans=nodes, tsize=nnode)
        CALL mesh%GetConnectivity_(globalElement=elements(iel), &
                                   opt="F", ans=faces, tsize=nface)
        want(1:nnode) = mesh%GetLocalNodeNumber( &
                        globalNode=nodes(1:nnode))
        CALL obj%GetConnectivity_(globalElement=elements(iel), &
                                  opt="Vertex", ans=found, tsize=n)
        CALL OK(n .EQ. nnode, "Vertex block size")
        CALL OK(ALL(found(1:n) .EQ. want(1:nnode)), "Vertex DOF IDs")
        jj = nnode
        DO iface = 1, nface
          offset = nv + (faces(iface) - 1) * (p - 1)
          DO ii = 1, p - 1
            jj = jj + 1
            want(jj) = offset + ii
          END DO
        END DO
        offset = nv + nf * (p - 1) + (iel - 1) * ni
        CALL obj%GetCellDOF(globalCell=elements(iel), &
                            ans=part, tsize=ncell)
        CALL OK(ncell .EQ. ni, "Cell-interior count")
        CALL OK(ALL(part(1:ncell) .EQ. [(offset + ii, ii=1, ni)]), &
                "Cell IDs follow the facet block")
        DO ii = 1, ni
          jj = jj + 1
          want(jj) = offset + ii
        END DO
        con = obj%GetConnectivity(opt="All", globalElement=elements(iel))
        CALL OK(SIZE(con) .EQ. localSize, "Local H1 basis size")
        CALL OK(ALL(con .EQ. want(1:jj)), "Complete entity-block layout")
        CALL OK(ALL(con .GE. 1 .AND. con .LE. ndof), "Valid DOF range")
        DO ii = 1, SIZE(con)
          CALL OK(COUNT(con .EQ. con(ii)) .EQ. 1, "Unique local DOF")
          usage(con(ii)) = usage(con(ii)) + 1
        END DO
      END DO
      CALL OK(ALL(usage .GE. 1), "No holes in global numbering")
      CALL OK(COUNT(usage .EQ. 2) .EQ. p + 1, "Complete shared H1 trace")
      CALL OK(ALL(usage .LE. 2), "No unrelated entities share DOFs")
      DEALLOCATE (usage)
    END DO
  END DO
  CALL obj%DEALLOCATE()
  mesh => NULL()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
  OPEN (NEWUNIT=unitno, FILE="H1Numbering_test_1.h5", &
        STATUS="old", ACTION="read")
  CLOSE (unitno, STATUS="delete")
END DO
END PROGRAM main
