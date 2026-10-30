!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check H1 continuity across oppositely directed local edges
!
! Run from this examples directory with the checked-in two-cell fixtures.

PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE HDF5File_Class, ONLY: HDF5File_
USE MSHFile_Class, ONLY: MSHFile_
USE QuadraturePoint_Method, ONLY: Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FEDOF_) :: obj, geometry
TYPE(FEDomain_), TARGET :: dom
TYPE(HDF5File_) :: meshfile
TYPE(MSHFile_) :: SOURCE
TYPE(ElemShapeData_) :: shapes, geoShapes
TYPE(QuadraturePoint_) :: quad
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CLASS(AbstractFE_), POINTER :: fe => NULL(), geo => NULL()
CHARACTER(32), PARAMETER :: fixtures(2) = &
                            [CHARACTER(32) :: "h1_two_triangles.msh.txt", &
                            "h1_two_quads.msh.txt"]
CHARACTER(12), PARAMETER :: families(2) = &
                            [CHARACTER(12) :: "Lagrange", "Hierarchical"]
INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
REAL(DFP), PARAMETER :: t(7) = &
                        [0.0_DFP, 0.13_DFP, 0.27_DFP, 0.51_DFP, 0.73_DFP, &
                        0.89_DFP, 1.0_DFP]
REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
REAL(DFP) :: points(3, 7), xij(2, 4), coord(3), direction(2)
REAL(DFP) :: refCoord(3, 8)
REAL(DFP) :: basis(100, 7, 2), tangent(100, 7, 2), physical(2, 7, 2)
INTEGER(I4B) :: shape, family, p, PASS, visit, iel, ii, jj, ip, ndof
INTEGER(I4B) :: nodes(4, 2), faces(4, 2), nv(2), nf(2), sharedNodes(2)
INTEGER(I4B) :: nshared, sharedFace, localFace(2), ends(2), orientation(2)
INTEGER(I4B) :: ncoord, con(64), ncon
INTEGER(I4B) :: cellOrient(3), faceOrient(3, 4), edgeOrient(12)
INTEGER(I4B) :: tc, tf(2), te, nrow, ncol
INTEGER :: unitno

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
DO shape = 1, SIZE(fixtures)
  CALL SOURCE%Initiate(filename="./fixtures/"//TRIM(fixtures(shape)), &
                       STATUS="OLD", ACTION="READ")
  CALL SOURCE%OPEN()
  CALL SOURCE%READ()
  CALL meshfile%Initiate(filename="H1Orientation_test_1.h5", mode="NEW")
  CALL meshfile%SetOverwriteStat(.TRUE.)
  CALL meshfile%OPEN()
  CALL SOURCE%Export(hdf5=meshfile, group="")
  CALL SOURCE%DEALLOCATE()
  CALL dom%Initiate(meshfile, "")
  mesh => dom%GetMeshPointer()
  DO iel = 1, 2
    CALL mesh%GetConnectivity_(globalElement=elements(iel), opt="V", &
                               ans=nodes(:, iel), tsize=nv(iel))
    CALL mesh%GetConnectivity_(globalElement=elements(iel), opt="F", &
                               ans=faces(:, iel), tsize=nf(iel))
  END DO
  nshared = 0
  sharedNodeLoop: DO ii = 1, nv(1)
    IF (.NOT. ANY(nodes(1:nv(2), 2) .EQ. nodes(ii, 1))) CYCLE sharedNodeLoop
    nshared = nshared + 1
    sharedNodes(nshared) = nodes(ii, 1)
  END DO sharedNodeLoop
  CALL OK(nshared .EQ. 2, "Shared edge has two vertices")
  sharedNodes = [MINVAL(sharedNodes), MAXVAL(sharedNodes)]
  sharedFace = 0
  DO ii = 1, nf(1)
    IF (ANY(faces(1:nf(2), 2) .EQ. faces(ii, 1))) THEN
      sharedFace = faces(ii, 1)
    END IF
  END DO
  CALL OK(sharedFace .GT. 0, "Cells have a shared facet")
  DO iel = 1, 2
    DO ii = 1, nf(iel)
      IF (faces(ii, iel) .EQ. sharedFace) localFace(iel) = ii
    END DO
  END DO
  DO family = 1, SIZE(families)
    CALL geometry%Initiate(order=1_I4B, dom=dom, baseContinuity="H1", &
                           baseInterpolation=TRIM(families(family)), &
                           ipType=TypeInterpolationOpt%Equidistance)
    DO p = 1, 4
      CALL obj%Initiate(order=p, dom=dom, baseContinuity="H1", &
                        baseInterpolation=TRIM(families(family)), &
                        ipType=TypeInterpolationOpt%Equidistance)
      ndof = obj%GetTotalDOF()
      DO PASS = 1, 2
        basis = 0.0_DFP
        tangent = 0.0_DFP
        DO visit = 1, 2
          iel = visit
          IF (PASS .EQ. 2) iel = 3 - visit
          CALL obj%SetFE(globalElement=elements(iel))
          fe => obj%GetFEPointer(globalElement=elements(iel))
          CALL geometry%SetFE(globalElement=elements(iel))
          geo => geometry%GetFEPointer(globalElement=elements(iel))
          CALL mesh%GetOrientation(globalElement=elements(iel), &
                                   cellOrient=cellOrient, &
                                   faceOrient=faceOrient, &
                                   edgeOrient=edgeOrient, tCellOrient=tc, &
                                   tFaceOrient=tf, &
                                   tEdgeOrient=te)
          orientation(iel) = faceOrient(1, localFace(iel))
          DO ii = 1, nv(iel)
            CALL mesh%GetNodeCoord(globalNode=nodes(ii, iel), &
                                   nodeCoord=coord, tsize=ncoord)
            xij(:, ii) = coord(1:2)
            DO jj = 1, 2
              IF (nodes(ii, iel) .EQ. sharedNodes(jj)) ends(jj) = ii
            END DO
          END DO
          CALL geo%opt%GetRefElemCoord(ans=refCoord, &
                                       nrow=nrow, ncol=ncol)
          direction = xij(:, ends(2)) - xij(:, ends(1))
          points(3, :) = 1.0_DFP
          DO ip = 1, SIZE(t)
            points(1:2, ip) = (1.0_DFP - t(ip)) * &
                              refCoord(1:2, ends(1)) + &
                              t(ip) * refCoord(1:2, ends(2))
          END DO
          CALL Initiate(quad, points=points)
          CALL geo%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
          CALL fe%GetLocalElemShapeData(elemsd=shapes, quad=quad)
          CALL fe%GetGlobalElemShapeData(elemsd=shapes, &
                                         xij=xij(:, 1:nv(iel)), &
                                         geoelemsd=geoShapes)
          CALL obj%GetConnectivity_(globalElement=elements(iel), &
                                    opt="All", ans=con, tsize=ncon)
          CALL OK(ncon .EQ. shapes%nns, "Basis rows match connectivity")
          physical(:, :, iel) = shapes%coord(1:2, 1:SIZE(t))
          DO ii = 1, ncon
            basis(con(ii), :, iel) = shapes%N(ii, 1:SIZE(t))
            DO ip = 1, SIZE(t)
              tangent(con(ii), ip, iel) = &
                DOT_PRODUCT(shapes%dNdXt(ii, 1:2, ip), direction)
            END DO
          END DO
        END DO
        CALL OK(orientation(1) * orientation(2) .EQ. -1, &
                "Shared local edges have opposite orientation")
        CALL OK(MAXVAL(ABS(physical(:, :, 1) - physical(:, :, 2))) &
                .LT. tol, "Matching physical sample points")
        CALL OK(MAXVAL(ABS(basis(1:ndof, :, 1) - &
                           basis(1:ndof, :, 2))) .LT. tol, &
                           "Every global basis trace agrees")
        CALL OK(MAXVAL(ABS(tangent(1:ndof, :, 1) - &
                           tangent(1:ndof, :, 2))) .LT. tol, &
                           "Tangential derivatives agree")
      END DO
    END DO
  END DO
  fe => NULL()
  geo => NULL()
  CALL obj%DEALLOCATE()
  CALL geometry%DEALLOCATE()
  mesh => NULL()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
  OPEN (NEWUNIT=unitno, FILE="H1Orientation_test_1.h5", &
        STATUS="old", ACTION="read")
  CLOSE (unitno, STATUS="delete")
END DO
END PROGRAM main
