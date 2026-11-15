!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check variable-order hierarchical H1 conformity
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
INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
REAL(DFP), PARAMETER :: t(7) = &
                        [0.0_DFP, 0.13_DFP, 0.27_DFP, 0.51_DFP, 0.73_DFP, &
                         0.89_DFP, 1.0_DFP]
REAL(DFP), PARAMETER :: tol = 1.0E-9_DFP
REAL(DFP) :: points(3, 7), xij(2, 4), coord(3), direction(2)
REAL(DFP) :: refCoord(3, 8)
REAL(DFP) :: basis(100, 7, 2), tangent(100, 7, 2), physical(2, 7, 2)
INTEGER(I4B) :: shape, inputMode, p, PASS, visit, iel, ii, jj, ip, ndof
INTEGER(I4B), PARAMETER :: pairs(2, 9) = RESHAPE( &
                           [1, 4, 2, 4, 4, 2, 4, 1, 3, 4, 4, 3, 2, 3, 3, 2, &
                           1, 1], [2, 9])
INTEGER(I4B) :: orderCase, cellOrders(2), globalOrders(205), minOrder
INTEGER(I4B) :: faceOrders(4), ni(2), localSize, expectedTotal, nsharedDOF
INTEGER(I4B) :: configuredFaces(3, 4), ncFace, nrFace
INTEGER(I4B) :: configuredCell(3), ntCell, usage(100), allCon(64, 2)
INTEGER(I4B) :: nall(2), facetCon(8, 2), nfacet(2)
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
  CALL meshfile%Initiate(filename="H1VariableOrder_test_1.h5", mode="NEW")
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
  CALL geometry%Initiate(order=1_I4B, dom=dom, baseContinuity="H1", &
                         baseInterpolation="Hierarchical")
  DO inputMode = 1, 2
    DO orderCase = 1, SIZE(pairs, 2)
      cellOrders = pairs(:, orderCase)
      minOrder = MINVAL(cellOrders)
      IF (shape .EQ. 1) THEN
        ni = (cellOrders - 1) * (cellOrders - 2) / 2
      ELSE
        ni = (cellOrders - 1)**2
      END IF
      expectedTotal = mesh%GetTotalVertexNodes() + &
                      (nv(1) - 1) * SUM(cellOrders - 1) + minOrder - &
                      1 + SUM(ni)
      IF (inputMode .EQ. 1) THEN
        CALL obj%Initiate(order=cellOrders, islocal=.TRUE., dom=dom, &
                          baseContinuity="H1", &
                          baseInterpolation="Hierarchical")
      ELSE
        globalOrders = 0
        globalOrders(elements) = cellOrders
        CALL obj%Initiate(order=globalOrders, islocal=.FALSE., dom=dom, &
                          baseContinuity="H1", &
                          baseInterpolation="Hierarchical")
      END IF
      ndof = obj%GetTotalDOF()
      CALL OK(ndof .EQ. expectedTotal, "Variable-order global DOF count")
      usage = 0
      DO iel = 1, 2
        CALL obj%GetConnectivity_(globalElement=elements(iel), opt="All", &
                                  ans=allCon(:, iel), tsize=nall(iel))
        CALL obj%GetFacetConnectivity_(globalElement=elements(iel), &
                                       localFaceNumber=localFace(iel), &
                                       ans=facetCon(:, iel), &
                                       tsize=nfacet(iel))
        CALL OK(nfacet(iel) .EQ. minOrder + 1, "Shared facet DOF count")
        DO ii = 1, nall(iel)
          usage(allCon(ii, iel)) = usage(allCon(ii, iel)) + 1
          CALL OK(COUNT(allCon(1:nall(iel), iel) .EQ. &
                        allCon(ii, iel)) .EQ. 1, "Unique local DOF IDs")
        END DO
      END DO
      nsharedDOF = 0
      DO ii = 1, nfacet(1)
        IF (ANY(facetCon(1:nfacet(2), 2) .EQ. facetCon(ii, 1))) THEN
          nsharedDOF = nsharedDOF + 1
        END IF
      END DO
      CALL OK(nsharedDOF .EQ. minOrder + 1, "Shared facet DOF IDs agree")
      CALL OK(COUNT(usage(1:ndof) .EQ. 2) .EQ. minOrder + 1, &
              "Only shared trace DOFs are shared")
      CALL OK(ALL(usage(1:ndof) .GE. 1), "No holes in numbering")
      DO PASS = 1, 2
        basis = 0.0_DFP
        tangent = 0.0_DFP
        DO visit = 1, 2
          iel = visit
          IF (PASS .EQ. 2) iel = 3 - visit
          CALL obj%SetFE(globalElement=elements(iel))
          fe => obj%GetFEPointer(globalElement=elements(iel))
          p = cellOrders(iel)
          faceOrders(1:nf(iel)) = p
          faceOrders(localFace(iel)) = minOrder
          localSize = nv(iel) + SUM(faceOrders(1:nf(iel)) - 1) + ni(iel)
          CALL OK(obj%GetTotalDOF(globalElement=elements(iel)) &
                  .EQ. localSize, "Variable-order local DOF count")
          CALL fe%opt%GetFaceOrder(ans=configuredFaces, &
                                   nrow=nrFace, ncol=ncFace)
          CALL OK(ncFace .EQ. nf(iel), "Configured facet count")
          CALL OK(ALL(configuredFaces(1, 1:nf(iel)) .EQ. &
                      faceOrders(1:nf(iel))), &
                      "FE preserves minimum facet orders")
          CALL fe%opt%GetCellOrder(ans=configuredCell, tsize=ntCell)
          CALL OK(configuredCell(1) .EQ. p, "FE preserves cell order")
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
  OPEN (NEWUNIT=unitno, FILE="H1VariableOrder_test_1.h5", &
        STATUS="old", ACTION="read")
  CLOSE (unitno, STATUS="delete")
END DO
END PROGRAM main
