!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Verify independent DG cell blocks and local basis sizes

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
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FEDOF_) :: obj
TYPE(FEDomain_), TARGET :: dom
TYPE(HDF5File_) :: meshfile
TYPE(MSHFile_) :: SOURCE
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CLASS(AbstractFE_), POINTER :: fe => NULL()
CHARACTER(32), PARAMETER :: fixtures(2) = &
                            [CHARACTER(32) :: "h1_two_triangles.msh.txt", &
                                              "h1_two_quads.msh.txt"]
INTEGER(I4B), PARAMETER :: elements(2) = [101, 205]
INTEGER(I4B), PARAMETER :: pairs(2, 8) = RESHAPE( &
                           [0, 0, 1, 1, 2, 2, 3, 3, 1, 3, 3, 1, 0, 2, 2, &
                            0], [2, 8])
CHARACTER(6), PARAMETER :: options(3) = &
                           [CHARACTER(6) :: "Vertex", "Edge", "Face"]
INTEGER(I4B) :: shape, icase, inputMode, iel, ii, jj, iface, n, ndof, offset
INTEGER(I4B) :: sizes(2), orders(2), globalOrders(205), cell(32), facet(32)
INTEGER(I4B) :: first(32), second(32), nfirst, nsecond, nf, vertex(1)
INTEGER(I4B), ALLOCATABLE :: con(:)
REAL(DFP) :: coords(3, 4), points(3, 32)
INTEGER(I4B) :: nr, nc, npointRows, npointCols
INTEGER :: unitno

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
DO shape = 1, SIZE(fixtures)
  CALL SOURCE%Initiate(filename="./fixtures/"//TRIM(fixtures(shape)), &
                       STATUS="OLD", ACTION="READ")
  CALL SOURCE%OPEN()
  CALL SOURCE%READ()
  CALL meshfile%Initiate(filename="DGNumbering_test_1.h5", mode="NEW")
  CALL meshfile%SetOverwriteStat(.TRUE.)
  CALL meshfile%OPEN()
  CALL SOURCE%Export(hdf5=meshfile, group="")
  CALL SOURCE%DEALLOCATE()
  CALL dom%Initiate(meshfile, "")
  mesh => dom%GetMeshPointer()
  DO inputMode = 1, 2
    DO icase = 1, SIZE(pairs, 2)
      orders = pairs(:, icase)
      IF (shape .EQ. 1) THEN
        sizes = (orders + 1) * (orders + 2) / 2
      ELSE
        sizes = (orders + 1)**2
      END IF
      IF (inputMode .EQ. 1) THEN
        CALL obj%Initiate(order=orders, islocal=.TRUE., dom=dom, &
                          baseContinuity="H1", isBrokenSpace=.TRUE., &
                          baseInterpolation="Lagrange", &
                          ipType=TypeInterpolationOpt%Equidistance)
      ELSE
        globalOrders = 0
        globalOrders(elements) = orders
        CALL obj%Initiate(order=globalOrders, islocal=.FALSE., dom=dom, &
                          baseContinuity="H1", isBrokenSpace=.TRUE., &
                          baseInterpolation="Lagrange", &
                          ipType=TypeInterpolationOpt%Equidistance)
      END IF
      ndof = SUM(sizes)
      CALL OK(obj%GetTotalDOF() .EQ. ndof, "DG global count is cell sum")
      CALL OK(obj%GetSumElemTotalDOF() .EQ. ndof, "DG sum matches total")
      CALL OK(obj%GetTotalVertexDOF() .EQ. 0, "No shared vertex DOFs")
      CALL obj%GetVertexDOF(globalNode=1, ans=vertex, tsize=n)
      CALL OK(n .EQ. 0, "Mesh vertex owns no DG DOF")
      offset = 0
      DO iel = 1, 2
        con = obj%GetConnectivity(opt="All", globalElement=elements(iel))
        CALL OK(SIZE(con) .EQ. sizes(iel), "DG local count")
        CALL OK(ALL(con .EQ. [(offset + ii, ii=1, sizes(iel))]), &
                "Contiguous cell block in local cell order")
        CALL obj%GetConnectivity_(opt="All", globalElement=iel, &
                                  islocal=.TRUE., ans=cell, tsize=n)
        CALL OK(n .EQ. SIZE(con), "Local/global connectivity sizes")
        CALL OK(ALL(cell(1:n) .EQ. con), "Local/global connectivity IDs")
        CALL obj%GetCellDOF(globalCell=elements(iel), ans=cell, tsize=n)
        CALL OK(ALL(cell(1:n) .EQ. con), "Cell owns every local DOF")
        DO jj = 1, SIZE(options)
          CALL obj%GetConnectivity_(opt=TRIM(options(jj)), &
                                    globalElement=elements(iel), &
                                    ans=cell, tsize=n)
          CALL OK(n .EQ. 0, "No globally shared entity block")
          CALL OK(obj%GetTotalDOF(opt=TRIM(options(jj)), &
                                  globalElement=elements(iel)) .EQ. 0, &
                  "Entity count is zero")
        END DO
        DO iface = 1, shape + 2
          CALL obj%GetFacetConnectivity_(globalElement=elements(iel), &
                                         localFaceNumber=iface, &
                                         ans=facet, tsize=nf)
          CALL OK(nf .EQ. SIZE(con), "Facet uses the cell trace block")
          CALL OK(ALL(facet(1:nf) .EQ. con), "Facet trace DOF IDs")
        END DO
        CALL obj%SetFE(globalElement=elements(iel))
        fe => obj%GetFEPointer(globalElement=elements(iel))
        CALL OK(fe%opt%GetTotalDOF() .EQ. sizes(iel), "Local FE size")
        coords = 0.0_DFP
        CALL mesh%GetNodeCoord(globalElement=elements(iel), &
                               nodeCoord=coords, nrow=nr, ncol=nc)
        CALL fe%GetInterpolationPoints(xij=coords(1:nr, 1:nc), &
                                       ans=points, nrow=npointRows, &
                                       ncol=npointCols, &
                                       order=[orders(iel)], ipType=[TypeInterpolationOpt%Equidistance])
        CALL OK(npointCols .EQ. sizes(iel), "One point per local nodal DOF")
        IF (orders(iel) .EQ. 0) CALL OK( &
          ALL(ABS(points(1:npointRows, 1) - &
                  SUM(coords(1:nr, 1:nc), DIM=2) / REAL(nc, DFP)) &
              .LT. 1.0E-12_DFP), "Constant interpolation point is centroid")
        CALL fe%GetQuadraturePoints(quad)
        CALL fe%GetLocalElemShapeData(elemsd=shapes, quad=quad)
        CALL OK(shapes%nns .EQ. SIZE(con), "Basis rows match DG block")
        CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, &
                                     1:shapes%nips), DIM=1) - 1.0_DFP) .LT. &
                    1.0E-10_DFP), &
                "Local partition of unity")
        IF (orders(iel) .EQ. 0) THEN
          CALL OK(ALL(ABS(shapes%N(1, 1:shapes%nips) - 1.0_DFP) &
                      .LT. 1.0E-10_DFP), "Degree-zero basis is constant")
        END IF
        IF (orders(iel) .EQ. 0) CALL OK( &
          ALL(ABS(shapes%dNdXi(1, 1:2, 1:shapes%nips)) &
              .LT. 1.0E-12_DFP), "Constant basis has zero gradients")
        offset = offset + sizes(iel)
      END DO
      CALL obj%GetConnectivity_(opt="All", globalElement=elements(1), &
                                ans=first, tsize=nfirst)
      CALL obj%GetConnectivity_(opt="All", globalElement=elements(2), &
                                ans=second, tsize=nsecond)
      DO ii = 1, nfirst
        CALL OK(.NOT. ANY(second(1:nsecond) .EQ. first(ii)), &
                "Adjacent DG cells share no DOFs")
      END DO
    END DO
  END DO
  fe => NULL()
  CALL obj%DEALLOCATE()
  mesh => NULL()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
  OPEN (NEWUNIT=unitno, FILE="DGNumbering_test_1.h5", &
        STATUS="old", ACTION="read")
  CLOSE (unitno, STATUS="delete")
END DO
END PROGRAM main
