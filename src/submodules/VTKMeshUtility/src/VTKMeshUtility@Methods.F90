! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Implementation of VTKMeshUtility

SUBMODULE(VTKMeshUtility) Methods
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeInterpolationOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: LGT
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: GetVTKElementType_
USE ReferenceElement_Method, ONLY: GetVTKElementTypeFromTopology_
USE ScalarField_Class, ONLY: ScalarField_
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "VTKMeshUtility@Methods.F90"

INTEGER(I4B), PARAMETER :: MAX_VTK_NODES = 1331
!! maximum number of nodes in a VTK cell (Lagrange hexahedron p=10)

CONTAINS

!----------------------------------------------------------------------------
!                                                          VTKGetLinearGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKGetLinearGrid
CHARACTER(*), PARAMETER :: myName = "VTKGetLinearGrid()"
INTEGER(I4B), ALLOCATABLE :: cellcon(:), vtkindx(:)
INTEGER(I4B) :: ii, jj, nrow, ncol, maxnne, nne, tsize, icell, elemtype, &
                nPoints, nCells
INTEGER(INT8) :: vtktype

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nPoints = mesh%GetTotalNodes()
nCells = mesh%GetTotalElements()

! points are set to zero first, the missing components (e.g., z in 2D)
! remain zero. nrow is not used, because GetNodeCoord2 does not return
! a reliable nrow when it is compiled with OpenMP.
CALL Reallocate(points, 3, nPoints)
points = 0.0_DFP
CALL mesh%GetNodeCoord(nodeCoord=points, nrow=nrow, ncol=ncol)

maxnne = mesh%GetMaxNNE()
CALL Reallocate(cellcon, maxnne)
CALL Reallocate(vtkindx, MAX(maxnne, MAX_VTK_NODES))
CALL Reallocate(connectivity, maxnne * nCells)
CALL Reallocate(offsets, nCells + 1)
IF (ALLOCATED(types)) DEALLOCATE (types)
ALLOCATE (types(nCells))

offsets(1) = 0
tsize = 0
icell = 0

cellLoop: DO ii = 1, nCells
  IF (.NOT. mesh%isElementActive(globalElement=ii, islocal=math%yes)) &
    CYCLE cellLoop

  elemtype = mesh%GetElemType(globalElement=ii, islocal=math%yes)
  CALL GetVTKElementType_(elemType=elemtype, vtk_type=vtktype, &
                          nptrs=vtkindx, tsize=nne)

  IF (vtktype .LT. 0_INT8) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[NOT IMPLEMENTED] :: No VTK cell type for '// &
                      'element type '//ToString(elemtype))
    RETURN
  END IF

  CALL mesh%GetConnectivity_(globalElement=ii, islocal=math%yes, &
                             ans=cellcon, tsize=jj)
  CALL mesh%GetLocalNodeNumber_(globalNode=cellcon(1:jj), &
                                ans=cellcon(1:jj), islocal=math%no)

  DO jj = 1, nne
    connectivity(tsize + jj) = cellcon(vtkindx(jj)) - 1
  END DO

  tsize = tsize + nne
  icell = icell + 1
  offsets(icell + 1) = tsize
  types(icell) = vtktype
END DO cellLoop

connectivity = connectivity(1:tsize)
offsets = offsets(1:icell + 1)
types = types(1:icell)

DEALLOCATE (cellcon, vtkindx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE VTKGetLinearGrid

!----------------------------------------------------------------------------
!                                                        VTKGetLagrangeGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKGetLagrangeGrid
CALL LagrangeWorker(fedof=fedof, geofedof=geofedof, points=points, &
                    connectivity=connectivity, offsets=offsets, &
                    types=types)
END PROCEDURE VTKGetLagrangeGrid

!----------------------------------------------------------------------------
!                                                        VTKGetVertexValues
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKGetVertexValues
CHARACTER(*), PARAMETER :: myName = "VTKGetVertexValues()"
INTEGER(I4B) :: tdof, tsize, nrow, ncol
REAL(DFP), ALLOCATABLE :: temp1(:), temp2(:, :)

tdof = field%fedof%GetTotalDOF()

IF (tdof .LT. nPoints) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: Total DOF of field '// &
                    ToString(tdof)//' is less than number of points '// &
                    ToString(nPoints))
  RETURN
END IF

SELECT TYPE (field)
TYPE IS (ScalarField_)
  CALL Reallocate(temp1, tdof)
  CALL field%Get(VALUE=temp1, tsize=tsize)
  CALL Reallocate(values, 1, nPoints)
  values(1, :) = temp1(1:nPoints)

TYPE IS (VectorField_)
  CALL Reallocate(temp2, 3, tdof)
  CALL field%Get(VALUE=temp2, nrow=nrow, ncol=ncol, &
                 storageFMT=TypeFieldOpt%storageFormatNodes, &
                 force3D=math%yes)
  CALL Reallocate(values, 3, nPoints)
  values(1:3, :) = temp2(1:3, 1:nPoints)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NOT IMPLEMENTED] :: Only ScalarField_ and '// &
                    'VectorField_ are supported.')
  RETURN
END SELECT
END PROCEDURE VTKGetVertexValues

!----------------------------------------------------------------------------
!                                                      VTKGetLagrangeValues
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKGetLagrangeValues
CHARACTER(*), PARAMETER :: myName = "VTKGetLagrangeValues()"

IF (.NOT. ASSOCIATED(field%fedof) .OR. &
    .NOT. ASSOCIATED(field%geofedof)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: fedof and geofedof of field should '// &
                    'be associated.')
  RETURN
END IF

CALL LagrangeWorker(fedof=field%fedof, geofedof=field%geofedof, &
                    field=field, values=values)
END PROCEDURE VTKGetLagrangeValues

!----------------------------------------------------------------------------
!                                                          VTKGetFieldCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKGetFieldCompo
SELECT TYPE (field)
TYPE IS (ScalarField_)
  ans = 1
TYPE IS (VectorField_)
  ans = 3
CLASS DEFAULT
  ans = 0
END SELECT
END PROCEDURE VTKGetFieldCompo

!----------------------------------------------------------------------------
!                                                            LagrangeWorker
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Loop over cells for Lagrange grid and Lagrange values
!
! If points is present, then the grid (points, connectivity, offsets,
! types) is computed. If field is present, then values are computed.
! Both use the same cells and the same points in the same order.

SUBROUTINE LagrangeWorker(fedof, geofedof, points, connectivity, &
                          offsets, types, field, values)
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  CLASS(FEDOF_), INTENT(INOUT) :: geofedof
  REAL(DFP), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: points(:, :)
  INTEGER(I4B), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: connectivity(:)
  INTEGER(I4B), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: offsets(:)
  INTEGER(INT8), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: types(:)
  CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: field
  REAL(DFP), ALLOCATABLE, OPTIONAL, INTENT(INOUT) :: values(:, :)

  CHARACTER(*), PARAMETER :: myName = "LagrangeWorker()"
  CLASS(AbstractMesh_), POINTER :: mesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  TYPE(QuadraturePoint_) :: quad
  TYPE(ElemShapeData_) :: elemsd, geoelemsd
  REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), &
                            elemCoord(:, :), sol(:), sol2(:, :)
  INTEGER(I4B), ALLOCATABLE :: vtkindx(:), solCon(:)
  INTEGER(I4B) :: tElements, iel, ii, nn, nPoints, nCells, nIds, &
                  icell, ip0, ic0, maxNNE, maxCon, cellOrder(3), &
                  ipType(3), topo, nrow, ncol, xrow, xcol, crow, ccol, &
                  tsol, ncomp
  INTEGER(INT8) :: vtktype
  LOGICAL(LGT) :: isGrid, isValue

  isGrid = PRESENT(points)
  isValue = PRESENT(field)

  mesh => fedof%GetMeshPointer()
  tElements = mesh%GetTotalElements()
  maxNNE = mesh%GetMaxNNE()
  maxCon = fedof%GetMaxTotalConnectivity()
  ipType = TypeInterpolationOpt%Equidistance

  CALL Reallocate(vtkindx, MAX_VTK_NODES)

  ! First pass: count points and cells
  nPoints = 0
  nCells = 0
  DO iel = 1, tElements
    IF (.NOT. mesh%isElementActive(globalElement=iel, islocal=math%yes)) &
      CYCLE
    CALL GetCellType(iel, vtktype, nn)
    IF (vtktype .LT. 0_INT8) RETURN
    nPoints = nPoints + nn
    nCells = nCells + 1
  END DO
  nIds = nPoints

  ncomp = 0
  IF (isValue) THEN
    ncomp = VTKGetFieldCompo(field)
    IF (ncomp .EQ. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[NOT IMPLEMENTED] :: Only ScalarField_ and '// &
                        'VectorField_ are supported.')
      RETURN
    END IF
    CALL Reallocate(values, ncomp, nPoints)
    CALL Reallocate(sol, maxCon)
    CALL Reallocate(sol2, 3, maxCon)
    CALL Reallocate(solCon, maxCon)
  END IF

  IF (isGrid) THEN
    CALL Reallocate(points, 3, nPoints)
    CALL Reallocate(connectivity, nIds)
    CALL Reallocate(offsets, nCells + 1)
    IF (ALLOCATED(types)) DEALLOCATE (types)
    ALLOCATE (types(nCells))
    offsets(1) = 0
  END IF

  CALL Reallocate(refElemCoord, 3, maxNNE)
  CALL Reallocate(elemCoord, 3, maxNNE)
  CALL Reallocate(xij, 4, MAX_VTK_NODES)

  ! Second pass: points, cells, and values
  icell = 0
  ip0 = 0
  ic0 = 0

  DO iel = 1, tElements
    IF (.NOT. mesh%isElementActive(globalElement=iel, islocal=math%yes)) &
      CYCLE

    CALL GetCellType(iel, vtktype, nn)

    CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
    CALL geofedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=nrow, ncol=ncol)

    xij = 0.0_DFP
    CALL feptr%GetInterpolationPoints( &
      xij=refElemCoord(1:nrow, 1:ncol), ans=xij, nrow=xrow, ncol=xcol, &
      order=cellOrder, ipType=ipType)

    IF (xcol .NE. nn) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[INTERNAL ERROR] :: Number of interpolation '// &
                        'points ('//ToString(xcol)//') does not match '// &
                        'number of nodes of VTK cell ('//ToString(nn)// &
                        ') in element '//ToString(iel))
      RETURN
    END IF

    CALL QuadraturePoint_Initiate(obj=quad, points=xij(1:4, 1:nn))
    CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

    CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                           nrow=crow, ncol=ccol, islocal=math%yes)

    IF (isGrid) THEN
      points(:, ip0 + 1:ip0 + nn) = 0.0_DFP
      points(1:crow, ip0 + 1:ip0 + nn) = &
        MATMUL(elemCoord(1:crow, 1:ccol), geoelemsd%N(1:ccol, 1:nn))

      DO ii = 1, nn
        connectivity(ic0 + ii) = ip0 + vtkindx(ii) - 1
      END DO

      icell = icell + 1
      offsets(icell + 1) = ic0 + nn
      types(icell) = vtktype
    END IF

    IF (isValue) THEN
      CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

      ! This step is necessary to fix the orientation of shape functions
      CALL feptr%GetGlobalElemShapeData( &
        elemsd=elemsd, geoelemsd=geoelemsd, &
        xij=elemCoord(1:crow, 1:ccol))

      CALL fedof%GetConnectivity_(ans=solCon, tsize=tsol, opt="A", &
                                  globalElement=iel, islocal=math%yes)

      SELECT TYPE (field)
      TYPE IS (ScalarField_)
        CALL field%Get(VALUE=sol, globalNode=solCon(1:tsol), &
                       islocal=math%yes, tsize=ii)
        values(1, ip0 + 1:ip0 + nn) = &
          MATMUL(sol(1:tsol), elemsd%N(1:tsol, 1:nn))

      TYPE IS (VectorField_)
        CALL field%Get(VALUE=sol2, nrow=nrow, ncol=ncol, &
                       storageFMT=TypeFieldOpt%storageFormatNodes, &
                       globalNode=solCon(1:tsol), islocal=math%yes, &
                       force3D=math%yes)
        values(1:3, ip0 + 1:ip0 + nn) = &
          MATMUL(sol2(1:3, 1:tsol), elemsd%N(1:tsol, 1:nn))
      END SELECT
    END IF

    ip0 = ip0 + nn
    ic0 = ic0 + nn
  END DO

  NULLIFY (mesh, feptr, geofeptr)

CONTAINS

  SUBROUTINE GetCellType(iel, vtktype, nn)
    INTEGER(I4B), INTENT(IN) :: iel
    INTEGER(INT8), INTENT(OUT) :: vtktype
    INTEGER(I4B), INTENT(OUT) :: nn

    INTEGER(I4B) :: tsize

    CALL fedof%GetCellOrder(ans=cellOrder, tsize=tsize, &
                            globalElement=iel, islocal=math%yes)
    cellOrder(2:3) = cellOrder(1)
    topo = mesh%GetElemTopology(globalElement=iel, islocal=math%yes)

    CALL GetVTKElementTypeFromTopology_(topoType=topo, &
                                        order=cellOrder(1), &
                                        vtk_type=vtktype, &
                                        nptrs=vtkindx, tsize=nn)

    IF (vtktype .LT. 0_INT8) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[NOT IMPLEMENTED] :: No VTK cell type for '// &
                        'topology '//ToString(topo)//' and order '// &
                        ToString(cellOrder(1))//' in element '// &
                        ToString(iel))
    END IF
  END SUBROUTINE GetCellType

END SUBROUTINE LagrangeWorker

END SUBMODULE Methods
