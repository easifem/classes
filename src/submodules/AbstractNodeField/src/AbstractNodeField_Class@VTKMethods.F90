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

SUBMODULE(AbstractNodeField_Class) VTKMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE ElemshapeData_Method, ONLY: ElemshapeDataDeallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: GetVTKElementTypeFromTopology_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "AbstractNodeField_Class@VTKMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                 GetVtkUnstructuredGridData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredGridData1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredGridData1()"
#endif

CLASS(AbstractFE_), POINTER :: feptr, geofeptr
REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), &
                          elemCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: vtkindx(:)
CLASS(FEDOF_), POINTER :: geofedof, fedof
LOGICAL(LGT) :: isok
INTEGER(I4B) :: maxNNE, maxCon, points_nrow, points_ncol, &
                con_tsize, offsets_tsize, types_tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(ivar)) THEN
  !add checks
#ifdef DEBUG_VER
  isok = ALLOCATED(obj%fedofs)
  CALL AssertError1(isok, myName, "obj%fedofs not allocated.")

  CALL AssertError3(ivar, SIZE(obj%fedofs), myName, &
                    "a=ivar, b=size(obj%fedofs)")

  isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
  CALL AssertError1(isok, myName, "obj%fedofs(ivar) not associated.")

  isok = ALLOCATED(obj%geofedofs)
  CALL AssertError1(isok, myName, "obj%geofedofs not allocated.")

  CALL AssertError3(ivar, SIZE(obj%geofedofs), myName, &
                    "a=ivar, b=size(obj%geofedofs)")

  isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
  CALL AssertError1(isok, myName, "obj%geofedofs(ivar) not associated.")
#endif

  fedof => obj%fedofs(ivar)%ptr
  geofedof => obj%geofedofs(ivar)%ptr

ELSE
  fedof => obj%fedof
  geofedof => obj%geofedof
END IF

#ifdef DEBUG_VER
isok = geofedof%IsInitiated()
CALL AssertError1(isok, myName, "geofedof is not initiated.")

isok = fedof%IsInitiated()
CALL AssertError1(isok, myName, "fedof is not initiated.")
#endif

maxNNE = geofedof%GetMaxTotalConnectivity()
maxCon = fedof%GetMaxTotalConnectivity()

con_tsize = obj%fedof%GetSumElemTotalDOF()
points_nrow = 3
points_ncol = con_tsize

types_tsize = obj%fedof%GetTotalActiveCells()
offsets_tsize = types_tsize + 1

CALL Reallocate(vtkindx, maxCon)
CALL Reallocate(connectivity, con_tsize)
CALL Reallocate(points, points_nrow, points_ncol)
CALL Reallocate(offsets, offsets_tsize)
CALL Reallocate(types, types_tsize)
CALL Reallocate(refElemCoord, 3, maxNNE)
CALL Reallocate(elemCoord, 3, maxNNE)
CALL Reallocate(xij, 4, maxCon)

CALL obj%GetVtkUnstructuredGridData2(points=points, &
                                     connectivity=connectivity, &
                                     offsets=offsets, types=types, &
                                     refElemCoord=refElemCoord, xij=xij, &
                                     elemCoord=elemCoord, vtkindx=vtkindx, &
                                     points_nrow=points_nrow, &
                                     points_ncol=points_ncol, &
                                     con_tsize=con_tsize, &
                                     offsets_tsize=offsets_tsize, &
                                     types_tsize=types_tsize, ivar=ivar)

NULLIFY (feptr, fedof, geofeptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVtkUnstructuredGridData1

!----------------------------------------------------------------------------
!                                                 GetVtkUnstructuredGridData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredGridData2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredGridData2()"
#endif

CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
CLASS(FEDOF_), POINTER :: geofedof, fedof
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: geoelemsd
INTEGER(I4B) :: tElements, iel, ii, nPoints, nCells, &
                icell, ip0, ic0, maxNNE, maxCon, cellOrder(3), &
                ipType(3), topo, vtkindx_tsize, refElemCoord_nrow, &
                refElemCoord_ncol, xij_nrow, xij_ncol, elemCoord_nrow, &
                elemCoord_ncol
INTEGER(INT8) :: vtktype
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(ivar)) THEN
  !add checks
#ifdef DEBUG_VER
  isok = ALLOCATED(obj%fedofs)
  CALL AssertError1(isok, myName, "obj%fedofs not allocated.")

  CALL AssertError3(ivar, SIZE(obj%fedofs), myName, &
                    "a=ivar, b=size(obj%fedofs)")

  isok = ASSOCIATED(obj%fedofs(ivar)%ptr)
  CALL AssertError1(isok, myName, "obj%fedofs(ivar) not associated.")

  isok = ALLOCATED(obj%geofedofs)
  CALL AssertError1(isok, myName, "obj%geofedofs not allocated.")

  CALL AssertError3(ivar, SIZE(obj%geofedofs), myName, &
                    "a=ivar, b=size(obj%geofedofs)")

  isok = ASSOCIATED(obj%geofedofs(ivar)%ptr)
  CALL AssertError1(isok, myName, "obj%geofedofs(ivar) not associated.")
#endif

  fedof => obj%fedofs(ivar)%ptr
  geofedof => obj%geofedofs(ivar)%ptr

ELSE
  fedof => obj%fedof
  geofedof => obj%geofedof
END IF

#ifdef DEBUG_VER
isok = geofedof%IsInitiated()
CALL AssertError1(isok, myName, "geofedof is not initiated.")

isok = fedof%IsInitiated()
CALL AssertError1(isok, myName, "fedof is not initiated.")
#endif

mesh => fedof%GetMeshPointer()

tElements = obj%fedof%GetTotalCells()
maxNNE = geofedof%GetMaxTotalConnectivity()
maxCon = fedof%GetMaxTotalConnectivity()
ipType = TypeInterpolationOpt%Equidistance
nPoints = obj%fedof%GetSumElemTotalDOF()
nCells = obj%fedof%GetTotalActiveCells()

#ifdef DEBUG_VER
CALL AssertError3(maxCon, SIZE(vtkindx), myName, &
                  "a=maxCon, b=size(vtkindx)")

CALL AssertError3(nPoints, SIZE(connectivity), myName, &
                  "a=nPoints, b=size(connectivity)")

CALL AssertError3(nCells + 1, SIZE(offsets), myName, &
                  "a=nCells+1, b=size(offsets)")

CALL AssertError3(nCells, SIZE(types), myName, &
                  "a=nCells, b=size(types)")

CALL AssertError3(math%three_i, SIZE(points, 1), myName, &
                  "a=3, b=size(points,1)")
CALL AssertError3(nPoints, SIZE(points, 2), myName, &
                  "a=nPoints, b=size(points,2)")

CALL AssertError3(math%three_i, SIZE(elemCoord, 1), myName, &
                  "a=3, b=size(elemCoord,1)")
CALL AssertError3(maxNNE, SIZE(elemCoord, 2), myName, &
                  "a=maxNNE, b=size(elemCoord,2)")

CALL AssertError3(math%three_i, SIZE(refElemCoord, 1), myName, &
                  "a=3, b=size(refElemCoord,1)")
CALL AssertError3(maxNNE, SIZE(refElemCoord, 2), myName, &
                  "a=maxNNE, b=size(refElemCoord,2)")

CALL AssertError3(math%four_i, SIZE(xij, 1), myName, &
                  "a=3, b=size(xij,1)")
CALL AssertError3(maxNNE, SIZE(xij, 2), myName, &
                  "a=maxNNE, b=size(xij,2)")
#endif

! Second pass: points, cells, and values
icell = 0
ip0 = 0
ic0 = 0
offsets(1) = 0
xij(1:4, 1:maxCon) = math%zero

points_nrow = 3
points_ncol = nPoints
con_tsize = nPoints
offsets_tsize = nCells + 1
types_tsize = nCells

loop1: DO iel = 1, tElements

  isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE loop1

  ! Get cellOrder
  CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, &
                          globalElement=iel, islocal=math%yes)
  cellOrder(2:3) = cellOrder(1)

  ! Get vtk element type from topology
  topo = mesh%GetElemTopology(globalElement=iel, islocal=math%yes)
  CALL GetVTKElementTypeFromTopology_(topoType=topo, &
                                      order=cellOrder(1), &
                                      vtk_type=vtktype, &
                                      nptrs=vtkindx, tsize=vtkindx_tsize)

  ! get feptr
  CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  ! get geofeptr
  CALL geofedof%SetFE(globalElement=iel, islocal=math%yes)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  ! get reference element coordinates
  CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_nrow, &
                             ncol=refElemCoord_ncol)

  ! xij = math%zero
  CALL feptr%GetInterpolationPoints(xij=refElemCoord(1:refElemCoord_nrow, &
                                                     1:refElemCoord_ncol), &
                                    ans=xij, nrow=xij_nrow, ncol=xij_ncol, &
                                    order=cellOrder, ipType=ipType)

  ! TODO: make the following call allocation free
  CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:xij_ncol))

  ! get local element shape data of geometry
  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  ! get nodal coordinates of element
  CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                         nrow=elemCoord_nrow, ncol=elemCoord_ncol, &
                         islocal=math%yes)

#ifdef DEBUG_VER
  CALL AssertError3(elemCoord_ncol, geoelemsd%nns, myName, &
                    "a=elemCoord_ncol, b=geoelemsd%nns.")

  CALL AssertError3(vtkindx_tsize, geoelemsd%nips, myName, &
                    "a=vtkindx_tsize, b=geoelemsd%nips.")
#endif

  points(1:elemCoord_nrow, ip0 + 1:ip0 + vtkindx_tsize) = &
    MATMUL(elemCoord(1:elemCoord_nrow, 1:elemCoord_ncol), &
           geoelemsd%N(1:elemCoord_ncol, 1:vtkindx_tsize))

  DO ii = 1, vtkindx_tsize
    connectivity(ic0 + ii) = ip0 + vtkindx(ii) - 1
  END DO

  icell = icell + 1
  offsets(icell + 1) = ic0 + vtkindx_tsize
  types(icell) = vtktype

  ip0 = ip0 + vtkindx_tsize
  ic0 = ic0 + vtkindx_tsize
END DO loop1

CALL QuadraturePointDeallocate(quad)
CALL ElemshapeDataDeallocate(geoelemsd)
NULLIFY (mesh, fedof, geofedof, feptr, geofeptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetVtkUnstructuredGridData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
