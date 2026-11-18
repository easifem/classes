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

SUBMODULE(ScalarField_Class) VTKMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ElemShapeData_Method, ONLY: ElemShapeDataDeallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "ScalarField_Class@VTKMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                               ExportToVTK
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ExportToVTK
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ExportToVTK()"
#endif

INTEGER(I4B) :: tsize, tnodes
REAL(DFP), ALLOCATABLE :: VALUE(:)
TYPE(String) :: name
CHARACTER(1), ALLOCATABLE :: dofnames(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalPhysicalVars()
ALLOCATE (dofnames(tsize))
CALL obj%GetPhysicalNames(dofnames)

tsize = obj%fedof%GetTotalDOF()
tnodes = obj%fedof%GetTotalVertexDOF()

ALLOCATE (VALUE(tsize))
CALL obj%Get(VALUE=VALUE, tsize=tsize)

! array name: <fieldname>_<dofname>, as in AbstractNodeField
name = obj%name%chars()//"_"//dofnames(1)

CALL vtk%WriteDataArray(name=name, x=VALUE(1:tnodes), numberOfComponents=1)

name = ''
DEALLOCATE (dofnames)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ExportToVTK

!----------------------------------------------------------------------------
!                                                   GetVtkUnstructuredValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredValue1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredValue1()"
#endif
REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), nodalValue(:)
INTEGER(I4B), ALLOCATABLE :: fecon(:)
INTEGER(I4B) :: maxNNE, maxCon, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

maxNNE = obj%geofedof%GetMaxTotalConnectivity()
maxCon = obj%fedof%GetMaxTotalConnectivity()
tsize = obj%fedof%GetSumElemTotalDOF()

CALL Reallocate(VALUE, tsize)
CALL Reallocate(nodalValue, maxCon)
CALL Reallocate(fecon, maxCon)
CALL Reallocate(xij, 4, maxCon)
CALL Reallocate(refElemCoord, 3, maxNNE)

CALL obj%GetVtkUnstructuredValue(VALUE=VALUE, tsize=tsize, &
                                 refElemCoord=refElemCoord, xij=xij, &
                                 nodalValue=nodalValue, fecon=fecon)

IF (ALLOCATED(nodalValue)) DEALLOCATE (nodalValue)
IF (ALLOCATED(fecon)) DEALLOCATE (fecon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(refElemCoord)) DEALLOCATE (refElemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetVtkUnstructuredValue1

!----------------------------------------------------------------------------
!                                                   GetVtkUnstructuredValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredValue2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredValue2()"
INTEGER(I4B) :: maxNNE, maxCon
#endif
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd
INTEGER(I4B) :: tElements, iel, ii, ip0, cellOrder(3), ipType(3), &
                xij_nrow, xij_ncol, refElemCoord_nrow, refElemCoord_ncol, &
                nodalValue_size, tfecon, topoIndx, lastTopoIndx, lastOrder
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mesh => obj%fedof%GetMeshPointer()
tElements = mesh%GetTotalElements()
ipType = TypeInterpolationOpt%equidistance

! lazy compute and cached
tsize = obj%fedof%GetSumElemTotalDOF()

#ifdef DEBUG_VER
maxNNE = obj%geofedof%GetMaxTotalConnectivity()
maxCon = obj%fedof%GetMaxTotalConnectivity()

CALL AssertError3(tsize, SIZE(VALUE), myName, &
                  "a=tsize, b=size(value)")
CALL AssertError3(maxCon, SIZE(nodalValue), myName, &
                  "a=maxCon, b=size(nodalValue)")
CALL AssertError3(maxCon, SIZE(fecon), myName, &
                  "a=maxCon, b=size(fecon)")
CALL AssertError3(math%four_i, SIZE(xij, 1), myName, &
                  "a=4, b=size(xij,1)")
CALL AssertError3(maxCon, SIZE(xij, 2), myName, &
                  "a=maxCon, b=size(xij,2)")
CALL AssertError3(math%three_i, SIZE(refElemCoord, 1), myName, &
                  "a=3, b=size(refElemCoord,1)")
CALL AssertError3(maxNNE, SIZE(refElemCoord, 2), myName, &
                  "a=maxNNE, b=size(refElemCoord,2)")
#endif

xij = math%zero
refElemCoord = math%zero

ip0 = 0
lastTopoIndx = 0
lastOrder = -1

elemLoop: DO iel = 1, tElements

  isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE elemLoop

  ! order of the VTK cell
  CALL obj%fedof%GetCellOrder(ans=cellOrder, tsize=ii, &
                              globalElement=iel, islocal=math%yes)
  cellOrder(2:3) = cellOrder(1)
  topoIndx = mesh%GetElemTopologyIndx(globalElement=iel, islocal=math%yes)

  ! finite element of fedof, SetFE also sets the orientation of the
  ! element, which is used by FixOrientOfNptrs below
  CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  ! The local shape data at the VTK points depends only on the finite
  ! element (topology) and the cell order, so it is computed again only
  ! when one of them changes
  isok = (topoIndx .NE. lastTopoIndx) .OR. (cellOrder(1) .NE. lastOrder)
  IF (isok) THEN
    lastTopoIndx = topoIndx
    lastOrder = cellOrder(1)

    ! VTK points of the cell in the reference element
    CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_nrow, &
                               ncol=refElemCoord_ncol)

    CALL feptr%GetInterpolationPoints( &
      xij=refElemCoord(1:refElemCoord_nrow, 1:refElemCoord_ncol), &
      ans=xij, nrow=xij_nrow, ncol=xij_ncol, order=cellOrder, &
      ipType=ipType)

    CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:xij_ncol))

    ! local shape functions, the orientation is fixed in the connectivity
    CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)
  END IF

  ! nodal values of the element in the local node order
  CALL obj%fedof%GetConnectivity_(ans=fecon, tsize=tfecon, opt="A", &
                                  globalElement=iel, islocal=math%yes)
  CALL feptr%FixOrientOfNptrs(fecon, tfecon)

  CALL obj%Get(VALUE=nodalValue, globalNode=fecon(1:tfecon), &
               islocal=math%yes, tsize=nodalValue_size)

#ifdef DEBUG_VER
  CALL AssertError3(nodalValue_size, elemsd%nns, myName, &
                    "a=nodalValue_size, b=elemsd%nns")
  CALL AssertError3(xij_ncol, elemsd%nips, myName, &
                    "a=xij_ncol, b=elemsd%nips")
#endif

  VALUE(ip0 + 1:ip0 + xij_ncol) = &
    MATMUL(nodalValue(1:nodalValue_size), &
           elemsd%N(1:nodalValue_size, 1:xij_ncol))

  ip0 = ip0 + xij_ncol
END DO elemLoop

NULLIFY (mesh, feptr)
CALL QuadraturePointDeallocate(quad)
CALL ElemShapeDataDeallocate(elemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetVtkUnstructuredValue2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
