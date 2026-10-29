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

SUBMODULE(ScalarField_Class) GetMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeInterpolationOpt
USE Display_Method, ONLY: Display
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE ElemshapeData_Method, ONLY: ElemshapeDataGetInterpolation => &
                                GetInterpolation
USE ElemshapeData_Method, ONLY: ElemshapeDataDeallocate => &
                                DEALLOCATE
USE FEVariable_Method, ONLY: FEVariableSet => Set
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => &
                                  DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: GetValue_
USE RealVector_Method, ONLY: Get
USE RealVector_Method, ONLY: GetValue
USE ReferenceElement_Method, ONLY: RefElementGetTotalNodes => GetTotalNodes

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "ScalarField_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./include/localNodeError.F90"
CALL obj%GetSingle(VALUE=VALUE, indx=globalNode)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif
INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

s = GetNodeLoc(obj=obj%dof, idof=1)
CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./include/localNodeError.F90"
CALL obj%GetMultiple(VALUE=VALUE, indx=globalNode, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./include/localNodeError.F90"

#ifdef DEBUG_VER
CALL AssertError1(VALUE%isInit, myName, &
                  "value is not initiated")
#endif

CALL obj%Get(VALUE=VALUE%val, globalNode=globalNode, islocal=islocal, &
             tsize=tsize)
VALUE%tshape = 1
VALUE%s(1) = tsize
VALUE%defineOn = TypeFieldOpt%nodal
VALUE%varType = TypeFieldOpt%space
VALUE%RANK = TypeFieldOpt%scalar
VALUE%len = tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                              GetFeVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%local_n
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                  MeshField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshField()"
#endif

LOGICAL(LGT) :: isMeshFieldInit
CHARACTER(:), ALLOCATABLE :: name, engine
INTEGER(I4B) :: spaceCompo, maxCon, tElements, iel, ii, maxNNE, &
                xij_i, xij_j, elemCoord_i, elemCoord_j, tsol, tSolCon, &
                refElemCoord_i, refElemCoord_j, maxFedofCon
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
TYPE(FEVariable_) :: fevar, sol_fevar
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :), sol(:), &
                          refElemCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: solCon(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isMeshFieldInit = meshField%IsInitiated()
name = obj%GetName()
engine = obj%GetEngineName()
spaceCompo = 1

mesh => obj%fedof%GetMeshPointer()
maxNNE = mesh%GetMaxNNE()
! maxNNE is the maximum number of nodes in an element of a mesh
! It will be used for allocating geocon, refElemCoord, and elemCoord
tElements = mesh%GetTotalElements()
! tElements is the total number of elements in the mesh
maxFedofCon = obj%fedof%GetMaxTotalConnectivity()
! maxFedofCon is the maximum number of connectivity in the fedof
! It will be used to allocate sol and solCon

maxCon = 0
DO iel = 1, tElements
  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  ii = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
  maxCon = MAX(maxCon, ii)
END DO
! maxCon is the maximum number of interpolation points in
! in the fedof, it will be used to allocate xij, quad, fevar

IF (.NOT. isMeshFieldInit) THEN
  CALL ScalarMeshFieldInitiate(obj=meshField, name=name, &
                               fieldType=TypeFieldOpt%normal, &
                               varType=TypeFieldOpt%space, engine=engine, &
                               defineOn=TypeFieldOpt%nodal, nns=maxCon, &
                               mesh=mesh)
END IF

CALL Reallocate(elemCoord, 3, maxNNE)
CALL Reallocate(refElemCoord, 3, maxNNE)
CALL Reallocate(sol, maxFedofCon)
CALL Reallocate(solCon, maxFedofCon)
CALL Reallocate(xij, 4, maxCon)
CALL QuadraturePointInitiate(obj=quad, txi=3, tpoints=maxCon)

fevar = QuadratureVariable( &
        tsize=maxCon, RANK=TypeFEVariableScalar, &
        varType=TypeFEVariableSpace)

sol_fevar = NodalVariable(tsize=maxFedofCon, RANK=TypeFEVariableScalar, &
                          varType=TypeFEVariableSpace)

DO iel = 1, tElements

  CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)
  CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)

  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_i, &
                             ncol=refElemCoord_j)

  CALL feptr%GetInterpolationPoints(xij=refElemCoord, ans=xij, nrow=xij_i, &
                                    ncol=xij_j, order=order, ipType=ipType)

  CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:xij_j))

  CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                         nrow=elemCoord_i, ncol=elemCoord_j, &
                         islocal=math%yes)

  ! This step is necessary to fix the orientation in Lagrange elements
  CALL feptr%GetGlobalElemShapeData(elemsd=elemsd, geoelemsd=geoelemsd, &
                                    xij=elemCoord(1:elemCoord_i, &
                                                  1:elemCoord_j))

  CALL obj%fedof%GetConnectivity_(ans=solCon, tsize=tSolCon, &
                                  globalElement=iel, islocal=math%yes, &
                                  opt='A')

  CALL obj%Get(VALUE=sol, tsize=tsol, globalNode=solCon(1:tSolCon), &
               islocal=math%yes)

  CALL FEVariableSet(obj=sol_fevar, val=sol(1:tsol), &
                     RANK=TypeFEVariableScalar, &
                     varType=TypeFEVariableSpace, scale=math%one, &
                     addContribution=math%no)

  CALL ElemshapeDataGetInterpolation(obj=elemsd, ans=fevar, val=sol_fevar)

  CALL meshField%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshField

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

END SUBMODULE GetMethods
