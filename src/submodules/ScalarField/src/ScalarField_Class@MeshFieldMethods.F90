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

SUBMODULE(ScalarField_Class) MeshFieldMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE ElemshapeData_Method, ONLY: ElemshapeDataGetInterpolation => &
                                GetInterpolation
USE ElemshapeData_Method, ONLY: ElemshapeDataDeallocate => &
                                DEALLOCATE
USE FEVariable_Method, ONLY: FEVariableSet => Set
USE FEVariable_Method, ONLY: FEVariableDeallocate => DEALLOCATE
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => &
                                  DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "ScalarField_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                  MeshField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshField
USE QuadraturePoint_Method, ONLY: QuadraturePoint
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

name = ""
engine = ""
mesh => NULL()
feptr => NULL()
geofeptr => NULL()

CALL FEVariableDeallocate(fevar)
CALL FEVariableDeallocate(sol_fevar)
CALL QuadraturePointDeallocate(quad)
CALL ElemShapeDataDeallocate(elemsd)
CALL ElemShapeDataDeallocate(geoelemsd)

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(elemCoord)) DEALLOCATE (elemCoord)
IF (ALLOCATED(sol)) DEALLOCATE (sol)
IF (ALLOCATED(refElemCoord)) DEALLOCATE (refElemCoord)
IF (ALLOCATED(solCon)) DEALLOCATE (solCon)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshField

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MeshFieldMethods
