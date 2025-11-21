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

SUBMODULE(MeshField_Class) QuadratureMethods
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE AbstractFE_Class, ONLY: AbstractFE_
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: FEVariable_, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    QuadraturePoint_, &
                    ElemShapeData_
USE FEVariable_Method, ONLY: NodalVariable, &
                             QuadratureVariable, &
                             FEVariable_Set => Set, &
                             FEVariable_Deallocate => DEALLOCATE
USE ElemshapeData_Method, ONLY: GetInterpolation_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InitiateQuadraturePoints()"
#endif
INTEGER(I4B) :: maxCon, iel, tElements, ii
CLASS(AbstractFE_), POINTER :: feptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()
maxCon = 0

maxCon = fedof%GetMaxTotalQuadraturePoints()

CALL VectorMeshFieldInitiate( &
  obj=obj, name="xij", fieldType=TypeFieldOpt%normal, &
  varType=TypeFieldOpt%space, engine=engine, &
  defineOn=TypeFieldOpt%quadrature, &
  spaceCompo=3_I4B, nns=maxCon, mesh=mesh)

CALL SetQuadraturePoints(obj=obj, fedof=fedof, mesh=mesh, geofedof=geofedof)

NULLIFY (feptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE InitiateQuadraturePoints

!----------------------------------------------------------------------------
!                                                      SetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE SetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetQuadraturePoints()"
#endif
INTEGER(I4B) :: maxCon, iel, tElements, ii, maxNNE, elemCoord_i, &
                elemCoord_j, xij_i, xij_j
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
TYPE(FEVariable_) :: fevar
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()
maxNNE = mesh%GetMaxNNE()

maxCon = 0

maxCon = fedof%GetMaxTotalQuadraturePoints()

CALL Reallocate(xij, 3, maxCon)
CALL Reallocate(elemCoord, 3, maxNNE)

fevar = QuadratureVariable( &
        val=xij, rank=TypeFEVariableVector, varType=TypeFEVariableSpace)

elemCoord = 0.0_DFP

DO iel = 1, tElements
  CALL fedof%SetFE(globalElement=iel, islocal=.TRUE.)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)
  CALL geofedof%SetFE(globalElement=iel, islocal=.TRUE.)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

  CALL mesh%GetNodeCoord( &
    globalElement=iel, nodeCoord=elemCoord, nrow=elemCoord_i, &
    ncol=elemCoord_j, islocal=.TRUE.)

  CALL feptr%GetQuadraturePoints(quad=quad)
  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  CALL GetInterpolation_(obj=geoelemsd, ans=xij, &
                         val=elemCoord, nrow=xij_i, ncol=xij_j)

  CALL FEVariable_Set( &
    obj=fevar, val=xij(1:3, 1:xij_j), rank=TypeFEVariableVector, &
    vartype=TypeFEVariableSpace, scale=1.0_DFP, addContribution=.FALSE.)

  CALL obj%Insert(globalElement=iel, islocal=.TRUE., fevar=fevar)
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(elemCoord)) DEALLOCATE (elemCoord)
CALL FEVariable_Deallocate(fevar)
NULLIFY (feptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetQuadraturePoints

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE QuadratureMethods
