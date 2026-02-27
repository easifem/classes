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
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableVector
USE ElemshapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE
USE FEVariable_Method, ONLY: FEVariable_Set => Set
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MeshField_Class@QuadratureMethods"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                   InitiateQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InitiateQuadraturePoints()"
#endif
INTEGER(I4B) :: maxCon, tElements
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
  spaceCompo=math%three_i, nns=maxCon, mesh=mesh)

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
INTEGER(I4B) :: maxCon, iel, tElements, maxNNE, elemCoord_i, &
                elemCoord_j, xij_i, xij_j
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
TYPE(FEVariable_) :: fevar
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()
maxNNE = mesh%GetMaxNNE()

maxCon = 0
maxCon = fedof%GetMaxTotalQuadraturePoints()

CALL Reallocate(xij, math%three_i, maxCon)
CALL Reallocate(elemCoord, math%three_i, maxNNE)

fevar = QuadratureVariable( &
        val=xij, rank=TypeFEVariableVector, varType=TypeFEVariableSpace)

elemCoord = math%zero

DO iel = 1, tElements
  CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  CALL geofedof%SetFE(globalElement=iel, islocal=math%yes)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL mesh%GetNodeCoord( &
    globalElement=iel, nodeCoord=elemCoord, nrow=elemCoord_i, &
    ncol=elemCoord_j, islocal=math%yes)

  CALL feptr%GetQuadraturePoints(quad=quad)
  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  CALL GetInterpolation_(obj=geoelemsd, ans=xij, &
                         val=elemCoord, nrow=xij_i, ncol=xij_j)

  CALL FEVariable_Set( &
    obj=fevar, val=xij(1:math%three_i, 1:xij_j), &
    rank=TypeFEVariableVector, vartype=TypeFEVariableSpace, &
    scale=math%one, addContribution=math%no)

  CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
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
