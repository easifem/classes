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

SUBMODULE(MeshField_Class) InterpolationMethods
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE AbstractFE_Class, ONLY: AbstractFE_
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: FEVariable_, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpace
USE FEVariable_Method, ONLY: NodalVariable, &
                             FEVariable_Set => Set, &
                             FEVariable_Deallocate => DEALLOCATE

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE InitiateInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InitiateInterpolationPoints()"
#endif
INTEGER(I4B) :: maxCon, iel, tElements, ii
CLASS(AbstractFE_), POINTER :: feptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()
maxCon = 0

DO iel = 1, tElements
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)
  ii = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
  maxCon = MAX(maxCon, ii)
END DO

CALL VectorMeshFieldInitiate( &
  obj=obj, name="xij", fieldType=TypeFieldOpt%normal, &
  varType=TypeFieldOpt%space, engine=engine, defineOn=TypeFieldOpt%nodal, &
  spaceCompo=3_I4B, nns=maxCon, mesh=mesh)

CALL SetInterpolationPoints(obj=obj, order=order, ipType=ipType, &
                            fedof=fedof, mesh=mesh)

NULLIFY (feptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE InitiateInterpolationPoints

!----------------------------------------------------------------------------
!                                                      SetInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE SetInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetInterpolationPoints()"
#endif
INTEGER(I4B) :: maxCon, iel, tElements, ii, maxNNE, elemCoord_i, &
                elemCoord_j, xij_i, xij_j
CLASS(AbstractFE_), POINTER :: feptr
TYPE(FEVariable_) :: fevar
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()
maxNNE = mesh%GetMaxNNE()

maxCon = 0
DO iel = 1, tElements
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)
  ii = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
  maxCon = MAX(maxCon, ii)
END DO

CALL Reallocate(xij, 3, maxCon)
CALL Reallocate(elemCoord, 3, maxNNE)
fevar = NodalVariable( &
        val=xij, rank=TypeFEVariableVector, varType=TypeFEVariableSpace)

elemCoord = 0.0_DFP

DO iel = 1, tElements
  CALL fedof%SetFE(globalElement=iel, islocal=.TRUE.)

  feptr => fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

  CALL mesh%GetNodeCoord( &
    globalElement=iel, nodeCoord=elemCoord, nrow=elemCoord_i, &
    ncol=elemCoord_j, islocal=.TRUE.)

  CALL feptr%GetInterpolationPoints( &
    xij=elemCoord, ans=xij, nrow=xij_i, ncol=xij_j, order=order, &
    ipType=ipType)

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
END PROCEDURE SetInterpolationPoints

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE InterpolationMethods
