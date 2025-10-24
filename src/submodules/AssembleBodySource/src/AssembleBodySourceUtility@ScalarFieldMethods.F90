! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(AssembleBodySourceUtility) ScalarFieldMethods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE ForceVector_Method, ONLY: ForceVector_, ForceVector
USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, &
                             FEVariable_Set => Set
USE AbstractFE_Class, ONLY: AbstractFE_

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    FEVariable_, &
                    TypeFEVariableScalar, &
                    TypeFEVariableSpace

#ifdef DEBUG_VER
USE FEVariable_Method, ONLY: Fevar_Display => Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              ScalarFieldAssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleBodySource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleBodySource1()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tgeoCellCon, tforceVec, xij_i, xij_j
TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), forceVec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()

maxNNEGeo = geofedof%GetMaxTotalConnectivity()
maxNNE = fedof%GetMaxTotalConnectivity()

CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellcon, maxNNE)
CALL Reallocate(forceVec, maxNNE)

forceVar = QuadratureVariable(val=forceVec, rank=TypeFEVariableScalar, &
                              vartype=TypeFEVariableSpace)

DO iel = 1, tElements

  CALL fedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)
  CALL geofedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)

  feptr => fedof%GetFEPointer(globalElement=iel, islocal=defaultOpt%yes)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=defaultOpt%yes)

  CALL feptr%GetQuadraturePoints(quad=quad)
  CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)
  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  CALL geofedof%GetConnectivity_( &
    globalElement=iel, islocal=defaultOpt%yes, ans=geoCellCon, &
    tsize=tgeoCellCon, opt="A")

  CALL mesh%GetNodeCoord( &
    nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=defaultOpt%yes, &
    globalElement=iel)

  CALL feptr%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                    geoelemsd=geoelemsd)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tcellCon, opt="A")

  CALL bodySource%Get_(fevar=forceVar, &
                       xij=elemsd%coord(1:xij_i, 1:elemsd%nips))

  CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                    ans=forceVec, tsize=tforceVec)

  CALL rhs%Set( &
    globalNode=cellcon(1:tcellCon), islocal=defaultOpt%yes, scale=scale, &
    addContribution=defaultOpt%yes, VALUE=forceVec(1:tforceVec))

END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(geoCellCon)) DEALLOCATE (geoCellCon)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
NULLIFY (feptr, geofeptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleBodySource1

!----------------------------------------------------------------------------
!                                               ScalarFieldAssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleBodySource2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleBodySource2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleBodySource2

!----------------------------------------------------------------------------
!                                              ScalarFieldAssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleBodySource3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleBodySource3()"
#endif

TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tgeoCellCon, tforceVec, xij_i, xij_j, &
                tfevec
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:), forceVec(:)
CLASS(AbstractFE_), POINTER :: feptr, geofeptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tElements = mesh%GetTotalElements()

maxNNEGeo = geofedof%GetMaxTotalConnectivity()
CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)

maxNNE = fedof%GetMaxTotalConnectivity()
CALL Reallocate(cellcon, maxNNE)
CALL Reallocate(fevec, maxNNE)
CALL Reallocate(forceVec, maxNNE)

forceVar = NodalVariable(val=forceVec, rank=TypeFEVariableScalar, &
                         vartype=TypeFEVariableSpace)

DO iel = 1, tElements

  CALL fedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)
  CALL geofedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)
  CALL geofedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                                 ans=geoCellCon, tsize=tgeoCellCon, opt="A")
  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tcellCon, opt="A")
  CALL mesh%GetNodeCoord(nodeCoord=xij, nrow=xij_i, ncol=xij_j, &
                         islocal=defaultOpt%yes, globalElement=iel)

  feptr => fedof%GetFEPointer(globalElement=iel, islocal=defaultOpt%yes)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=defaultOpt%yes)

  CALL feptr%GetQuadraturePoints(quad=quad)
  CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)
  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)
  CALL feptr%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                    geoelemsd=geoelemsd)

  CALL bodySource%Get(VALUE=forceVec, globalNode=cellcon(1:tcellCon), &
                      tsize=tforceVec, islocal=defaultOpt%yes)

  CALL FEVariable_Set( &
    obj=forceVar, val=forceVec(1:tforceVec), rank=TypeFEVariableScalar, &
    vartype=TypeFEVariableSpace, scale=1.0_DFP, addContribution=defaultOpt%no)

  CALL ForceVector_(test=elemsd, c=forceVar, crank=TypeFEVariableScalar, &
                    ans=fevec, tsize=tfevec)

  CALL rhs%Set(globalNode=cellcon(1:tcellCon), islocal=defaultOpt%yes, &
               scale=scale, addContribution=defaultOpt%yes, &
               VALUE=fevec(1:tfevec))

END DO

IF (ALLOCATED(cellcon)) DEALLOCATE (cellcon)
IF (ALLOCATED(geoCellCon)) DEALLOCATE (geoCellCon)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
NULLIFY (feptr, geofeptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleBodySource3

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods
