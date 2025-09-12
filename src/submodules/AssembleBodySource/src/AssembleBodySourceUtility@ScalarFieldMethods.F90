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

USE BaseType, ONLY: QuadraturePoint_, &
                    ElemshapeData_, &
                    FEVariable_, &
                    TypeFEVariableScalar, &
                    TypeFEVariableSpace

USE ReallocateUtility, ONLY: Reallocate

USE ForceVector_Method, ONLY: ForceVector

USE FEVariable_Method, ONLY: NodalVariable

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              ScalarFieldAssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleBodySource1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleBodySource1()"
#endif

TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
INTEGER(I4B) :: nrow, ncol, iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tgeoCellCon
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:)

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

DO iel = 1, tElements

  CALL fedof%GetQuadraturePoints(quad=quad, globalElement=iel, &
                                 islocal=defaultOpt%yes)

  CALL fedof%GetLocalElemShapeData(globalElement=iel, elemsd=elemsd, &
                                   islocal=defaultOpt%yes, quad=quad)

  CALL geofedof%GetLocalElemShapeData(globalElement=iel, &
                                      elemsd=geoelemsd, &
                                      islocal=defaultOpt%yes, &
                                      quad=quad)

  CALL geofedof%GetConnectivity_(globalElement=iel, &
                                 islocal=defaultOpt%yes, &
                                 ans=geoCellCon, &
                                 tsize=tgeoCellCon, opt="A")

  CALL nodeCoord%Get(VALUE=xij, nrow=nrow, ncol=ncol, &
                     storageFMT=defaultOpt%storageFormatNodes, &
                     globalNode=geoCellCon(1:tgeoCellCon), &
                     islocal=defaultOpt%yes)

  CALL fedof%GetGlobalElemShapeData(globalElement=iel, &
                                    elemsd=elemsd, &
                                    xij=xij(1:nrow, 1:ncol), &
                                    geoelemsd=geoelemsd, &
                                    islocal=defaultOpt%yes)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tcellCon, opt="A")

  ! TODO: Use Get method which does not reallocate forceVar
  CALL bodySource%Get(fevar=forceVar, xij=xij(1:nrow, 1:ncol), times=times)

  ! TODO: Use ForceVector which does not reallocate fevec
  fevec = ForceVector(test=elemsd, c=forceVar, crank=TypeFEVariableScalar)

  CALL rhs%Set(globalNode=cellcon(1:tcellCon), islocal=defaultOpt%yes, &
               scale=scale, addContribution=defaultOpt%yes, VALUE=fevec)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleBodySource1

!----------------------------------------------------------------------------
!                                              ScalarFieldAssembleBodySource
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleBodySource2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleBodySource2()"
#endif

TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
TYPE(FEVariable_) :: forceVar
INTEGER(I4B) :: nrow, ncol, iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, tgeoCellCon, tforceVec
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:)
REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:), forceVec(:)

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
CALL Reallocate(forcevec, maxNNE)

DO iel = 1, tElements

  CALL fedof%GetQuadraturePoints(quad=quad, globalElement=iel, &
                                 islocal=defaultOpt%yes)

  CALL fedof%GetLocalElemShapeData(globalElement=iel, elemsd=elemsd, &
                                   islocal=defaultOpt%yes, quad=quad)

  CALL geofedof%GetLocalElemShapeData(globalElement=iel, &
                                      elemsd=geoelemsd, &
                                      islocal=defaultOpt%yes, &
                                      quad=quad)

  CALL geofedof%GetConnectivity_(globalElement=iel, &
                                 islocal=defaultOpt%yes, &
                                 ans=geoCellCon, &
                                 tsize=tgeoCellCon, opt="A")

  CALL nodeCoord%Get(VALUE=xij, nrow=nrow, ncol=ncol, &
                     storageFMT=defaultOpt%storageFormatNodes, &
                     globalNode=geoCellCon(1:tgeoCellCon), &
                     islocal=defaultOpt%yes)

  CALL fedof%GetGlobalElemShapeData(globalElement=iel, &
                                    elemsd=elemsd, &
                                    xij=xij(1:nrow, 1:ncol), &
                                    geoelemsd=geoelemsd, &
                                    islocal=defaultOpt%yes)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tcellCon, opt="A")

  CALL bodySource%Get(VALUE=forceVec, globalNode=cellcon(1:tcellCon), &
                      tsize=tforceVec, islocal=defaultOpt%yes)
  ! TODO:
  forceVar = NodalVariable(val=forceVec(1:tforceVec), &
                           rank=TypeFEVariableScalar, &
                           vartype=TypeFEVariableSpace)
  fevec = ForceVector(test=elemsd, c=forceVar, crank=TypeFEVariableScalar)

  CALL rhs%Set(globalNode=cellcon(1:tcellCon), islocal=defaultOpt%yes, &
               scale=scale, addContribution=defaultOpt%yes, VALUE=fevec)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleBodySource2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods
