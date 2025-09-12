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

SUBMODULE(AssembleDiffusionMatrixUtility) Methods
USE Display_Method, ONLY: ToString, Display
USE ExceptionHandler_Class, ONLY: e
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: QuadraturePoint_, ElemshapeData_, FEVariable_, &
                    TypeFEVariableScalar
USE MassMatrix_Method, ONLY: MassMatrix_
USE DiffusionMatrix_Method, ONLY: DiffusionMatrix_

#ifdef DEBUG_VER
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
USE ElemshapeData_Method, ONLY: ElemshapeData_Display => Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                         ScalarFieldAssembleDiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleDiffusionMatrix1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleDiffusionMatrix1()"
#endif

INTEGER(I4B) :: nrow, ncol, tsize, iel, tElements, maxNNE, maxNNEGeo
TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), ks(:, :)
INTEGER(I4B), ALLOCATABLE :: cellcon(:), geoCellCon(:)
TYPE(FEVariable_) :: diffCoeffVar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (reset) CALL tanmat%set(VALUE=defaultOpt%zero)

tElements = mesh%GetTotalElements()

maxNNEGeo = geofedof%GetMaxTotalConnectivity()
CALL Reallocate(geoCellCon, maxNNEGeo)
CALL Reallocate(xij, 3, maxNNEGeo)

maxNNE = fedof%GetMaxTotalConnectivity()
CALL Reallocate(cellcon, maxNNE)

CALL Reallocate(ks, maxNNE, maxNNE)

DO iel = 1, tElements

  ! TODO: No allocation GetQuadraturePoints_ necessary
  CALL fedof%GetQuadraturePoints(quad=quad, globalElement=iel, &
                                 islocal=defaultOpt%yes)

  ! TODO: No allocation GetLocalElemShapeData_ needed
  CALL fedof%GetLocalElemShapeData(globalElement=iel, elemsd=elemsd, &
                                   islocal=defaultOpt%yes, quad=quad)

  ! TODO:: No allocation GetLocalElemShapeData_ needed
  CALL geofedof%GetLocalElemShapeData(globalElement=iel, &
                                      elemsd=geoelemsd, &
                                      islocal=defaultOpt%yes, &
                                      quad=quad)

  CALL geofedof%GetConnectivity_(globalElement=iel, &
                                 islocal=defaultOpt%yes, &
                                 ans=geoCellCon, &
                                 tsize=tsize, opt="A")

  CALL nodeCoord%Get(VALUE=xij, nrow=nrow, ncol=ncol, &
                     storageFMT=defaultOpt%storageFormatNodes, &
                     globalNode=geoCellCon(1:tsize), &
                     islocal=defaultOpt%yes)

  CALL fedof%GetGlobalElemShapeData(globalElement=iel, &
                                    elemsd=elemsd, &
                                    xij=xij(1:elemsd%nsd, 1:geoelemsd%nns), &
                                    geoelemsd=geoelemsd, &
                                    islocal=defaultOpt%yes)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tsize, opt="A")

  CALL diffCoeffField%Get(globalElement=iel, islocal=defaultOpt%yes, &
                          fevar=diffCoeffVar)

  ks = defaultOpt%zero
  CALL DiffusionMatrix_(test=elemsd, trial=elemsd, k=diffCoeffVar, &
                        krank=TypeFEVariableScalar, ans=ks, &
                        nrow=nrow, ncol=ncol)

  CALL tanmat%Set(globalNode=cellcon(1:tsize), &
                  islocal=defaultOpt%yes, &
                  VALUE=ks(1:nrow, 1:ncol), &
                  storageFMT=defaultOpt%storageFormatDOF, &
                  scale=scale, &
                  addContribution=defaultOpt%yes)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleDiffusionMatrix1

!----------------------------------------------------------------------------
!                                                          Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
