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

SUBMODULE(OneDimBasisOpt_Class) SetMethods
USE StringUtility, ONLY: UpperCase
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(order)) obj%order = order
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(baseContinuity)) obj%baseContinuity = &
  UpperCase(baseContinuity(1:2))
IF (PRESENT(baseInterpolation)) obj%baseInterpolation = &
  UpperCase(baseInterpolation(1:4))
IF (PRESENT(refElemDomain)) obj%refElemDomain = &
  UpperCase(refElemDomain(1:1))
IF (PRESENT(basisType)) obj%basisType = basisType
IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda
IF (PRESENT(firstCall)) obj%firstCall = firstCall
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType

CALL obj%quadOpt%SetParam( &
  quadratureType=quadratureType, order=quadratureOrder, &
  nips=quadratureNips, alpha=quadratureAlpha, beta=quadratureBeta, &
  lambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                              SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%order = order
obj%tdof = order + 1
obj%firstCall = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                              SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%SetOrder(order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                           SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%SetQuadratureType( &
  quadratureType=quadratureType, alpha=alpha, beta=beta, lambda=lambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
