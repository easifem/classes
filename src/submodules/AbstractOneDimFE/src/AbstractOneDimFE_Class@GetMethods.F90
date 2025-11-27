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

SUBMODULE(AbstractOneDimFE_Class) GetMethods
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData
USE ElemshapeData_Method, ONLY: Elemsd_Set => Set
USE BaseType, ONLY: elemNameOpt => TypeElemNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

! INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetParam( &
  order=order, fetype=fetype, ipType=ipType, basisType=basisType, &
  alpha=alpha, beta=beta, lambda=lambda, refElemDomain=refElemDomain, &
  baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
  firstCall=firstCall, dofType=dofType, transformType=transformType, &
  quadratureType=quadratureType, quadratureOrder=quadratureOrder, &
  quadratureAlpha=quadratureAlpha, quadratureBeta=quadratureBeta, &
  quadratureLambda=quadratureLambda)

IF (PRESENT(isInitiated)) isInitiated = obj%isInit

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                     GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[IMPLEMENTATION ERROR] :: This routine should be implemented by '// &
                  'child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                     GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalFacetElemShapeData()"
#endif

REAL(DFP) :: xij(2, 2)
INTEGER(I4B) :: i1, i2
TYPE(QuadraturePoint_) :: quad

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xij = 0.0_DFP

CALL obj%opt%GetRefElemCoord(ans=xij, nrow=i1, ncol=i2)
CALL QuadraturePoint_Initiate(obj=quad, points=xij)

CALL obj%GetLocalElemShapeData(elemsd=elemsd, quad=quad)
CALL QuadraturePoint_Deallocate(quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                     GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

INTEGER(I4B) :: nns, nips, nsd, xidim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = geoelemsd%nns
nips = geoelemsd%nips
nsd = geoelemsd%nsd
xidim = geoelemsd%xidim

CALL Elemsd_Set( &
  obj=elemsd, val=xij(1:nsd, 1:nns), N=geoelemsd%N(1:nns, 1:nips), &
  dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                  GetGlobalTimeElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalTimeElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalTimeElemShapeData()"
#endif

INTEGER(I4B) :: order
REAL(DFP) :: xij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetQuadraturePoints(quad=quad)

CALL obj%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

order = obj%GetOrder()
CALL obj%SetOrder(order=1_I4B)

CALL obj%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

CALL obj%SetOrder(order=order)
xij(1, 1:2) = times(1:2)

CALL obj%GetGlobalElemShapeData(geoelemsd=geoelemsd, xij=xij, elemsd=elemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalTimeElemShapeData

!----------------------------------------------------------------------------
!                                            GetGlobalTimeFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalTimeFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalTimeFacetElemShapeData()"
#endif

INTEGER(I4B) :: order, i1, i2
REAL(DFP) :: xij(1, 2), refelemCoord(2, 2)
TYPE(QuadraturePoint_) :: quad

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

refelemCoord = 0.0_DFP
CALL obj%opt%GetRefElemCoord(ans=refelemCoord, nrow=i1, ncol=i2)

CALL QuadraturePoint_Initiate(obj=quad, points=refelemCoord)
CALL obj%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

order = obj%GetOrder()
CALL obj%SetOrder(order=math%one_i)

CALL obj%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

CALL obj%SetOrder(order=order)
xij(1, 1:2) = times(1:2)

CALL obj%GetGlobalElemShapeData(geoelemsd=geoelemsd, xij=xij, elemsd=elemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalTimeFacetElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
ans = obj%opt%GetBaseInterpolation()
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseContinuity
ans = obj%opt%GetBaseContinuity()
END PROCEDURE obj_GetBaseContinuity

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
ans = obj%opt%GetOrder()
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%GetTotalQuadraturePoints()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
