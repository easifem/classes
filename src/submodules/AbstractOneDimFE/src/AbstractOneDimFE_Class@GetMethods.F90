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
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set

USE BaseType, ONLY: elemNameOpt => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: Initiate

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

! CHARACTER(4) :: baseInterpolation
! INTEGER(I4B), PARAMETER :: one = 1
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! baseInterpolation = obj%opt%GetBaseInterpolation()
!
! SELECT CASE (baseInterpolation)
! CASE ("LAGR")
!   CALL LagrangeElemShapeData(obj=elemsd, &
!                              quad=quad, &
!                              nsd=one, &
!                              xidim=one, &
!                              elemType=elemNameOpt%line, &
!                              refelemCoord=obj%opt%refelemCoord, &
!                              domainName=obj%opt%refelemDomain, &
!                              order=obj%opt%order, &
!                              ipType=obj%opt%ipType, &
!                              basisType=obj%opt%basisType, &
!                              coeff=obj%coeff, &
!                              firstCall=obj%opt%firstCall, &
!                              alpha=obj%opt%alpha, &
!                              beta=obj%opt%beta, &
!                              lambda=obj%opt%lambda)
!
! CASE ("HIER", "HEIR")
! ! CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
! !       xidim=obj%xidim, elemType=obj%elemType, refelemCoord=obj%refelemCoord, &
! !                       domainName=obj%refelemDomain, cellOrder=obj%cellOrder, &
! !                            faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder, &
! !                        cellOrient=obj%cellOrient, faceOrient=obj%faceOrient, &
! !                                edgeOrient=obj%edgeOrient)
!
! CASE ("ORTH")
!
! CASE DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for baseInterpolation')
!   RETURN
! END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

INTEGER(I4B) :: nns, nips, nsd, xidim
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(geoelemsd)

IF (isok) THEN
  nns = geoelemsd%nns
  nips = geoelemsd%nips
  nsd = geoelemsd%nsd
  xidim = geoelemsd%xidim
  CALL Elemsd_Set( &
    obj=elemsd, val=xij(1:nsd, 1:nns), N=geoelemsd%N(1:nns, 1:nips), &
    dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))

ELSE

  nns = elemsd%nns
  nips = elemsd%nips
  nsd = elemsd%nsd
  xidim = elemsd%xidim

  CALL Elemsd_Set( &
    obj=elemsd, val=xij(1:nsd, 1:nns), N=elemsd%N(1:nns, 1:nips), &
    dNdXi=elemsd%dNdXi(1:nns, 1:xidim, 1:nips))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

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

CALL obj%opt%GetQuadraturePoints( &
  quad=quad, order=order, quadratureType=quadratureType, alpha=alpha, &
  beta=beta, lambda=lambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%opt%GetCaseName()
END PROCEDURE obj_GetCaseName

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
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
