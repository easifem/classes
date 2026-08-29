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
USE Projection_Method, ONLY: GetL2ProjectionDOFValueFromQuadrature
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: Display

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
!                                                     GetGlobalElemShapeData2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData2()"
#endif

INTEGER(I4B) :: order
LOGICAL(LGT) :: doNotInitQuad0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

doNotInitQuad0 = Input(option=doNotInitQuad, default=math%no)

IF (.NOT. doNotInitQuad0) THEN
  CALL obj%GetQuadraturePoints(quad=quad)
END IF

CALL obj%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

order = obj%GetOrder()
CALL obj%SetOrder(order=math%one_i)

CALL obj%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

CALL obj%SetOrder(order=order)

CALL obj%GetGlobalElemShapeData(geoelemsd=geoelemsd, xij=xij, elemsd=elemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData2

!----------------------------------------------------------------------------
!                                                  GetGlobalTimeElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalTimeElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalTimeElemShapeData()"
#endif

INTEGER(I4B) :: order
REAL(DFP) :: xij(1, 2)
LOGICAL(LGT) :: doNotInitQuad0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

doNotInitQuad0 = Input(option=doNotInitQuad, default=math%no)

IF (.NOT. doNotInitQuad0) THEN
  CALL obj%GetQuadraturePoints(quad=quad)
END IF

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
!                                                         GetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%opt%GetQuadratureOrder()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadratureOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInterpolationPoints
CALL obj%opt%GetInterpolationPoints(xij, nrow, ncol)
END PROCEDURE obj_GetInterpolationPoints

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
!                                              GetTimeDOFValueFromSTFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeDOFValueFromSTFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeDOFValueFromSTFunction()"
#endif

INTEGER(I4B) :: ipt, nipt, ii
REAL(DFP) :: args(4), scale, vertexInterpol, vertexValue(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

args = 0.0_DFP
args(1:nsd) = x(1:nsd)
nipt = elemsd%nips

IF (onlyFaceBubble) THEN
  scale = 1.0_DFP
ELSE
  scale = 0.0_DFP
END IF

! make vertex values
DO ii = 1, 2
  args(4) = times(ii)
  CALL func%Get(args=args, val=vertexValue(ii))
END DO

DO ipt = 1, nipt
  args(4) = elemsd%coord(1, ipt)
  CALL func%Get(args=args, val=funcValue(ipt))
  vertexInterpol = DOT_PRODUCT(elemsd%N(1:2, ipt), vertexValue(1:2))
  funcValue(ipt) = funcValue(ipt) - scale * vertexInterpol
END DO

CALL GetL2ProjectionDOFValueFromQuadrature( &
  elemsd=elemsd, func=funcValue, ans=ans, tsize=tsize, massMat=massMat, &
  ipiv=ipiv, skipVertices=onlyFaceBubble, tVertices=2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeDOFValueFromSTFunction

!----------------------------------------------------------------------------
!                                              GetTimeDOFValueFromSTFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeDOFValueFromConstant
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeDOFValueFromSTFunction()"
#endif

INTEGER(I4B) :: ipt
REAL(DFP) :: scale, vertexInterpol, vertexValue(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! make vertex values
vertexValue = 1.0_DFP

scale = 0.0_DFP
IF (onlyFaceBubble) scale = 1.0_DFP

DO ipt = 1, elemsd%nips
  funcValue(ipt) = 1.0_DFP
  vertexInterpol = DOT_PRODUCT(elemsd%N(1:2, ipt), vertexValue(1:2))
  funcValue(ipt) = funcValue(ipt) - scale * vertexInterpol
END DO

CALL GetL2ProjectionDOFValueFromQuadrature( &
  elemsd=elemsd, func=funcValue, ans=ans, tsize=tsize, massMat=massMat, &
  ipiv=ipiv, skipVertices=onlyFaceBubble, tVertices=math%two_i)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeDOFValueFromConstant

!----------------------------------------------------------------------------
!                                                  GetDOFValueFromSTFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromConstant
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromConstant()"
#endif

INTEGER(I4B) :: ipt
REAL(DFP) :: scale, vertexInterpol, vertexValue(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! make vertex values
vertexValue = math%one

scale = math%zero
IF (onlyFaceBubble) scale = math%zero

DO ipt = 1, elemsd%nips
  funcValue(ipt) = math%one
  vertexInterpol = DOT_PRODUCT(elemsd%N(1:2, ipt), vertexValue(1:2))
  funcValue(ipt) = funcValue(ipt) - scale * vertexInterpol
END DO

CALL GetL2ProjectionDOFValueFromQuadrature( &
  elemsd=elemsd, func=funcValue, ans=ans, tsize=tsize, massMat=massMat, &
  ipiv=ipiv, skipVertices=onlyFaceBubble, tVertices=math%two_i)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromConstant

!----------------------------------------------------------------------------
!                                               GetDOFValueFromSpaceFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromSpaceFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromSpaceFunction()"
#endif

INTEGER(I4B) :: ii
REAL(DFP) :: args(1), scale, vertexInterpol, vertexValue(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (onlyFaceBubble) THEN
  scale = math%one
ELSE
  scale = math%zero
END IF

! make vertex values
DO ii = 1, 2
  args(1) = x(ii)
  CALL func%Get(args=args, val=vertexValue(ii))
END DO

DO ii = 1, elemsd%nips
  args(1) = elemsd%coord(1, ii)
  CALL func%Get(args=args, val=funcValue(ii))
  vertexInterpol = DOT_PRODUCT(elemsd%N(1:2, ii), vertexValue(1:2))
  funcValue(ii) = funcValue(ii) - scale * vertexInterpol
END DO

IF (onlyFaceBubble) THEN

  CALL GetL2ProjectionDOFValueFromQuadrature( &
    elemsd=elemsd, func=funcValue, ans=ans, tsize=tsize, massMat=massMat, &
    ipiv=ipiv, skipVertices=onlyFaceBubble, tVertices=math%two_i)

ELSE

  IF (elemsd%nns .EQ. math%two_i) THEN
    tsize = math%two_i
    ans(1:tsize) = vertexValue(1:tsize)

  ELSE

    CALL GetL2ProjectionDOFValueFromQuadrature( &
      elemsd=elemsd, func=funcValue, ans=ans(3:), &
      tsize=tsize, massMat=massMat, &
      ipiv=ipiv, skipVertices=math%yes, tVertices=math%two_i)

    tsize = tsize + math%two_i
    ans(1:2) = vertexValue(1:2)
  END IF

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromSpaceFunction

!----------------------------------------------------------------------------
!                                               GetDOFValueFromTimeFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFValueFromTimeFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDOFValueFromTimeFunction()"
#endif

INTEGER(I4B), PARAMETER :: tVertices = math%two_i
INTEGER(I4B) :: ii
REAL(DFP) :: args(1), scale, vertexInterpol, vertexValue(2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (onlyFaceBubble) THEN
  scale = math%one
ELSE
  scale = math%zero
END IF

! make vertex values
DO ii = 1, tVertices
  args(1) = times(ii)
  CALL func%Get(args=args, val=vertexValue(ii))
END DO

DO ii = 1, elemsd%nips
  args(1) = elemsd%coord(1, ii)
  CALL func%Get(args=args, val=funcValue(ii))
  vertexInterpol = DOT_PRODUCT(elemsd%N(1:2, ii), vertexValue(1:2))
  funcValue(ii) = funcValue(ii) - scale * vertexInterpol
END DO

IF (onlyFaceBubble) THEN
  CALL GetL2ProjectionDOFValueFromQuadrature( &
    elemsd=elemsd, func=funcValue, ans=ans, &
    tsize=tsize, massMat=massMat, ipiv=ipiv, &
    skipVertices=onlyFaceBubble, tVertices=math%two_i)
ELSE

  IF (elemsd%nns .EQ. math%two_i) THEN
    tsize = math%two_i
    ans(1:tsize) = vertexValue(1:tsize)
  ELSE
    CALL GetL2ProjectionDOFValueFromQuadrature( &
      elemsd=elemsd, func=funcValue, ans=ans(3:), &
      tsize=tsize, massMat=massMat, ipiv=ipiv, &
      skipVertices=math%yes, tVertices=math%two_i)
    ans(1:2) = vertexValue(1:2)
    tsize = tsize + 2
  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDOFValueFromTimeFunction

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
