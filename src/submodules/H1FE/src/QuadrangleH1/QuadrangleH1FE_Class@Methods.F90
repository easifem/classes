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
!

SUBMODULE(QuadrangleH1FE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString, Display
USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    InterpolationPoint_Line_
USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle, &
                                          InterpolationPoint_Quadrangle_, &
                                          FacetConnectivity_Quadrangle

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                    GetFacetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                         SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_SetQuadratureOrder(order=order, order1=order1, &
                                           order2=order2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                         SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_SetQuadratureType( &
  quadratureType=quadratureType, quadratureType1=quadratureType1, &
  quadratureType2=quadratureType2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                 GetTotalInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalInterpolationPoints()"
#endif

INTEGER(I4B) :: p, q, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

p = order(1)
q = p

tsize = SIZE(order)
isok = tsize .GT. 1
IF (isok) q = order(2)

ans = GetTotalDOF_Line(order=p, baseContinuity="H1", &
                       baseInterpolation="")

tsize = GetTotalDOF_Line(order=q, baseContinuity="H1", &
                         baseInterpolation="")

ans = ans * tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalInterpolationPoints

!----------------------------------------------------------------------------
!                                                      GetInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInterpolationPoints()"
#endif

REAL(DFP) :: alpha1, beta1, lambda1, alpha2, beta2, lambda2
INTEGER(I4B) :: ipType1, ipType2, order1, order2, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

alpha1 = 0.0_DFP; beta1 = 0.0_DFP; lambda1 = 0.5_DFP
alpha2 = 0.0_DFP; beta2 = 0.0_DFP; lambda2 = 0.5_DFP

order1 = order(1)
order2 = order1
tsize = SIZE(order)
isok = tsize .GT. 1
IF (isok) order2 = order(2)

ipType1 = ipType(1)
ipType2 = ipType1
tsize = SIZE(ipType)
isok = tsize .GT. 1
IF (isok) ipType2 = ipType(2)

IF (PRESENT(alpha)) THEN
  alpha1 = alpha(1)
  alpha2 = alpha1
  tsize = SIZE(alpha)
  isok = tsize .GT. 1
  IF (isok) alpha2 = alpha(2)
END IF

IF (PRESENT(beta)) THEN
  beta1 = beta(1)
  beta2 = beta1
  tsize = SIZE(beta)
  isok = tsize .GT. 1
  IF (isok) beta2 = beta(2)
END IF

IF (PRESENT(lambda)) THEN
  lambda1 = lambda(1)
  lambda2 = lambda1
  tsize = SIZE(lambda)
  isok = tsize .GT. 1
  IF (isok) lambda2 = lambda(2)
END IF

CALL InterpolationPoint_Quadrangle_( &
  p=order1, q=order2, ipType1=ipType1, ipType2=ipType2, ans=ans, &
  nrow=nrow, ncol=ncol, layout="VEFC", xij=xij, alpha1=alpha1, beta1=beta1, &
  lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInterpolationPoints

!----------------------------------------------------------------------------
!                                                             SetOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrientation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%SetCellOrientation(cellOrient=cellOrient, tCell=tCell, &
                                errCheck=errCheck)

CALL obj%opt%SetFaceOrientation(faceOrient=faceOrient, tFace=tFace, &
                                errCheck=errCheck)

CALL obj%opt%SetEdgeOrientation(edgeOrient=edgeOrient, tEdge=tEdge, &
                                errCheck=.FALSE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrientation

!----------------------------------------------------------------------------
!                                   GetFacetDOFValueFromSpaceTimeUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromSTFunc()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tReturns, tArgs
#endif

INTEGER(I4B), PARAMETER :: tVertices = 2
INTEGER(I4B) :: ii, nips, nns, nsd, faceCon(tVertices, 4)
REAL(DFP) :: args(4), scale, vertexVal(tVertices), xijLine(3, tVertices), &
             vertexInterpol
LOGICAL(LGT) :: onlyFaceBubble0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tReturns = func%GetNumReturns()
isok = tReturns .EQ. 1
CALL AssertError1(isok, myName, &
                  "WIP: the user function must return a single value")
#endif

#ifdef DEBUG_VER
tArgs = func%GetNumArgs()
isok = tArgs .GE. 4_I4B
CALL AssertError1(isok, myName, &
           "WIP: the user function must have at least 4 arguments, (x,y,z,t)")
#endif

nips = facetElemsd%nips
nns = facetElemsd%nns
nsd = obj%opt%GetNSD()

scale = 0.0_DFP
vertexVal = 0.0_DFP

onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)

args(1:3) = 0.0_DFP
args(4) = times
IF (onlyFaceBubble0) THEN
  faceCon = FacetConnectivity_Quadrangle()
  xijLine(1:nsd, :) = xij(1:nsd, faceCon(:, localFaceNumber))

  DO ii = 1, tVertices
    args(1:nsd) = xijLine(1:nsd, ii)
    CALL func%GetScalarValue(args=args, val=vertexVal(ii))
  END DO

  scale = 1.0_DFP
END IF

args(1:3) = 0.0_DFP
args(4) = times
DO ii = 1, nips
  args(1:nsd) = facetElemsd%coord(1:nsd, ii)
  CALL func%GetScalarValue(args=args, val=funcValue(ii))

  vertexInterpol = DOT_PRODUCT(facetElemsd%N(1:tVertices, ii), &
                               vertexVal(1:tVertices))

  funcValue(ii) = funcValue(ii) - scale * vertexInterpol
END DO

CALL obj%GetFacetDOFValueFromQuadrature( &
  elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, &
  localFaceNumber=localFaceNumber, func=funcValue, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, onlyFaceBubble=onlyFaceBubble, &
  tVertices=tVertices)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromSTFunc

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
