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

SUBMODULE(TriangleH1FE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle, &
                                        InterpolationPoint_Triangle_, &
                                        FacetConnectivity_Triangle
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString

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

CALL obj%opt%Triangle_GetQuadraturePoints(quad=quad)

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

CALL obj%opt%Triangle_GetFacetQuadraturePoints( &
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

LOGICAL(LGT) :: isok
INTEGER(I4B) :: order0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(order) .OR. PRESENT(order1) .OR. &
       PRESENT(order2) .OR. PRESENT(order3)
CALL AssertError1(isok, myName, &
                  'order, order1, order2, or order3 must be provided')
#endif

IF (PRESENT(order)) THEN
  order0 = order(1)
ELSE IF (PRESENT(order1)) THEN
  order0 = order1
ELSE IF (PRESENT(order2)) THEN
  order0 = order2
ELSE IF (PRESENT(order3)) THEN
  order0 = order3
END IF

CALL obj%opt%Triangle_SetQuadratureOrder(order=order0)

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

LOGICAL(LGT) :: isok
INTEGER(I4B) :: quadratureType0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(quadratureType) .OR. PRESENT(quadratureType1) .OR. &
       PRESENT(quadratureType2) .OR. PRESENT(quadratureType3)
CALL AssertError1(isok, myName, &
                  'quadratureType, quadratureType1, quadratureType2, or &
                  &quadratureType3 must be provided')
#endif

IF (PRESENT(quadratureType)) THEN
  quadratureType0 = quadratureType(1)
ELSE IF (PRESENT(quadratureType1)) THEN
  quadratureType0 = quadratureType1
ELSE IF (PRESENT(quadratureType2)) THEN
  quadratureType0 = quadratureType2
ELSE IF (PRESENT(quadratureType3)) THEN
  quadratureType0 = quadratureType3
END IF

CALL obj%opt%Triangle_SetQuadratureType(quadratureType=quadratureType0)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GetTotalDOF_Triangle(order=order(1), baseContinuity="H1", &
                           baseInterpolation="")

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

REAL(DFP) :: alpha0, beta0, lambda0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

alpha0 = 0.0_DFP; beta0 = 0.0_DFP; lambda0 = 0.5_DFP

IF (PRESENT(alpha)) alpha0 = alpha(1)
IF (PRESENT(beta)) beta0 = beta(1)
IF (PRESENT(lambda)) lambda0 = lambda(1)

! order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
CALL InterpolationPoint_Triangle_( &
  order=order(1), ipType=ipType(1), ans=ans, nrow=nrow, ncol=ncol, &
  layout="VEFC", xij=xij, alpha=alpha0, beta=beta0, lambda=lambda0)

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
!                                                            GetFacetDOFValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromSTFunc
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "obj_GetFacetDOFValueFromSTFunc()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tReturns
#endif

INTEGER(I4B), PARAMETER :: tVertices = 2
INTEGER(I4B) :: tArgs, ii, nips, nns, nsd, faceCon(tVertices, 3), &
                returnType, icompo0
REAL(DFP) :: args(4), scale, vertexVal(tVertices), xijLine(3, tVertices), &
             vertexInterpol, temp_ans(10)
LOGICAL(LGT) :: onlyFaceBubble0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = facetElemsd%nips
nns = facetElemsd%nns
nsd = facetElemsd%nsd

tArgs = func%GetNumArgs()
scale = 0.0_DFP
vertexVal = 0.0_DFP

returnType = func%GetReturnType()
tReturns = func%GetNumReturns()
onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)

SELECT CASE (returnType)
CASE (TypeFEVariableOpt%scalar)

#ifdef DEBUG_VER
  isok = tReturns .EQ. 1
  CALL AssertError1(isok, myName, &
                    "WIP: the user function must return a single value")
#endif

  IF (onlyFaceBubble0) THEN
    faceCon = FacetConnectivity_Triangle()
    xijLine(1:nsd, :) = xij(1:nsd, faceCon(:, localFaceNumber))

    DO ii = 1, tVertices
      args(1:nsd) = xijLine(1:nsd, ii)
      CALL func%GetScalarValue(args=args, val=vertexVal(ii))
    END DO

    scale = 1.0_DFP
  END IF

  args = times
  DO ii = 1, nips
    args(1:nsd) = facetElemsd%coord(1:nsd, ii)
    CALL func%GetScalarValue(args=args, val=funcValue(ii))

    vertexInterpol = DOT_PRODUCT(facetElemsd%N(1:tVertices, ii), &
                                 vertexVal(1:tVertices))

    funcValue(ii) = funcValue(ii) - scale * vertexInterpol
  END DO

CASE (TypeFEVariableOpt%vector)

  icompo0 = Input(default=1_I4B, option=icompo)

#ifdef DEBUG_VER
  isok = tReturns .GE. icompo0
  CALL AssertError1(isok, myName, &
                    "WIP: the user function must return " &
                    //ToString(icompo0)//" values")
#endif

  IF (onlyFaceBubble0) THEN
    faceCon = FacetConnectivity_Triangle()
    xijLine(1:nsd, :) = xij(1:nsd, faceCon(:, localFaceNumber))

    DO ii = 1, tVertices
      args(1:nsd) = xijLine(1:nsd, ii)
      CALL func%GetVectorValue(args=args, val=temp_ans(1:tReturns), &
                               n=tReturns)
      vertexVal(ii) = temp_ans(icompo0)
    END DO

    scale = 1.0_DFP
  END IF

  args = times
  DO ii = 1, nips
    args(1:nsd) = facetElemsd%coord(1:nsd, ii)
    CALL func%GetVectorValue(args=args, val=temp_ans(1:tReturns), &
                             n=tReturns)
    funcValue(ii) = temp_ans(icompo0)

    vertexInterpol = DOT_PRODUCT(facetElemsd%N(1:tVertices, ii), &
                                 vertexVal(1:tVertices))

    funcValue(ii) = funcValue(ii) - scale * vertexInterpol
  END DO

END SELECT

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
!                                                              include Error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
