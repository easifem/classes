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

SUBMODULE(QuadratureOpt_Class) GetMethods
USE Display_Method, ONLY: Display, ToString
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger, &
                                  QuadraturePoint_Initiate => Initiate, &
                                  GetTotalQuadraturePoints, &
                                  InitiateFacetQuadrature, &
                                  GetQuadratureWeights_
USE InputUtility, ONLY: Input
USE BaseType, ONLY: TypeElemNameOpt

USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_, &
                                    QuadratureNumber_Line

USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_, &
                                        QuadratureNumber_Triangle, &
                                        FacetConnectivity_Triangle

USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_, &
                                          QuadratureNumber_Quadrangle, &
                                          FacetConnectivity_Quadrangle

USE ReallocateUtility, ONLY: Reallocate

USE MappingUtility, ONLY: FromBiUnitLine2Segment_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isHomogeneous)) isHomogeneous = obj%isHomogeneous
IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(topoType)) topoType = obj%topoType
IF (PRESENT(isOrder)) isOrder = obj%isOrder
IF (PRESENT(isNips)) isNips = obj%isNips

IF (PRESENT(quadratureType)) quadratureType(1:3) = obj%quadratureType
IF (PRESENT(quadratureType1)) quadratureType1 = obj%quadratureType(1)
IF (PRESENT(quadratureType2)) quadratureType2 = obj%quadratureType(2)
IF (PRESENT(quadratureType3)) quadratureType3 = obj%quadratureType(3)

IF (PRESENT(order)) order(1:3) = obj%order
IF (PRESENT(order1)) order1 = obj%order(1)
IF (PRESENT(order2)) order2 = obj%order(2)
IF (PRESENT(order3)) order3 = obj%order(3)

IF (PRESENT(nips)) nips(1:3) = obj%nips(1:3)
IF (PRESENT(nips1)) nips1 = obj%nips(1)
IF (PRESENT(nips2)) nips2 = obj%nips(2)
IF (PRESENT(nips3)) nips3 = obj%nips(3)

IF (PRESENT(alpha)) alpha(1:3) = obj%alpha
IF (PRESENT(alpha1)) alpha1 = obj%alpha(1)
IF (PRESENT(alpha2)) alpha2 = obj%alpha(2)
IF (PRESENT(alpha3)) alpha3 = obj%alpha(3)

IF (PRESENT(beta)) beta(1:3) = obj%beta
IF (PRESENT(beta1)) beta1 = obj%beta(1)
IF (PRESENT(beta2)) beta2 = obj%beta(2)
IF (PRESENT(beta3)) beta3 = obj%beta(3)

IF (PRESENT(lambda)) lambda(1:3) = obj%lambda
IF (PRESENT(lambda1)) lambda1 = obj%lambda(1)
IF (PRESENT(lambda2)) lambda2 = obj%lambda(2)
IF (PRESENT(lambda3)) lambda3 = obj%lambda(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                          GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                order=obj%order(1), &
                                quadratureType=obj%quadratureType(1), &
                                alpha=obj%alpha(1), &
                                beta=obj%beta(1), &
                                lambda=obj%lambda(1), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (obj%isHomogeneous .AND. obj%isNips) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                nips=obj%nips(1:1), &
                                quadratureType=obj%quadratureType(1), &
                                alpha=obj%alpha(1), &
                                beta=obj%beta(1), &
                                lambda=obj%lambda(1), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                p=obj%order(1), &
                                q=obj%order(2), &
                                r=obj%order(3), &
                                quadratureType1=obj%quadratureType(1), &
                                quadratureType2=obj%quadratureType(2), &
                                quadratureType3=obj%quadratureType(3), &
                                alpha1=obj%alpha(1), &
                                alpha2=obj%alpha(2), &
                                alpha3=obj%alpha(3), &
                                beta1=obj%beta(1), &
                                beta2=obj%beta(2), &
                                beta3=obj%beta(3), &
                                lambda1=obj%lambda(1), &
                                lambda2=obj%lambda(2), &
                                lambda3=obj%lambda(3), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isNips) THEN
  CALL QuadraturePoint_Initiate(obj=quad, &
                                elemType=obj%topoType, &
                                domainName=obj%refelemDomain, &
                                nipsx=obj%order(1:1), &
                                nipsy=obj%order(2:2), &
                                nipsz=obj%order(3:3), &
                                quadratureType1=obj%quadratureType(1), &
                                quadratureType2=obj%quadratureType(2), &
                                quadratureType3=obj%quadratureType(3), &
                                alpha1=obj%alpha(1), &
                                alpha2=obj%alpha(2), &
                                alpha3=obj%alpha(3), &
                                beta1=obj%beta(1), &
                                beta2=obj%beta(2), &
                                beta3=obj%beta(3), &
                                lambda1=obj%lambda(1), &
                                lambda2=obj%lambda(2), &
                                lambda3=obj%lambda(3), &
                                xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif

END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                     Line_GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_GetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(1), nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isOrder) THEN
  nips(1) = QuadratureNumber_Line(order=obj%order(1), &
                                  quadtype=obj%quadratureType(1))
ELSE
  nips(1) = obj%nips(1)
END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1))

CALL QuadraturePoint_Line_( &
  nips=nips, quadType=obj%quadratureType(1), layout="INCREASING", &
  xij=obj%refelemCoord(1:obj%xidim, 1:2), alpha=obj%alpha(1), &
  beta=obj%beta(1), lambda=obj%lambda(1), ans=quad%points, &
  nrow=nrow, ncol=ncol)

quad%txi = obj%xidim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                Line_GetFacetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_GetFacetQuadraturePoints()"
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
END PROCEDURE Line_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                 Triangle_GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_GetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(1), nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isOrder) THEN
  nips(1) = QuadratureNumber_Triangle(order=obj%order(1), &
                                      quadtype=obj%quadratureType(1))
ELSE
  nips(1) = obj%nips(1)
END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1))

! nips, quadType, refTriangle, xij, ans, nrow, ncol
CALL QuadraturePoint_Triangle_( &
  nips=nips, quadType=obj%quadratureType(1), refTriangle=obj%refelemDomain, &
  xij=obj%refelemCoord(1:obj%xidim, 1:3), ans=quad%points, nrow=nrow, &
  ncol=ncol)

quad%txi = obj%xidim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                            Triangle_GetFacetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_GetFacetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(1), nrow, ncol, faceCon(2, 3)
REAL(DFP) :: x1(3), x2(3)
REAL(DFP), PARAMETER :: reflineCoord(1, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

nips(1) = obj%nips(1)
IF (obj%isOrder) THEN
  nips(1) = QuadratureNumber_Line(order=obj%order(1), &
                                  quadtype=obj%quadratureType(1))
END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1))
CALL Reallocate(facetQuad%points, 2, nips(1))
quad%txi = obj%xidim
facetQuad%txi = 1

!! Get quadrature points on [-1, 1]
CALL QuadraturePoint_Line_( &
  nips=nips, quadType=obj%quadratureType(1), layout="INCREASING", &
  xij=reflineCoord, alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=facetQuad%points, nrow=nrow, ncol=ncol)

facecon(1:2, 1:3) = FacetConnectivity_Triangle()
x1(1:obj%xidim) = obj%refelemCoord(1:obj%xidim, facecon(1, localFaceNumber))
x2(1:obj%xidim) = obj%refelemCoord(1:obj%xidim, facecon(2, localFaceNumber))

! Map quadrature points from[-1, 1] to the face of quadrangle
CALL FromBiUnitLine2Segment_( &
  xin=facetQuad%points(1, 1:nips(1)), x1=x1(1:obj%xidim), &
  x2=x2(1:obj%xidim), ans=quad%points, nrow=nrow, ncol=ncol)

! Get weights from faceQuad to quad
CALL GetQuadratureWeights_( &
  obj=facetQuad, weights=quad%points(obj%xidim + 1, :), tsize=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                         Quadrangle_GetFacetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_GetFacetQuadraturePoints()"
#endif

REAL(DFP), PARAMETER :: reflineCoord(1, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
INTEGER(I4B) :: nips(1), nrow, ncol, faceCon(2, 4)
REAL(DFP) :: x1(3), x2(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

nips(1) = obj%nips(1)
IF (obj%isOrder) THEN
  nips(1) = QuadratureNumber_Line(order=obj%order(1), &
                                  quadType=obj%quadratureType(1))
END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1))
CALL Reallocate(facetQuad%points, 2, nips(1))
quad%txi = obj%xidim
facetQuad%txi = 1

!! Get quadrature points on [-1, 1]
CALL QuadraturePoint_Line_( &
  nips=nips, quadType=obj%quadratureType(1), layout="INCREASING", &
  xij=reflineCoord, alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=facetQuad%points, nrow=nrow, ncol=ncol)

facecon(1:2, 1:4) = FacetConnectivity_Quadrangle()
x1(1:obj%xidim) = obj%refelemCoord(1:obj%xidim, facecon(1, localFaceNumber))
x2(1:obj%xidim) = obj%refelemCoord(1:obj%xidim, facecon(2, localFaceNumber))

! Map quadrature points from[-1, 1] to the face of quadrangle
CALL FromBiUnitLine2Segment_( &
  xin=facetQuad%points(1, 1:nips(1)), x1=x1(1:obj%xidim), &
  x2=x2(1:obj%xidim), ans=quad%points, nrow=nrow, ncol=ncol)

! Get weights from faceQuad to quad
CALL GetQuadratureWeights_( &
  obj=facetQuad, weights=quad%points(obj%xidim + 1, :), tsize=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                               Quadrangle_GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_GetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(2), nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

nips = obj%nips(1:2)
IF (obj%isOrder) THEN
  nips = QuadratureNumber_Quadrangle( &
         p=obj%order(1), q=obj%order(2), quadType1=obj%quadratureType(1), &
         quadType2=obj%quadratureType(2))

END IF

nrow = obj%xidim + 1
CALL Reallocate(quad%points, nrow, nips(1) * nips(2))

! nipsx, nipsy, quadType1, quadType2, refQuadrangle, xij, alpha1, beta1, &
! lambda1, alpha2, beta2, lambda2, ans, nrow, ncol)
CALL QuadraturePoint_Quadrangle_( &
  nipsx=nips(1:1), nipsy=nips(2:2), quadType1=obj%quadratureType(1), &
  quadType2=obj%quadratureType(2), refQuadrangle=obj%refelemDomain, &
  xij=obj%refelemCoord(1:obj%xidim, 1:4), alpha1=obj%alpha(1), &
  beta1=obj%beta(1), lambda1=obj%lambda(1), alpha2=obj%alpha(2), &
  beta2=obj%beta(2), lambda2=obj%lambda(2), ans=quad%points, &
  nrow=nrow, ncol=ncol)

quad%txi = obj%xidim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                          GetQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (obj%isOrder .AND. obj%isNips) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

IF (obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               order=obj%order(1), &
                               quadratureType=obj%quadratureType(1), &
                               alpha=obj%alpha(1), &
                               beta=obj%beta(1), &
                               lambda=obj%lambda(1), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (obj%isHomogeneous .AND. obj%isNips) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               nips=obj%nips(1:1), &
                               quadratureType=obj%quadratureType(1), &
                               alpha=obj%alpha(1), &
                               beta=obj%beta(1), &
                               lambda=obj%lambda(1), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isOrder) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               p=obj%order(1), &
                               q=obj%order(2), &
                               r=obj%order(3), &
                               quadratureType1=obj%quadratureType(1), &
                               quadratureType2=obj%quadratureType(2), &
                               quadratureType3=obj%quadratureType(3), &
                               alpha1=obj%alpha(1), &
                               alpha2=obj%alpha(2), &
                               alpha3=obj%alpha(3), &
                               beta1=obj%beta(1), &
                               beta2=obj%beta(2), &
                               beta3=obj%beta(3), &
                               lambda1=obj%lambda(1), &
                               lambda2=obj%lambda(2), &
                               lambda3=obj%lambda(3), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

IF (.NOT. obj%isHomogeneous .AND. obj%isNips) THEN
  CALL InitiateFacetQuadrature(obj=quad, facetQuad=facetQuad, &
                               localFaceNumber=localFaceNumber, &
                               elemType=obj%topoType, &
                               domainName=obj%refelemDomain, &
                               nipsx=obj%order(1:1), &
                               nipsy=obj%order(2:2), &
                               nipsz=obj%order(3:3), &
                               quadratureType1=obj%quadratureType(1), &
                               quadratureType2=obj%quadratureType(2), &
                               quadratureType3=obj%quadratureType(3), &
                               alpha1=obj%alpha(1), &
                               alpha2=obj%alpha(2), &
                               alpha3=obj%alpha(3), &
                               beta1=obj%beta(1), &
                               beta2=obj%beta(2), &
                               beta3=obj%beta(3), &
                               lambda1=obj%lambda(1), &
                               lambda2=obj%lambda(2), &
                               lambda3=obj%lambda(3), &
                               xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif

END PROCEDURE obj_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
abool = obj%isOrder .AND. obj%isNips
IF (abool) THEN
  CALL AssertError1(.TRUE., myName, &
                    "Both isOrder and isNips is set, I am confuse what to do")
END IF
#endif

abool = obj%isHomogeneous .AND. obj%isNips
IF (abool) THEN

  SELECT CASE (obj%topoType)
  CASE (TypeElemNameOpt%quadrangle)
    ans = obj%nips(1) * obj%nips(1)
  CASE (TypeElemNameOpt%hexahedron)
    ans = obj%nips(1) * obj%nips(1) * obj%nips(1)
  CASE DEFAULT
    ans = obj%nips(1)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

abool = .NOT. obj%isHomogeneous .AND. obj%isNips
IF (abool) THEN
  SELECT CASE (obj%topoType)
  CASE (TypeElemNameOpt%quadrangle)
    ans = PRODUCT(obj%nips(1:2))
  CASE (TypeElemNameOpt%hexahedron)
    ans = PRODUCT(obj%nips(1:3))
  CASE DEFAULT
    ans = obj%nips(1)
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

abool = obj%isHomogeneous .AND. obj%isOrder
IF (abool) THEN
  ans = GetTotalQuadraturePoints( &
        elemType=obj%topoType, &
        p=obj%order(1), &
        q=obj%order(1), &
        r=obj%order(1), &
        quadratureType1=obj%quadratureType(1), &
        quadratureType2=obj%quadratureType(1), &
        quadratureType3=obj%quadratureType(1))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

abool = .NOT. obj%isHomogeneous .AND. obj%isOrder
IF (abool) THEN
  ans = GetTotalQuadraturePoints( &
        elemType=obj%topoType, &
        p=obj%order(1), &
        q=obj%order(2), &
        r=obj%order(3), &
        quadratureType1=obj%quadratureType(1), &
        quadratureType2=obj%quadratureType(2), &
        quadratureType3=obj%quadratureType(3))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'No valid case found')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
