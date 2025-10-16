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

SUBMODULE(BasisOpt_Class) QuadrangleH1LagrangeFEMethods
USE Display_Method, ONLY: ToString, Display
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Set => Set, &
                                LagrangeFacetElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                Elemsd_SetNormal => SetNormal

USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle, &
                                          InterpolationPoint_Quadrangle_, &
                                          LagrangeEvalAll_Quadrangle_, &
                                        LagrangeGradientEvalAll_Quadrangle_, &
                                          FacetConnectivity_Quadrangle

USE LineInterpolationUtility, ONLY: LagrangeDOF_Line, &
                                    InterpolationPoint_Line_, &
                                    LagrangeEvalAll_Line_, &
                                    LagrangeGradientEvalAll_Line_

USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_
USE ReallocateUtility, ONLY: Reallocate
USE SwapUtility, ONLY: SWAP_
USE ReverseUtility, ONLY: Reverse

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                     QuadrangleH1LagFE_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "QuadrangleH1LagFE_GetLocalElemShapeData()"
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
tdof = obj%GetTotalDOF()

CALL Elemsd_Allocate(obj=elemsd, nsd=obj%nsd, xidim=obj%xidim, &
                     nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

! order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
CALL InterpolationPoint_Quadrangle_( &
  order=obj%order, ipType=obj%ipType, ans=obj%xij, nrow=indx(1), &
  ncol=indx(2), layout="VEFC", xij=obj%refelemCoord(1:obj%xidim, 1:4), &
  alpha=obj%alpha(1), beta=obj%beta(1), lambda=obj%lambda(1))

! order, x, xij, ans, nrow, ncol, coeff, firstCall, basisType, alpha, &
! beta, lambda
CALL LagrangeEvalAll_Quadrangle_( &
  order=obj%order, x=quad%points(1:quad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), ans=obj%temp(:, :, 1), nrow=indx(3), &
  ncol=indx(4), coeff=obj%coeff(1:tdof, 1:tdof), firstCall=obj%firstCall, &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

! order, x, xij, ans, dim1, dim2, dim3, coeff, firstCall, basisType, &
! alpha, beta, lambda
CALL LagrangeGradientEvalAll_Quadrangle_( &
  order=obj%order, x=quad%points(1:quad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), ans=obj%temp, dim1=indx(5), &
  dim2=indx(6), dim3=indx(7), coeff=obj%coeff(1:tdof, 1:tdof), &
  firstCall=.FALSE., basisType=obj%basisType(1), alpha=obj%alpha(1), &
  beta=obj%beta(1), lambda=obj%lambda(1))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1LagFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                  QuadrangleH1LagFE_SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "QuadrangleH1LagFE_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isIsotropicOrder = .TRUE.
CALL obj%ResetAnisotropicOrder()
obj%order = order
obj%tdof = LagrangeDOF_Quadrangle(order=obj%order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1LagFE_SetOrder

!----------------------------------------------------------------------------
!                                    QuadrangleH1LagFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

INTEGER(I4B) :: nns, nips, nsd, xidim, n1, n2
LOGICAL(LGT) :: isok, bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = geoelemsd%nns
nips = geoelemsd%nips
nsd = geoelemsd%nsd
xidim = geoelemsd%xidim

CALL Elemsd_Set(obj=elemsd, val=xij(1:nsd, 1:nns), &
                N=geoelemsd%N(1:nns, 1:nips), &
                dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))

bool1 = (nns .NE. elemsd%nns) .AND. (obj%order .GT. 2_I4B) &
        .AND. obj%isFaceOrient

IF (.NOT. bool1) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

n1 = 1; n2 = 4
! Edge-1: 1 --> 2
n1 = n2 + 1; n2 = n2 + obj%order - 1
isok = bool1 .AND. (obj%faceOrient(1, 1) .EQ. -1_I4B)
IF (isok) THEN
  CALL Reverse(ans=elemsd%N, r1=n1, r2=n2, c1=1, c2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXt, r1=n1, r2=n2, c1=1, c2=elemsd%nsd, &
               d1=1, d2=elemsd%nips, dim=1)
END IF

! Edge-2: 2 --> 3
n1 = n2 + 1; n2 = n2 + obj%order - 1
isok = bool1 .AND. (obj%faceOrient(1, 2) .EQ. -1_I4B)
IF (isok) THEN
  CALL Reverse(ans=elemsd%N, r1=n1, r2=n2, c1=1, c2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXt, r1=n1, r2=n2, c1=1, c2=elemsd%nsd, &
               d1=1, d2=elemsd%nips, dim=1)
END IF

! Edge-3: 3 --> 4
n1 = n2 + 1; n2 = n2 + obj%order - 1
isok = bool1 .AND. (obj%faceOrient(1, 3) .EQ. -1_I4B)
IF (isok) THEN
  CALL Reverse(ans=elemsd%N, r1=n1, r2=n2, c1=1, c2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXt, r1=n1, r2=n2, c1=1, c2=elemsd%nsd, &
               d1=1, d2=elemsd%nips, dim=1)
END IF

! Edge-4: 4 --> 1
n1 = n2 + 1; n2 = n2 + obj%order - 1
isok = bool1 .AND. (obj%faceOrient(1, 4) .EQ. -1_I4B)
IF (isok) THEN
  CALL Reverse(ans=elemsd%N, r1=n1, r2=n2, c1=1, c2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXi, r1=n1, r2=n2, c1=1, c2=elemsd%xidim, &
               d1=1, d2=elemsd%nips, dim=1)
  CALL Reverse(ans=elemsd%dNdXt, r1=n1, r2=n2, c1=1, c2=elemsd%nsd, &
               d1=1, d2=elemsd%nips, dim=1)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1LagFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                QuadrangleH1LagFE_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "QuadrangleH1LagFE_GetLocalFacetElemShapeData()"
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj, xidim
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

nips = GetTotalQuadraturePoints(obj=facetQuad)
tdof = obj%order + 1

xidim = obj%xidim - 1
CALL Elemsd_Allocate(obj=facetElemsd, nsd=obj%nsd, xidim=xidim, &
                     nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=facetQuad, weights=facetElemsd%ws, &
                           tsize=nips)

CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

! order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
CALL InterpolationPoint_Line_( &
  order=obj%order, ipType=obj%ipType, layout="VEFC", &
  xij=lineCoord, alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=obj%xij, nrow=indx(1), ncol=indx(2))

CALL LagrangeEvalAll_Line_( &
  order=obj%order, xij=obj%xij(1:indx(1), 1:indx(2)), &
  x=facetQuad%points(1:facetQuad%txi, 1:nips), &
  coeff=obj%coeff(1:tdof, 1:tdof), firstCall=.TRUE., &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=obj%temp(:, :, 1), nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  facetElemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL LagrangeGradientEvalAll_Line_( &
  order=obj%order, x=facetQuad%points(1:facetQuad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7), &
  coeff=obj%coeff(1:tdof, 1:tdof), firstCall=.FALSE., &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1))

CALL SWAP_(a=facetElemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1LagFE_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                QuadrangleH1LagFE_GetGlobalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_GetGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "QuadrangleH1LagFE_GetGlobalFacetElemShapeData()"
#endif

INTEGER(I4B) :: faceCon(2, 4), nns, nips, nsd, xidim, n1, n2
REAL(DFP) :: line_xij(3, 2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%QuadrangleH1LagFE_GetGlobalElemShapeData( &
  elemsd=elemsd, geoElemsd=geoElemsd, xij=xij)

faceCon = FacetConnectivity_Quadrangle()

nns = geoFacetElemsd%nns

#ifdef DEBUG_VER
isok = nns .EQ. 2_I4B
CALL AssertError1(isok, myName, &
        "WIP, this routine currently works for nns .eq. 2 for geoFacetElemsd")
#endif

nips = geoFacetElemsd%nips
nsd = geoFacetElemsd%nsd
xidim = geoFacetElemsd%xidim

line_xij = 0.0_DFP
line_xij(1:nsd, 1:nns) = xij(1:nsd, faceCon(1:2, localFaceNumber))

CALL Elemsd_Set(obj=facetElemsd, val=line_xij(1:nsd, 1:nns), &
                N=geoFacetElemsd%N(1:nns, 1:nips), &
                dNdXi=geoFacetElemsd%dNdXi(1:nns, 1:xidim, 1:nips))

CALL Elemsd_SetNormal(obj=facetElemsd)

isok = obj%isFaceOrient &
       .AND. (obj%faceOrient(1, localFaceNumber) .EQ. -1_I4B)

! We reverse end points too
n1 = 1; n2 = nns
IF (isok) THEN
  CALL Reverse(ans=facetElemsd%N, r1=n1, r2=n2, c1=1, c2=facetElemsd%nips, &
               dim=1)
  CALL Reverse(ans=facetElemsd%dNdXi, r1=n1, r2=n2, c1=1, &
               c2=facetElemsd%xidim, d1=1, d2=facetElemsd%nips, dim=1)
  CALL Reverse(ans=facetElemsd%dNdXi, r1=n1, r2=n2, c1=1, &
               c2=facetElemsd%xidim, d1=1, d2=facetElemsd%nips, dim=1)
  CALL Reverse(ans=facetElemsd%dNdXt, r1=n1, r2=n2, c1=1, &
               c2=facetElemsd%nsd, d1=1, d2=facetElemsd%nips, dim=1)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1LagFE_GetGlobalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE QuadrangleH1LagrangeFEMethods
