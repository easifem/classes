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

SUBMODULE(BasisOpt_Class) TriangleH1HierarchicalFEMethods
USE Display_Method, ONLY: ToString, Display
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                HierarchicalFacetElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                Elemsd_SetNormal => SetNormal

USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle, &
                                        InterpolationPoint_Triangle_, &
                                        FacetConnectivity_Triangle, &
                                        HeirarchicalBasis_Triangle_, &
                                        HeirarchicalBasisGradient_Triangle_

USE LineInterpolationUtility, ONLY: LagrangeDOF_Line, &
                                    InterpolationPoint_Line_, &
                                    HeirarchicalBasis_Line_, &
                                    HeirarchicalBasisGradient_Line_

USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_
USE ReallocateUtility, ONLY: Reallocate
USE SwapUtility, ONLY: SWAP_
USE ReverseUtility, ONLY: Reverse

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                     TriangleH1HieFE_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleH1HieFE_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "TriangleH1HieFE_GetLocalElemShapeData()"
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

CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

! HeirarchicalBasis_Triangle3_(&
! order, pe1, pe2, pe3, xij, refTriangle, edgeOrient1, edgeOrient2, &
! edgeOrient3, faceOrient, ans, nrow, ncol)
CALL HeirarchicalBasis_Triangle_( &
  order=obj%cellOrder(1), pe1=obj%faceOrder(1, 1), pe2=obj%faceOrder(1, 2), &
  pe3=obj%faceOrder(1, 3), xij=quad%points(1:quad%txi, 1:nips), &
  refTriangle=obj%refelemDomain, edgeOrient1=obj%faceOrient(1, 1), &
  edgeOrient2=obj%faceOrient(1, 2), edgeOrient3=obj%faceOrient(1, 3), &
  faceOrient=obj%cellOrient, ans=obj%temp(:, :, 1), &
  nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

! order, pe1, pe2, pe3, xij, edgeOrient1, edgeOrient2, edgeOrient3, &
! faceOrient, refTriangle, ans, tsize1, tsize2, tsize3)
CALL HeirarchicalBasisGradient_Triangle_( &
  order=obj%cellOrder(1), pe1=obj%faceOrder(1, 1), pe2=obj%faceOrder(1, 2), &
  pe3=obj%faceOrder(1, 3), xij=quad%points(1:quad%txi, 1:nips), &
  edgeOrient1=obj%faceOrient(1, 1), edgeOrient2=obj%faceOrient(1, 2), &
  edgeOrient3=obj%faceOrient(1, 3), faceOrient=obj%cellOrient, &
  refTriangle=obj%refelemDomain, ans=obj%temp, tsize1=indx(5), &
  tsize2=indx(6), tsize3=indx(7))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TriangleH1HieFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                    TriangleH1HieFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleH1HieFE_GetGlobalElemShapeData
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

CALL Elemsd_Set(obj=elemsd, val=xij(1:nsd, 1:nns), &
                N=geoelemsd%N(1:nns, 1:nips), &
                dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TriangleH1HieFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                TriangleH1HieFE_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleH1HieFE_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "TriangleH1HieFE_GetLocalFacetElemShapeData()"
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj, xidim
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

nips = GetTotalQuadraturePoints(obj=facetQuad)
tdof = obj%faceOrder(1, localFaceNumber) + 1

xidim = obj%xidim - 1
CALL Elemsd_Allocate(obj=facetElemsd, nsd=obj%nsd, xidim=xidim, &
                     nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=facetQuad, weights=facetElemsd%ws, &
                           tsize=nips)

CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

! order, xij, refLine, orient, ans, nrow, ncol
CALL HeirarchicalBasis_Line_( &
  order=obj%faceOrder(1, localFaceNumber), &
  xij=facetQuad%points(1:facetQuad%txi, 1:nips), refLine=obj%refelemDomain, &
  ans=obj%temp(:, :, 1), nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  facetElemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL HeirarchicalBasisGradient_Line_( &
  order=obj%faceOrder(1, localFaceNumber), &
  xij=facetQuad%points(1:facetQuad%txi, 1:nips), refLine=obj%refelemDomain, &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7))

CALL SWAP_(a=facetElemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TriangleH1HieFE_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                TriangleH1HieFE_GetGlobalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE TriangleH1HieFE_GetGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "TriangleH1HieFE_GetGlobalFacetElemShapeData()"
#endif

INTEGER(I4B) :: faceCon(2, 3), nns, nips, nsd, xidim
REAL(DFP) :: line_xij(3, 2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
  elemsd=elemsd, geoElemsd=geoElemsd, xij=xij)

faceCon = FacetConnectivity_Triangle()

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE TriangleH1HieFE_GetGlobalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TriangleH1HierarchicalFEMethods
