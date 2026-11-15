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

SUBMODULE(BasisOpt_Class) QuadrangleH1HierarchicalFEMethods
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE ElemshapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE
USE ElemshapeData_Method, ONLY: Elemsd_Set => Set
USE ElemshapeData_Method, ONLY: Elemsd_SetNormal => SetNormal
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData
USE ElemshapeData_Method, ONLY: HierarchicalFacetElemShapeData
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
USE QuadrangleInterpolationUtility, ONLY: FacetConnectivity_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: HeirarchicalBasis_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: &
  HeirarchicalBasisGradient_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
USE QuadraturePoint_Method, ONLY: GetQuadratureWeights_
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE ReallocateUtility, ONLY: Reallocate
USE ReverseUtility, ONLY: Reverse
USE SwapUtility, ONLY: SWAP_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "BasisOpt_Class@QuadrangleH1HierarchicalFEMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                     QuadrangleH1HieFE_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1HieFE_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "QuadrangleH1HieFE_GetLocalElemShapeData()"
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

! HeirarchicalBasis_Quadrangle3_( &
! pb, qb, pe3, pe4, qe1, qe2, xij, pe3Orient, pe4Orient, qe1Orient, &
! qe2Orient, faceOrient, ans, nrow, ncol)
CALL HeirarchicalBasis_Quadrangle_( &
  pb=obj%cellOrder(1), qb=obj%cellOrder(2), pe3=obj%faceOrder(1, 1), &
  pe4=obj%faceOrder(1, 3), qe1=obj%faceOrder(1, 4), qe2=obj%faceOrder(1, 2), &
  xij=quad%points(1:quad%txi, 1:nips), &
  pe3Orient=obj%faceOrient(1, 1), pe4Orient=obj%faceOrient(1, 3), &
  qe1Orient=obj%faceOrient(1, 4), qe2Orient=obj%faceOrient(1, 2), &
  faceOrient=obj%cellOrient, ans=obj%temp(:, :, 1), nrow=indx(3), &
  ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL HeirarchicalBasisGradient_Quadrangle_( &
  pb=obj%cellOrder(1), qb=obj%cellOrder(2), pe3=obj%faceOrder(1, 1), &
  pe4=obj%faceOrder(1, 3), qe1=obj%faceOrder(1, 4), qe2=obj%faceOrder(1, 2), &
  xij=quad%points(1:quad%txi, 1:nips), pe3Orient=obj%faceOrient(1, 1), &
  pe4Orient=obj%faceOrient(1, 3), qe1Orient=obj%faceOrient(1, 4), &
  qe2Orient=obj%faceOrient(1, 2), faceOrient=obj%cellOrient, &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1HieFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                    QuadrangleH1HieFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1HieFE_GetGlobalElemShapeData
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
END PROCEDURE QuadrangleH1HieFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                QuadrangleH1HieFE_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1HieFE_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "QuadrangleH1HieFE_GetLocalFacetElemShapeData()"
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj, xidim
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

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
  ans=obj%temp(:, :, 1), nrow=indx(3), ncol=indx(4), &
  orient=obj%faceOrient(1, localFaceNumber))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  facetElemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL HeirarchicalBasisGradient_Line_( &
  order=obj%faceOrder(1, localFaceNumber), &
  xij=facetQuad%points(1:facetQuad%txi, 1:nips), refLine=obj%refelemDomain, &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7), &
  orient=obj%faceOrient(1, localFaceNumber))

CALL SWAP_(a=facetElemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1HieFE_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                               QuadrangleH1HieFE_GetGlobalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1HieFE_GetGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = &
                           "QuadrangleH1HieFE_GetGlobalFacetElemShapeData()"
#endif

INTEGER(I4B) :: faceCon(2, 4), nns, nips, nsd, xidim
REAL(DFP) :: line_xij(3, 2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%QuadrangleH1HieFE_GetGlobalElemShapeData( &
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE QuadrangleH1HieFE_GetGlobalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE QuadrangleH1HierarchicalFEMethods
