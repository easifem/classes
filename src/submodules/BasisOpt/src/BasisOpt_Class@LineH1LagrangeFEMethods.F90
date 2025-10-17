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

SUBMODULE(BasisOpt_Class) LineH1LagrangeFEMethods
USE Display_Method, ONLY: ToString, Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Set => Set, &
                                LagrangeFacetElemShapeData, &
                                Elemsd_Allocate => ALLOCATE

USE LineInterpolationUtility, ONLY: LagrangeDOF_Line, &
                                    InterpolationPoint_Line_, &
                                    LagrangeEvalAll_Line_, &
                                    LagrangeGradientEvalAll_Line_

USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_

USE ReallocateUtility, ONLY: Reallocate

USE SwapUtility, ONLY: SWAP_, SWAP
USE ReverseUtility, ONLY: Reverse

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                           LineH1LagFE_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1LagFE_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LineH1LagFE_GetLocalElemShapeData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
tdof = obj%GetTotalDOF()

#ifdef DEBUG_VER
isok = tdof .GT. 0
CALL AssertError1(isok, myName, &
                  "LagrangeDOF_Line returned zero DOF")
#endif

CALL Elemsd_Allocate(obj=elemsd, nsd=obj%nsd, xidim=obj%xidim, &
                     nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

CALL InterpolationPoint_Line_( &
  order=obj%order, ipType=obj%ipType, layout="VEFC", &
  xij=obj%refelemCoord(1:obj%xidim, 1:2), &
  alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=obj%xij, nrow=indx(1), ncol=indx(2))

CALL LagrangeEvalAll_Line_( &
  order=obj%order, xij=obj%xij(1:indx(1), 1:indx(2)), &
  x=quad%points(1:quad%txi, 1:nips), &
  coeff=obj%coeff(1:tdof, 1:tdof), firstCall=obj%firstCall, &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=obj%temp(:, :, 1), nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL LagrangeGradientEvalAll_Line_( &
  order=obj%order, x=quad%points(1:quad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7), &
  coeff=obj%coeff(1:tdof, 1:tdof), firstCall=.FALSE., &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LineH1LagFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                          LineH1LagFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1LagFE_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

INTEGER(I4B) :: nns, nips, nsd, xidim, n1, n2
LOGICAL(LGT) :: isok

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

isok = (nns .NE. elemsd%nns) .AND. obj%isCellOrient .AND. &
       (obj%cellOrient(1) .EQ. -1_I4B) .AND. &
       (elemsd%nns .GT. 3_I4B)

n1 = 3
n2 = obj%order + 1
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

END PROCEDURE LineH1LagFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                     LineH1LagFE_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1LagFE_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LineH1LagFE_GetLocalFacetElemShapeData()"
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
END PROCEDURE LineH1LagFE_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LineH1LagrangeFEMethods
