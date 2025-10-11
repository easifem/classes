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
USE Display_Method, ONLY: ToString
USE LagrangePolynomialUtility, ONLY: LagrangeDOF

USE BasisOptUtility, ONLY: SetIntegerType

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

REAL(DFP), ALLOCATABLE :: xij(:, :), coeff0(:, :), temp(:, :, :)
INTEGER(I4B) :: ipType0, basisType0, nips, nns, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
nns = LagrangeDOF_Line(order=obj%order)

#ifdef DEBUG_VER
isok = nns .GT. 0
CALL AssertError1(isok, myName, &
                  "LagrangeDOF_Line returned zero DOF")
#endif

CALL Elemsd_Allocate(obj=elemsd, nsd=obj%nsd, xidim=obj%xidim, &
                     nns=nns, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

! TODO: Reallocate if necessary
ALLOCATE (xij(3, nns), temp(nips, nns, 3))

! CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
!                            xidim=obj%xidim, elemType=obj%elemType, &
!                            refelemCoord=obj%refelemCoord, &
!                            domainName=obj%refelemDomain, &
!                            order=obj%order, &
!                            ipType=obj%ipType, &
!                            basisType=obj%basisType(1), &
!                            coeff=obj%coeff, firstCall=obj%firstCall, &
!                            alpha=obj%alpha(1), beta=obj%beta(1), &
!                            lambda=obj%lambda(1))

! CALL InterpolationPoint_Line_(order=order, ipType=ipType, ans=ans, &
!      nrow=nrow, ncol=ncol, xij=xij, layout=layout, alpha=alpha, beta=beta, &
!                               lambda=lambda)

CALL InterpolationPoint_Line_( &
  order=obj%order, ipType=obj%ipType, layout="VEFC", &
  xij=obj%refelemCoord(1:obj%xidim, :), &
  alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=xij, nrow=indx(1), ncol=indx(2))

CALL LagrangeEvalAll_Line_( &
order=obj%order, xij=xij(1:obj%xidim, :), x=quad%points(1:quad%txi, 1:nips), &
  coeff=obj%coeff(1:nns, 1:nns), firstCall=obj%firstCall, &
  basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
  lambda=obj%lambda(1), ans=temp(:, :, 1), nrow=indx(1), ncol=indx(2))

DO CONCURRENT(ii=1:nns, jj=1:nips)
  elemsd%N(ii, jj) = temp(jj, ii, 1)
END DO

! CALL LagrangeGradientEvalAll_Line_( &
!   order=obj%order, x=quad%points(1:quad%txi, 1:nips), &
!   xij=xij(1:obj%xidim, :), domainName=obj%refelemDomain, &
!   basisType=obj%basisType(1), alpha=obj%alpha(1), beta=obj%beta(1), &
!   lambda=obj%lambda(1), coeff=obj%coeff(1:nns, 1:nns), &
!   firstCall=.FALSE., ans=temp, dim1=indx(1), dim2=indx(2), dim3=indx(3))
! CALL SWAP_(a=obj%dNdXi, b=temp(1:indx(1), 1:indx(2), 1:indx(3)), i1=2, &
!            i2=3, i3=1)

IF (ALLOCATED(temp)) DEALLOCATE (temp)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(coeff0)) DEALLOCATE (coeff0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LineH1LagFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LineH1LagrangeFEMethods
