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
                                Elemsd_Allocate => ALLOCATE

USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle, &
                                          InterpolationPoint_Quadrangle_, &
                                          LagrangeEvalAll_Quadrangle_, &
                                          LagrangeGradientEvalAll_Quadrangle_

USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_

USE ReallocateUtility, ONLY: Reallocate

USE SwapUtility, ONLY: SWAP_

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
!                                                        QuadrangleH1LagFE_SetOrder
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
!                                     QuadrangleH1LagFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadrangleH1LagFE_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName= "QuadrangleH1LagFE_GetGlobalElemShapeData()"
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
END PROCEDURE QuadrangleH1LagFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE QuadrangleH1LagrangeFEMethods
