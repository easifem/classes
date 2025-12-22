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

SUBMODULE(BasisOpt_Class) LineH1HierarchicalFEMethods
USE Display_Method, ONLY: ToString, Display

USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                HierarchicalFacetElemShapeData, &
                                Elemsd_Allocate => ALLOCATE

USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_, &
                                    HeirarchicalBasisGradient_Line_

USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints, &
                                  GetQuadratureWeights_

USE ReallocateUtility, ONLY: Reallocate

USE SwapUtility, ONLY: SWAP_, SWAP
USE ReverseUtility, ONLY: Reverse

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                           LineH1HieFE_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1HieFE_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LineH1HieFE_GetLocalElemShapeData()"
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
CALL AssertError1(isok, myName, "zero tdof is found")
#endif

CALL Elemsd_Allocate(obj=elemsd, nsd=obj%nsd, xidim=obj%xidim, &
                     nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

CALL HeirarchicalBasis_Line_( &
  order=obj%cellOrder(1), xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, orient=obj%cellOrient(1), &
  ans=obj%temp(:, :, 1), nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL HeirarchicalBasisGradient_Line_( &
  order=obj%cellOrder(1), xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, orient=obj%cellOrient(1), &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE LineH1HieFE_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                          LineH1HieFE_GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1HieFE_GetGlobalElemShapeData
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

END PROCEDURE LineH1HieFE_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                     LineH1HieFE_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE LineH1HieFE_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "LineH1HieFE_GetLocalFacetElemShapeData()"
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
END PROCEDURE LineH1HieFE_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LineH1HierarchicalFEMethods
