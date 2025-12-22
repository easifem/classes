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

SUBMODULE(OneDimQuadratureOpt_Class) GetMethods
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line, &
                                    QuadraturePoint_Line_
USE ReallocateUtility, ONLY: Reallocate

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

IF (PRESENT(quadratureType)) quadratureType = obj%quadratureType
IF (PRESENT(order)) order = obj%order
IF (PRESENT(nips)) nips = obj%nips(1)
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(refelemCoord)) refelemCoord(1:1, 1:2) = obj%refelemCoord(1:1, 1:2)
IF (PRESENT(refelemDomain)) refelemDomain(1:1) = obj%refelemDomain(1:1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                    GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isNips .OR. obj%isOrder
CALL AssertError1(isok, myName, &
                  'Either nips or order must be specified before '// &
                  'getting total quadrature points.')

isok = .NOT. (obj%isNips .AND. obj%isOrder)
CALL AssertError1(isok, myName, &
                  'Both nips or order cannot be specified')
#endif

IF (obj%isNips) THEN
  ans = obj%nips(1)
ELSE
  ans = QuadratureNumber_Line(order=obj%order, quadType=obj%quadratureType)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                        GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

INTEGER(I4B) :: nips(1), nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = obj%GetTotalQuadraturePoints()

nrow = 2_I4B
quad%txi = 1_I4B
CALL Reallocate(quad%points, nrow, nips(1))

CALL QuadraturePoint_Line_( &
  nips=nips, quadType=obj%quadratureType, layout="INCREASING", &
  xij=obj%refelemCoord(1:1, 1:2), alpha=obj%alpha, &
  beta=obj%beta, lambda=obj%lambda, ans=quad%points, &
  nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

