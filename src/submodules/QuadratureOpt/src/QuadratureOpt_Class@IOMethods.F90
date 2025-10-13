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

SUBMODULE(QuadratureOpt_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE QuadraturePoint_Method, ONLY: QuadraturePoint_ToChar, &
                                  QuadraturePoint_ToInteger, &
                                  QuadraturePoint_Initiate => Initiate, &
                                  GetTotalQuadraturePoints, &
                                  InitiateFacetQuadrature
USE InputUtility, ONLY: Input
USE BaseType, ONLY: TypeElemNameOpt

USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_, &
                                    QuadratureNumber_Line

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "isInit: ", unitNo=unitNo)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isHomogeneous, "isHomogeneous: ", unitNo=unitNo)
CALL Display(obj%isOrder, "isOrder: ", unitNo=unitNo)
CALL Display(obj%isNips, "isNips: ", unitNo=unitNo)

CALL Display(obj%topoType, "topoType: ", unitNo=unitNo)
CALL Display(obj%nsd, "nsd: ", unitNo=unitNo)
CALL Display(obj%xidim, "xidim: ", unitNo=unitNo)

DO ii = 1, 3
  CALL Display(obj%quadratureType(ii), &
               "quadratureType("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(TRIM(obj%quadratureType_char(ii)), &
               "quadratureType_char("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%alpha(ii), "alpha("//ToString(ii)//"): ", unitNo=unitNo)

  CALL Display(obj%beta(ii), "beta("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%lambda(ii), "lambda("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%order(ii), "order("//ToString(ii)//"): ", &
               unitNo=unitNo)

  CALL Display(obj%nips(ii), "nips("//ToString(ii)//', 1): ', &
               unitNo=unitNo)
END DO

CALL Display(obj%refelemCoord, "refelemCoord: ", unitNo=unitNo)

CALL Display(obj%refelemDomain, "refelemDomain: ", unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                              Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
