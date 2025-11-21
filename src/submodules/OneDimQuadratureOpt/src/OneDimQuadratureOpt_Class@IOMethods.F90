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

SUBMODULE(OneDimQuadratureOpt_Class) IOMethods
USE Display_Method, ONLY: Display
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%quadratureType, "quadratureType: ", unitNo)
CALL Display(TRIM(obj%quadratureType_char), "quadratureType_char: ", unitNo)
CALL Display(obj%alpha, "alpha: ", unitNo)
CALL Display(obj%beta, "beta: ", unitNo)
CALL Display(obj%lambda, "lambda: ", unitNo)
CALL Display(obj%order, "order: ", unitNo)
CALL Display(obj%nips(1), "nips(1): ", unitNo)
CALL Display(obj%refelemCoord, "refelemCoord: ", unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods

