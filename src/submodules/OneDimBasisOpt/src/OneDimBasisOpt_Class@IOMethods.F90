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

SUBMODULE(OneDimBasisOpt_Class) IOMethods
USE Display_Method, ONLY: Display
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg=msg, unitno=unitno)
CALL Display(obj%firstCall, msg="firstCall: ", unitno=unitno)
CALL Display(obj%tdof, msg="tdof: ", unitno=unitno)
CALL Display(obj%order, msg="order: ", unitno=unitno)
CALL Display(obj%feType, msg="feType: ", unitno=unitno)
CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
CALL Display(obj%beta, msg="beta: ", unitno=unitno)
CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)
CALL Display(obj%baseContinuity, msg="baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, msg="baseInterpolation: ", &
             unitno=unitno)
CALL Display(obj%refelemCoord, msg="refelemCoord: ", unitno=unitno)
CALL Display(TRIM(obj%basisType_char), msg="basisType_char: ", &
             unitno=unitno)
CALL Display(TRIM(obj%ipType_char), msg="ipType_char: ", &
             unitno=unitno)

CALL obj%quadOpt%Display(msg="QuadratureOpt: ", unitno=unitno)

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
