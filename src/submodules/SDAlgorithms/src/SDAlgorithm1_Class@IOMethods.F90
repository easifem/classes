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

SUBMODULE(SDAlgorithm1_Class) IOMethods
USE Display_Method, ONLY: Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
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
CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%tanmat, "tanmat: ", unitno=unitno, advance="NO")
CALL Display(obj%tanmat_zero, "tanmat_zero: ", unitno=unitno)
CALL Display(obj%rhs_u1, "rhs_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_u1_zero, "rhs_u1_zero: ", unitno=unitno)
CALL Display(obj%rhs_v1, "rhs_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_v1_zero, "rhs_v1_zero: ", unitno=unitno)
CALL Display(obj%rhs_f1, "rhs_f1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_f1_zero, "rhs_f1_zero: ", unitno=unitno)
CALL Display(obj%rhs_f2, "rhs_f2: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_f2_zero, "rhs_f2_zero: ", unitno=unitno)
CALL Display(obj%dis, "dis: ", unitno=unitno, advance="NO")
CALL Display(obj%dis_zero, "dis_zero: ", unitno=unitno)
CALL Display(obj%vel, "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero, "vel_zero: ", unitno=unitno)
CALL Display(obj%initialGuess, "initialGuess: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%initialGuess_zero, "initialGuess_zero: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                            Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
