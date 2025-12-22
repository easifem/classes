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

SUBMODULE(TDGAlgorithm2_Class) IOMethods
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

INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%nrow, "nrow: ", unitno=unitno)
CALL Display(obj%ncol, "ncol: ", unitno=unitno)

nrow = obj%nrow
ncol = obj%ncol

CALL Display(obj%initialGuess(1:nrow + 3), "initialGuess: ", &
             unitno=unitno, advance="NO")
CALL Display(obj%initialGuess_zero(1:nrow + 3), "initialGuess_zero: ", &
             unitno=unitno)

CALL Display(obj%dis(1:nrow + 3), "dis: ", unitno=unitno, advance="NO")
CALL Display(obj%dis_zero(1:nrow + 3), "dis_zero: ", unitno=unitno)

CALL Display(obj%vel(1:nrow + 3), "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero(1:nrow + 3), "vel_zero: ", unitno=unitno)

CALL Display(obj%acc(1:nrow + 3), "acc: ", unitno=unitno, advance="NO")
CALL Display(obj%acc_zero(1:nrow + 3), "acc_zero: ", unitno=unitno)

CALL Display(obj%mt(1:nrow, 1:ncol), "mt: ", unitno=unitno)
CALL Display(obj%ct(1:nrow, 1:ncol), "ct: ", unitno=unitno)
CALL Display(obj%kt(1:nrow, 1:ncol), "kt: ", unitno=unitno)

CALL Display(obj%rhs_m_u1(1:nrow), "rhs_m_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_u1_zero(1:nrow), "rhs_m_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_v1(1:nrow), "rhs_m_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_v1_zero(1:nrow), "rhs_m_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_a1(1:nrow), "rhs_m_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_a1_zero(1:nrow), "rhs_m_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_u1(1:nrow), "rhs_c_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_u1_zero(1:nrow), "rhs_c_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_v1(1:nrow), "rhs_c_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_v1_zero(1:nrow), "rhs_c_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_a1(1:nrow), "rhs_c_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_a1_zero(1:nrow), "rhs_c_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_u1(1:nrow), "rhs_k_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_u1_zero(1:nrow), "rhs_k_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_v1(1:nrow), "rhs_k_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_v1_zero(1:nrow), "rhs_k_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_a1(1:nrow), "rhs_k_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_a1_zero(1:nrow), "rhs_k_a1_zero: ", unitno=unitno)

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
