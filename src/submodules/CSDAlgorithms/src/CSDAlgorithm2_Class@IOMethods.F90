! This program is a part of EASIFEM library
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

SUBMODULE(CSDAlgorithm2_Class) IOMethods

USE Display_Method, ONLY: Display, &
                          EqualLine, &
                          BlankLines, &
                          tostring
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
INTEGER(I4B) :: ii

CALL Display(msg, unitno=unitno)
CALL EqualLine(unitno=unitno)
CALL BlankLines(unitno=unitno)

DO ii = 1, SIZE(obj%substeps)
  IF (ASSOCIATED(obj%substeps(ii)%ptr)) THEN
    CALL obj%substeps(ii)%ptr%Display(msg="substep "//tostring(ii), &
                                      unitno=unitno)
  END IF
END DO

CALL BlankLines(unitno=unitno)
CALL Display(obj%splitRatios, "splitRatios: ", unitno=unitno, advance="NO")

CALL BlankLines(unitno=unitno)
CALL Display(obj%rhs_subsol, "rhs_subsol: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_subsol_zero, "rhs_subsol_zero: ", unitno=unitno)

CALL BlankLines(unitno=unitno)
CALL Display(obj%rhs_subf, "rhs_subf: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_subf_zero, "rhs_subf_zero: ", unitno=unitno)

CALL BlankLines(unitno=unitno)
CALL Display(obj%dis_subsol, "dis_subsol: ", unitno=unitno, advance="NO")
CALL Display(obj%dis_subsol_zero, "dis_subsol_zero: ", unitno=unitno)

CALL BlankLines(unitno=unitno)
CALL Display(obj%vel_subsol, "vel_subsol: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_subsol_zero, "vel_subsol_zero: ", unitno=unitno)

CALL BlankLines(unitno=unitno)
CALL Display(obj%acc_subsol, "acc_subsol_zero: ", unitno=unitno,  &
            & advance="NO")
CALL Display(obj%acc_subsol_zero, "acc_subsol_zero: ", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
