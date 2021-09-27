! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

SUBMODULE( BlockNodeField_Class ) IOMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Display
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Display"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Display

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Import
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Import"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Export
  CHARACTER( LEN = * ), PARAMETER :: myName="Block_Export"
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This routine is under condtruction!')
END PROCEDURE Block_Export

END SUBMODULE IOMethods