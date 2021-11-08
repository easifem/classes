! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
! The FLAP lib is located at https://github.com/szaghi/FLAP
! I have made minor changes (mostly renaming)
! Most of the functionality is kept the same
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
!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-08
! update: 2021-11-08
! summary: Fortran command Line Arguments Parset for poor People

MODULE CommandLine_Method
USE flap_command_line_argument_t, ONLY: command_line_argument
USE flap_command_line_arguments_group_t, ONLY: command_line_arguments_group
USE flap_command_line_interface_t, ONLY : command_line_interface, ERROR_UNKNOWN_CLAS_IGNORED
IMPLICIT NONE
PRIVATE
PUBLIC :: command_line_argument
PUBLIC :: command_line_arguments_group
PUBLIC :: command_line_interface
PUBLIC :: ERROR_UNKNOWN_CLAS_IGNORED
ENDMODULE CommandLine_Method
