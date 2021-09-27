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
!

SUBMODULE( Vector_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_Display
  INTEGER( I4B ) :: I
  I = Input( option=unitNo, default=stdout )
  IF( LEN_TRIM( msg) .NE. 0 ) WRITE( I, "(A)") "# "//TRIM( msg )
  IF( obj%isInitiated ) THEN
    WRITE( I, "(A)" ) "# isInitiated : TRUE"
  ELSE
    WRITE( I, "(A)" ) "# isInitiated : FALSE"
  END IF
  CALL Display( obj%tSize, "# tSize : " )
  CALL Display( obj%realVec, "# realVec : " )
  CALL Display( obj%dof, "# dof : " )
END PROCEDURE vec_Display

END SUBMODULE IO