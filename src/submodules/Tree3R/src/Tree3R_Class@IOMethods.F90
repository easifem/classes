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

SUBMODULE(Tree3R_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE TR_Display
  !!
  CALL Display( msg, unitno=unitno )
  CALL Display( obj%n, "obj%n=", unitno=unitno )
  CALL Display( obj%leftCoeff, "leftCoeff=", unitno=unitno)
  CALL Display( obj%rightCoeff, "rightCoeff=", unitno=unitno)
  !!
  IF( ASSOCIATED( obj%left ) ) CALL obj%left%Display( msg="left=", &
    & unitno=unitno )
  IF( ASSOCIATED( obj%right ) ) CALL obj%right%Display( msg="right=", &
    & unitno=unitno )
  !!
END PROCEDURE TR_Display

END SUBMODULE IOMethods