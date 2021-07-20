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

SUBMODULE( STVectorField_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Display
  INTEGER( I4B ) :: I
  I = Input( option=unitNo, default=stdout )

  IF( LEN_TRIM( msg) .NE. 0 ) WRITE( I, "(A)") "# "//TRIM( msg )
  CALL Display( obj%name, "# name : ")
  IF( obj%isInitiated ) THEN
    WRITE( I, "(A)" ) "# isInitiated : TRUE"
  ELSE
    WRITE( I, "(A)" ) "# isInitiated : FALSE"
  END IF
  CALL Display( obj%spaceCompo, "# space components : " )
  CALL Display( obj%timeCompo, "# space components : " )
  CALL Display( obj%tSize, "# tSize : " )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    WRITE( I, "(A)" ) "# fieldType : constant"
  ELSE
    WRITE( I, "(A)" ) "# fieldType : normal"
  END IF
  IF( ASSOCIATED( obj%domain )  ) THEN
    WRITE( I, "(A)" ) "# domain : associated"
  ELSE
    WRITE( I, "(A)" ) "# domain : not associated"
  END IF
  CALL Display( obj%realVec, obj%dof, msg="# realVec : ", unitNo=I )
END PROCEDURE stvField_Display

END SUBMODULE IO