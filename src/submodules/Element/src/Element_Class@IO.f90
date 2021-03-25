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

SUBMODULE( Element_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE s_display_Obj
  INTEGER( I4B ) :: I
  !
  I = INPUT( option=UnitNo, Default=stdout )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( Msg )
  END IF
  !
  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "  TYPE : ELEMENT_"
  WRITE( I, "(A, I4)" ) "  MAT-TYPE : ", Obj%Mat_Type
  CALL Display( Obj%Nptrs, "  NPTRS : ")
  CALL Blanklines( NOL = 1, UnitNo = I )
  !
  IF( PRESENT( FullDisp ) ) THEN
    IF( FullDisp ) THEN
      IF( ASSOCIATED( Obj%RefElem ) ) THEN
        CALL Display( Obj%RefElem, "# Reference Element", I )
      END IF
    END IF
  ELSE
    RETURN
  END IF
END PROCEDURE s_display_Obj

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE m_display_obj
  CALL Display( Obj = Obj, Msg = Msg, UnitNo = UnitNo, FullDisp = FullDisp )
END PROCEDURE m_display_obj

END SUBMODULE IO