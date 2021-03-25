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

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	IO methods for FacetElement

SUBMODULE( FacetElement_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS



MODULE PROCEDURE s_display_Obj
  INTEGER( I4B ) :: I

  I = INPUT( Option = UnitNo, Default = stdout )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( Msg )
  END IF
  CALL Blanklines( NOL = 1, UnitNo = I )
  WRITE( I, "(A)" ) "  TYPE : FACETELEMENT_"
  WRITE( I, "(A, I4)" ) "  MAT-TYPE : ", Obj % Mat_Type
  CALL Display( Obj % Nptrs, "  NPTRS : ")

  IF( ASSOCIATED( Obj % Cell ) ) THEN
    WRITE( I, "(A)" ) "  INNER CELL : ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "  INNER CELL : NOT ASSOCIATED"
  END IF

  IF( ASSOCIATED( Obj % OuterCell ) ) THEN
    WRITE( I, "(A)" ) "  OUTER CELL : ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "  OUTER CELL : NOT ASSOCIATED"
  END IF

  CALL Blanklines( NOL = 1, UnitNo = I )
  !
  IF( PRESENT( FullDisp ) ) THEN
    IF( FullDisp ) THEN
      IF( ASSOCIATED( Obj % RefElem ) ) THEN
        CALL Display( Obj % RefElem, "Reference Element", I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( Obj % Cell ) ) THEN
        CALL Display( Obj % Cell, "Inner Cell", I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( Obj % OuterCell ) ) THEN
        CALL Display( Obj % OuterCell, "Outer Cell", I )
      END IF
    END IF
  END IF
END PROCEDURE s_display_Obj

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE m_display_obj
  INTEGER( I4B ) :: i
  LOGICAL :: full

  SELECT TYPE( Obj )
  TYPE IS ( FacetElement_ )
    CALL Display( Obj = Obj, Msg = Msg, UnitNo = UnitNo, FullDisp = FullDisp)
  END SELECT
END PROCEDURE m_display_obj

END SUBMODULE IO