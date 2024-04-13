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

SUBMODULE(FacetElement_Class) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_display
  INTEGER( I4B ) :: I
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  CLASS( ReferenceElement_ ), POINTER :: refelem

  I = INPUT( Option = UnitNo, Default = stdout )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( Msg )
  END IF
  WRITE( I, "(A)" ) "# TYPE : FACETELEMENT_"
  WRITE( I, "(A, I4)" ) "# MAT-TYPE : ", obj%getMaterialType()
  nptrs = obj%getNptrs()
  IF( SIZE(nptrs) .NE. 0 ) THEN
    CALL Display( nptrs, "# NPTRS : ")
  END IF
  IF( ASSOCIATED( obj%Cell ) ) THEN
    WRITE( I, "(A)" ) "# INNER CELL : ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "# INNER CELL : NOT ASSOCIATED"
  END IF
  IF( ASSOCIATED( obj%OuterCell ) ) THEN
    WRITE( I, "(A)" ) "# OUTER CELL : ASSOCIATED"
  ELSE
    WRITE( I, "(A)" ) "# OUTER CELL : NOT ASSOCIATED"
  END IF
  CALL Blanklines( NOL = 1, UnitNo = I )
  IF( PRESENT( FullDisp ) ) THEN
    IF( FullDisp ) THEN
      refelem => obj%getRefElemPointer()
      IF( ASSOCIATED( refelem ) ) THEN
        CALL Display( refelem, "Reference Element", I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( obj%Cell ) ) THEN
        CALL obj%Cell%Display( msg="Inner Cell", unitNo=I )
      END IF
      !
      CALL Blanklines( NOL = 1, UnitNo = I )
      IF( ASSOCIATED( obj%OuterCell ) ) THEN
        CALL obj%OuterCell%Display( msg="Outer Cell", unitNo=I )
      END IF
    END IF
  END IF
  NULLIFY( refelem )
  IF( ALLOCATED( nptrs ) ) DEALLOCATE( nptrs )
END PROCEDURE faceElem_display

END SUBMODULE IO