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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Block_Display
  INTEGER( I4B ) :: ii
  IF( LEN_TRIM( msg) .NE. 0 ) THEN
    CALL Display( "# "//TRIM( msg ), unitNo=unitNo )
  END IF
  !>
  IF( obj%isInitiated ) THEN
    CALL Display( "# isInitiated : TRUE", unitNo=unitNo )
  ELSE
    CALL Display( "# isInitiated : FALSE, Nothing to Display!", &
      & unitNo=unitNo )
    RETURN
  END IF
  CALL Display( "# engine : NATIVE_SERIAL", unitNo=unitNo )
  CALL Display( obj%name, "# name : ", unitNo=unitNo )
  CALL Display( obj%tSize, "# tSize : ", unitNo=unitNo )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL Display( "# fieldType : CONSTANT", unitNo=unitNo)
  ELSE
    CALL Display( "# fieldType : NORMAL", unitNo=unitNo)
  END IF
  IF( ASSOCIATED( obj%domain )  ) THEN
    CALL Display( "# domain : ASSOCIATED", unitNo=unitNo )
  ELSE
    CALL Display( "# domain : NOT ASSOCIATED", unitNo=unitNo )
  END IF
  IF( ALLOCATED( obj%domains )  ) THEN
    CALL Display( "# domains : ALLOCATED", unitNo=unitNo )
    DO ii = 1, SIZE(obj%domains)
      IF( ASSOCIATED( obj%domains(ii)%ptr )  ) THEN
        CALL Display( "# domains("//TOSTRING(ii)//"): ASSOCIATED", &
          & unitNo=unitNo )
      ELSE
        CALL Display( "# domains("//TOSTRING(ii)//"): NOT ASSOCIATED", &
          & unitNo=unitNo )
      END IF
    END DO
  ELSE
    CALL Display( "# domains : NOT ALLOCATED", unitNo=unitNo )
  END IF
  CALL Display( obj%realVec, obj%dof, msg="# realVec : ", unitNo=unitNo )
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