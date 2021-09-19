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

SUBMODULE( mshNodes_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Final
  CALL obj%DeallocateData()
END PROCEDURE n_Final

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE n_deallocatedata
SELECT TYPE (obj)
TYPE IS (mshNodes_)
  obj = TypeMSHNodes
END SELECT
END PROCEDURE n_deallocatedata

!----------------------------------------------------------------------------
!                                                                    GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_GotoTag
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen, unitNo
  CHARACTER( LEN = 100 ) :: Dummy
  CHARACTER( LEN = * ), PARAMETER :: myName = "n_GotoTag"
  !
  ! Find $meshFormat

  IF( .NOT. mshFile%isOpen() .OR. .NOT. mshFile%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'mshFile is either not opened or does not have read access!')
    error = -1
  ELSE
    Reopen = 0
    error = 0
    DO
      unitNo = mshFile%getUnitNo()
      READ( unitNo, "(A)", IOSTAT = IOSTAT ) Dummy
      IF( mshFile%isEOF() ) THEN
        CALL mshFile%Rewind()
        Reopen = Reopen + 1
      END IF
      IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
        CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'Could not find $Nodes !')
        error = -2
        EXIT
      ELSE IF( TRIM( Dummy ) .EQ. '$Nodes' ) THEN
        EXIT
      END IF
    END DO
  END IF
END PROCEDURE n_GotoTag

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Read
  CALL obj%GotoTag( mshFile, error )
  IF( error .EQ. 0 ) THEN
    IF( mshFormat%getMajorVersion() .GT. 2 ) THEN
      READ( mshFile%getUnitNo(), * ) obj%numEntityBlocks, obj%numNodes, &
        & obj%minNodeTag, obj%maxNodeTag
      IF( ( obj%maxNodeTag - obj%minNodeTag ) &
        & .EQ. ( obj%numNodes - 1 ) ) THEN
        obj%isSparse = .FALSE.
      ELSE
        obj%isSparse = .TRUE.
      END IF
    ELSE
      READ( mshFile%getUnitNo(), * ) obj%numNodes
    END IF
  END IF
END PROCEDURE n_Read

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE n_write

END PROCEDURE n_write

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE n_Display
  ! Define internal variables
  INTEGER( I4B ) :: I
  !
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  !
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  !
  CALL BlankLines( UnitNo = I, NOL = 1 )
  WRITE( I, "(A)" ) "| Property | Value |"
  WRITE( I, "(A)" ) "| :----    | ---:  |"
  WRITE( I, "(A, I4, A)" ) "| Total Nodes    | ", obj%NumNodes, " | "
  WRITE( I, "(A, I4, A)" ) "| Total Entities | ", obj%NumEntityBlocks, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Min Node Tag   | ", obj%minNodeTag, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Max Node Tag   | ", obj%maxNodeTag, &
    & " | "
  WRITE( I, "(A, G5.2, A)" ) "| isSparse       | ", obj%isSparse, &
    & " | "
END PROCEDURE n_Display

!----------------------------------------------------------------------------
!                                                        getNumEntityBlocks
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getNumEntityBlocks
  ans = obj%NumEntityBlocks
END PROCEDURE n_getNumEntityBlocks

!----------------------------------------------------------------------------
!                                                                getNumNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getNumNodes
  ans = obj%NumNodes
END PROCEDURE n_getNumNodes

!----------------------------------------------------------------------------
!                                                            getMaxNodeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getMaxNodeTag
  ans = obj%maxNodeTag
END PROCEDURE n_getMaxNodeTag

!----------------------------------------------------------------------------
!                                                            getMinNodeTag
!----------------------------------------------------------------------------

MODULE PROCEDURE n_getMinNodeTag
  ans = obj%MinNodeTag
END PROCEDURE n_getMinNodeTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
