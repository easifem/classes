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

SUBMODULE( mshFormat_Class ) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_display
  INTEGER( I4B ) :: I
  I = Input( Option=unitNo, Default=stdout )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  WRITE( I, "(A)" ) TRIM( obj%meshFormat )
END PROCEDURE fmt_display

!----------------------------------------------------------------------------
!                                                                 getVersion
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getVersion
  Ans = obj%version
END PROCEDURE fmt_getVersion

!----------------------------------------------------------------------------
!                                                            getMajorVersion
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getMajorVersion
  Ans = obj%majorVersion
END PROCEDURE fmt_getMajorVersion

!----------------------------------------------------------------------------
!                                                            getMinorVersion
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getMinorVersion
  Ans = obj%minorVersion
END PROCEDURE fmt_getMinorVersion

!----------------------------------------------------------------------------
!                                                                getFileType
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getFileType
  Ans=obj%fileType
END PROCEDURE fmt_getFileType

!----------------------------------------------------------------------------
!                                                                getDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getDataSize
  Ans = obj%dataSize
END PROCEDURE fmt_getDataSize

!----------------------------------------------------------------------------
!                                                             getMeshFormat
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getMeshFormat
  Ans = obj%meshFormat
END PROCEDURE fmt_getMeshFormat

!----------------------------------------------------------------------------
!                                                              ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_Read
  INTEGER( I4B ) :: unitNo
  CHARACTER( LEN = * ), PARAMETER :: myName = "fmt_Read"

  CALL obj%GotoTag( mshFile, error )
  IF( error .EQ. 0 ) THEN
    unitNo = mshFile%getUnitNo()
    READ( unitNo, * ) obj%version, obj%fileType, obj%dataSize
    obj%majorVersion = INT(obj%version)
    obj%minorVersion = INT( 10*(obj%version - obj%majorVersion) )
    obj%meshFormat = TRIM(str( obj%majorVersion, .TRUE. )) // "." // TRIM(str(obj%minorVersion, .TRUE.)) // " " // &
      & TRIM( INT2STR( obj%fileType ) ) // " " &
      & // TRIM( INT2STR( obj%dataSize ) )
    IF( obj%fileType .EQ. 1 ) THEN
      obj%isASCII = .FALSE.
    ELSE
      obj%isASCII = .TRUE.
    END IF
  ELSE
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Could not read mesh format from mshFile !')
  END IF
END PROCEDURE fmt_Read

!----------------------------------------------------------------------------
!                                                               WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_Write
  ! Define internal variables
  TYPE( TxtFile_ ) :: outFile
  TYPE( String ) :: path, filename, ext
  INTEGER( I4B ) :: unitNo

  CALL mshFile%getFileParts(path, filename, ext)
  CALL outFile%initiate(file = path // filename // "_Format" // ext, action = "WRITE" )
  CALL outFile%open()
  unitNo = outFile%getUnitNo()
  IF( PRESENT( Str ) ) THEN
    WRITE( unitNo, "( A )") TRIM( Str )
  END IF
  WRITE( unitNo, "( A )") TRIM( obj%meshFormat )
  IF( PRESENT( EndStr ) ) THEN
    WRITE( unitNo, "( A )") TRIM( EndStr )
  END IF
  CALL outFile%close()
  CALL outFile%DeallocateData()
END PROCEDURE fmt_Write

!----------------------------------------------------------------------------
!                                                                   GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_GotoTag
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen, unitNo
  CHARACTER( LEN = 100 ) :: Dummy
  CHARACTER( LEN = * ), PARAMETER :: myName = "fmt_GotoTag"
  !
  ! Find $meshFormat

  IF( .NOT. mshFile%isOpen() .OR. .NOT. mshFile%isRead() ) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'mshFile is either not opened or does not have read access!')
    error = -1
  ELSE
    Reopen = 0; error = 0; CALL mshFile%Rewind()
    DO
      unitNo = mshFile%getUnitNo()
      READ( unitNo, "(A)", IOSTAT = IOSTAT ) Dummy
      IF( IS_IOSTAT_END( IOSTAT ) ) THEN
        CALL mshFile%setEOFStat( .TRUE. )
        Reopen = Reopen + 1
      END IF
      IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
        CALL e%raiseError(modName//'::'//myName//' - '// &
        & 'Could not find $MeshFormat!')
        error = -2
        EXIT
      ELSE IF( TRIM( Dummy ) .EQ. '$MeshFormat' ) THEN
        EXIT
      END IF
    END DO
  END IF
END PROCEDURE fmt_GotoTag

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_Finalize
SELECT TYPE( obj )
TYPE IS (mshFormat_)
  obj = TypemshFormat
END SELECT
END PROCEDURE fmt_Finalize

END SUBMODULE Methods