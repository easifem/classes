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
  I = Input( Option=UnitNo, Default=stdout )
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  WRITE( I, "(A)" ) TRIM( obj%MeshFormat )
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
  Ans = obj%MajorVersion
END PROCEDURE fmt_getMajorVersion

!----------------------------------------------------------------------------
!                                                            getMinorVersion
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getMinorVersion
  Ans = obj%MinorVersion
END PROCEDURE fmt_getMinorVersion

!----------------------------------------------------------------------------
!                                                                getFileType
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getFileType
  IF( obj%isASCII ) THEN
    Ans = "ASCII "
  ELSE
    Ans = "BINARY"
  END IF
END PROCEDURE fmt_getFileType

!----------------------------------------------------------------------------
!                                                                getDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getDataSize
  Ans = obj%DataSize
END PROCEDURE fmt_getDataSize

!----------------------------------------------------------------------------
!                                                             getMeshFormat
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_getMeshFormat
  Ans = obj%MeshFormat
END PROCEDURE fmt_getMeshFormat

!----------------------------------------------------------------------------
!                                                              ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_ReadFromFile
  INTEGER( I4B ) :: unitNo
  CALL obj%GotoTag( mshFile, ierr )
  IF( .NOT. ierr ) THEN
    unitNo = mshFile%getUnitNo()
    READ( UnitNo, * ) obj%Version, obj%FileType, obj%DataSize
    obj%MajorVersion = INT(obj%Version)
    obj%MinorVersion = INT( 10*(obj%version - obj%MajorVersion) )
    obj%MeshFormat = TRIM( REAL2STR( obj%Version ) ) // " " // &
      & TRIM( INT2STR( obj%FileType ) ) // " " &
      & // TRIM( INT2STR( obj%DataSize ) )
    IF( obj%FileType .EQ. 1 ) THEN
      obj%isASCII = .FALSE.
    ELSE
      obj%isASCII = .TRUE.
    END IF
  END IF
END PROCEDURE fmt_ReadFromFile

!----------------------------------------------------------------------------
!                                                               WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_WriteToFile
  ! ! Define internal variables
  ! TYPE( File_ ) :: outFile
  ! CALL OpenFileToWrite( outFile, mshFile%Path%Raw, &
  !   & TRIM( mshFile%FileName%Raw )//"_Format", &
  !   & mshFile%Extension%Raw )
  ! IF( PRESENT( Str ) ) THEN
  !   WRITE( outFile%UnitNo, "( A )") TRIM( Str )
  ! END IF
  ! WRITE( outFile%UnitNo, "( A )") TRIM( obj%MeshFormat )
  ! IF( PRESENT( EndStr ) ) THEN
  !   WRITE( outFile%UnitNo, "( A )") TRIM( EndStr )
  ! END IF
  ! CALL CloseFile( outFile )
END PROCEDURE fmt_WriteToFile

!----------------------------------------------------------------------------
!                                                                   GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_GotoTag
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen, UnitNo
  CHARACTER( LEN = 100 ) :: Dummy
  !
  ! Find $MeshFormat
  Reopen = 0
  ierr = .FALSE.
  DO
    UnitNo = mshFile%getUnitNo()
    READ( UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL mshFile%Close()
      CALL mshFile%Open()
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.
      EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$MeshFormat' ) THEN
      EXIT
    END IF
  END DO
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