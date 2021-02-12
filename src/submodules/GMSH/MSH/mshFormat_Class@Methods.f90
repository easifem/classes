SUBMODULE( mshFormat_Class ) Methods
  !! This submodule implements methods for [[mshFormat_]]

USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_goto
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen
  CHARACTER( LEN = 100 ) :: Dummy
  !
  ! Find $MeshFormat
  Reopen = 0
  ierr = .FALSE.
  DO
    READ( mshFile % UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL ReOpenFile( mshFile )
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.
      EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$MeshFormat' ) THEN
      EXIT
    END IF
  END DO
END PROCEDURE fmt_goto

!----------------------------------------------------------------------------
!                                                              ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_read_file
  CALL Obj % GotoTag( mshFile, ierr )
  IF( .NOT. ierr ) THEN
    READ( mshFile % UnitNo, * ) Obj % Version, Obj % FileType, Obj % DataSize
    Obj % MeshFormat = TRIM( REAL2STR( Obj % Version ) ) // " " // &
      & TRIM( INT2STR( Obj % FileType ) ) // " " &
      & // TRIM( INT2STR( Obj % DataSize ) )
    IF( Obj % FileType .NE. 1 ) THEN
      Obj % isASCII = .TRUE.
    END IF
  END IF
END PROCEDURE fmt_read_file

!----------------------------------------------------------------------------
!                                                               WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_write_file
    ! Define internal variables
  TYPE( File_ ) :: outFile
  CALL OpenFileToWrite( outFile, mshFile % Path % Raw, &
    & TRIM( mshFile % FileName % Raw )//"_Format", &
    & mshFile % Extension % Raw )
  IF( PRESENT( Str ) ) THEN
    WRITE( outFile % UnitNo, "( A )") TRIM( Str )
  END IF
  WRITE( outFile % UnitNo, "( A )") TRIM( Obj % MeshFormat )
  IF( PRESENT( EndStr ) ) THEN
    WRITE( outFile % UnitNo, "( A )") TRIM( EndStr )
  END IF
  CALL CloseFile( outFile )
END PROCEDURE fmt_write_file

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_display
  INTEGER( I4B ) :: I
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( Msg )
  END IF
  WRITE( I, "(A)" ) TRIM( Obj % MeshFormat )
END PROCEDURE fmt_display

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE fmt_deallocatedata
SELECT TYPE( Obj )
TYPE IS (mshFormat_)
  Obj = TypemshFormat
END SELECT
END PROCEDURE fmt_deallocatedata

END SUBMODULE Methods