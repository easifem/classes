SUBMODULE( mshElements_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GotoTag
!----------------------------------------------------------------------------

MODULE PROCEDURE el_goto
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen
  CHARACTER( LEN = 100 ) :: Dummy
  !
  ! Find $Nodes
  Reopen = 0; ierr = .FALSE.
  DO
    READ( mshFile % UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL ReopenFile( mshFile )
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.; EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$Elements' ) THEN
      EXIT
    END IF
  END DO
END PROCEDURE el_goto

!----------------------------------------------------------------------------
!                                                              ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE el_read_file
  CALL Obj % GotoTag( mshFile, ierr )
  IF( .NOT. ierr ) THEN
    IF( mshFormat % Version .GT. 2.0 ) THEN
      READ( mshFile % UnitNo, * ) Obj % numEntityBlocks, Obj % numElements, &
        & Obj % minElementTag, Obj % maxElementTag
      IF( ( Obj % maxElementTag - Obj % minElementTag ) &
        & .EQ. ( Obj % numElements - 1 ) ) THEN
        Obj % isSparse = .FALSE.
      ELSE
        Obj % isSparse = .TRUE.
      END IF
    ELSE
      READ( mshFile % UnitNo, * ) Obj % numElements
    END IF
  END IF
END PROCEDURE el_read_file

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE el_display
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
  WRITE( I, "(A, I4, A)" ) "| Total Elements    | ", Obj % NumElements, " | "
  WRITE( I, "(A, I4, A)" ) "| Total Entities | ", Obj % NumEntityBlocks, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Min Element Tag   | ", Obj % minElementTag, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Max Element Tag   | ", Obj % maxElementTag, &
    & " | "
  WRITE( I, "(A, G5.2, A)" ) "| isSparse       | ", Obj % isSparse, &
    & " | "
END PROCEDURE el_display

!----------------------------------------------------------------------------
!                                                                 WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE el_write_file

END PROCEDURE el_write_file

!----------------------------------------------------------------------------
!                                                            ReadElementLine
!----------------------------------------------------------------------------

MODULE PROCEDURE el_read_elem_line

END PROCEDURE el_read_elem_line

!----------------------------------------------------------------------------
!                                                              TotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE el_telements_1
  Ans = Obj % numElements
END PROCEDURE el_telements_1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE el_deallocatedata
SELECT TYPE( Obj )
TYPE IS (mshElements_)
  Obj = TypemshElements
END SELECT
END PROCEDURE el_deallocatedata

END SUBMODULE Methods