SUBMODULE( mshNodes_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GotoNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE n_goto
  ! Define internal variables
  INTEGER( I4B ) :: IOSTAT, Reopen
  CHARACTER( LEN = 100 ) :: Dummy
  ! Find $Nodes
  Reopen = 0; ierr = .FALSE.
  DO
    READ( mshFile % UnitNo, "(A)", IOSTAT = IOSTAT ) Dummy
    IF( IOSTAT .LT. 0 ) THEN
      CALL ReopenFile( mshFile )
      Reopen = Reopen + 1
    ELSE IF( IOSTAT .GT. 0 .OR. Reopen .GT. 1 ) THEN
      ierr = .TRUE.; EXIT
    ELSE IF( TRIM( Dummy ) .EQ. '$Nodes' ) THEN
      EXIT
    END IF
  END DO
END PROCEDURE n_goto

!----------------------------------------------------------------------------
!                                                               ReadFromFile
!----------------------------------------------------------------------------

MODULE PROCEDURE n_read_file
  CALL Obj % GotoTag( mshFile, ierr )
  IF( .NOT. ierr ) THEN
    IF( mshFormat % Version .GT. 2.0 ) THEN
      READ( mshFile % UnitNo, * ) Obj % numEntityBlocks, Obj % numNodes, &
        & Obj % minNodeTag, Obj % maxNodeTag
      IF( ( Obj % maxNodeTag - Obj % minNodeTag ) &
        & .EQ. ( Obj % numNodes - 1 ) ) THEN
        Obj % isSparse = .FALSE.
      ELSE
        Obj % isSparse = .TRUE.
      END IF
    ELSE
      READ( mshFile % UnitNo, * ) Obj % numNodes
    END IF
  END IF
END PROCEDURE n_read_file

!----------------------------------------------------------------------------
!                                                                WriteToFile
!----------------------------------------------------------------------------

MODULE PROCEDURE n_write_file

END PROCEDURE n_write_file

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE n_display
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
  WRITE( I, "(A, I4, A)" ) "| Total Nodes    | ", Obj % NumNodes, " | "
  WRITE( I, "(A, I4, A)" ) "| Total Entities | ", Obj % NumEntityBlocks, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Min Node Tag   | ", Obj % minNodeTag, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Max Node Tag   | ", Obj % maxNodeTag, &
    & " | "
  WRITE( I, "(A, G5.2, A)" ) "| isSparse       | ", Obj % isSparse, &
    & " | "
END PROCEDURE n_display

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE n_deallocatedata
SELECT TYPE (Obj)
TYPE IS (mshNodes_)
  Obj = TypeMSHNodes
END SELECT
END PROCEDURE n_deallocatedata

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
