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
  CALL obj % GotoTag( mshFile, ierr )
  IF( .NOT. ierr ) THEN
    IF( mshFormat % Version .GT. 2.0 ) THEN
      READ( mshFile % UnitNo, * ) obj % numEntityBlocks, obj % numNodes, &
        & obj % minNodeTag, obj % maxNodeTag
      IF( ( obj % maxNodeTag - obj % minNodeTag ) &
        & .EQ. ( obj % numNodes - 1 ) ) THEN
        obj % isSparse = .FALSE.
      ELSE
        obj % isSparse = .TRUE.
      END IF
    ELSE
      READ( mshFile % UnitNo, * ) obj % numNodes
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
  WRITE( I, "(A, I4, A)" ) "| Total Nodes    | ", obj % NumNodes, " | "
  WRITE( I, "(A, I4, A)" ) "| Total Entities | ", obj % NumEntityBlocks, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Min Node Tag   | ", obj % minNodeTag, &
    & " | "
  WRITE( I, "(A, I4, A)" ) "| Max Node Tag   | ", obj % maxNodeTag, &
    & " | "
  WRITE( I, "(A, G5.2, A)" ) "| isSparse       | ", obj % isSparse, &
    & " | "
END PROCEDURE n_display

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
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
