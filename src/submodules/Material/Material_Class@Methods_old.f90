SUBMODULE( Material_Class ) Methods
!! This submodule implements the type bound procedure of [[material_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_initiate
  IF( ALLOCATED( Obj % Props ) ) DEALLOCATE( Obj % Props )
  ALLOCATE( Obj % Props( tprop ) )
END PROCEDURE mat_initiate

!----------------------------------------------------------------------------
!                                                                 Append
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_append
  CALL Append( Obj % Props, KeyValObj )
END PROCEDURE mat_append

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_size
  IF( ALLOCATED( Obj % Props ) ) THEN
    Ans = SIZE( Obj % Props )
  ELSE
    Ans = 0
  END IF
END PROCEDURE mat_size

!----------------------------------------------------------------------------
!                                                                   Property
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_get_prop

  INTEGER( I4B ) :: i

  i = Obj % Props .INDEX. key

  IF( i .NE. 0 ) THEN
    Ans = Obj % Props( i )
  ELSE
    Ans = KeyValue( 'NONE', 0 )
  END IF

END PROCEDURE mat_get_prop

!----------------------------------------------------------------------------
!                                                              DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_deallocate
  IF( ALLOCATED( Obj % Props ) ) DEALLOCATE( Obj % Props )
END PROCEDURE mat_deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_display
  INTEGER( I4B ) :: i
  i =stdout
  IF ( PRESENT( unitno ) ) i = unitno
  IF( LEN_TRIM( msg ) .NE. 0  ) WRITE( I, "(A)" ) TRIM( msg )
  CALL Display( Obj % Props, "", i )
  CALL Dashline( Unitno = i )
END PROCEDURE mat_display

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_display_vec
  INTEGER( I4B ) :: i, j
  i =stdout
  IF ( PRESENT( unitno ) ) i = unitno
  IF( LEN_TRIM( msg ) .NE. 0  ) WRITE( I, "(A)" ) TRIM( msg )
  WRITE( I, "(A, I4)" ) "Total Materials :: ", SIZE( Obj )

  DO j = 1, SIZE( Obj )
    CALL Display( Obj( j ) % Props, "", i )
  END DO
END PROCEDURE mat_display_vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods