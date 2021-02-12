SUBMODULE( QuadratureVariables_Class ) Methods
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_initiate
  INTEGER( I4B ) :: i

  Obj % tprop = tprop
  IF( ALLOCATED( Obj % Names ) ) DEALLOCATE( Obj % Names )
  ALLOCATE( Obj % Names( tprop ) )

  IF( PRESENT( Names ) ) THEN
    DO i = 1, Obj % tprop
      Obj % Names( i ) = TRIM( Names( i ) % chars() )
    END DO
  ELSE
    DO i = 1, Obj % tprop
      Obj % Names( i ) = "V"//TRIM( INT2STR( i ) )
    END DO
  END IF

  Obj % tpoint = tpoint

  CALL Reallocate( Obj % Val, Obj % tprop, Obj % tpoint, telem )
END PROCEDURE elem_var_initiate

!----------------------------------------------------------------------------
!                                                                  setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_set_value
  INTEGER( I4B ) :: ip, i_s, i_e

  ip = MIN( SIZE( Obj % Val, 2 ), ipoint )

  IF( PRESENT( is ) ) THEN
    i_s = is
  ELSE
    i_s = 1
  END IF

  IF( PRESENT( ie ) ) THEN
    i_e = ie
  ELSE
    i_e = size( Val )
  END IF

  Obj % Val( i_s:i_e, ip, elemnum ) = Val( : )

END PROCEDURE elem_var_set_value

!----------------------------------------------------------------------------
!                                                           addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_add_val
  INTEGER( I4B ) :: ip, i_s, i_e

  ip = MIN( SIZE( Obj % Val, 2 ), ipoint )

  IF( PRESENT( is ) ) THEN
    i_s = is
  ELSE
    i_s = 1
  END IF

  IF( PRESENT( ie ) ) THEN
    i_e = ie
  ELSE
    i_e = size( Val )
  END IF

  Obj % Val( i_s:i_e, ip, elemnum ) &
    & = Obj % Val( i_s:i_e, ip, elemnum )  &
    & + Scale * Val( : )

END PROCEDURE elem_var_add_val

!----------------------------------------------------------------------------
!                                                                  getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_get_value
  Ans = Obj % Val( :, :, elemnum )
END PROCEDURE elem_var_get_value

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_disp
  INTEGER( I4B ) :: i

  IF( PRESENT( unitno ) ) THEN
    i = unitno
  ELSE
    i = stdout
  END IF
  CALL Display( Obj % Val, msg, i )

END PROCEDURE elem_var_disp

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_var_deallocate_data
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
  IF( ALLOCATED( Obj % Names ) ) DEALLOCATE( Obj % Names )
  obj%tprop=0
  obj%tpoint=0
END PROCEDURE elem_var_deallocate_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods