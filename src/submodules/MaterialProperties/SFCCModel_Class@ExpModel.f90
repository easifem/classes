SUBMODULE( SFCCModel_Class ) ExpModel
  IMPLICIT NONE
  CONTAINS

!----------------------------------------------------------------------------
!                                                            ExpSFCC_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_Pointer
  ALLOCATE( ans )
  ans%Theta_r = Theta_r
  ans%Theta_w = Theta_w
  ans%Temp_l = Temp_l
  ans%Temp_s = Temp_s
  IF( PRESENT( Coeff ) ) ans%Coeff=Coeff
  ans%getValue => ExpSFCC_get_val
  ans%getSlope => ExpSFCC_get_slope
  ans%PhaseInfo => ExpSFCC_PhaseInfo
END PROCEDURE ExpSFCC_Pointer

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_get_val
  REAL( DFP ) :: c
  SELECT TYPE( obj )
  CLASS IS( ExpSFCC_ )
    IF( Temp .GE. obj%Temp_l ) THEN
      ans=obj%Theta_w
      RETURN
    END IF

    IF( Temp .LE. obj%Temp_s ) THEN
      ans = obj%Theta_r
      RETURN
    END IF

    c = -obj%Coeff*((Temp-obj%Temp_l)/(obj%Temp_s-obj%Temp_l))**2
    ans = obj%Theta_r + (obj%Theta_w-obj%Theta_r)*EXP(c)
  END SELECT
END PROCEDURE ExpSFCC_get_val

!----------------------------------------------------------------------------
!                                                                 getSlope
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_get_slope
  REAL( DFP ) :: c, b

  SELECT TYPE( obj )
  CLASS IS( ExpSFCC_ )
    IF( Temp .GE. obj%Temp_l ) THEN
      ans=0.0_DFP
      RETURN
    END IF

    IF( Temp .LE. obj%Temp_s ) THEN
      ans = 0.0_DFP
      RETURN
    END IF

    b = -2.0*obj%Coeff * (Temp-obj%Temp_l)*(obj%Theta_w - obj%Theta_r) &
      & / (obj%Temp_s-obj%Temp_l)**2

    c = -obj%Coeff*((Temp-obj%Temp_l)/(obj%Temp_s-obj%Temp_l))**2

    ans = b*EXP(c)
  END SELECT
END PROCEDURE ExpSFCC_get_slope

!----------------------------------------------------------------------------
!                                                                 PhaseInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_PhaseInfo
  SELECT TYPE( obj )
  CLASS IS( ExpSFCC_ )
    IF( ABS(Temp - obj%Temp_l) .LE. 1.0E-10 .OR. Temp .GE. obj%Temp_l ) THEN
      ans = 'L'
      RETURN
    END IF

    IF( ABS(Temp - obj%Temp_s) .LE. 1.0E-10 .OR. Temp .LE. obj%Temp_s ) THEN
      ans = 'S'
      RETURN
    END IF

    ans = 'M'
  END SELECT
END PROCEDURE ExpSFCC_PhaseInfo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ExpModel