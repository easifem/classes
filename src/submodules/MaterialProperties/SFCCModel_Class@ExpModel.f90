SUBMODULE( SFCCModel_Class ) ExpModel
  IMPLICIT NONE
  CONTAINS

!----------------------------------------------------------------------------
!                                                            ExpSFCC_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_Pointer
  ALLOCATE( Ans )
  Ans%Theta_r = Theta_r
  Ans%Theta_w = Theta_w
  Ans%Temp_l = Temp_l
  Ans%Temp_s = Temp_s
  IF( PRESENT( Coeff ) ) Ans%Coeff=Coeff
  Ans%getValue => ExpSFCC_get_val
  Ans%getSlope => ExpSFCC_get_slope
  Ans%PhaseInfo => ExpSFCC_PhaseInfo
END PROCEDURE ExpSFCC_Pointer

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_get_val
  REAL( DFP ) :: c
  SELECT TYPE( Obj )
  CLASS IS( ExpSFCC_ )
    IF( Temp .GE. Obj%Temp_l ) THEN
      Ans=Obj%Theta_w
      RETURN
    END IF

    IF( Temp .LE. Obj%Temp_s ) THEN
      Ans = Obj%Theta_r
      RETURN
    END IF

    c = -Obj%Coeff*((Temp-Obj%Temp_l)/(Obj%Temp_s-Obj%Temp_l))**2
    Ans = Obj%Theta_r + (Obj%Theta_w-Obj%Theta_r)*EXP(c)
  END SELECT
END PROCEDURE ExpSFCC_get_val

!----------------------------------------------------------------------------
!                                                                 getSlope
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_get_slope
  REAL( DFP ) :: c, b

  SELECT TYPE( Obj )
  CLASS IS( ExpSFCC_ )
    IF( Temp .GE. Obj%Temp_l ) THEN
      Ans=0.0_DFP
      RETURN
    END IF

    IF( Temp .LE. Obj%Temp_s ) THEN
      Ans = 0.0_DFP
      RETURN
    END IF

    b = -2.0*Obj%Coeff * (Temp-Obj%Temp_l)*(Obj%Theta_w - Obj%Theta_r) &
      & / (Obj%Temp_s-Obj%Temp_l)**2

    c = -Obj%Coeff*((Temp-Obj%Temp_l)/(Obj%Temp_s-Obj%Temp_l))**2

    Ans = b*EXP(c)
  END SELECT
END PROCEDURE ExpSFCC_get_slope

!----------------------------------------------------------------------------
!                                                                 PhaseInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpSFCC_PhaseInfo
  SELECT TYPE( Obj )
  CLASS IS( ExpSFCC_ )
    IF( ABS(Temp - Obj%Temp_l) .LE. 1.0E-10 .OR. Temp .GE. Obj%Temp_l ) THEN
      Ans = 'L'
      RETURN
    END IF

    IF( ABS(Temp - Obj%Temp_s) .LE. 1.0E-10 .OR. Temp .LE. Obj%Temp_s ) THEN
      Ans = 'S'
      RETURN
    END IF

    Ans = 'M'
  END SELECT
END PROCEDURE ExpSFCC_PhaseInfo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ExpModel