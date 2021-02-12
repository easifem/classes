SUBMODULE( ThermCondModel_Class ) Johansen
USE stdMaterials
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                               johansen_constructor_pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE JohansenThermCond_Pointer
  ALLOCATE( Ans )
  IF( PRESENT( Lambda_Sat ) ) THEN
    Ans%isLambda_sat_given=.TRUE.
  ELSE
    Ans%isLambda_sat_given=.FALSE.
  END IF

  IF( PRESENT( Lambda_dry ) ) THEN
    Ans%isLambda_dry_given=.TRUE.
  ELSE
    Ans%isLambda_dry_given=.FALSE.
  END IF

  IF( PRESENT( Lambda_e ) ) THEN
    Ans%isLambda_e_given=.TRUE.
  ELSE
    Ans%isLambda_e_given=.FALSE.
  END IF

  IF( PRESENT( Gamma_d ) ) THEN
    Ans%Gamma_d = Gamma_d
  END IF

  IF( PRESENT( QuartzContent ) ) THEN
    Ans%QuartzContent = QuartzContent
  END IF

  IF( PRESENT( SoilType ) ) THEN
    Ans%SoilType = SoilType
  END IF

  IF( PRESENT( SoilState ) ) THEN
    Ans%State=SoilState
  END IF

  Ans % getValue => johansen_getval

  IF( .NOT. Ans%isLambda_sat_given ) THEN
    IF( Ans%QuartzContent .GT. 0.2_DFP ) THEN
      Ans%Lambda_s = (7.7_DFP**Ans%QuartzContent) &
        & * (2.0_DFP**(1.0_DFP-Ans%QuartzContent))
    ELSE
      Ans%Lambda_s = (7.7_DFP**Ans%QuartzContent) &
        & * (3.0_DFP**(1.0_DFP-Ans%QuartzContent))
    END IF
  END IF

END PROCEDURE JohansenThermCond_Pointer

!----------------------------------------------------------------------------
!                                                           getValue@Johansen
!----------------------------------------------------------------------------

MODULE PROCEDURE johansen_getval
  REAL( DFP ) :: Lam_w, Lam_i, Lam_sat, Lam_dry, Lam_e

  !
  SELECT TYPE( Obj )
  TYPE IS (JohansenThermCond_)

    ! compute Lambda_sat
    SELECT CASE( Obj%SoilType )
      ! Case for peaty soils
    CASE( SOIL_PEAT )

      ! Case for non peaty soils
    CASE DEFAULT

      ! Calculate Lambda_sat
      IF( Obj%isLambda_sat_given ) THEN
        Lam_sat = Obj%Lambda_sat
      ELSE

        IF( PRESENT( Temp ) ) THEN
          Lam_w = ThermCond_Water(Temp=Temp)
        ELSE
          Lam_w = ThermCond_Water()
        END IF

        IF( Obj%State .EQ. STATE_Unfrozen ) THEN

          Lam_sat = (Obj%Lambda_s**volFrac_solid)&
            & *(Lam_w**volFrac_water)

        ELSE

          ! get thermal conductivity of ice
          IF( PRESENT( Temp ) ) THEN
            Lam_i = ThermCond_Ice(Temp=Temp)
          ELSE
            Lam_i = ThermCond_Ice()
          END IF

          Lam_sat = ( Obj%Lambda_s**volFrac_solid ) &
            & * ( Lam_w**volFrac_water ) &
            & * ( Lam_i**volFrac_ice )
        END IF
      END IF

      ! Calculate Lambda_dry
      IF( Obj%isLambda_dry_given ) THEN
        Lam_dry = Obj%Lambda_dry
      ELSE
        Lam_dry = 1.2_DFP * ( 0.135 * Obj%Gamma_d + 64.7_DFP ) &
          & / ( 2700.0_DFP - 0.947 * Obj%Gamma_d )
      END IF

      ! Calculate Lambda_e
      IF( Obj%isLambda_e_given ) THEN
        Lam_e = Obj%Lambda_e
      ELSE
        IF( Obj%State .EQ. STATE_Frozen ) THEN
          Lam_e = volFrac_water/(volFrac_ice+volFrac_air+volFrac_water)
        ELSE
          IF( Obj%SoilType .EQ. SOIL_FINE_GRAINED) THEN
            Lam_e = 1.0_DFP + LOG10(volFrac_water / &
              & (volFrac_ice+volFrac_air+volFrac_water))
          ELSE
            Lam_e = 1.0_DFP + 0.7_DFP * LOG10(volFrac_water / &
              & (volFrac_ice+volFrac_air+volFrac_water))
          END IF
        END IF
      END IF

      Ans = ( Lam_sat - Lam_dry ) * Lam_e + Lam_dry

    END SELECT
  END SELECT

END PROCEDURE johansen_getval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Johansen