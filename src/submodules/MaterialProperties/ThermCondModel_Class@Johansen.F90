SUBMODULE(ThermCondModel_Class) Johansen
USE stdMaterials
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                               johansen_constructor_pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE JohansenThermCond_Pointer
  ALLOCATE( ans )
  IF( PRESENT( Lambda_Sat ) ) THEN
    ans%isLambda_sat_given=.TRUE.
  ELSE
    ans%isLambda_sat_given=.FALSE.
  END IF

  IF( PRESENT( Lambda_dry ) ) THEN
    ans%isLambda_dry_given=.TRUE.
  ELSE
    ans%isLambda_dry_given=.FALSE.
  END IF

  IF( PRESENT( Lambda_e ) ) THEN
    ans%isLambda_e_given=.TRUE.
  ELSE
    ans%isLambda_e_given=.FALSE.
  END IF

  IF( PRESENT( Gamma_d ) ) THEN
    ans%Gamma_d = Gamma_d
  END IF

  IF( PRESENT( QuartzContent ) ) THEN
    ans%QuartzContent = QuartzContent
  END IF

  IF( PRESENT( SoilType ) ) THEN
    ans%SoilType = SoilType
  END IF

  IF( PRESENT( SoilState ) ) THEN
    ans%State=SoilState
  END IF

  ans % getValue => johansen_getval

  IF( .NOT. ans%isLambda_sat_given ) THEN
    IF( ans%QuartzContent .GT. 0.2_DFP ) THEN
      ans%Lambda_s = (7.7_DFP**ans%QuartzContent) &
        & * (2.0_DFP**(1.0_DFP-ans%QuartzContent))
    ELSE
      ans%Lambda_s = (7.7_DFP**ans%QuartzContent) &
        & * (3.0_DFP**(1.0_DFP-ans%QuartzContent))
    END IF
  END IF

END PROCEDURE JohansenThermCond_Pointer

!----------------------------------------------------------------------------
!                                                           getValue@Johansen
!----------------------------------------------------------------------------

MODULE PROCEDURE johansen_getval
  REAL( DFP ) :: Lam_w, Lam_i, Lam_sat, Lam_dry, Lam_e

  !
  SELECT TYPE( obj )
  TYPE IS (JohansenThermCond_)

    ! compute Lambda_sat
    SELECT CASE( obj%SoilType )
      ! Case for peaty soils
    CASE( SOIL_PEAT )

      ! Case for non peaty soils
    CASE DEFAULT

      ! Calculate Lambda_sat
      IF( obj%isLambda_sat_given ) THEN
        Lam_sat = obj%Lambda_sat
      ELSE

        IF( PRESENT( Temp ) ) THEN
          Lam_w = ThermCond_Water(Temp=Temp)
        ELSE
          Lam_w = ThermCond_Water()
        END IF

        IF( obj%State .EQ. STATE_Unfrozen ) THEN

          Lam_sat = (obj%Lambda_s**volFrac_solid)&
            & *(Lam_w**volFrac_water)

        ELSE

          ! get thermal conductivity of ice
          IF( PRESENT( Temp ) ) THEN
            Lam_i = ThermCond_Ice(Temp=Temp)
          ELSE
            Lam_i = ThermCond_Ice()
          END IF

          Lam_sat = ( obj%Lambda_s**volFrac_solid ) &
            & * ( Lam_w**volFrac_water ) &
            & * ( Lam_i**volFrac_ice )
        END IF
      END IF

      ! Calculate Lambda_dry
      IF( obj%isLambda_dry_given ) THEN
        Lam_dry = obj%Lambda_dry
      ELSE
        Lam_dry = 1.2_DFP * ( 0.135 * obj%Gamma_d + 64.7_DFP ) &
          & / ( 2700.0_DFP - 0.947 * obj%Gamma_d )
      END IF

      ! Calculate Lambda_e
      IF( obj%isLambda_e_given ) THEN
        Lam_e = obj%Lambda_e
      ELSE
        IF( obj%State .EQ. STATE_Frozen ) THEN
          Lam_e = volFrac_water/(volFrac_ice+volFrac_air+volFrac_water)
        ELSE
          IF( obj%SoilType .EQ. SOIL_FINE_GRAINED) THEN
            Lam_e = 1.0_DFP + LOG10(volFrac_water / &
              & (volFrac_ice+volFrac_air+volFrac_water))
          ELSE
            Lam_e = 1.0_DFP + 0.7_DFP * LOG10(volFrac_water / &
              & (volFrac_ice+volFrac_air+volFrac_water))
          END IF
        END IF
      END IF

      ans = ( Lam_sat - Lam_dry ) * Lam_e + Lam_dry

    END SELECT
  END SELECT

END PROCEDURE johansen_getval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Johansen