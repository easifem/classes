SUBMODULE(Material_Class) Soil
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Soil_display
  INTEGER( I4B ) :: I

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  IF( LEN_TRIM( msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) TRIM( msg )
  END IF

  CALL Display( STATE_NAME(obj%State), "State :: ", UnitNo = I )
  CALL Display( SOILTYPE_NAME(obj%SoilType), "SoilType :: ", UnitNo = I )
  CALL Display( obj%Gravel, "Gravel :: ", UnitNo = I )
  CALL Display( obj%Sand, "Sand :: ", UnitNo = I )
  CALL Display( obj%Silt, "Silt :: ", UnitNo = I )
  CALL Display( obj%Clay, "Clay :: ", UnitNo = I )
  CALL Display( obj%OrganicMatter, "OrganicMatter :: ", UnitNo = I )
  CALL Display( obj%SpecificGravity, "SpecificGravity :: ", UnitNo = I )
  CALL Display( obj%DryDensity, "DryDensity :: ", UnitNo = I )
  CALL Display( obj%Gravimetric_Moisture, "Gravimetric_Moisture :: ", &
    & UnitNo = I )
  CALL Display( obj%Porosity, "Porosity :: ", UnitNo = I )
  CALL Display( MINERAL_NAME(MINERAL_QUARTZ), "Quartz :: ", UnitNo = I )
  CALL Display( THERMCONDMODEL_NAME(obj%ThermCond_Model), &
    & "ThermCondModel :: ", UnitNo = I )
  CALL Display( VOLHEATCAPMODEL_NAME(obj%volHeatCap_Model), &
    & "volHeatCapModel :: ", UnitNo = I )
  CALL Display( SFCCMODEL_NAME(obj%SFCC_Model), &
    & "SFCC Model :: ", UnitNo = I )
END PROCEDURE Soil_display

!----------------------------------------------------------------------------
!                                                                      Soil
!----------------------------------------------------------------------------

MODULE PROCEDURE Soil_Constructor
  CHARACTER( LEN = 3 ) :: JohansenCase

  IF( PRESENT( State ) ) THEN
    ans%State= State
  ELSE
    ans%State= TypeSoil%State
  END IF

  IF( PRESENT( SoilType ) ) THEN
    ans%SoilType = SoilType
  ELSE
    ans%SoilType = TypeSoil%SoilType
  END IF

  IF( PRESENT( Gravel ) ) THEN
    ans%Gravel = Gravel
  ELSE
    ans%Gravel = TypeSoil%Gravel
  END IF

  IF( PRESENT( Sand ) ) THEN
    ans%Sand = Sand
  ELSE
    ans%Sand = TypeSoil%Sand
  END IF

  IF( PRESENT( Silt ) ) THEN
    ans%Silt = Silt
  ELSE
    ans%Silt = TypeSoil%Silt
  END IF

  IF( PRESENT( Clay ) ) THEN
    ans%Clay = Clay
  ELSE
    ans%Clay = TypeSoil%Clay
  END IF

  IF( PRESENT( OrganicMatter ) ) THEN
    ans%OrganicMatter = OrganicMatter
  ELSE
    ans%OrganicMatter = TypeSoil%OrganicMatter
  END IF

  IF( PRESENT( SpecificGravity ) ) THEN
    ans%SpecificGravity = SpecificGravity
  ELSE
    ans%SpecificGravity = TypeSoil%SpecificGravity
  END IF

  IF( PRESENT( DryDensity ) ) THEN
    ans%DryDensity = DryDensity
  ELSE
    ans%DryDensity = TypeSoil%DryDensity
  END IF

  IF( PRESENT( Gravimetric_Moisture ) ) THEN
    ans%Gravimetric_Moisture = Gravimetric_Moisture
  ELSE
    ans%Gravimetric_Moisture = TypeSoil%Gravimetric_Moisture
  END IF

  IF( PRESENT( Porosity ) ) THEN
    ans%Porosity = Porosity
    ans%voidRatio = Porosity/(1-Porosity)
  ELSE
    ans%Porosity = TypeSoil%Porosity
  END IF

  IF( PRESENT( voidRatio ) ) THEN
    ans%voidRatio = voidRatio
    ans%Porosity = voidRatio/(1 + voidRatio)
  ELSE
    ans%voidRatio = TypeSoil%voidRatio
  END IF

  IF( PRESENT( volFrac_solid ) ) THEN
    ans%volFrac_solid = volFrac_solid
  END IF

  IF( PRESENT( volFrac_water ) ) THEN
    ans%volFrac_water = volFrac_water
  END IF

  IF( PRESENT( volFrac_ice ) ) THEN
    ans%volFrac_ice = volFrac_ice
  END IF

  IF( PRESENT( volFrac_air ) ) THEN
    ans%volFrac_air = volFrac_air
  END IF

  IF( PRESENT( Minerals ) ) THEN
    ans%Minerals( 1:SIZE(Minerals) ) = Minerals
  ELSE
    ans%Minerals = TypeSoil%Minerals
  END IF

  !! Setting thermal properties
  IF( PRESENT( ThermCondModel ) ) THEN
    ans%ThermCond_Model = ThermCondModel

    !! Select thermal conductivity models
    SELECT CASE( ThermCondModel )
    !! User defined thermal conductivity model
    CASE( User_ThermCond )

      IF( .NOT. PRESENT( UserThermCond ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        UserThemCond procedure should be given")
        STOP
      END IF

      ans%ThermCondModel => UserThermCond_Pointer()
      ans%ThermCondModel%getValue => UserThermCond

    !! Johansen thermal conductivity model
    CASE( Johansen_ThermCond )

      IF( PRESENT( Lambda_Sat ) ) THEN
        JohansenCase(1:1) = 'S'
      ELSE
        JohansenCase(1:1) = 's'
      END IF

      IF( PRESENT( Lambda_Dry ) ) THEN
        JohansenCase(2:2) = 'D'
      ELSE
        JohansenCase(2:2) = 'd'
      END IF

      IF( PRESENT( Lambda_e ) ) THEN
        JohansenCase(3:3) = 'E'
      ELSE
        JohansenCase(3:3) = 'e'
      END IF

      SELECT CASE( JohansenCase )
      CASE( 'SDE' )
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, Lambda_Dry = Lambda_Dry, &
          & Lambda_e = Lambda_e, Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'SDe')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, Lambda_Dry = Lambda_Dry,&
          & Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'SdE')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, &
          & Lambda_e = Lambda_e, Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'Sde')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, &
          & Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'sDE' )
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Dry = Lambda_Dry, &
          & Lambda_e = Lambda_e, Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'sDe')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Dry = Lambda_Dry, &
          & Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'sdE')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_e = Lambda_e, Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      CASE( 'sde')
        ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Gamma_d=ans%DryDensity, &
          & QuartzContent = ans%Minerals(ans%Quartz), &
          & SoilState = ans%State, SoilType = ans%SoilType )
      END SELECT

    !! Constant thermal conductivity model
    CASE( Constant_ThermCond )
      IF( .NOT. PRESENT( ThermCondVal ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        ThermCondVal should be given")
        STOP
      END IF

      ans%ThermCondModel => UserThermCond_Pointer()
      ans%ThermCondModel%ConstThermCondVal= ThermCondVal
    END SELECT
  END IF

  IF( PRESENT( volHeatCapModel ) ) THEN
    ans%volHeatCap_Model = volHeatCapModel

    SELECT CASE( volHeatCapModel )
    CASE( CONSTANT_VOLHEATCAP )

      IF( .NOT. PRESENT( volHeatCapVal ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        volHeatCap should be given")
        STOP
      END IF

      ans%volHeatCapModel => UserVolHeatCap_Pointer()
      ans%volHeatCapModel%ConstVolHeatCapVal= volHeatCapVal
      IF( PRESENT( volHeatCap_solid ) ) THEN
        ans%VolHeatCapModel%volHeatCap_solid = volHeatCap_solid
      END IF

    CASE( USER_VOLHEATCAP )

      IF( .NOT. PRESENT( UserVolHeatCap ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        UserVolHeatCap procedure should be given")
        STOP
      END IF

      ans%VolHeatCapModel => UserVolHeatCap_Pointer()
      ans%VolHeatCapModel%getValue => UserVolHeatCap
      IF( PRESENT( volHeatCap_solid ) ) THEN
        ans%VolHeatCapModel%volHeatCap_solid = volHeatCap_solid
      END IF

    CASE( MIX_VOLHEATCAP )

      IF( PRESENT( volHeatCap_solid ) ) THEN
        ans%volHeatCapModel => MixVolHeatCap_Pointer( &
          & volHeatCap_solid = volHeatCap_solid, SoilState = ans%State )
      ELSE
        ans%volHeatCapModel => MixVolHeatCap_Pointer( SoilState = ans%State )
      END IF

    END SELECT
  END IF


  IF( PRESENT( SFCCModel ) ) THEN
    ans%SFCC_Model = SFCCModel

    SELECT CASE( SFCCModel )
    CASE( User_SFCC )

      IF( .NOT. PRESENT( UserSFCC_Value ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        UserSFCC_Value procedure should be given")
        STOP
      END IF

      IF( .NOT. PRESENT( UserSFCC_Slope ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        UserSFCC_Slope procedure should be given")
        STOP
      END IF

      ans%SFCCModel => UserSFCC_Pointer()
      ans%SFCCModel%getValue => UserSFCC_Value
      ans%SFCCModel%getSlope => UserSFCC_Slope

    CASE( EXP_SFCC )
      IF( .NOT. PRESENT( volFrac_water ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        volFrac_water should be given")
        STOP
      END IF

      IF( .NOT. PRESENT( SFCC_Theta_r ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        SFCC_Theta_r should be given")
        STOP
      END IF

      IF( .NOT. PRESENT( SFCC_Temp_l ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        SFCC_Temp_l should be given")
        STOP
      END IF

      IF( .NOT. PRESENT( SFCC_Temp_s ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        SFCC_Temp_s should be given")
        STOP
      END IF

      IF( PRESENT( SFCC_Coeff ) ) THEN
        ans%SFCCModel => ExpSFCC_Pointer(&
          & Theta_r = SFCC_Theta_r, &
          & Theta_w = volFrac_water, &
          & Temp_l = SFCC_Temp_l, &
          & Temp_s = SFCC_Temp_s, &
          & Coeff = SFCC_Coeff )
      ELSE
        ans%SFCCModel => ExpSFCC_Pointer(&
          & Theta_r = SFCC_Theta_r, &
          & Theta_w = volFrac_water, &
          & Temp_l = SFCC_Temp_l, &
          & Temp_s = SFCC_Temp_s )
      END IF

    END SELECT
  END IF

END PROCEDURE Soil_Constructor

END SUBMODULE Soil