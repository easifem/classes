SUBMODULE( Material_Class ) Soil
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

  CALL Display( STATE_NAME(Obj%State), "State :: ", UnitNo = I )
  CALL Display( SOILTYPE_NAME(Obj%SoilType), "SoilType :: ", UnitNo = I )
  CALL Display( Obj%Gravel, "Gravel :: ", UnitNo = I )
  CALL Display( Obj%Sand, "Sand :: ", UnitNo = I )
  CALL Display( Obj%Silt, "Silt :: ", UnitNo = I )
  CALL Display( Obj%Clay, "Clay :: ", UnitNo = I )
  CALL Display( Obj%OrganicMatter, "OrganicMatter :: ", UnitNo = I )
  CALL Display( Obj%SpecificGravity, "SpecificGravity :: ", UnitNo = I )
  CALL Display( Obj%DryDensity, "DryDensity :: ", UnitNo = I )
  CALL Display( Obj%Gravimetric_Moisture, "Gravimetric_Moisture :: ", &
    & UnitNo = I )
  CALL Display( Obj%Porosity, "Porosity :: ", UnitNo = I )
  CALL Display( MINERAL_NAME(MINERAL_QUARTZ), "Quartz :: ", UnitNo = I )
  CALL Display( THERMCONDMODEL_NAME(Obj%ThermCond_Model), &
    & "ThermCondModel :: ", UnitNo = I )
  CALL Display( VOLHEATCAPMODEL_NAME(Obj%volHeatCap_Model), &
    & "volHeatCapModel :: ", UnitNo = I )
  CALL Display( SFCCMODEL_NAME(Obj%SFCC_Model), &
    & "SFCC Model :: ", UnitNo = I )
END PROCEDURE Soil_display

!----------------------------------------------------------------------------
!                                                                      Soil
!----------------------------------------------------------------------------

MODULE PROCEDURE Soil_Constructor
  CHARACTER( LEN = 3 ) :: JohansenCase

  IF( PRESENT( State ) ) THEN
    Ans%State= State
  ELSE
    Ans%State= TypeSoil%State
  END IF

  IF( PRESENT( SoilType ) ) THEN
    Ans%SoilType = SoilType
  ELSE
    Ans%SoilType = TypeSoil%SoilType
  END IF

  IF( PRESENT( Gravel ) ) THEN
    Ans%Gravel = Gravel
  ELSE
    Ans%Gravel = TypeSoil%Gravel
  END IF

  IF( PRESENT( Sand ) ) THEN
    Ans%Sand = Sand
  ELSE
    Ans%Sand = TypeSoil%Sand
  END IF

  IF( PRESENT( Silt ) ) THEN
    Ans%Silt = Silt
  ELSE
    Ans%Silt = TypeSoil%Silt
  END IF

  IF( PRESENT( Clay ) ) THEN
    Ans%Clay = Clay
  ELSE
    Ans%Clay = TypeSoil%Clay
  END IF

  IF( PRESENT( OrganicMatter ) ) THEN
    Ans%OrganicMatter = OrganicMatter
  ELSE
    Ans%OrganicMatter = TypeSoil%OrganicMatter
  END IF

  IF( PRESENT( SpecificGravity ) ) THEN
    Ans%SpecificGravity = SpecificGravity
  ELSE
    Ans%SpecificGravity = TypeSoil%SpecificGravity
  END IF

  IF( PRESENT( DryDensity ) ) THEN
    Ans%DryDensity = DryDensity
  ELSE
    Ans%DryDensity = TypeSoil%DryDensity
  END IF

  IF( PRESENT( Gravimetric_Moisture ) ) THEN
    Ans%Gravimetric_Moisture = Gravimetric_Moisture
  ELSE
    Ans%Gravimetric_Moisture = TypeSoil%Gravimetric_Moisture
  END IF

  IF( PRESENT( Porosity ) ) THEN
    Ans%Porosity = Porosity
    Ans%voidRatio = Porosity/(1-Porosity)
  ELSE
    Ans%Porosity = TypeSoil%Porosity
  END IF

  IF( PRESENT( voidRatio ) ) THEN
    Ans%voidRatio = voidRatio
    Ans%Porosity = voidRatio/(1 + voidRatio)
  ELSE
    Ans%voidRatio = TypeSoil%voidRatio
  END IF

  IF( PRESENT( volFrac_solid ) ) THEN
    Ans%volFrac_solid = volFrac_solid
  END IF

  IF( PRESENT( volFrac_water ) ) THEN
    Ans%volFrac_water = volFrac_water
  END IF

  IF( PRESENT( volFrac_ice ) ) THEN
    Ans%volFrac_ice = volFrac_ice
  END IF

  IF( PRESENT( volFrac_air ) ) THEN
    Ans%volFrac_air = volFrac_air
  END IF

  IF( PRESENT( Minerals ) ) THEN
    Ans%Minerals( 1:SIZE(Minerals) ) = Minerals
  ELSE
    Ans%Minerals = TypeSoil%Minerals
  END IF

  !! Setting thermal properties
  IF( PRESENT( ThermCondModel ) ) THEN
    Ans%ThermCond_Model = ThermCondModel

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

      Ans%ThermCondModel => UserThermCond_Pointer()
      Ans%ThermCondModel%getValue => UserThermCond

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
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, Lambda_Dry = Lambda_Dry, &
          & Lambda_e = Lambda_e, Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'SDe')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, Lambda_Dry = Lambda_Dry,&
          & Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'SdE')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, &
          & Lambda_e = Lambda_e, Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'Sde')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Sat = Lambda_Sat, &
          & Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'sDE' )
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Dry = Lambda_Dry, &
          & Lambda_e = Lambda_e, Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'sDe')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_Dry = Lambda_Dry, &
          & Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'sdE')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Lambda_e = Lambda_e, Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
      CASE( 'sde')
        Ans%ThermCondModel => JohansenThermCond_Pointer( &
          & Gamma_d=Ans%DryDensity, &
          & QuartzContent = Ans%Minerals(Ans%Quartz), &
          & SoilState = Ans%State, SoilType = Ans%SoilType )
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

      Ans%ThermCondModel => UserThermCond_Pointer()
      Ans%ThermCondModel%ConstThermCondVal= ThermCondVal
    END SELECT
  END IF

  IF( PRESENT( volHeatCapModel ) ) THEN
    Ans%volHeatCap_Model = volHeatCapModel

    SELECT CASE( volHeatCapModel )
    CASE( CONSTANT_VOLHEATCAP )

      IF( .NOT. PRESENT( volHeatCapVal ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        volHeatCap should be given")
        STOP
      END IF

      Ans%volHeatCapModel => UserVolHeatCap_Pointer()
      Ans%volHeatCapModel%ConstVolHeatCapVal= volHeatCapVal
      IF( PRESENT( volHeatCap_solid ) ) THEN
        Ans%VolHeatCapModel%volHeatCap_solid = volHeatCap_solid
      END IF

    CASE( USER_VOLHEATCAP )

      IF( .NOT. PRESENT( UserVolHeatCap ) ) THEN
        CALL Display( "ERROR:: In File "// TRIM( __FILE__ ) )
        CALL Display( "         in function Soil_Constructor()" )
        CALL Display( "        Line Number:: " // TRIM( INT2STR(__LINE__) ) )
        CALL Display( "        UserVolHeatCap procedure should be given")
        STOP
      END IF

      Ans%VolHeatCapModel => UserVolHeatCap_Pointer()
      Ans%VolHeatCapModel%getValue => UserVolHeatCap
      IF( PRESENT( volHeatCap_solid ) ) THEN
        Ans%VolHeatCapModel%volHeatCap_solid = volHeatCap_solid
      END IF

    CASE( MIX_VOLHEATCAP )

      IF( PRESENT( volHeatCap_solid ) ) THEN
        Ans%volHeatCapModel => MixVolHeatCap_Pointer( &
          & volHeatCap_solid = volHeatCap_solid, SoilState = Ans%State )
      ELSE
        Ans%volHeatCapModel => MixVolHeatCap_Pointer( SoilState = Ans%State )
      END IF

    END SELECT
  END IF


  IF( PRESENT( SFCCModel ) ) THEN
    Ans%SFCC_Model = SFCCModel

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

      Ans%SFCCModel => UserSFCC_Pointer()
      Ans%SFCCModel%getValue => UserSFCC_Value
      Ans%SFCCModel%getSlope => UserSFCC_Slope

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
        Ans%SFCCModel => ExpSFCC_Pointer(&
          & Theta_r = SFCC_Theta_r, &
          & Theta_w = volFrac_water, &
          & Temp_l = SFCC_Temp_l, &
          & Temp_s = SFCC_Temp_s, &
          & Coeff = SFCC_Coeff )
      ELSE
        Ans%SFCCModel => ExpSFCC_Pointer(&
          & Theta_r = SFCC_Theta_r, &
          & Theta_w = volFrac_water, &
          & Temp_l = SFCC_Temp_l, &
          & Temp_s = SFCC_Temp_s )
      END IF

    END SELECT
  END IF

END PROCEDURE Soil_Constructor

END SUBMODULE Soil