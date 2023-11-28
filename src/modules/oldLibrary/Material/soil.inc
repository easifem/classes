TYPE, EXTENDS( Material_ ) :: Soil_

  !! soil state; frozen and unfrozen
  INTEGER( I4B ) :: Frozen=STATE_FROZEN
  INTEGER( I4B ) :: Unfrozen=STATE_UNFROZEN
  INTEGER( I4B ) :: State = STATE_UNFROZEN

  !! soil type; fine-grained, coarse-grained, peat
  INTEGER( I4B ) :: SoilType = SOIL_COARSE_GRAINED
  INTEGER( I4B ) :: FineGrained = SOIL_FINE_GRAINED
  INTEGER( I4B ) :: CoarseGrained = SOIL_COARSE_GRAINED
  INTEGER( I4B ) :: Peat = SOIL_PEAT

  !! grain size distribution
  REAL( DFP ) :: Gravel = 0.0_DFP
  REAL( DFP ) :: Sand = 0.0_DFP
  REAL( DFP ) :: Silt = 0.0_DFP
  REAL( DFP ) :: Clay = 0.0_DFP
  REAL( DFP ) :: OrganicMatter = 0.0_DFP

  !! index properties
  REAL( DFP ) :: SpecificGravity = 0.0_DFP
  REAL( DFP ) :: DryDensity = 0.0_DFP
  REAL( DFP ) :: Gravimetric_Moisture = 0.0_DFP
  REAL( DFP ) :: Porosity = 0.0_DFP
  REAL( DFP ) :: voidRatio = 0.0_DFP
  REAL( DFP ) :: volFrac_solid = 0.0_DFP
  REAL( DFP ) :: volFrac_water = 0.0_DFP
  REAL( DFP ) :: volFrac_ice = 0.0_DFP
  REAL( DFP ) :: volFrac_air = 0.0_DFP

  !! mineral composition
  REAL( DFP ) :: Minerals( MAX_NUM_MINERALS ) = 0.0_DFP
  INTEGER( I4B ) :: Quartz = Mineral_Quartz

  !! thermal conductivity model
  INTEGER( I4B ) :: ThermCond_Model = Constant_ThermCond
  INTEGER( I4B ) :: UserThermCond = User_ThermCond
  INTEGER( I4B ) :: JohansenThermCond = Johansen_ThermCond
  INTEGER( I4B ) :: ConstantThermCond = Constant_ThermCond
  CLASS( ThermCondModel_ ), POINTER :: thermCondModel => NULL()

  !! volumetric heat capacity model
  INTEGER( I4B ) :: volHeatCap_Model = Constant_volHeatCap
  INTEGER( I4B ) :: UserVolHeatCap = User_volHeatCap
  INTEGER( I4B ) :: ConstantVolHeatCap = Constant_volHeatCap
  INTEGER( I4B ) :: mixVolHeatCap = Mix_VolHeatCap
  CLASS( volHeatCapModel_ ), POINTER :: volHeatCapModel => NULL()

  !! volumetric heat capacity model
  INTEGER( I4B ) :: SFCC_Model = Exp_SFCC
  INTEGER( I4B ) :: UserSFCC = User_SFCC
  INTEGER( I4B ) :: ExpSFCC = Exp_SFCC
  CLASS( SFCCModel_ ), POINTER :: SFCCModel => NULL()

  !! Consolidation characteristics
  !! Hydraulic characteristics
  !! Undrained characteristics
  !! Drained characteristics
END TYPE Soil_

PUBLIC :: Soil_

TYPE( Soil_ ), PUBLIC, PARAMETER :: TypeSoil = Soil_()

!----------------------------------------------------------------------------
!                                                               Display@Soil
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE soil_display( Obj, msg, unitNo )
  CLASS( Soil_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE soil_display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE soil_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Soil@Soil
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Soil_Constructor( State, SoilType, Gravel, Sand, Silt, &
  & Clay, OrganicMatter, SpecificGravity, DryDensity,Gravimetric_Moisture, &
  & Porosity, voidRatio, volFrac_solid, volFrac_water, volFrac_ice, &
  & volFrac_air, &
  & Minerals, Lambda_sat, Lambda_dry, Lambda_e, &
  & ThermCondModel, ThermCondVal, UserThermCond, &
  & volHeatCap_solid, volHeatCapModel, volHeatCapVal, UserVolHeatCap, &
  & SFCCModel, UserSFCC_Value, UserSFCC_Slope, &
  & SFCC_Theta_r, SFCC_Temp_l, SFCC_Temp_s, SFCC_Coeff ) RESULT( Ans )

  TYPE( Soil_ ) :: Ans
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: State
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: SoilType
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Gravel
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Sand
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Silt
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Clay
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: OrganicMatter
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SpecificGravity
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: DryDensity
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Gravimetric_Moisture
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Porosity
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: voidRatio
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Minerals( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Lambda_sat
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Lambda_dry
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Lambda_e
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: ThermCondModel
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ThermCondVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volHeatCap_solid
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: volHeatCapModel
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volHeatCapVal
  PROCEDURE( thermCond_get_value ), POINTER, OPTIONAL, INTENT( IN ) :: &
    & UserThermCond
  PROCEDURE( volHeatCap_get_value ), POINTER, OPTIONAL, INTENT( IN ) :: &
    & UservolHeatCap
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: SFCCModel
  PROCEDURE( sfcc_get_value ), POINTER, OPTIONAL, INTENT( IN ) :: UserSFCC_Value
  PROCEDURE( sfcc_get_value ), POINTER, OPTIONAL, INTENT( IN ) :: UserSFCC_Slope
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SFCC_Theta_r
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SFCC_Temp_l
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SFCC_Temp_s
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SFCC_Coeff
END FUNCTION Soil_Constructor
END INTERFACE

ABSTRACT INTERFACE
PURE FUNCTION sfcc_get_value( Obj, Temp ) RESULT( Ans )
  IMPORT :: SFCCModel_, DFP
  CLASS( SFCCModel_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: Ans
END FUNCTION sfcc_get_value
END INTERFACE

ABSTRACT INTERFACE
  PURE FUNCTION thermcond_get_value( Obj, volFrac_solid, volFrac_water, &
    & volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )

    IMPORT :: ThermCondModel_, DFP
    CLASS( ThermCondModel_ ), INTENT( IN ) :: Obj
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: x
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: y
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: z
    REAL( DFP ) :: Ans
  END FUNCTION
END INTERFACE

ABSTRACT INTERFACE
  PURE FUNCTION volHeatCap_get_value( Obj, volFrac_solid, volFrac_water, &
    & volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )

    IMPORT :: volHeatCapModel_, DFP
    CLASS( volHeatCapModel_ ), INTENT( IN ) :: Obj
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: x
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: y
    REAL( DFP ), OPTIONAL, INTENT( IN ) :: z
    REAL( DFP ) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE Soil
  MODULE PROCEDURE Soil_Constructor
END INTERFACE Soil

PUBLIC :: Soil

