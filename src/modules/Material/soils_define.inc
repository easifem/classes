!! State of soil option
INTEGER( I4B ), PARAMETER :: MAX_CHAR_LEN= 50

INTEGER( I4B ), PARAMETER :: STATE_FROZEN = 1
INTEGER( I4B ), PARAMETER :: STATE_UNFROZEN = 2
CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: State_Name(2) = &
  & [ &
      & "Frozen  ", &
      & "Unfrozen"]

!! Soil Type options
INTEGER( I4B ), PARAMETER :: SOIL_FINE_GRAINED = 1
INTEGER( I4B ), PARAMETER :: SOIL_COARSE_GRAINED = 2
INTEGER( I4B ), PARAMETER :: SOIL_PEAT = 300
CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: SoilType_Name(3) = &
  & [ &
      & "Fine Grained  ", &
      & "Coarse Grained", &
      & "Peat          "]

!! Soil minerals
INTEGER( I4B ), PARAMETER :: MAX_NUM_MINERALS=20
INTEGER( I4B ), PARAMETER  :: MINERAL_QUARTZ=1
CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: Mineral_Name(1) = &
  & [ &
      & "Quartz" ]

!! ThermCondModel
INTEGER( I4B ), PARAMETER :: Constant_ThermCond=1
INTEGER( I4B ), PARAMETER :: User_ThermCond=2
INTEGER( I4B ), PARAMETER :: Johansen_ThermCond=3

CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: ThermCondModel_Name(3) = &
  & [ &
      & "Constant    ", &
      & "User Defined", &
      & "Johansen    " ]

!! volumetric heat capacity
INTEGER( I4B ), PARAMETER :: constant_volHeatCap=1
INTEGER( I4B ), PARAMETER :: user_volHeatCap=2
INTEGER( I4B ), PARAMETER :: mix_volHeatCap=3

CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: volHeatCapModel_Name(3) = &
  & [ &
      & "Constant    ", &
      & "User Defined", &
      & "Mixture     " ]

!! SFCC
INTEGER( I4B ), PARAMETER :: User_SFCC=1
INTEGER( I4B ), PARAMETER :: Exp_SFCC=2

CHARACTER( LEN = MAX_CHAR_LEN ), PARAMETER :: SFCCModel_Name(2) = &
  & [ "User Defined", "Exp         " ]
