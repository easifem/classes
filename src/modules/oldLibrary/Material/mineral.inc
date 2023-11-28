TYPE( Material_ ), PARAMETER, PUBLIC :: Quartz = Material_( &
  & thermCond = 8.8_DFP, &
  !! W/m/K at 20 degree C
  & specificHeatCap =  733.0_DFP, &
  !! J/Kg/K at 20 degree C
  & density = 2650.0_DFP, &
  !! kg/m3 at 20 degree C
  & volHeatCap = 2650.0_DFP * 733.0_DFP, &
  !! J/m3/K at 20 degree C
  & thermDiffusivity =  8.8_DFP/733.0_DFP/2650.0_DFP &
  !! m2/sec at 20 degree C
  & )

  PUBLIC :: ThermCond_Quartz
  PUBLIC :: SpecificHeatCap_Quartz
  PUBLIC :: volHeatCap_Quartz