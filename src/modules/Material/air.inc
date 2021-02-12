TYPE( Material_ ), PARAMETER, PUBLIC :: Air = Material_( &
  & thermCond = 0.02514, &
  !! W/m/K at 20 degree C
  & specificHeatCap =  1007.0_DFP, &
  !! J/Kg/K at 20 degree C
  & density = 1.204_DFP, &
  !! kg/m3 at 20 degree C
  & volHeatCap = 1000.0_DFP * 1.177_DFP, &
  !! J/m3/K at 20 degree C
  & thermDiffusivity =  2.074E-5, &
  !! m2/sec at 20 degree C
  & DynamicViscosity = 1.825E-5 )


PUBLIC :: ThermCond_Air
PUBLIC :: specificHeatCap_Air
PUBLIC :: volHeatCap_Air
