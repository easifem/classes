TYPE( Material_ ), PARAMETER, PUBLIC :: Ice = Material_( &
  & thermCond = 2.30_DFP, &
  !! W/m/K at -10 degree C
  & specificHeatCap =  2000.0_DFP, &
  !! J/Kg/K at -10 degree C
  & density = 918.9_DFP, &
  !! kg/m3 at -10 degree C
  & volHeatCap = 2000.0_DFP * 918.9_DFP, &
  !! J/m3/K at -10 degree C
  & thermDiffusivity =  2.30_DFP / 2000.0_DFP / 918.9_DFP, &
  !! m2/sec at -10 degree C
  & latentHeatMelt = 334000.0_DFP, &
  !! J/Kg
  & meltTemp = 273.15_DFP )
