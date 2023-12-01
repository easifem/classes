!! These properties are taken from following references
!! https://www.engineeringtoolbox.com/water-liquid-gas-thermal-conductivity-temperature-pressure-d_2012.html
!! http://thermopedia.com/content/1254/


TYPE( Material_ ), PARAMETER, PUBLIC :: Water = Material_( &
  & thermCond = 0.59803_DFP, &
!! W/m/K at 20 degree C
!! https://www.engineeringtoolbox.com/water-liquid-gas-thermal-conductivity-temperature-pressure-d_2012.html
  & specificHeatCap = 4184.4_DFP, &
  & volHeatCap = 4184.4_DFP * 998.207_DFP, &
!! J/m3/K at 20 degree C
!! https://www.engineeringtoolbox.com/specific-heat-capacity-water-d_660.html
  & thermDiffusivity = 0.143D-6,&
!! m2/sec at 20 degree C
!! https://www.engineeringtoolbox.com/water-steam-thermal-diffusivity-d_2058.html
  & density = 999.975_DFP, &
!! kg/m3 at 4 degree C
!! https://www.engineeringtoolbox.com/water-temperature-specific-gravity-d_1179.html
  & DynamicViscosity = 0.0010016_DFP, &
!! N-s/m2 at 20 degree C
!! https://www.engineeringtoolbox.com/water-dynamic-kinematic-viscosity-d_596.html
  & latentHeatMelt = 334000.0_DFP, &
!! J/Kg
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & latentHeatBoil = 2256.0D+3, &
!! J/Kg
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & meltTemp = 273.15_DFP, &
!! in Kelvin
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & boilTemp = 373.15_DFP, &
!! in Kelvin
!! At 1 atm pressure
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & criticalTemp = 273.15_DFP + 373.946_DFP, &
!! Degree C
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & criticalPressure = 22.06D+6, &
!! Pa
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & triplePointPressure = 611.657_DFP, &
!! Pa
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & triplePointTemp = 273.16_DFP, &
!! in Kelvin
!! https://www.engineeringtoolbox.com/water-thermal-properties-d_162.html
  & bulkModulus = 2.15D+9 &
!! N/m2
  & )

 !----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: thermCond_water
PUBLIC :: specificHeatCap_water
PUBLIC :: density_water
PUBLIC :: volHeatCap_water