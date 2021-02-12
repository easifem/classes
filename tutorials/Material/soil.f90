program main
use easifem
implicit none

type( soil_ ) :: soils( 1 )

soils(1) = Soil( State=TypeSoil%Frozen, &
  & SoilType=TypeSoil%CoarseGrained, &
  & Gravel = 1.0_DFP, &
  & SpecificGravity = 2.5_DFP, &
  & DryDensity = 1600.0_DFP, &
  & Gravimetric_Moisture = 0.25_DFP, &
  & Porosity = 0.40_DFP, &
  & ThermCondModel = TypeSoil%JohansenThermCond, &
  & volHeatCapModel = TypeSoil%mixVolHeatCap, &
  & volFrac_solid= 0.6_DFP )

CALL Display( soils(1), 'soils(1) properties are :: ')

end program main