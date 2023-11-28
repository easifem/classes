MODULE Material_Class
USE GlobalData
USE ThermCondModel_Class
USE volHeatCapModel_Class
USE SFCCModel_Class
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                  Material_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! `Material_` type contains materials properties
TYPE :: Material_
  REAL( DFP ) :: thermCond = 0.0_DFP
  REAL( DFP ) :: specificHeatCap = 0.0_DFP
  REAL( DFP ) :: volHeatCap = 0.0_DFP
  REAL( DFP ) :: thermDiffusivity = 0.0_DFP
  REAL( DFP ) :: density = 0.0_DFP
  REAL( DFP ) :: DynamicViscosity = 0.0_DFP
  REAL( DFP ) :: meltTemp = 0.0_DFP
  REAL( DFP ) :: boilTemp = 0.0_DFP
  REAL( DFP ) :: criticalTemp = 0.0_DFP
  REAL( DFP ) :: criticalPressure = 0.0_DFP
  REAL( DFP ) :: triplePointPressure = 0.0_DFP
  REAL( DFP ) :: triplePointTemp = 0.0_DFP
  REAL( DFP ) :: latentHeatMelt = 0.0_DFP
  REAL( DFP ) :: latentHeatBoil = 0.0_DFP

  REAL( DFP ) :: volThermCoeff = 0.0_DFP
  REAL( DFP ) :: bulkModulus = 0.0_DFP

END TYPE Material_

PUBLIC :: Material_

TYPE( Material_ ), PARAMETER, PUBLIC :: TypeMaterial=Material_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: MatetrialPointer_
  CLASS( Material_ ), POINTER :: Ptr => NULL( )
END TYPE MatetrialPointer_

PUBLIC :: MatetrialPointer_

!----------------------------------------------------------------------------
!                                                                    Soil_
!----------------------------------------------------------------------------

#include "./soils_define.inc"
#include "./soil.inc"

END MODULE Material_Class