!> authors: Dr. Vikas Sharma
!
!  This module consists of several thermal conductivity model for porous
! medium
!
MODULE ThermCondModel_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./PMDefine.inc"

!----------------------------------------------------------------------------
!                                                           ThermCondModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: ThermCondModel_
  REAL( DFP ) :: ConstThermCondVal = 0.0_DFP
  PROCEDURE( johansen_getval ), POINTER, PASS( Obj ) :: getValue => NULL()
END TYPE ThermCondModel_

PUBLIC :: ThermCondModel_

!----------------------------------------------------------------------------
!                                                     ThermCondModelPointer_
!----------------------------------------------------------------------------
TYPE :: ThermCondModelPointer_
  CLASS( ThermCondModel_ ), POINTER :: Ptr=>NULL()
END TYPE ThermCondModelPointer_

PUBLIC :: ThermCondModelPointer_

!----------------------------------------------------------------------------
!                                                             UserThermCond_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ThermCondModel_ ) :: UserThermCond_
END TYPE UserThermCond_

PUBLIC :: UserThermCond_

TYPE( UserThermCond_ ), PUBLIC, PARAMETER::TypeUserThermCond=UserThermCond_()

!----------------------------------------------------------------------------
!                                                         UserThermCond@User
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION UserThermCond_Pointer(  ) RESULT( Ans )
  CLASS( UserThermCond_ ), POINTER :: Ans
END FUNCTION UserThermCond_Pointer
END INTERFACE

PUBLIC :: UserThermCond_Pointer

!----------------------------------------------------------------------------
!                                                              getValue@User
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the thermal conductivity values

!> authors: Dr. Vikas Sharma
!
! This function returns the thermal conductivity values
! - `volFrac` contains volumetric fraction of solid, water, ice, air
MODULE PURE FUNCTION UserThermCond_getval( Obj, volFrac_solid, &
  & volFrac_water, volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )
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
END FUNCTION UserThermCond_getval
END INTERFACE

!----------------------------------------------------------------------------
!                                                          JohansenThermCond_
!----------------------------------------------------------------------------

TYPE, EXTENDS( ThermCondModel_ ) :: JohansenThermCond_
  INTEGER( I4B ) :: State=STATE_UNFROZEN
  INTEGER( I4B ) :: Frozen=STATE_FROZEN
  INTEGER( I4B ) :: Unfrozen=STATE_UNFROZEN
  INTEGER( I4B ) :: SoilType = SOIL_COARSE_GRAINED
  INTEGER( I4B ) :: FineGrained = SOIL_FINE_GRAINED
  INTEGER( I4B ) :: CoarseGrained = SOIL_COARSE_GRAINED
  INTEGER( I4B ) :: Peat = SOIL_PEAT

  REAL( DFP ) :: Lambda_Sat = 0.0_DFP
  REAL( DFP ) :: Lambda_Dry = 0.0_DFP
  REAL( DFP ) :: Lambda_e = 0.0_DFP
  REAL( DFP ) :: Lambda_s = 0.0_DFP
  LOGICAL :: isLambda_sat_given = .FALSE.
  LOGICAL :: isLambda_dry_given = .FALSE.
  LOGICAL :: isLambda_e_given = .FALSE.

  REAL( DFP ) :: Gamma_d = 1600.0_DFP
  REAL( DFP ) :: QuartzContent = 1.0_DFP

END TYPE JohansenThermCond_

PUBLIC :: JohansenThermCond_

TYPE( JohansenThermCond_ ), PUBLIC, PARAMETER :: TypeJohansenThermCond &
  & =JohansenThermCond_()

!----------------------------------------------------------------------------
!                                                        Constructor@Johansen
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION JohansenThermCond_Pointer( Lambda_Sat, Lambda_Dry, &
  & Lambda_e, Gamma_d, QuartzContent, SoilState, SoilType ) &
  & RESULT( Ans )
  CLASS( JohansenThermCond_ ), POINTER :: Ans
  REAL( DFP ), OPTIONAL :: Lambda_Sat
  REAL( DFP ), OPTIONAL :: Lambda_Dry
  REAL( DFP ), OPTIONAL :: Lambda_e
  REAL( DFP ), OPTIONAL :: Gamma_d
  REAL( DFP ), OPTIONAL :: QuartzContent
  INTEGER( I4B ), OPTIONAL :: SoilState
  INTEGER( I4B ), OPTIONAL :: SoilType
END FUNCTION JohansenThermCond_Pointer
END INTERFACE

PUBLIC :: JohansenThermCond_Pointer

!----------------------------------------------------------------------------
!                                                           getValue@Johansen
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the thermal conductivity values

!> authors: Dr. Vikas Sharma
!
! This function returns the thermal conductivity values
! - `volFrac` contains volumetric fraction of solid, water, ice, air
MODULE PURE FUNCTION johansen_getval( Obj, volFrac_solid, volFrac_water, &
  & volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )
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
END FUNCTION johansen_getval
END INTERFACE
END MODULE ThermCondModel_Class