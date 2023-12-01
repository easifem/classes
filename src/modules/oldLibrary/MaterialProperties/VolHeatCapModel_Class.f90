!> authors: Dr. Vikas Sharma
!
!  This module consists of volumetric heat capacity model for porous media
!
MODULE VolHeatCapModel_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./PMDefine.inc"

!----------------------------------------------------------------------------
!                                                           VolHeatCapModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: VolHeatCapModel_
  REAL( DFP ) :: ConstVolHeatCapVal=0.0_DFP
  REAL( DFP ) :: volHeatCap_solid = 2650.0_DFP * 733.0_DFP
  LOGICAL( LGT ) :: is_volHeatCap_solid_given = .FALSE.
  PROCEDURE( mixvolheatcap_getval ), POINTER, PASS( Obj ) :: &
    & getValue => NULL()
END TYPE VolHeatCapModel_

PUBLIC :: VolHeatCapModel_

!----------------------------------------------------------------------------
!                                                   VolHeatCapModelPointer_
!----------------------------------------------------------------------------
TYPE :: VolHeatCapModelPointer_
  CLASS( VolHeatCapModel_ ), POINTER :: Ptr=>NULL()
END TYPE VolHeatCapModelPointer_

PUBLIC :: VolHeatCapModelPointer_


!----------------------------------------------------------------------------
!                                                      UserVolHeatCapModel_
!----------------------------------------------------------------------------

TYPE, EXTENDS( volHeatCapModel_ ) :: UserVolHeatCap_
END TYPE UserVolHeatCap_

TYPE( UserVolHeatCap_ ), PARAMETER, PUBLIC :: &
  & TypeUserVolHeatCap = UserVolHeatCap_()

!----------------------------------------------------------------------------
!                                                UserVolHeatCap_Pointer@User
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION UserVolHeatCap_Pointer(  ) RESULT( Ans )
  CLASS( UserVolHeatCap_ ), POINTER :: Ans
END FUNCTION UserVolHeatCap_Pointer
END INTERFACE

PUBLIC :: UserVolHeatCap_Pointer

!----------------------------------------------------------------------------
!                                                 userVolHeatCap_getval@User
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the volumetric heat capacity values

MODULE PURE FUNCTION userVolHeatCap_getval( Obj, volFrac_solid, &
  & volFrac_water, volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )
  CLASS( VolHeatCapModel_ ), INTENT( IN ) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: y
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: z
  REAL( DFP ) :: Ans
END FUNCTION userVolHeatCap_getval
END INTERFACE

!----------------------------------------------------------------------------
!                                                             MixVolHeatCap_
!----------------------------------------------------------------------------

TYPE, EXTENDS( VolHeatCapModel_ ) :: MixVolHeatCap_
  INTEGER( I4B ) :: State=STATE_UNFROZEN
  INTEGER( I4B ) :: Frozen=STATE_FROZEN
  INTEGER( I4B ) :: Unfrozen=STATE_UNFROZEN
END TYPE MixVolHeatCap_

PUBLIC :: MixVolHeatCap_

TYPE( MixVolHeatCap_ ), PUBLIC, PARAMETER :: TypeVolHeatCap=MixVolHeatCap_()

!----------------------------------------------------------------------------
!                                                       Constructor@mixModel
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION MixVolHeatCap_Pointer( volHeatCap_solid, SoilState ) &
  & RESULT( Ans )
  CLASS( MixVolHeatCap_ ), POINTER :: Ans
  REAL( DFP ), OPTIONAL :: volHeatCap_solid
  INTEGER( I4B ), OPTIONAL :: SoilState
END FUNCTION MixVolHeatCap_Pointer
END INTERFACE

PUBLIC :: MixVolHeatCap_Pointer

!----------------------------------------------------------------------------
!                                                         getValue@mixModel
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the volumetric heat capacity values

MODULE PURE FUNCTION mixvolheatcap_getval( Obj, volFrac_solid, &
  & volFrac_water, volFrac_ice, volFrac_air, Temp, x, y, z ) RESULT( Ans )
  CLASS( VolHeatCapModel_ ), INTENT( IN ) :: Obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_solid
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_water
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_ice
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: volFrac_air
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: x
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: y
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: z
  REAL( DFP ) :: Ans
END FUNCTION mixvolheatcap_getval
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VolHeatCapModel_Class