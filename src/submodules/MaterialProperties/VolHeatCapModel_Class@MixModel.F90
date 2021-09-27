SUBMODULE( VolHeatCapModel_Class ) MixModel
USE stdMaterials
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                               johansen_constructor_pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE MixVolHeatCap_Pointer
  ALLOCATE( ans )

  IF( PRESENT( volHeatCap_Solid ) ) THEN
    ans%volHeatCap_solid= volHeatCap_solid
    ans%is_volHeatCap_solid_given = .TRUE.
  ELSE
    ans%volHeatCap_solid = volHeatCap_Quartz()
    ans%is_volHeatCap_solid_given = .FALSE.
  END IF

  IF( PRESENT( SoilState ) ) THEN
    ans%State = SoilState
  ELSE
    ans%State = ans%Unfrozen
  END IF

  ans % getValue => mixvolheatcap_getval

END PROCEDURE MixVolHeatCap_Pointer

!----------------------------------------------------------------------------
!                                                                  getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE mixvolheatcap_getval
  REAL( DFP ) :: c_s, c_i, c_w, c_a

SELECT TYPE( obj )
TYPE IS (MixVolHeatCap_)

  IF( PRESENT( Temp ) ) THEN
    c_s = VolHeatCap_Quartz(Temp=Temp)
  ELSE
    c_s = VolHeatCap_Quartz()
  END IF

  IF( PRESENT( Temp ) ) THEN
    c_w = VolHeatCap_Water(Temp=Temp)
  ELSE
    c_w = VolHeatCap_Water()
  END IF

  IF( PRESENT( Temp ) ) THEN
    c_i = VolHeatCap_Ice(Temp=Temp)
  ELSE
    c_i = VolHeatCap_Ice()
  END IF

  IF( PRESENT( Temp ) ) THEN
    c_a = VolHeatCap_Air(Temp=Temp)
  ELSE
    c_a = VolHeatCap_Air()
  END IF

  ans = VolFrac_solid * c_s + VolFrac_water * c_w + VolFrac_ice * c_i &
    & + VolFrac_air * c_a

END SELECT
END PROCEDURE mixvolheatcap_getval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MixModel