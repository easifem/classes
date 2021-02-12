SUBMODULE( VolHeatCapModel_Class ) MixModel
USE stdMaterials
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                               johansen_constructor_pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE MixVolHeatCap_Pointer
  ALLOCATE( Ans )

  IF( PRESENT( volHeatCap_Solid ) ) THEN
    Ans%volHeatCap_solid= volHeatCap_solid
    Ans%is_volHeatCap_solid_given = .TRUE.
  ELSE
    Ans%volHeatCap_solid = volHeatCap_Quartz()
    Ans%is_volHeatCap_solid_given = .FALSE.
  END IF

  IF( PRESENT( SoilState ) ) THEN
    Ans%State = SoilState
  ELSE
    Ans%State = Ans%Unfrozen
  END IF

  Ans % getValue => mixvolheatcap_getval

END PROCEDURE MixVolHeatCap_Pointer

!----------------------------------------------------------------------------
!                                                                  getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE mixvolheatcap_getval
  REAL( DFP ) :: c_s, c_i, c_w, c_a

SELECT TYPE( Obj )
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

  Ans = VolFrac_solid * c_s + VolFrac_water * c_w + VolFrac_ice * c_i &
    & + VolFrac_air * c_a

END SELECT
END PROCEDURE mixvolheatcap_getval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MixModel