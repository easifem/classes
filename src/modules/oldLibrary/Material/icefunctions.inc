
!----------------------------------------------------------------------------
!                                                              thermCond_ice
!----------------------------------------------------------------------------

PURE FUNCTION thermCond_ice( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans

  IF( PRESENT( Temp ) ) THEN
    Ans = -0.0129_DFP * Temp + 5.6716_DFP
  ELSE
    Ans = Ice%ThermCond
  END IF
END FUNCTION thermCond_ice

!----------------------------------------------------------------------------
!                                                        specificHeatCap_ice
!----------------------------------------------------------------------------

PURE FUNCTION specificHeatCap_ice( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans

  IF( PRESENT( Temp ) ) THEN
    Ans = 6.6701_DFP * Temp + 250.54_DFP
  ELSE
    Ans = Ice%specificHeatCap
  END IF

END FUNCTION specificHeatCap_ice

!----------------------------------------------------------------------------
!                                                               density_ice
!----------------------------------------------------------------------------

PURE FUNCTION density_ice( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans

  IF( PRESENT( Temp ) ) THEN
    Ans = -0.0842_DFP * Temp + 940.42_DFP
  ELSE
    Ans = Ice%density
  END IF

END FUNCTION density_ice

!----------------------------------------------------------------------------
!                                                             volHeatCap_ice
!----------------------------------------------------------------------------

PURE FUNCTION volHeatCap_ice( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans

  IF( PRESENT( Temp ) ) THEN
    Ans = -12.565_DFP * Temp ** 2 + 11636_DFP * Temp - 356326_DFP
  ELSE
    Ans = Ice%volHeatCap
  END IF

END FUNCTION volHeatCap_ice

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
