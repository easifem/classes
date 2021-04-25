!----------------------------------------------------------------------------
!                                                          ThermCond_Air
!----------------------------------------------------------------------------

PURE FUNCTION ThermCond_Air( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Air%ThermCond
END FUNCTION ThermCond_Air

!----------------------------------------------------------------------------
!                                                     SpecificHeatCap_Air
!----------------------------------------------------------------------------

PURE FUNCTION SpecificHeatCap_Air( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Air%SpecificHeatCap
END FUNCTION SpecificHeatCap_Air

!----------------------------------------------------------------------------
!                                                          volHeatCap_Air
!----------------------------------------------------------------------------

PURE FUNCTION volHeatCap_Air( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Air%volHeatCap
END FUNCTION volHeatCap_Air
