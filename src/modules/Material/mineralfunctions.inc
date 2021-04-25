!----------------------------------------------------------------------------
!                                                          ThermCond_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION ThermCond_Quartz( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Quartz%ThermCond
END FUNCTION ThermCond_Quartz

!----------------------------------------------------------------------------
!                                                     SpecificHeatCap_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION SpecificHeatCap_Quartz( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Quartz%SpecificHeatCap
END FUNCTION SpecificHeatCap_Quartz

!----------------------------------------------------------------------------
!                                                          volHeatCap_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION volHeatCap_Quartz( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Quartz%volHeatCap
END FUNCTION volHeatCap_Quartz
