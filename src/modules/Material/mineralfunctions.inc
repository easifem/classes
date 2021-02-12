!----------------------------------------------------------------------------
!                                                          ThermCond_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION ThermCond_Quartz( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans
  Ans = Quartz%ThermCond
END FUNCTION ThermCond_Quartz

!----------------------------------------------------------------------------
!                                                     SpecificHeatCap_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION SpecificHeatCap_Quartz( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans
  Ans = Quartz%SpecificHeatCap
END FUNCTION SpecificHeatCap_Quartz

!----------------------------------------------------------------------------
!                                                          volHeatCap_Quartz
!----------------------------------------------------------------------------

PURE FUNCTION volHeatCap_Quartz( Temp, Pressure ) RESULT( Ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: Ans
  Ans = Quartz%volHeatCap
END FUNCTION volHeatCap_Quartz
