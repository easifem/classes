
!----------------------------------------------------------------------------
!                                                       ThermalConductivity
!----------------------------------------------------------------------------

PURE FUNCTION thermCond_water( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  !! internal variables
  CHARACTER( LEN = 2 ) :: thermCondCase

  IF( PRESENT( Temp ) ) THEN
    thermCondCase( 1:1 ) = 'T'
  ELSE
    thermCondCase( 1:1 ) = 't'
  END IF

  IF( PRESENT( Pressure ) ) THEN
    thermCondCase( 2:2 ) = 'P'
  ELSE
    thermCondCase( 2:2 ) = 'p'
  END IF

  SELECT CASE( thermCondCase )
  CASE( 'TP' )
    ans = thermCond_water_Temp_Pressure( Temp, Pressure )
  CASE( 'Tp' )
    ans = thermCond_water_Temp( Temp )
  CASE( 'tP' )
    ans = thermCond_water_Pressure( Pressure )
  CASE( 'tp' )
    ans = Water%thermCond
  END SELECT

END FUNCTION thermCond_water

!----------------------------------------------------------------------------
!                                              thermCond_water_Temp_Pressure
!----------------------------------------------------------------------------

PURE FUNCTION thermCond_water_Temp_Pressure(Temp, Pressure) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  ans = 0.6065 * ( -1.48445 + 4.12292 * ( Temp / 298.15_DFP ) - 1.63866 * &
    & ( Temp / 298.15_DFP ) ** 2 )

END FUNCTION thermCond_water_Temp_Pressure

!----------------------------------------------------------------------------
!                                                      thermCond_water_Temp
!----------------------------------------------------------------------------

PURE FUNCTION thermCond_water_Temp(Temp) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: ans

  ans = 0.6065 * ( -1.48445 + 4.12292 * ( Temp / 298.15_DFP ) - 1.63866 * &
    & ( Temp / 298.15_DFP ) ** 2 )

END FUNCTION thermCond_water_Temp

!----------------------------------------------------------------------------
!                                                  thermCond_water_Pressure
!----------------------------------------------------------------------------

PURE FUNCTION thermCond_water_pressure(Pressure) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  ans = Water%thermCond
END FUNCTION thermCond_water_pressure

!----------------------------------------------------------------------------
!                                                       SpecificHeatCapacity
!----------------------------------------------------------------------------

PURE FUNCTION specificHeatCap_water( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  !! internal variables
  CHARACTER( LEN = 2 ) :: specificHeatCapCase

  IF( PRESENT( Temp ) ) THEN
    specificHeatCapCase( 1:1 ) = 'T'
  ELSE
    specificHeatCapCase( 1:1 ) = 't'
  END IF

  IF( PRESENT( Pressure ) ) THEN
    specificHeatCapCase( 2:2 ) = 'P'
  ELSE
    specificHeatCapCase( 2:2 ) = 'p'
  END IF

  SELECT CASE( specificHeatCapCase )
  CASE( 'TP' )
    ans = specificHeatCap_water_Temp_Pressure( Temp, Pressure )
  CASE( 'Tp' )
    ans = specificHeatCap_water_Temp( Temp )
  CASE( 'tP' )
    ans = specificHeatCap_water_Pressure( Pressure )
  CASE( 'tp' )
    ans = Water%specificHeatCap
  END SELECT

END FUNCTION specificHeatCap_water


!----------------------------------------------------------------------------
!                                        specificHeatCap_water_Temp_Pressure
!----------------------------------------------------------------------------

PURE FUNCTION specificHeatCap_water_Temp_Pressure(Temp, Pressure) &
  & RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  IF( Temp .LE. 373.15_DFP ) THEN
    ans = Water%SpecificHeatCap
  ELSE IF( Temp .GT. 373.15_DFP ) THEN
    ans = 0.0196_DFP * Temp**2 - 13.785_DFP * Temp + 6638.6_DFP
  END IF

END FUNCTION specificHeatCap_water_Temp_Pressure

!----------------------------------------------------------------------------
!                                                specificHeatCap_water_Temp
!----------------------------------------------------------------------------

PURE FUNCTION specificHeatCap_water_Temp(Temp) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: ans

  IF( Temp .LE. 373.15_DFP ) THEN
    ans = Water%SpecificHeatCap
  ELSE IF( Temp .GT. 373.15_DFP ) THEN
    ans = 0.0196_DFP * Temp**2 - 13.785_DFP * Temp + 6638.6_DFP
  END IF

END FUNCTION specificHeatCap_water_Temp

!----------------------------------------------------------------------------
!                                             specificHeatCap_water_Pressure
!----------------------------------------------------------------------------

PURE FUNCTION specificHeatCap_water_pressure(Pressure) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Water%specificHeatCap
END FUNCTION specificHeatCap_water_pressure

!----------------------------------------------------------------------------
!                                                             density_water
!----------------------------------------------------------------------------

PURE FUNCTION density_water( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  !! internal variables
  CHARACTER( LEN = 2 ) :: densityCase

  IF( PRESENT( Temp ) ) THEN
    densityCase( 1:1 ) = 'T'
  ELSE
    densityCase( 1:1 ) = 't'
  END IF

  IF( PRESENT( Pressure ) ) THEN
    densityCase( 2:2 ) = 'P'
  ELSE
    densityCase( 2:2 ) = 'p'
  END IF

  SELECT CASE( densityCase )
  CASE( 'TP ')
    ans = density_water_Temp_Pressure( Temp, Pressure )
  CASE( 'Tp ')
    ans = density_water_Temp( Temp )
  CASE( 'tP ')
    ans = density_water_Pressure( Pressure )
  CASE( 'tp ')
    ans = Water%density
  END SELECT

END FUNCTION density_water

!----------------------------------------------------------------------------
!                                                density_water_Temp_Pressure
!----------------------------------------------------------------------------

PURE FUNCTION density_water_Temp_Pressure( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  IF( Temp .LE. 290.0_DFP ) THEN
    ans=Water%density
  ELSE
    ans=-0.0036*Temp**2 + 1.8817*Temp + 752.94
    ans = ans / (1.0_DFP - (Pressure - 1.0E+5) / Water%bulkModulus )
  END IF

END FUNCTION density_water_Temp_Pressure

!----------------------------------------------------------------------------
!                                                         density_water_Temp
!----------------------------------------------------------------------------

PURE FUNCTION density_water_temp( Temp ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: ans

  IF( Temp .LE. 290.0_DFP ) THEN
    ans=Water%density
  ELSE
    ans=-0.0036*Temp**2 + 1.8817*Temp + 752.94
  END IF

END FUNCTION density_water_temp

!----------------------------------------------------------------------------
!                                                     density_water_pressure
!----------------------------------------------------------------------------

PURE FUNCTION density_water_pressure( Pressure ) RESULT( ans )
  REAL( DFP ), INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans
  ans = Water%density / (1.0_DFP - (Pressure - 1.0E+5) / Water%bulkModulus )
END FUNCTION density_water_pressure

!----------------------------------------------------------------------------
!                                                          volHeatCap_water
!----------------------------------------------------------------------------

PURE FUNCTION volHeatCap_water( Temp, Pressure ) RESULT( ans )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Temp
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Pressure
  REAL( DFP ) :: ans

  !! internal variables
  CHARACTER( LEN = 2 ) :: temp_press

  IF( PRESENT( Temp ) ) THEN
    temp_press( 1:1 ) = 'T'
  ELSE
    temp_press( 1:1 ) = 't'
  END IF

  IF( PRESENT( Pressure ) ) THEN
    temp_press( 2:2 ) = 'P'
  ELSE
    temp_press( 2:2 ) = 'p'
  END IF

  SELECT CASE( temp_press )
  CASE( 'TP ')
    ans = density_water_Temp_Pressure( Temp, Pressure ) * &
      & specificHeatCap_water_Temp_Pressure( Temp, Pressure )
  CASE( 'Tp ')
    ans = density_water_Temp( Temp ) * &
      & specificHeatCap_water_Temp( Temp )
  CASE( 'tP ')
    ans = density_water_Pressure( Pressure ) * &
      & specificHeatCap_water_pressure( Pressure )
  CASE( 'tp ')
    ans = Water%density * Water%specificHeatCap
  END SELECT
END FUNCTION volHeatCap_water

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
