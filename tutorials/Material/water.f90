program main
  use easifem
  implicit none

  call display( thermCond_water(), "thermCond_water() :: " )
  call display( thermCond_water( Temp = 280.0_DFP ), &
    & "thermCond_water(280.0_DFP) :: " )

  call display( specificHeatCap_water( Temp = 280.0_DFP ), "specific :: " )
  call display( density_water( Temp = 280.0_DFP ), "density :: " )
  call display( volHeatCap_water( Temp = 280.0_DFP ), "vol heat capeas :: " )


end program main