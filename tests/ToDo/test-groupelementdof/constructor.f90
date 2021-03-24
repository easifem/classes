program main
  use easifem

  type( groupelementdof_ ) :: obj
  integer( i4b ) :: telements
  class( blockmat_ ), pointer :: blockobj
  real( dfp ), pointer :: mat( :, : )

  telements = 4
  call obj % initiate( [telements] )
  call obj % display( )

  call equalline()
  call obj % initiate( [telements, 1, 1] )
  call obj % display( )

  call equalline( )
  call obj % initiate( [telements, 2, 2] )
  call obj % display( )

  call equalline( )
  call obj % reset( 1.0_dfp )
  call obj % display( )

end program main
