program main
  use easifem

  type( groupelementdof_ ) :: obj
  integer( i4b ) :: telements
  class( blockmat_ ), pointer :: blockobj
  real( dfp ), pointer :: mat( :, : )

  telements = 4
  call obj % initiate( telements )
  call obj % display( )
  call obj % initiate( telements, 1, 1 )
  call obj % display( )
  call obj % initiate( telements, 2, 2 )
  call obj % display( )
  call obj % reset( 1.0_dfp )
  call obj % display( )
  call obj % initiate( 1, 2, 3 )
  call obj % reset( 2.0_dfp )
  call obj % display( )
  call displayarray( (obj .value. 1), ".value." )
 
  blockobj => obj .BlockmatPointer. 1
  call blockobj % display( )
  
  mat => obj .valuepointer. 1
  call displayarray( mat, ".valuepointer" )

end program main
