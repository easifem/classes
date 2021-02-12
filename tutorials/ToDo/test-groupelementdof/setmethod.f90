program main
  use easifem

  type( groupelementdof_ ) :: obj
  integer( i4b ) :: telements
  real( dfp ) :: mat( 3, 3 )
  type( blockmat_ ) :: blockobj

  telements = 4
  call obj % initiate( [telements] )
  call obj % display( )

  call equalline()
  call random_number( mat )
  call obj % setValue( mat, 1 )
  call obj % display( )

  call equalline( )
  call obj % setValue( mat, 2 )
  call obj % display( )

  call equalline( )
  blockobj = mat
  call obj % setvalue( blockobj, 3 )
  call obj % display( )

end program main
