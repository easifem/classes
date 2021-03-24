program main
  use easifem

  type( groupelementdof_ ) :: obj
  integer( i4b ) :: telements, i
  real( dfp ) :: mat( 3, 3 )
  real( dfp ), allocatable :: mat2( :, : )
  type( blockmat_ ) :: blockobj

  telements = 4
  call obj % initiate( [telements] )
  call obj % display( )

  call equalline()
  call random_number( mat )
  do i=1,telements; call obj % setValue( mat, i ); enddo
  call obj % display( )

  call equalline( )
  call obj % getvalue( mat2, 1 )
  call displayarray( mat2, "mat" )

  call equalline()
  call obj % getvalue( blockobj, 2 )
  call blockobj % display( )


end program main
