program main
  use easifem

  type( lis_gpbicg_ ) :: linsolver
  type( SparseTanMat_ ) :: mat
  type( groupdof_ ) :: x, b
  type( iterationdata_ ) :: iterdata
  integer( i4b ), parameter :: n = 3
  real( dfp ), allocatable :: Ke( :, : )
  
  call x % initiate( [n], ["x"], [1] )
  call b % initiate( [n], ["x"], [1] )
  b % dof( 1 ) % val = [2.0, 12.0, 5.0]
  iterdata = iterationdata( 100, 1.0d-4 )

  call mat % initiate( 1, n )
  call mat % setNptrs( [1,2,3] )
  call mat % setTanMat( )
  call mat % display( )
  Ke = RESHAPE( [1.0, 2.0, 1.0, 2.0, 2.0, -1.0, 1.0, 2.0, 2.0 ], [n,n] )
  call mat % AddContribution( Ke, [1,2,3])

  call linsolver % initiate( iterdata )
  call linsolver % solve( x, mat, b)

  call linsolver % display( )

end program main
