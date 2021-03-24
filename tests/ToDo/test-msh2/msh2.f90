program main

  use EASIFEM

  type( msh2_ ) :: mshFile
  type( mesh_ ) :: omega
  type( node_ ) :: nodes
  type( groupdof_ ) :: x
  integer( i4b ) :: iel, telements
  integer( i4b), allocatable :: nptrs( : ), indx( : )

  call mshFile % initiate( "./", "mesh", ".msh", 2 )
  !call omega % initiate( element( ) )

  ! call mshFile % getElements( omega )
  ! call mshFile % getElements( omega, "Nitsche" )
  !call mshFile % getElements( omega, [String("Nitsche")] )
  !tElements = .tElements. omega
  !x = mshFile
  !call x % display( )
  !nodes = mshFile
  !call nodes % display( )

  call mshfile % getNptrs( Nptrs, [String("Nitsche")] )
  write( *, * ) nptrs

end program
