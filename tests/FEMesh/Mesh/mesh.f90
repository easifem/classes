program main

  use EASIFEM

  type( msh2_ ) :: mshFile
  type( mesh_ ) :: omega
  integer( i4b ) :: iel, telements

  call mshFile % initiate( "./", "mesh", ".msh", 2 )
  call omega % initiate( stelement( ) )
  call mshFile % getElements( omega, [String("Nitsche")] )
  tElements = .tElements. omega

  do iel = 1, tElements
    call omega % elements( iel ) % display( )
  end do
end program