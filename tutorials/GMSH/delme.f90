program main
  real, pointer :: ptr

  ptr => null( )
  write( *, * ) "ptr => null( ) :: ", ASSOCIATED( ptr )
  allocate( ptr )
  write( *, * ) "allocate(ptr) :: ", ASSOCIATED( ptr )

end program main