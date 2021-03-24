program main
  implicit none
  integer :: nptrs( 3 ) = [1,2,99], po

  po = minloc( abs( nptrs - 99 ), dim=1 )
  write( *, *  ) po

  po = minloc( abs( nptrs - 50 ), dim=1 )
  write( *, *  ) po

end program main
