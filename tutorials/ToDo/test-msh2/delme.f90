program main
  implicit none

  integer :: i, j, k, n, p
  integer, allocatable :: nptrs( : )
  character( len = 200 ) :: str

  read( *, "(A)" ) str

  read( str, * ) i, j, k, n
  
  write( *, * ) i, j, k, n
  
  if( n .ne. 0 ) then
    allocate( nptrs( n ) )
    read( str, * ) i, j, k, n, ( nptrs( p ), p = 1, n )
  end if

  write( *, * ) nptrs

end program main