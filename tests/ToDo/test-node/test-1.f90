program main
  use easifem

  type( node_ ) :: nodes
  real( dfp ), allocatable :: x(:), y(:), z(:), xyz(:,:)
  integer( i4b ) :: n, nptrs( 5 )
  real( dfp ) :: t1, t2
  real( dfp ), allocatable :: xiJ( :, : )

  n = 5000000

  allocate( x( n ), y( n ), z( n ), xyz(3,n) )
  
  call random_number( x )
  call random_number( y )
  call random_number( z )

  xyz( 1, : ) = x; xyz( 2, : ) = y; xyz( 3, : ) = z

  nptrs = [1, n/5, n/10, n/2, n]

  call nodes % initiate( x, y, z )

  call cpu_time( t1 )
    call nodes % getNodes( XiJ, Nptrs )
  call cpu_time( t2 )
    write( *, * ) "n :: ", n, "time::", t2-t1

  call cpu_time( t1 )
    IF( ALLOCATED( XiJ ) ) DEALLOCATE( XiJ )
    ALLOCATE( XiJ( 3, SIZE( Nptrs ) ) )
    XiJ( 1, : ) = x( Nptrs )
    XiJ( 2, : ) = y( Nptrs )
    XiJ( 3, : ) = z( Nptrs )
  call cpu_time( t2 )
    write( *, * ) "n :: ", n, "time::", t2-t1

  call cpu_time( t1 )
    XiJ = xyz( :, Nptrs )
  call cPu_time( t2 )
  write( *, * ) "n :: ", n, "time::", t2-t1
end program main
