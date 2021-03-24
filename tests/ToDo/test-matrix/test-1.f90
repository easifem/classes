program main
implicit none

real :: a( 3, 3 )
integer :: n, nptrs( 4 )
integer, allocatable :: m( : )

n = size( a )

write( *, * ) "n = ", n

m = shape( nptrs )
write( *, * ) "m :: ", m

end program main