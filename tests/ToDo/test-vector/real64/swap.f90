program main
	use globaldata
	use io
	use Vector_Class
	implicit none

	! !test-1
	! BLOCK
	! 	real( dfp ), allocatable :: y( : ), x( : )
	! 	integer( i4b ) :: i, m = 10

	! 	allocate( y( m ) )
	! 	CALL RANDOM_NUMBER( y )
	! 	x = 2.0 * y
	! 	CALL SWAP( x, y )
	! 	WRITE( *, "(A, G18.9)" ) "SWAP( x, y ) :: error :: ", NORM2( y - 2.0*x )

	! END BLOCK

	! !test-2
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	type( Real64Vector_ ) :: obj1, obj2
	! 	integer( i4b ) :: i, m = 4

	! 	allocate( y( m ) )
	! 	CALL RANDOM_NUMBER( y )
	! 	obj1 = Real64Vector( y )
	! 	obj2 = Real64Vector( 2.0 * y )
	! 	CALL SWAP( obj1, obj2 )
	! 	WRITE( *, "(A, G18.9)" ) "SWAP( obj1, obj2 ) :: error :: ", NORM2( 2 * obj2 % val - obj1 % val )

	! END BLOCK

	!test-3
	BLOCK
		real( dfp ), allocatable :: y( : )
		type( Real64Vector_ ), ALLOCATABLE :: obj1( : ), obj2( : )
		integer( i4b ) :: i, m = 2

		allocate( y( m ) )
		CALL RANDOM_NUMBER( y )
		obj1 = [Real64Vector( y ), Real64Vector( y ), Real64Vector( y )]
		obj2 = [Real64Vector( 2*y ), Real64Vector( 2*y ), Real64Vector( 2*y )]
		call display( obj1( 1 ) )
		CALL SWAP( obj2, obj1 )
		call display( obj2( 1 ) )
	END BLOCK

	! !test-3
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	type( Real64Vector_ ), ALLOCATABLE :: obj2( : )
	! 	type( Real64Vector_ ) :: obj1
	! 	integer( i4b ) :: i, m = 10

	! 	y = [ 1., 2., 3.]
	! 	obj2 = [Real64Vector( y ), Real64Vector( y ), Real64Vector( y )]
	! 	CALL COPY( obj1, obj2 )
	! 	call obj1 % display( )

	! END BLOCK

	! !test-timing
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	type( Real64Vector_ ) :: obj1, obj2
	! 	integer( i4b ) :: i, m = 1000000, n = 1000
	! 	real( dfp ) :: t1, t2

	! 	allocate( y( m ) )
	! 	call RANDOM_NUMBER( y )
	! 	obj1 = Real64Vector( y )

	! 	call cpu_time( t1 )
	! 	do i = 1, n
	! 		CALL COPY( obj2, obj1 )
	! 	end do
	! 	call cpu_time( t2 )
	! 	write( *, "(A, G15.6)") "COPY( obj2, obj1 ) :: ", t2 - t1

	! 	call cpu_time( t1 )
	! 	do i = 1, n
	! 		CALL COPY( obj2 % Val, obj1 % Val )
	! 	end do
	! 	call cpu_time( t2 )
	! 	write( *, "(A, G15.6)") "COPY( obj2 % Val, obj1 % Val ) :: ", t2 - t1

	! 	call cpu_time( t1 )
	! 	do i = 1, n
	! 		obj2 % Val = Obj1 % Val
	! 	end do
	! 	call cpu_time( t2 )
	! 	write( *, "(A, G15.6)") "COPY( obj1 % Val, obj2 % Val ) :: ", t2 - t1

	! END BLOCK


end program main
