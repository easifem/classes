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
	! 	CALL COPY( x, y )
	! 	WRITE( *, "(A, G18.9)" ) "COPY( x, y ) :: error :: ", NORM2( x - y )

	! END BLOCK

	! !test-2
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	type( Real64Vector_ ) :: obj1, obj2
	! 	integer( i4b ) :: i, m = 10

	! 	allocate( y( m ) )
	! 	CALL RANDOM_NUMBER( y )
	! 	obj1 = Real64Vector( y )
	! 	CALL COPY( obj2, obj1 )
	! 	call obj2 % display( )

	! END BLOCK

	! !test-3
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	type( Real64Vector_ ), ALLOCATABLE :: obj1( : ), obj2( : )
	! 	integer( i4b ) :: i, m = 10

	! 	allocate( y( m ) )
	! 	CALL RANDOM_NUMBER( y )
	! 	obj1 = [Real64Vector( y ), Real64Vector( y ), Real64Vector( y )]
	! 	CALL COPY( obj2, obj1 )
	! 	call obj2( 1 ) % display( )

	! END BLOCK

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

	!test-timing
	BLOCK
		real( dfp ), allocatable :: y( : )
		type( Real64Vector_ ) :: obj1, obj2
		integer( i4b ) :: i, m = 1000000, n = 1000
		real( dfp ) :: t1, t2

		allocate( y( m ) )
		call RANDOM_NUMBER( y )
		obj1 = Real64Vector( y )

		call cpu_time( t1 )
		do i = 1, n
			CALL COPY( obj2, obj1 )
		end do
		call cpu_time( t2 )
		write( *, "(A, G15.6)") "COPY( obj2, obj1 ) :: ", t2 - t1

		call cpu_time( t1 )
		do i = 1, n
			CALL COPY( obj2 % Val, obj1 % Val )
		end do
		call cpu_time( t2 )
		write( *, "(A, G15.6)") "COPY( obj2 % Val, obj1 % Val ) :: ", t2 - t1

		call cpu_time( t1 )
		do i = 1, n
			obj2 % Val = Obj1 % Val
		end do
		call cpu_time( t2 )
		write( *, "(A, G15.6)") "COPY( obj1 % Val, obj2 % Val ) :: ", t2 - t1

	END BLOCK


end program main
