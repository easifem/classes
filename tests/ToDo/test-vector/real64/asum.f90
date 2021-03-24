program main
	use globaldata
	use io
	use Vector_Class
	use BLASInterface

	! !test-timing
	! BLOCK

	! 	type( Real64Vector_ ), target :: scalar1
	! 	real( dfp ) :: x, t1, t2, meanT
	! 	real( dfp ), allocatable, target :: y( : )
	! 	real( dfp ), pointer :: ptr( : )
	! 	integer( i4b ) :: i, n = 1000, m = 5000000

	! 	allocate( y( m ) )
	! 	CALL RANDOM_NUMBER( y )
	! 	scalar1 = Real64Vector( y )

	! 	CALL CPU_TIME( t1 )
	! 	DO i = 1, n
	! 		x = ASUM( SIZE( y ), y, 1 )
	! 	END DO
	! 	CALL CPU_TIME( t2 )
	! 	WRITE( *, "(A, G18.9)" ) "Intrinsic ASUM :: time:: ", ( t2 - t1 )

	! 	CALL CPU_TIME( t1 )
	! 	DO i = 1, n
	! 		x = ASUM( scalar1 )
	! 	END DO
	! 	CALL CPU_TIME( t2 )

	! 	WRITE( *, "(A, G18.9)" ) "ASUM( scalar1 ) :: error :: ", SUM( y ) - x
	! 	WRITE( *, "(A, G18.9)" ) "ASUM( scalar1 ) :: time :: ", t2 - t1

	! 	CALL CPU_TIME( t1 )
	! 		x = ASUM( scalar1 % Val )
	! 	CALL CPU_TIME( t2 )
	! 	WRITE( *, "(A, G18.9)" ) "ASUM( scalar1 % Val ) :: error :: ", SUM( y ) - x
	! 	WRITE( *, "(A, G18.9)" ) "ASUM( scalar1 % Val ) :: time :: ", t2 - t1

	! END BLOCK

	!test-1
	BLOCK
		type( Real64Vector_ ) :: scalar1
		real( dfp ) :: x, t1, t2, meanT
		real( dfp ), allocatable :: y( : )
		integer( i4b ) :: i, m = 10

		allocate( y( m ) )
		CALL RANDOM_NUMBER( y )
		scalar1 = Real64Vector( y )
		x = ASUM( scalar1 )
		WRITE( *, "(A, G18.9)" ) "ASUM( scalar1 ) :: error :: ", SUM( ABS( y ) ) - x

	END BLOCK

	! !test-2
	! BLOCK
	! 	type( Real64Vector_ ) :: scalar1, vec1( 3 )
	! 	real( dfp ) :: y( 10 ), x, t1, t2

	! 	call RANDOM_NUMBER( y )
	! 	scalar1 = Real64Vector( y )
	! 	vec1 = [scalar1, scalar1, scalar1]

	! 	CALL CPU_TIME( t1 )
	! 		x = ASUM( vec1 )
	! 	CALL CPU_TIME( t2 )
	! 	write( *, * ) "test-2:: error:: ", 3.0 * SUM( y ) - x, "  time:: ", t2 - t1

	! END BLOCK

end program main
