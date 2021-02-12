program main
	use globaldata
	use io
	use Real64Vector_Class

	!test-1
	! BLOCK
	!   type( Real64Vector_ ) :: scalar1, scalar2
	!   real( dfp ) :: y( 10 ), x, t1, t2

	!   call RANDOM_NUMBER( y )
	!   scalar1 = Real64Vector( y )
	! 	scalar2 = Real64Vector( scalar1 )

	! 	CALL scalar1 % display( )
	! 	CALL scalar2 % display( )

	!   CALL CPU_TIME( t1 )
	!     x = DOT( scalar1, scalar2 )
	!   CALL CPU_TIME( t2 )

	!   write( *, * ) "test-1:: error:: ", DOT_PRODUCT( y, y ) - x, "  time:: ", t2 - t1

	! END BLOCK

	!test-2
	! BLOCK
	! 	type( Real64Vector_ ) :: scalar1, vec1( 3 ), vec2( 3 )
	! 	real( dfp ) :: y( 10 ), x, t1, t2

	! 	call RANDOM_NUMBER( y )
	! 	scalar1 = Real64Vector( y )

	! 	vec1 = [scalar1, scalar1, scalar1]
	! 	vec2 = vec1

	! 	CALL CPU_TIME( t1 )
	! 	x = DOT( vec1, vec2 )
	! 	CALL CPU_TIME( t2 )
	! 	write( *, * ) "test-2:: error:: ", 3.0 * DOT_PRODUCT( y, y ) - x, "  time:: ", t2 - t1

	! END BLOCK

	! !test-3
	! BLOCK
	! 	type( Real64Vector_ ) :: scalar1, vec1( 3 )
	! 	real( dfp ) :: y( 10 ), x, t1, t2

	! 	call RANDOM_NUMBER( y )
	! 	scalar1 = Real64Vector( y )

	! 	vec1 = [scalar1, scalar1, scalar1]

	! 	CALL CPU_TIME( t1 )
	! 	x = DOT( vec1, scalar1 )
	! 	CALL CPU_TIME( t2 )
	! 	write( *, * ) "test3:: error:: ", 3.0 * DOT_PRODUCT( y, y ) - x, "  time:: ", t2 - t1

	! END BLOCK

	!test-4
	BLOCK
		type( Real64Vector_ ) :: scalar1, vec1( 3 )
		real( dfp ) :: y( 10 ), x, t1, t2

		call RANDOM_NUMBER( y )
		scalar1 = Real64Vector( y )

		vec1 = [scalar1, scalar1, scalar1]

		CALL CPU_TIME( t1 )
		x = DOT( scalar1, vec1 )
		CALL CPU_TIME( t2 )
		write( *, * ) "test4:: error:: ", 3.0 * DOT_PRODUCT( y, y ) - x, "  time:: ", t2 - t1

	END BLOCK
end program main
