program main
	use globaldata
	use io
	use Vector_Class
	implicit none

	! !test-1
	! BLOCK
	! 	real( dfp ), allocatable :: y( : ), x( : )
	! 	integer( i4b ) :: i, m = 10

	! 	y = [ 1, 2, 3]
	! 	x = [ 0, 0, 0 ]
	! 	CALL AXPY( x, 1.0_dfp, y )
	! 	CALL Display( x, "x = " )

	! END BLOCK

	! !test-2
	! BLOCK
	! 	type( Real64Vector_ ) :: obj1, obj2
	! 	integer( i4b ) :: i, m = 10

	! 	obj1 = Real64Vector( [1, 2] )
	! 	obj2 = Real64Vector( [0, 0] )

	! 	CALL AXPY(obj1, 1.0_dfp, obj2 )
	! 	CALL Display( obj1, "obj = " )

	! END BLOCK

	!test-3
	BLOCK
		type( Real64Vector_ ) :: obj1( 3 ), obj2( 3 )
		integer( i4b ) :: i, m = 10

		obj1( 1 : 3 ) = Real64Vector( [1, 2] )
		obj2( 1 : 3 ) = Real64Vector( [0, 0] )

		CALL AXPY(obj1, 1.0_dfp, obj2 )
		CALL Display( obj1, "obj = " )

	END BLOCK

end program main
