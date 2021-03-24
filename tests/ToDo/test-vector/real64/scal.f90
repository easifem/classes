program main
	use globaldata
	use io
	use Vector_Class
	implicit none

	! !test-1
	! BLOCK
	! 	real( dfp ), allocatable :: y( : )
	! 	integer( i4b ) :: i, m = 10

	! 	y = [ 1, 2, 3]
	! 	CALL SCAL( 2.0_DFP, y )
	! 	CALL Display( y, "y = " )

	! END BLOCK

	!test-2
	BLOCK
		real( dfp ), allocatable :: y( : )
		type( Real64Vector_ ) :: obj
		integer( i4b ) :: i, m = 10

		y = [ 1, 2, 3]
		obj = Real64Vector( y )
		CALL SCAL( 2.0_DFP, obj )
		CALL Display( obj, "2*obj = " )

	END BLOCK

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

end program main
