program main
use basetype
use basemethod
implicit none

!Trace

! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), invar

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 1.0; T( 3, 3 ) = 1.0
! 	obj = T
! 	call display( obj, "obj = " )
! 	invar = trace( obj, 1 )
! 	write( *, * ) invar
! end block

! ! J2
! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), invar

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 1.0; T( 3, 3 ) = 1.0
! 	obj = T
! 	call display( obj, "obj = " )
! 	invar = J2( obj )
! 	write( *, * ) invar
! end block

!J3
! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), invar

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 2.0; T( 3, 3 ) = 3.0
! 	obj = T
! 	call display( obj, "obj = " )
! 	invar = J3( obj, .false. )
! 	write( *, * ) invar
! end block

!Det
! block
! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), invar

! 	call RANDOM_NUMBER( T )
! 	CALL convert( From = T, To = Obj )
! 	call display( obj, "obj = " )
! 	invar = Det( obj )
! 	write( *, * ) invar
! end block

! ! iso part
! block
! 	type( rank2tensor_ ) :: obj, iso
! 	real( dfp ) :: T( 3, 3 )

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 1.0; T( 3, 3 ) = 1.0
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 2.0; T( 3, 3 ) = 3.0
! 	obj = T
! 	call display( obj, "obj = " )

! 	iso = IsotropicPart( obj )
! 	call display( iso, "isotropic part = " )

! end block

! ! dev part
! block
! 	type( rank2tensor_ ) :: obj, dev
! 	real( dfp ) :: T( 3, 3 )

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 1.0; T( 3, 3 ) = 1.0
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 2.0; T( 3, 3 ) = 3.0
! 	obj = T
! 	call display( obj, "obj = " )

! 	dev = DeviatoricPart( obj )
! 	call display( dev, "DeviatoricPart = " )

! end block

! ! dev part
! block
! 	type( rank2tensor_ ) :: obj, dev
! 	real( dfp ) :: T( 3, 3 )

! 	T = 0.0_DFP
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 1.0; T( 3, 3 ) = 1.0
! 	T( 1, 1 ) = 1.0; T( 2, 2 ) = 2.0; T( 3, 3 ) = 3.0
! 	obj = T
! 	call display( obj, "obj = " )

! 	dev = DeviatoricPart( obj )
! 	call display( dev, "DeviatoricPart = " )

! end block

! ! contraction part

! block

! 	type( rank2tensor_ ) :: obj
! 	type( voigtrank2tensor_) :: v, w
! 	real( dfp ) :: T( 3, 3 ), ans

! 	T = RESHAPE( [1, 2, 3, 2, 4, 5, 3, 5, 6], [3, 3] )
! 	obj = T
! 	call display( obj, "sym(obj)")

! 	ans = Contraction( obj, obj )
! 	write( *, * ) ans

! 	call initiate( v, obj, stresstypevoigt )
! 	ans = Contraction( v, v )
! 	write( *, * ) ans

! 	call initiate( w, obj, straintypevoigt )
! 	ans = Contraction( w, w )
! 	write( *, * ) ans

! 	ans = Contraction( v, w )
! 	write( *, * ) ans

! end block

! ! spectral part

! block

! 	use io

! 	type( rank2tensor_ ) :: obj
! 	real( dfp ) :: T( 3, 3 ), Q( 3, 3 ), W( 3 ), Dummy( 3 )

! 	T = RESHAPE( [2, 1, 0, 1, 2, 1, 0, 1, 2], [3, 3] )
! 	obj = T
! 	call display( obj, "sym(obj)")

! 	call symEigen( Obj, Q, W )

! 	call display_array( Q, "Q" )
! 	call display_array( W, "W" )

! 	write( *, * ) maxval( W ), MAXLOC( W )

! 	! call display_array( MATMUL( T, Q( :, 1 ) ) / W( 1 ) - Q( :, 1 ), "0" )
! 	! call display_array( MATMUL( T, Q( :, 2 ) ) / W( 2 ) - Q( :, 2 ), "0" )
! 	! call display_array( MATMUL( T, Q( :, 3 ) ) / W( 3 ) - Q( :, 3 ), "0" )

! end block

! ! spectral part

! block

! 	use io

! 	type( rank2tensor_ ) :: obj
! 	REAL( dfp ) :: T( 3, 3 )
! 	COMPLEX( dfp ) :: Q( 3, 3 ), W( 3 ), Dummy( 3 )

! 	T = RESHAPE( [2, 1, 0, 1, 2, 1, 0, 1, 2], [3, 3] )
! 	obj = T
! 	call display( obj, "sym(obj)")

! 	call Eigen( Obj, Q, W )

! 	call display_array( REAL( Q ), "REAL Q" )
! 	call display_array( REAL( W ), "REAL W" )

! 	call display_array( AIMAG( Q ), "IMG Q" )
! 	call display_array( AIMAG( W ), "IMG W" )

! end block

! polar decomposition
! block

! 	use io

! 	type( rank2tensor_ ) :: obj, R, U
! 	real( dfp ) :: T( 3, 3 )

! 	T = RESHAPE( [2, 1, 0, 1, 2, 1, 0, 1, 2], [3, 3] )
! 	obj = T
! 	call display( obj, "sym(obj)")

! 	!F = RU
! 	call rightpolardecomp( obj, R, U )
! 	call display( R, "R" )
! 	call display( U, "U" )

! end block

! block

! 	use io

! 	type( rank2tensor_ ) :: obj, R, U
! 	real( dfp ) :: T( 3, 3 )

! 	T = RESHAPE( [1.0, -0.333, 0.959, 0.495, 1.0, .0, 0.5, -0.247, 1.5], [3, 3] )
! 	obj = T
! 	call display( obj, "sym(obj)")

! 	!F = RU
! 	call rightpolardecomp( obj, R, U )
! 	call display( R, "R" )
! 	call display( U, "U" )

! end block

!exponential

block

	use io

	type( rank2tensor_ ) :: obj, expobj
	real( dfp ) :: T( 3, 3 )

	T = RESHAPE( [2, 1, 0, 1, 2, 1, 0, 1, 2], [3, 3] )
	obj = T
	call display( obj, "sym(obj)")

	expobj = exp( obj )

	call display( expobj, "exp" )
	
	! call display_array( MATMUL( T, Q( :, 1 ) ) / W( 1 ) - Q( :, 1 ), "0" )
	! call display_array( MATMUL( T, Q( :, 2 ) ) / W( 2 ) - Q( :, 2 ), "0" )
	! call display_array( MATMUL( T, Q( :, 3 ) ) / W( 3 ) - Q( :, 3 ), "0" )

end block

end program main
