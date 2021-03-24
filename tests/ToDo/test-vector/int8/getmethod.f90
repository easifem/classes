program main
	use globaldata
	use io
	use int8vector_class
	implicit none

	!test-1
	! BLOCK
	! 	type( int8vector_ ) :: obj
	! 	obj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )
	! 	call obj % display( )
	! END BLOCK

	!test-2
	! BLOCK
	! 	type( int8vector_ ) :: obj
	! 	integer( int8 ), allocatable :: z( : )
	! 	obj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )
	! 	z = ArrayValues( obj, TypeInt )
	! 	obj = Int8Vector( z )
	! 	call obj % display( )
	! END BLOCK

	!test-3
	! BLOCK
	! 	type( int8vector_ ) :: obj, obj2
	! 	obj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )
	! 	obj2 = ArrayValues( obj, TypeInt8Vector )
	! 	call obj2 % display( )
	! END BLOCK

	!test-4
	! BLOCK
	! 	type( int8vector_ ) :: obj
	! 	INTEGER( Int8 ), ALLOCATABLE :: z( : )
	! 	obj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )
	! 	z = ArrayValues( obj, [1,2], TypeInt )
	! END BLOCK

	!test-5
	! BLOCK
	! 	type( int8vector_ ) :: obj, obj2
	! 	INTEGER( Int8 ), ALLOCATABLE :: z( : )
	! 	obj = Int8Vector( INT( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], int8 ) )
	! 	obj2 = ArrayValues( obj, [1,2], TypeInt8Vector )
	! 	call obj2 % display( )
	! 	z = ArrayValues( obj, 1, 10, 2, TypeInt8 )
	! 	write( *, * ) z
	! 	obj2 = ArrayValues( obj, 1, 10, 2, TypeInt8Vector )
	! 	call obj2 % display( )
	! END BLOCK

	! BLOCK
	! 	TYPE(int8vector_) :: Obj( 3 )
	! 	INTEGER( Int8 ), ALLOCATABLE :: z( : )
	! 	Obj( 1 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	Obj( 2 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	Obj( 3 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	z = ArrayValues( Obj, TypeInt8 )
	! 	write( *, * ) z
	! END BLOCK

	! BLOCK
	! 	TYPE(int8vector_) :: Obj( 3 )
	! 	INTEGER( Int8 ), ALLOCATABLE :: z( : )
	! 	Obj( 1 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	Obj( 2 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	Obj( 3 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
	! 	z = ArrayValues( Obj, [1,2,3], TypeInt8 )
	! 	write( *, * ) z
	! END BLOCK

	BLOCK
		TYPE(int8vector_) :: Obj( 3 )
		INTEGER( Int8 ), ALLOCATABLE :: z( : )
		Obj( 1 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 2 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 3 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		z = ArrayValues( Obj, 1, 10, 2, TypeInt8 )
		write( *, * ) z
	END BLOCK

	call equalline( )

	BLOCK
		TYPE(int8vector_) :: Obj( 3 ), Obj2
		Obj( 1 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 2 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 3 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj2 = ArrayValues( Obj, TypeInt8Vector )
		CALL Obj2 % Display( )
	END BLOCK

	call equalline( )

	BLOCK

		TYPE(int8vector_) :: Obj( 4 ), Obj2
		Obj( 1 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 2 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 3 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj( 4 ) = Int8Vector( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] )
		Obj2 = ArrayValues( Obj( 1:4:2 ), TypeInt8Vector )
		CALL Obj2 % Display( )
	END BLOCK

end program main