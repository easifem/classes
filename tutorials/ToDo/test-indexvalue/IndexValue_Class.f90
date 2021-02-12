MODULE IndexValue_Class
	USE GlobalData

	IMPLICIT NONE

	PRIVATE

	TYPE :: IndexValue_
		INTEGER( I4B ) :: Indx
		REAL( DFP ) :: Val
	END TYPE

	PUBLIC :: IndexValue_

	INTERFACE IndexValue
		MODULE PROCEDURE Constructor1, Constructor2, Constructor3
	END INTERFACE IndexValue

	PUBLIC :: IndexValue

	CONTAINS

		PURE FUNCTION Constructor1( Indx, Val ) RESULT( Obj )
			INTEGER( I4B ), INTENT( IN ) :: Indx
			REAL( DFP ), INTENT( IN ) :: Val
			TYPE(IndexValue_) :: Obj

			Obj % Indx = Indx
			Obj % Val = Val

		END FUNCTION Constructor1

		PURE FUNCTION Constructor2( Indx, Val ) RESULT( Obj )
			INTEGER( I4B ), INTENT( IN ) :: Indx( : )
			REAL( DFP ), INTENT( IN ) :: Val( : )
			TYPE(IndexValue_), ALLOCATABLE :: Obj( : )

			INTEGER( I4B ) :: n, i

			n = SIZE( Indx )
			ALLOCATE( Obj( n ) )
			DO i = 1, n
				Obj( i ) % Indx = Indx( i )
				Obj( i ) % Val = Val( i )
			END DO

		END FUNCTION Constructor2

		PURE FUNCTION Constructor3( Indx, Val ) RESULT( Obj )
			INTEGER( I4B ), INTENT( IN ) :: Indx( : )
			REAL( DFP ), INTENT( IN ) :: Val
			TYPE(IndexValue_), ALLOCATABLE :: Obj( : )

			INTEGER( I4B ) :: n, i

			n = SIZE( Indx )
			ALLOCATE( Obj( n ) )
			DO i = 1, n
				Obj( i ) % Indx = Indx( i )
				Obj( i ) % Val = Val
			END DO

		END FUNCTION Constructor3

END MODULE IndexValue_Class