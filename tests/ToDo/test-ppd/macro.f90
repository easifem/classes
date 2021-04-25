#define _X_
#define _X1_() _X_

#define _AxB_(a, x, b) _AxB1_(a, x) ## b
#define _AxB1_(a,x) _AxB2_(a,x)
#define _AxB2_(a,x) a ## x

#define _Ax_( a, x ) _Ax1_( a, x )
#define _Ax1_( a, x ) a ## x


MODULE PROCEDURE _AxB_(Set, _X_, NeumannBoundary)

	IF( PRESENT( Meshobj ) ) THEN
		obj % _Ax_( _X_, NeumannBoundary ) => Meshobj
		obj % _AxB_( is, _X_, NeumannBoundary ) = .TRUE.
	ELSE IF( PRESENT( Tag ) ) THEN
		ALLOCATE( obj % _Ax_( _X_, NeumannBoundary ) )
		CALL obj % _Ax_( _X_, NeumannBoundary ) % Initiate( FacetElement( ) )
		CALL obj % mshFile % getElements( obj % _Ax_( _X_, NeumannBoundary ), Tag )
		CALL ConnectFacetToCell( obj % Omega, obj % _Ax_( _X_, NeumannBoundary ) )
		obj % _AxB_( is, _X_, NeumannBoundary ) = .TRUE.
	END IF

END PROCEDURE _AxB_(Set, _X_, NeumannBoundary)
