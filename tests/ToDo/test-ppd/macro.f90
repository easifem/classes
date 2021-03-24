#define _X_
#define _X1_() _X_

#define _AxB_(a, x, b) _AxB1_(a, x) ## b
#define _AxB1_(a,x) _AxB2_(a,x)
#define _AxB2_(a,x) a ## x

#define _Ax_( a, x ) _Ax1_( a, x )
#define _Ax1_( a, x ) a ## x


MODULE PROCEDURE _AxB_(Set, _X_, NeumannBoundary)

	IF( PRESENT( MeshObj ) ) THEN
		Obj % _Ax_( _X_, NeumannBoundary ) => MeshObj
		Obj % _AxB_( is, _X_, NeumannBoundary ) = .TRUE.
	ELSE IF( PRESENT( Tag ) ) THEN
		ALLOCATE( Obj % _Ax_( _X_, NeumannBoundary ) )
		CALL Obj % _Ax_( _X_, NeumannBoundary ) % Initiate( FacetElement( ) )
		CALL Obj % mshFile % getElements( Obj % _Ax_( _X_, NeumannBoundary ), Tag )
		CALL ConnectFacetToCell( Obj % Omega, Obj % _Ax_( _X_, NeumannBoundary ) )
		Obj % _AxB_( is, _X_, NeumannBoundary ) = .TRUE.
	END IF

END PROCEDURE _AxB_(Set, _X_, NeumannBoundary)
