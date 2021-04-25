

#define TOSTRING( x ) STR( x )
#define STR( x ) #x

MODULE PROCEDURE _SUBNAME_
	! Define internal variables
	INTEGER( I4B ) :: iFacet, iel, tElements, MatProp
	CLASS( MeshConnectivity_ ), POINTER :: FacetToCell
	CLASS( GenericElement_ ), POINTER :: Elem, FacetElem

	IF( .NOT. obj % isMeshInfoInitiated ) THEN
		ALLOCATE( obj % MeshInfo )
		obj % isMeshInfoInitiated = .TRUE.
		CALL obj % MeshInfo % Initiate( obj % Omega )
	END IF

	obj % _isxABC_ = .TRUE.

	IF( PRESENT( Meshobj ) ) THEN
		obj % _xABC_ => Meshobj
	ELSE IF( PRESENT( Tag ) ) THEN
		ALLOCATE( obj % _xABC_ )
		CALL obj % _xABC_ % Initiate( Element( ) )
		CALL obj % mshFile % getElements( obj % _xABC_, Tag )
	ELSE
		CALL Err_Msg( &
			& "Kernel_Class@SetMethod.f90", &
			& TOSTRING( _SUBNAME_ ) // "(Tag, Meshobj)", &
			& "One of the Tag/Meshobj should be present")
			STOP
	END IF

	ALLOCATE( FacetToCell )
	CALL FacetToCell % InitiateCellFacet( obj % Omega, obj % _xABC_, &
		& obj % MeshInfo )

	! Here we set the material properties
	tElements = .tElements. obj % _xABC_

	DO iFacet = 1, tElements
		iel = FacetToCell .CellOfFacet. iFacet
		Elem => obj % Omega .ElementPointer. iel
		MatProp = .MaterialType. Elem
		FacetElem => obj % _xABC_ .ElementPointer. iFacet
		CALL FacetElem % setMatType( MatProp )
	END DO

	DEALLOCATE( FacetToCell )
	NULLIFY( FacetToCell, Elem, FacetElem )

END PROCEDURE _SUBNAME_

#undef _xABC_
#undef _isxABC_
#undef _SUBNAME_
#undef TOSTRING
#undef STR