

#define TOSTRING( x ) STR( x )
#define STR( x ) #x

MODULE PROCEDURE _SUBNAME_
	! Define internal variables
	INTEGER( I4B ) :: iFacet, iel, tElements, MatProp
	CLASS( MeshConnectivity_ ), POINTER :: FacetToCell
	CLASS( GenericElement_ ), POINTER :: Elem, FacetElem

	IF( .NOT. Obj % isMeshInfoInitiated ) THEN
		ALLOCATE( Obj % MeshInfo )
		Obj % isMeshInfoInitiated = .TRUE.
		CALL Obj % MeshInfo % Initiate( Obj % Omega )
	END IF

	Obj % _isxABC_ = .TRUE.

	IF( PRESENT( MeshObj ) ) THEN
		Obj % _xABC_ => MeshObj
	ELSE IF( PRESENT( Tag ) ) THEN
		ALLOCATE( Obj % _xABC_ )
		CALL Obj % _xABC_ % Initiate( Element( ) )
		CALL Obj % mshFile % getElements( Obj % _xABC_, Tag )
	ELSE
		CALL Err_Msg( &
			& "Kernel_Class@SetMethod.f90", &
			& TOSTRING( _SUBNAME_ ) // "(Tag, MeshObj)", &
			& "One of the Tag/MeshObj should be present")
			STOP
	END IF

	ALLOCATE( FacetToCell )
	CALL FacetToCell % InitiateCellFacet( Obj % Omega, Obj % _xABC_, &
		& Obj % MeshInfo )

	! Here we set the material properties
	tElements = .tElements. Obj % _xABC_

	DO iFacet = 1, tElements
		iel = FacetToCell .CellOfFacet. iFacet
		Elem => Obj % Omega .ElementPointer. iel
		MatProp = .MaterialType. Elem
		FacetElem => Obj % _xABC_ .ElementPointer. iFacet
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