SUBMODULE( MeshConnectivity_Class ) Methods
  !! This module contains type bound procedures of [[MeshConnectivity_]]
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_deallocate_data
  IF( ALLOCATED( Obj % CellFacet ) ) DEALLOCATE( Obj % CellFacet )
  IF( ALLOCATED( Obj % CellCell ) ) DEALLOCATE( Obj % CellCell )
  IF( ALLOCATED( Obj % NodeToNodes ) ) DEALLOCATE( Obj % NodeToNodes )
END PROCEDURE mc_deallocate_data

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_init_cell_facet
  LOGICAL( LGT ) :: found
  INTEGER( I4B ) :: ifacet, icell, elemNum, i, r, j, k
  CLASS( Element_ ), POINTER :: Elem, FacetElem
  INTEGER( I4B ), ALLOCATABLE :: FacetNptrs( : ), pt2elem( : ), bndyData( : )
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs( : ),CellFacetNptrs(:)

  !<--- if cell mesh data not initiated then initiate it
  IF( .NOT. CellMeshData % isInitiated ) THEN
    CALL CellMeshData % Initiate( CellMesh )
  END IF

  !<--- generate node to elements and boundary data
  CALL CellMeshData % InitiateNodeToElements( CellMesh )
  CALL CellMeshData % InitiateBoundaryData( CellMesh )

  !<--- allocate CellFacet size
  CALL Reallocate( Obj % CellFacet, 2_I4B, FacetMesh % SIZE() )

  Elem => NULL( ); FacetElem => NULL( )

  DO ifacet = 1, SIZE( Obj % CellFacet, 2 )
    found = .false.

    !<--- get facet element and its nptrs
    FacetElem => FacetMesh % Elem( ifacet ) % Ptr
    FacetNptrs = .Nptrs. FacetElem

    !<--- select an arbitrary node in FacetNptr; if this is not
    !<--- present inside the CellMesh then this means this iFacet is
    !<--- orphan and we go back to the next Facet Element
    IF( .NOT. CellMeshData % isNodePresent( FacetNptrs( 1 ) ) ) CYCLE

    !<--- Now we have ensured that iFacet can be a facet element
    !<--- so we get the element in Cell mesh surrouding this node
    pt2elem = CellMeshData % NodetoElements( FacetNptrs( 1 ))

    DO icell = 1, SIZE( pt2elem )
      elemNum = pt2elem( icell )

      !<--- if this element is not a boundary element then discard it
      IF( .NOT. CellMeshData % isBoundaryElement( ElemNum ) ) CYCLE

      bndyData = CellMeshData % BoundaryElementData( elemNum )
      Elem => CellMesh % Elem( elemNum ) % Ptr
      FM = FacetMatrix( Elem % RefElem )
      CellNptrs = .Nptrs. Elem

      DO i = 1, SIZE( bndyData )
        CellFacetNptrs = &
        & CellNptrs( FM( BndyData( i ), 4 : FM( BndyData( i ), 3 ) + 3 ) )
        r = 0

        DO j = 1, SIZE( FacetNptrs )
          DO k = 1, SIZE( CellFacetNptrs )
            IF( FacetNptrs( j ) .EQ. CellFacetNptrs( k ) ) THEN
              r = r + 1
            END IF
          END DO
        END DO
        IF( r .EQ. SIZE( FacetNptrs ) ) THEN
          Found = .TRUE.
          Obj % CellFacet( 1, iFacet ) = elemNum
          Obj % CellFacet( 2, iFacet ) = BndyData( i )
          EXIT
        END IF
      END DO
      IF( Found ) EXIT
    END DO
  END DO

  NULLIFY( FacetElem, Elem )
  IF( ALLOCATED( FacetNptrs ) ) DEALLOCATE( FacetNptrs )
  IF( ALLOCATED( pt2elem ) ) DEALLOCATE( pt2elem )
  IF( ALLOCATED( bndyData ) ) DEALLOCATE( bndyData )
  IF( ALLOCATED( FM ) ) DEALLOCATE( FM )
  IF( ALLOCATED( CellNptrs ) ) DEALLOCATE( CellNptrs )
  IF( ALLOCATED( CellFacetNptrs ) ) DEALLOCATE( CellFacetNptrs )

END PROCEDURE mc_init_cell_facet

!----------------------------------------------------------------------------
!                                                                 CellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_cell_of_facet
  Ans = Obj % CellFacet( 1, FacetNum )
END PROCEDURE mc_cell_of_facet

!----------------------------------------------------------------------------
!                                                                 CellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_cells_of_facets
  Ans = Obj % CellFacet( 1, FacetNum )
END PROCEDURE mc_cells_of_facets

!----------------------------------------------------------------------------
!                                                               FacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_facet_local_id_1
  Ans = Obj % CellFacet( 2, FacetNum )
END PROCEDURE mc_facet_local_id_1

!----------------------------------------------------------------------------
!                                                               FacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_facet_local_id_2
  Ans = Obj % CellFacet( 2, FacetNum )
END PROCEDURE mc_facet_local_id_2

!----------------------------------------------------------------------------
!                                                          setNodeToNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE mc_init_node_node
! Define internal variables
  INTEGER( I4B ), ALLOCATABLE :: Nptrs1( : ), Nptrs2( : ), &
    & Mapping( :, : )
  INTEGER( I4B ) :: i, j, k, nsd
  TYPE( BoundingBox_ ) :: Box1, Box2, Box
  LOGICAL( LGT ) :: isEqual
  REAL( DFP ) :: X( 3 )

  ! bounding box aroung the mesh
  Box1 = BoundingBox( Obj = MeshData1, nodes = Node1 )
  Box2 = BoundingBox( Obj = MeshData2, nodes = Node2 )
  Box = Box1 .Intersection. Box2

  nsd = SIZE( Node1, 1 )

  ! now we get Nptrs in Box for Node1, Node2
  Nptrs1 = Box .Nptrs. Node1
  Nptrs2 = Box .Nptrs. Node2

  ALLOCATE( Mapping( 2, SIZE( Nptrs1 ) ) )
  Mapping = 0

  k = 0
  DO i = 1, SIZE( Nptrs1 )
    X( 1 : nsd ) =  Node1( 1:nsd, Nptrs1( i ) )
    DO j = 1, SIZE( Nptrs2 )
      IF( ALL( X( 1:nsd ) .EQ. Node2( 1:nsd, Nptrs2( j ) ) ) ) THEN
        k = k + 1
        Mapping( :, i ) = [Nptrs1( i ),  Nptrs2( j )]
        EXIT
      END IF
    END DO
  END DO

  CALL Reallocate( Obj % NodeToNodes, k, 2_I4B )

  k = 0
  DO i = 1, SIZE( Nptrs1 )
    IF( Mapping( 1, i ) .NE. 0 ) THEN
      k = k + 1
      Obj % NodeToNodes( k, : ) = Mapping( :, i )
    END IF
  END DO

  DEALLOCATE( Nptrs1, Nptrs2, Mapping )
END PROCEDURE mc_init_node_node

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
