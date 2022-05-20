
MODULE PROCEDURE Domain_setDomainFacetElement
  CLASS( Mesh_ ), POINTER :: masterMesh, slaveMesh
  INTEGER( I4B ) :: tsize, ii, jj, kk, ll, mm, nn, iel, &
    & tDomFacet, tMeshFacet, tNeighbors, indx2( 2 )
  INTEGER( I4B ), ALLOCATABLE :: faceNptrs( : ), meshFacetMap( :, : ), &
    & dummyInt( : ), localToGlobalMesh( : ), cellNptrs( : ), faceType(:)
  LOGICAL( LGT ) :: faceFound
  !!
  !! main
  !!
  tsize = obj%getTotalMesh( dim=obj%nsd )
  !!
  DO ii = 1, tsize
    !!
    masterMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=ii )
    tDomFacet = masterMesh%getTotalDomainFacetElements( )
    tMeshFacet = 0
    !!
    !! meshFacetMap( 1, : ) denotes BOUNDARY_ELEMENT or
    !! DOMAIN_BOUNDARY_ELEMENT
    !! meshFacetMap( 2, : ) denotes the slave-cell's mesh entity-number
    !!
    CALL Reallocate( meshFacetMap, 2, tDomFacet )
    CALL Reallocate( dummyInt, tSize )
    !!
    DO iel = 1, tDomFacet
      !!
      faceNptrs = masterMesh%getFacetConnectivity( &
        & facetElement=iel, &
        & elementType=DOMAIN_BOUNDARY_ELEMENT, &
        & isMaster=.TRUE. )
      !!
      faceFound = .FALSE.
      !!
      !! The code below checks if any other mesh contains the
      !! facetNptrs; if there exists such as mesh, then
      !! the face element is actually meshFacet.
      !!
      DO jj = 1, tsize
        IF( jj .NE. ii ) THEN
          !!
          slaveMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=jj )
          !!
          IF( slaveMesh%isAllNodePresent( faceNptrs ) ) THEN
            !!
            faceFound = .TRUE.
            !! the following means iel face is mesh Facet
            meshFacetMap( 1, iel ) = BOUNDARY_ELEMENT
            !! the following means jj is the mesh number of iel facet
            meshFacetMap( 2, iel ) = jj
            tMeshFacet = tMeshFacet + 1
            EXIT
            !!
          END IF
        END IF
      END DO
      !!
      IF( .NOT. faceFound ) THEN
        !! the following means iel facet element is domainFacet
        meshFacetMap( 1, iel ) = DOMAIN_BOUNDARY_ELEMENT
        meshFacetMap( 2, iel ) = 0
      END IF
      !!
    END DO
    !!
    !! Now we know that there are tMeshFacet number of
    !! meshFacet element, and tDomFacet-tMeshFacet number of
    !! domainFacet elements.
    !!
    !! First we need to determine the number of neighboring meshes
    !! for ii mesh.That means distinct nonzero values in meshFacetMap(2,:)
    !!
    DO jj = 1, tDomFacet
      !!
      !! Get the mesh number
      !!
      kk = meshFacetMap( 2, jj )
      !!
      IF( kk .NE. 0 ) dummyInt( kk ) = dummyInt( kk ) + 1
      !!
    END DO
    !!
    !! Find out how many neighbour mesh is there
    !!
    tNeighbors = 0
    !!
    DO jj = 1, SIZE( dummyInt )
      IF( dummyInt( jj ) .NE. 0 ) tNeighbors = tNeighbors + 1
    END DO
    !!
    IF( ALLOCATED( masterMesh%meshFacetData ) ) &
      & DEALLOCATE( masterMesh%meshFacetData )
    ALLOCATE( masterMesh%meshFacetData( tNeighbors ) )
    CALL Reallocate( localToGlobalMesh, tNeighbors )
    !!
    kk = 0
    !!
    DO jj = 1, SIZE( dummyInt )
      IF( dummyInt( jj ) .NE. 0 ) THEN
        kk = kk + 1
        !! jj mesh is a neighbour, and kk is the local number of this mesh
        CALL masterMesh%meshFacetData( kk )%Initiate( dummyInt( jj ) )
        localToGlobalMesh( kk ) = jj
      END IF
    END DO
    !!
    !!
    !!
    DO ll = 1, tNeighbors
      !!
      indx2 = [BOUNDARY_ELEMENT, localToGlobalMesh( ll )]
      kk = 0
      !!
      DO jj = 1, tDomFacet
        !!
        IF( ALL(meshFacetMap( :, jj ) .EQ. indx2) ) THEN
          !!
          kk = kk + 1
          CALL masterMesh%meshFacetData( ll )%Set( &
            & facetElement=kk, &
            & domainFacetData=masterMesh%domainFacetData(jj) )
          !!
        END IF
      END DO
      !!
      slaveMesh => obj%getMeshPointer( dim=obj%nsd, &
        & entityNum=indx2( 2 ) )
      !!
      DO jj = 1, masterMesh%meshFacetData( ll )%Size()
        !!
        faceNptrs = masterMesh%getFacetConnectivity( &
          & facetElement=jj, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE., &
          & facetBoundary=ll )
        !!
        DO kk = 1, slaveMesh%getTotalElements()
          !!
          mm = slaveMesh%getGlobalElemNumber( localElement=kk )
          !!
          IF( slaveMesh%isBoundaryElement( globalElement=mm ) ) THEN
            !!
            !! get facet type of slaveMesh
            !!
            faceType = slaveMesh%getFacetElementType( globalElement=mm )
            !!
            IF( ANY( faceType .EQ. BOUNDARY_ELEMENT ) ) THEN
              !!
              DO nn = 1, SIZE( slaveMesh%facetElements )
                !!
                cellNptrs = slaveMesh%getFacetConnectivity( &
                  & globalElement=mm, iface=nn )
                !!
                IF( faceNptrs .IN. cellNptrs ) THEN
                  !!
                  CALL masterMesh%meshFacetData( ll )%SetSlaveData( &
                    & facetElement=jj, &
                    & slaveCellNumber=mm, &
                    & slaveLocalFacetID=nn )
                  !!
                END IF
                !!
              END DO
              !!
            END IF
            !!
          END IF
          !!
        END DO
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
END PROCEDURE Domain_setDomainFacetElement