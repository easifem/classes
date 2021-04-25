!! check
IF( obj % geo % tSurfaces .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Surfaces should be added first")
  STOP
END IF

IF( ALLOCATED( obj % PhysicalSurfaceUID ) ) THEN

  !! check
  IF( ANY( obj % PhysicalSurfaceUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Surface (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( obj % PhysicalSurfaceUID, uid )
  n = SIZE( obj % PhysicalSurfaceUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = obj % PhysicalSurfaceName( ii )
  END DO
  DEALLOCATE( obj % PhysicalSurfaceName )
  ALLOCATE( obj % PhysicalSurfaceName( n ) )
  DO ii = 1, n-1
    obj % PhysicalSurfaceName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  obj % PhysicalSurfaceUID = [uid]
  n = 1
  ALLOCATE( obj % PhysicalSurfaceName( 1 ) )

END IF

!! If it is the first call then
IF( .NOT. ALLOCATED( obj % Surface_PhysicalToEntity ) ) THEN
  ALLOCATE( obj % Surface_PhysicalToEntity( def_Surface_phy2ent ) )
END IF

tp2e = SIZE( obj % Surface_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => obj % Surface_PhysicalToEntity( ii ) % ptr
      obj % Surface_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Surface_PhysicalToEntity )
    ALLOCATE( obj % Surface_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      obj % Surface_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( obj % Surface_PhysicalToEntity( n ) % ptr )
obj % Surface_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( obj % Surface_EntityToPhysical ) ) THEN
  ALLOCATE( obj % Surface_EntityToPhysical( def_Surface_ent2phy ) )
END IF

! check size of geo % Surface with size of Surface_EntityToPhysical
te2p = SIZE( obj % Surface_EntityToPhysical )

IF( te2p .LT. obj % geo % tSurfaces ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => obj % Surface_EntityToPhysical( ii ) % ptr
      obj % Surface_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Surface_EntityToPhysical )
    ALLOCATE( obj % Surface_EntityToPhysical( 2*obj % geo % tSurfaces ) )

    DO ii = 1, te2p
      obj % Surface_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, obj % geo % tSurfaces

    IF( tags( ii ) .EQ. obj % geo % Surface( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( obj % Surface_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( obj % Surface_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( obj % Surface_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( obj % Surface_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO