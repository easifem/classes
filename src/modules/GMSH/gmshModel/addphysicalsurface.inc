!! check
IF( Obj % geo % tSurfaces .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Surfaces should be added first")
  STOP
END IF

IF( ALLOCATED( Obj % PhysicalSurfaceUID ) ) THEN

  !! check
  IF( ANY( Obj % PhysicalSurfaceUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Surface (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( Obj % PhysicalSurfaceUID, uid )
  n = SIZE( Obj % PhysicalSurfaceUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = Obj % PhysicalSurfaceName( ii )
  END DO
  DEALLOCATE( Obj % PhysicalSurfaceName )
  ALLOCATE( Obj % PhysicalSurfaceName( n ) )
  DO ii = 1, n-1
    Obj % PhysicalSurfaceName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  Obj % PhysicalSurfaceUID = [uid]
  n = 1
  ALLOCATE( Obj % PhysicalSurfaceName( 1 ) )

END IF

!! If it is the first call then
IF( .NOT. ALLOCATED( Obj % Surface_PhysicalToEntity ) ) THEN
  ALLOCATE( Obj % Surface_PhysicalToEntity( def_Surface_phy2ent ) )
END IF

tp2e = SIZE( Obj % Surface_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => Obj % Surface_PhysicalToEntity( ii ) % ptr
      Obj % Surface_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Surface_PhysicalToEntity )
    ALLOCATE( Obj % Surface_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      Obj % Surface_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( Obj % Surface_PhysicalToEntity( n ) % ptr )
Obj % Surface_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( Obj % Surface_EntityToPhysical ) ) THEN
  ALLOCATE( Obj % Surface_EntityToPhysical( def_Surface_ent2phy ) )
END IF

! check size of geo % Surface with size of Surface_EntityToPhysical
te2p = SIZE( Obj % Surface_EntityToPhysical )

IF( te2p .LT. Obj % geo % tSurfaces ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => Obj % Surface_EntityToPhysical( ii ) % ptr
      Obj % Surface_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Surface_EntityToPhysical )
    ALLOCATE( Obj % Surface_EntityToPhysical( 2*Obj % geo % tSurfaces ) )

    DO ii = 1, te2p
      Obj % Surface_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, Obj % geo % tSurfaces

    IF( tags( ii ) .EQ. Obj % geo % Surface( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( Obj % Surface_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( Obj % Surface_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( Obj % Surface_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( Obj % Surface_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO