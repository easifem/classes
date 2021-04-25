!! check
IF( obj % geo % tPoints .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Points should be added first")
  STOP
END IF

IF( ALLOCATED( obj % PhysicalPointUID ) ) THEN

  !! check
  IF( ANY( obj % PhysicalPointUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Point (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( obj % PhysicalPointUID, uid )
  n = SIZE( obj % PhysicalPointUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = obj % PhysicalPointName( ii )
  END DO
  DEALLOCATE( obj % PhysicalPointName )
  ALLOCATE( obj % PhysicalPointName( n ) )
  DO ii = 1, n-1
    obj % PhysicalPointName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  obj % PhysicalPointUID = [uid]
  n = 1

  ALLOCATE( obj % PhysicalPointName( 1 ) )

END IF



!! If it is the first call then
IF( .NOT. ALLOCATED( obj % Point_PhysicalToEntity ) ) THEN
  ALLOCATE( obj % Point_PhysicalToEntity( def_point_phy2ent ) )
END IF

tp2e = SIZE( obj % Point_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => obj % Point_PhysicalToEntity( ii ) % ptr
      obj % Point_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Point_PhysicalToEntity )
    ALLOCATE( obj % Point_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      obj % Point_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( obj % Point_PhysicalToEntity( n ) % ptr )
obj % Point_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( obj % Point_EntityToPhysical ) ) THEN
  ALLOCATE( obj % Point_EntityToPhysical( def_point_ent2phy ) )
END IF

! check size of geo % point with size of Point_EntityToPhysical
te2p = SIZE( obj % Point_EntityToPhysical )

IF( te2p .LT. obj % geo % tPoints ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => obj % Point_EntityToPhysical( ii ) % ptr
      obj % Point_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Point_EntityToPhysical )
    ALLOCATE( obj % Point_EntityToPhysical( 2*obj % geo % tPoints ) )

    DO ii = 1, te2p
      obj % Point_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, obj % geo % tPoints

    IF( tags( ii ) .EQ. obj % geo % Point( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( obj % Point_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( obj % Point_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( obj % Point_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( obj % Point_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO
