!! check
IF( Obj % geo % tPoints .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Points should be added first")
  STOP
END IF

IF( ALLOCATED( Obj % PhysicalPointUID ) ) THEN

  !! check
  IF( ANY( Obj % PhysicalPointUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Point (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( Obj % PhysicalPointUID, uid )
  n = SIZE( Obj % PhysicalPointUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = Obj % PhysicalPointName( ii )
  END DO
  DEALLOCATE( Obj % PhysicalPointName )
  ALLOCATE( Obj % PhysicalPointName( n ) )
  DO ii = 1, n-1
    Obj % PhysicalPointName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  Obj % PhysicalPointUID = [uid]
  n = 1

  ALLOCATE( Obj % PhysicalPointName( 1 ) )

END IF



!! If it is the first call then
IF( .NOT. ALLOCATED( Obj % Point_PhysicalToEntity ) ) THEN
  ALLOCATE( Obj % Point_PhysicalToEntity( def_point_phy2ent ) )
END IF

tp2e = SIZE( Obj % Point_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => Obj % Point_PhysicalToEntity( ii ) % ptr
      Obj % Point_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Point_PhysicalToEntity )
    ALLOCATE( Obj % Point_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      Obj % Point_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( Obj % Point_PhysicalToEntity( n ) % ptr )
Obj % Point_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( Obj % Point_EntityToPhysical ) ) THEN
  ALLOCATE( Obj % Point_EntityToPhysical( def_point_ent2phy ) )
END IF

! check size of geo % point with size of Point_EntityToPhysical
te2p = SIZE( Obj % Point_EntityToPhysical )

IF( te2p .LT. Obj % geo % tPoints ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => Obj % Point_EntityToPhysical( ii ) % ptr
      Obj % Point_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Point_EntityToPhysical )
    ALLOCATE( Obj % Point_EntityToPhysical( 2*Obj % geo % tPoints ) )

    DO ii = 1, te2p
      Obj % Point_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, Obj % geo % tPoints

    IF( tags( ii ) .EQ. Obj % geo % Point( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( Obj % Point_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( Obj % Point_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( Obj % Point_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( Obj % Point_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO
