!! check
IF( obj % geo % tVolumes .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Volumes should be added first")
  STOP
END IF

IF( ALLOCATED( obj % PhysicalVolumeUID ) ) THEN

  !! check
  IF( ANY( obj % PhysicalVolumeUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Volume (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( obj % PhysicalVolumeUID, uid )
  n = SIZE( obj % PhysicalVolumeUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = obj % PhysicalVolumeName( ii )
  END DO
  DEALLOCATE( obj % PhysicalVolumeName )
  ALLOCATE( obj % PhysicalVolumeName( n ) )
  DO ii = 1, n-1
    obj % PhysicalVolumeName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  obj % PhysicalVolumeUID = [uid]
  n = 1
  ALLOCATE( obj % PhysicalVolumeName( 1 ) )

END IF



!! If it is the first call then
IF( .NOT. ALLOCATED( obj % Volume_PhysicalToEntity ) ) THEN
  ALLOCATE( obj % Volume_PhysicalToEntity( def_Volume_phy2ent ) )
END IF

tp2e = SIZE( obj % Volume_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => obj % Volume_PhysicalToEntity( ii ) % ptr
      obj % Volume_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Volume_PhysicalToEntity )
    ALLOCATE( obj % Volume_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      obj % Volume_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( obj % Volume_PhysicalToEntity( n ) % ptr )
obj % Volume_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( obj % Volume_EntityToPhysical ) ) THEN
  ALLOCATE( obj % Volume_EntityToPhysical( def_Volume_ent2phy ) )
END IF

! check size of geo % Volume with size of Volume_EntityToPhysical
te2p = SIZE( obj % Volume_EntityToPhysical )

IF( te2p .LT. obj % geo % tVolumes ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => obj % Volume_EntityToPhysical( ii ) % ptr
      obj % Volume_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Volume_EntityToPhysical )
    ALLOCATE( obj % Volume_EntityToPhysical( 2*obj % geo % tVolumes ) )

    DO ii = 1, te2p
      obj % Volume_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, obj % geo % tVolumes

    IF( tags( ii ) .EQ. obj % geo % Volume( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( obj % Volume_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( obj % Volume_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( obj % Volume_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( obj % Volume_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO