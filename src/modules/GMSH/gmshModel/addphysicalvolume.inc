!! check
IF( Obj % geo % tVolumes .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Volumes should be added first")
  STOP
END IF

IF( ALLOCATED( Obj % PhysicalVolumeUID ) ) THEN

  !! check
  IF( ANY( Obj % PhysicalVolumeUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Volume (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( Obj % PhysicalVolumeUID, uid )
  n = SIZE( Obj % PhysicalVolumeUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = Obj % PhysicalVolumeName( ii )
  END DO
  DEALLOCATE( Obj % PhysicalVolumeName )
  ALLOCATE( Obj % PhysicalVolumeName( n ) )
  DO ii = 1, n-1
    Obj % PhysicalVolumeName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  Obj % PhysicalVolumeUID = [uid]
  n = 1
  ALLOCATE( Obj % PhysicalVolumeName( 1 ) )

END IF



!! If it is the first call then
IF( .NOT. ALLOCATED( Obj % Volume_PhysicalToEntity ) ) THEN
  ALLOCATE( Obj % Volume_PhysicalToEntity( def_Volume_phy2ent ) )
END IF

tp2e = SIZE( Obj % Volume_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => Obj % Volume_PhysicalToEntity( ii ) % ptr
      Obj % Volume_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Volume_PhysicalToEntity )
    ALLOCATE( Obj % Volume_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      Obj % Volume_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( Obj % Volume_PhysicalToEntity( n ) % ptr )
Obj % Volume_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( Obj % Volume_EntityToPhysical ) ) THEN
  ALLOCATE( Obj % Volume_EntityToPhysical( def_Volume_ent2phy ) )
END IF

! check size of geo % Volume with size of Volume_EntityToPhysical
te2p = SIZE( Obj % Volume_EntityToPhysical )

IF( te2p .LT. Obj % geo % tVolumes ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => Obj % Volume_EntityToPhysical( ii ) % ptr
      Obj % Volume_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Volume_EntityToPhysical )
    ALLOCATE( Obj % Volume_EntityToPhysical( 2*Obj % geo % tVolumes ) )

    DO ii = 1, te2p
      Obj % Volume_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, SIZE( tags )

  DO jj = 1, Obj % geo % tVolumes

    IF( tags( ii ) .EQ. Obj % geo % Volume( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( Obj % Volume_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( Obj % Volume_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( Obj % Volume_EntityToPhysical( jj ) % ptr, [uid] )
      CALL RemoveDuplicates( Obj % Volume_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO