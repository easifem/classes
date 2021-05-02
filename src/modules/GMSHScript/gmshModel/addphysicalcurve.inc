!! check
IF( obj % geo % tCurves .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Curves should be added first")
  STOP
END IF

IF( ALLOCATED( obj % PhysicalCurveUID ) ) THEN

  !! check
  IF( ANY( obj % PhysicalCurveUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Curve (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( obj % PhysicalCurveUID, uid )
  n = SIZE( obj % PhysicalCurveUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = obj % PhysicalCurveName( ii )
  END DO
  DEALLOCATE( obj % PhysicalCurveName )
  ALLOCATE( obj % PhysicalCurveName( n ) )
  DO ii = 1, n-1
    obj % PhysicalCurveName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  obj % PhysicalCurveUID = [uid]
  n = 1
  ALLOCATE( obj % PhysicalCurveName( 1 ) )

END IF

!! If it is the first call then
IF( .NOT. ALLOCATED( obj % Curve_PhysicalToEntity ) ) THEN
  ALLOCATE( obj % Curve_PhysicalToEntity( def_Curve_phy2ent ) )
END IF

tp2e = SIZE( obj % Curve_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => obj % Curve_PhysicalToEntity( ii ) % ptr
      obj % Curve_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Curve_PhysicalToEntity )
    ALLOCATE( obj % Curve_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      obj % Curve_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( obj % Curve_PhysicalToEntity( n ) % ptr )
obj % Curve_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( obj % Curve_EntityToPhysical ) ) THEN
  ALLOCATE( obj % Curve_EntityToPhysical( def_Curve_ent2phy ) )
END IF

! check size of geo % Curve with size of Curve_EntityToPhysical
te2p = SIZE( obj % Curve_EntityToPhysical )

IF( te2p .LT. obj % geo % tCurves ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => obj % Curve_EntityToPhysical( ii ) % ptr
      obj % Curve_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( obj % Curve_EntityToPhysical )
    ALLOCATE( obj % Curve_EntityToPhysical( 2*obj % geo % tCurves ) )

    DO ii = 1, te2p
      obj % Curve_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, n

  DO jj = 1, obj % geo % tCurves

    IF( tags( ii ) .EQ. obj % geo % Curve( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( obj % Curve_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( obj % Curve_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( obj % Curve_EntityToPhysical( jj ) % ptr, uid )
      CALL RemoveDuplicates( obj % Curve_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO