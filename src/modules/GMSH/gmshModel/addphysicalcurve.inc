!! check
IF( Obj % geo % tCurves .EQ. 0 ) THEN
  CALL Display( "ERROR:: gmshModel_Class.f90")
  CALL Display( "        model_addPhysicalGroup( )")
  CALL Display( "          Curves should be added first")
  STOP
END IF

IF( ALLOCATED( Obj % PhysicalCurveUID ) ) THEN

  !! check
  IF( ANY( Obj % PhysicalCurveUID .EQ. uid ) ) THEN
    CALL Display( "ERROR:: gmshModel_Class.f90")
    CALL Display( "        model_addPhysicalGroup( )")
    CALL Display( "          Physical Curve (" //  trim( int2str( uid ) ) // &
      & ") is already defined")
    STOP
  END IF

  CALL Append( Obj % PhysicalCurveUID, uid )
  n = SIZE( Obj % PhysicalCurveUID )

  ALLOCATE( s( n - 1 ) )
  DO ii = 1, n-1
    s( ii ) = Obj % PhysicalCurveName( ii )
  END DO
  DEALLOCATE( Obj % PhysicalCurveName )
  ALLOCATE( Obj % PhysicalCurveName( n ) )
  DO ii = 1, n-1
    Obj % PhysicalCurveName( ii ) = s( ii )
  END DO
  DEALLOCATE( s )

ELSE

  Obj % PhysicalCurveUID = [uid]
  n = 1
  ALLOCATE( Obj % PhysicalCurveName( 1 ) )

END IF

!! If it is the first call then
IF( .NOT. ALLOCATED( Obj % Curve_PhysicalToEntity ) ) THEN
  ALLOCATE( Obj % Curve_PhysicalToEntity( def_Curve_phy2ent ) )
END IF

tp2e = SIZE( Obj % Curve_PhysicalToEntity )

! not enough space, do memory expansion
IF( tp2e .LT. n ) THEN

  BLOCK
    TYPE( IntVectorPointer_ ) :: temp( tp2e )

    DO ii = 1, tp2e
      temp( ii ) % ptr => Obj % Curve_PhysicalToEntity( ii ) % ptr
      Obj % Curve_PhysicalToEntity( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Curve_PhysicalToEntity )
    ALLOCATE( Obj % Curve_PhysicalToEntity( 2 * n ) )

    DO ii = 1, tp2e
      Obj % Curve_PhysicalToEntity( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK

END IF

! ALLOCATE( Obj % Curve_PhysicalToEntity( n ) % ptr )
Obj % Curve_PhysicalToEntity( n ) % ptr => IntVector_Pointer( tags )

! now we create entityToPhysical map

IF( .NOT. ALLOCATED( Obj % Curve_EntityToPhysical ) ) THEN
  ALLOCATE( Obj % Curve_EntityToPhysical( def_Curve_ent2phy ) )
END IF

! check size of geo % Curve with size of Curve_EntityToPhysical
te2p = SIZE( Obj % Curve_EntityToPhysical )

IF( te2p .LT. Obj % geo % tCurves ) THEN
  BLOCK
  TYPE( IntVectorPointer_ ) :: temp( te2p )

    DO ii = 1, te2p
      temp( ii ) % ptr => Obj % Curve_EntityToPhysical( ii ) % ptr
      Obj % Curve_EntityToPhysical( ii ) % ptr => NULL( )
    END DO

    DEALLOCATE( Obj % Curve_EntityToPhysical )
    ALLOCATE( Obj % Curve_EntityToPhysical( 2*Obj % geo % tCurves ) )

    DO ii = 1, te2p
      Obj % Curve_EntityToPhysical( ii ) % ptr => temp( ii ) % ptr
      temp( ii ) % ptr => NULL( )
    END DO
  END BLOCK
END IF

! Update the cache

n = SIZE( tags )

DO ii = 1, n

  DO jj = 1, Obj % geo % tCurves

    IF( tags( ii ) .EQ. Obj % geo % Curve( jj ) % ptr % uid ) THEN

      IF( .NOT. ASSOCIATED( Obj % Curve_EntityToPhysical( jj ) % ptr ) ) THEN
        ALLOCATE( Obj % Curve_EntityToPhysical( jj ) % ptr )
      END IF

      CALL APPEND( Obj % Curve_EntityToPhysical( jj ) % ptr, uid )
      CALL RemoveDuplicates( Obj % Curve_EntityToPhysical( jj ) % ptr )

    END IF

  END DO

END DO