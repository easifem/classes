  IF( ALLOCATED( obj % CurveLoop ) ) THEN

    n = SIZE( obj % CurveLoop )

    ! we will expand the CurveLoop now to accomodate one
    IF( n .EQ. obj % tCurveLoops ) THEN

      BLOCK

        TYPE( gmshCurveLoopPointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % CurveLoop( ii ) % ptr
          obj % CurveLoop( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % CurveLoop )
        ALLOCATE( obj % CurveLoop( 2*(n + 1) ) )
        obj % tCurveLoops = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % CurveLoop( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tCurveLoops = obj % tCurveLoops + 1
      ip = obj % tCurveLoops

    END IF

  ELSE

    ! CALL Display( "    allocating memory for gmsh%model%geo%curveloop:: ")
    ALLOCATE( obj % CurveLoop( def_max_CurveLoop ) )
    obj % tCurveLoops = 1
    ip = 1

  END IF