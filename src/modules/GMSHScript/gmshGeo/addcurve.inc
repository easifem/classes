  IF( ALLOCATED( obj % curve ) ) THEN

    n = SIZE( obj % curve )

    ! we will expand the curve now to accomodate one
    IF( n .EQ. obj % tCurves ) THEN

      BLOCK

        TYPE( gmshCurvePointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % curve( ii ) % ptr
          obj % curve( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % curve )
        ALLOCATE( obj % curve( 2*(n + 1) ) )
        obj % tcurves = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % curve( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tcurves = obj % tcurves + 1
      ip = obj % tcurves

    END IF

  ELSE

    ! CALL Display( "    allocating memory for gmsh % model geo % curve:: ")
    ALLOCATE( obj % curve( def_max_curve ) )
    obj % tcurves = 1
    ip = 1

  END IF