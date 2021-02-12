  IF( ALLOCATED( obj % SurfaceLoop ) ) THEN

    n = SIZE( obj % SurfaceLoop )

    ! we will expand the SurfaceLoop now to accomodate one
    IF( n .EQ. obj % tSurfaceLoops ) THEN

      BLOCK

        TYPE( gmshSurfaceLoopPointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % SurfaceLoop( ii ) % ptr
          obj % SurfaceLoop( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % SurfaceLoop )
        ALLOCATE( obj % SurfaceLoop( 2*(n + 1) ) )
        obj % tSurfaceLoops = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % SurfaceLoop( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tSurfaceLoops = obj % tSurfaceLoops + 1
      ip = obj % tSurfaceLoops

    END IF

  ELSE

    ! CALL Display( "    allocating memory for gmsh%model%geo%surfaceloop:: ")
    ALLOCATE( obj % SurfaceLoop( def_max_SurfaceLoop ) )
    obj % tSurfaceLoops = 1
    ip = 1

  END IF