  IF( ALLOCATED( obj % Surface ) ) THEN

    n = SIZE( obj % Surface )

    ! we will expand the Surface now to accomodate one
    IF( n .EQ. obj % tSurfaces ) THEN

      BLOCK

        TYPE( gmshSurfacePointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % Surface( ii ) % ptr
          obj % Surface( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % Surface )
        ALLOCATE( obj % Surface( 2*(n + 1) ) )
        obj % tSurfaces = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % Surface( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tSurfaces = obj % tSurfaces + 1
      ip = obj % tSurfaces

    END IF

  ELSE

    ! CALL Display( "    allocating memory for gmsh%model%geo%surface:: ")
    ALLOCATE( obj % Surface( def_max_Surface ) )
    obj % tSurfaces = 1
    ip = 1

  END IF