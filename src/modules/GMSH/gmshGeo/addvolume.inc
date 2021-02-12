  IF( ALLOCATED( obj % Volume ) ) THEN

    n = SIZE( obj % Volume )

    ! we will expand the Volume now to accomodate one
    IF( n .EQ. obj % tVolumes ) THEN

      BLOCK

        TYPE( gmshVolumePointer_ ) :: temp( n )
        INTEGER( I4B ) :: ii

        DO ii = 1, n
          temp( ii ) % ptr => obj % Volume( ii ) % ptr
          obj % Volume( ii ) % ptr => NULL( )
        END DO

        DEALLOCATE( obj % Volume )
        ALLOCATE( obj % Volume( 2*(n + 1) ) )
        obj % tVolumes = n + 1
        ip = n + 1

        ! copy back
        DO ii = 1, n
          obj % Volume( ii ) % ptr => temp( ii ) % ptr
          temp( ii ) % ptr => NULL( )
        END DO

      END  BLOCK

    ELSE
    ! enough size is there
      obj % tVolumes = obj % tVolumes + 1
      ip = obj % tVolumes

    END IF

  ELSE

    ! CALL Display( "    allocating memory for gmsh%model%geo%volumeloop:: ")
    ALLOCATE( obj % Volume( def_max_Volume ) )
    obj % tVolumes = 1
    ip = 1

  END IF