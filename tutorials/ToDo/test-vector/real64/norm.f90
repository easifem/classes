program main
  use globaldata
  use io
  use Array_Class
  use Array_Method

  BLOCK
    type( Real64Vector_ ) :: obj
    real( dfp ) :: y( 5 ), x, t1, t2, meanT = 0.0
    integer( i4b ) :: i, n

    n = 1000
    call RANDOM_NUMBER( y )
    obj = Real64Vector( y )

    call display( obj, "obj=")

    ! DO i = 1, n

      CALL CPU_TIME( t1 )
        x = NRM2( obj )
      CALL CPU_TIME( t2 )

      meanT = meanT + t2 - t1

    ! END DO

    write( *, * ) "obj:: error:: ", x - NORM2( y )
    write( *, * ) "obj::", meanT / n


    ! meanT = 0.0

    ! DO i = 1, n
    !   CALL CPU_TIME( t1 )
    !     x = NORM2( y )
    !   CALL CPU_TIME( t2 )
    !   meanT = meanT + t2 - t1
    ! END DO

    ! write( *, * ) "from intrinsic::", meanT/ n

  END BLOCK

end program main