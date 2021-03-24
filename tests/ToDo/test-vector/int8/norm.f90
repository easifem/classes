program main
  use globaldata
  use io
  use Int8Vector_Class

  BLOCK
    type( Int8Vector_ ) :: obj
    real( dfp ) :: x
    obj = Int8Vector( [1,2,3,4,5,6] )
    x = NRM2( obj )
    write( *, * ) x
  END BLOCK

end program main