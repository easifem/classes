program main
  use globaldata
  use io
  use utility

  real(dfp) :: a( 3, 3 ), b( 3, 1 ), c(3,4)

  call random_number( a )
  call random_number( b )

  c = (a .hcat. b)

  call displayarray( a, "a" )
  call displayarray( b, "b" )
  call displayarray( c, "c" )

end program main