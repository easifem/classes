program main
  use globaldata
  use io
  use utility

  real(dfp) :: a( 3, 3 ), b( 1, 3 ), c(4,3)

  call random_number( a )
  call random_number( b )

  c = (a .vcat. b)

  call displayarray( a, "a" )
  call displayarray( b, "b" )
  call displayarray( c, "c" )

end program main