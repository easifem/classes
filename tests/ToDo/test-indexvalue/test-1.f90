program main
  use globaldata
  use io
  use indexvalue_class
  implicit none

  type( indexvalue_ ), allocatable :: Obj( : )
  real( dfp ) :: x( 10 )

  Obj = IndexValue( [1,2,3,4], 0.0_DFP )

  call RANDOM_NUMBER( x )

  x( Obj % Indx ) = Obj % Val


  call display_array( x, "x" )

end program main