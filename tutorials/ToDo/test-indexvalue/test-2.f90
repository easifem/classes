program main
  use globaldata
  use io
  use indexvalue_class
  implicit none

  type( indexvalue_ ), allocatable ::
  real( dfp ) :: x( 10 )

  Obj = IndexValue( [1,2,3,4], 0.0_DFP )

  Obj % Indx

  Obj % Val

  X( Obj % Indx ) = Obj % Val








  call RANDOM_NUMBER( x )

  x( Obj % Indx ) = Obj % Val


  call display_array( x, "x" )

end program main