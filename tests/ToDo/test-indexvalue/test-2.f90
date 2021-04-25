program main
  use globaldata
  use io
  use indexvalue_class
  implicit none

  type( indexvalue_ ), allocatable ::
  real( dfp ) :: x( 10 )

  obj = IndexValue( [1,2,3,4], 0.0_DFP )

  obj % Indx

  obj % Val

  X( obj % Indx ) = obj % Val








  call RANDOM_NUMBER( x )

  x( obj % Indx ) = obj % Val


  call display_array( x, "x" )

end program main