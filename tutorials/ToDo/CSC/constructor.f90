program main
  use globaldata
  use typedef_module
  use csc_method

  implicit none

  type( csc_ ) :: obj
  obj = csc( 1, 1, 1, .true., .true. )

end program main