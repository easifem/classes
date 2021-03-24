program main
  use globaldata
  use triplet_class
  implicit none

  type( triplet_ ) :: obj
  call obj % test( )
end program main