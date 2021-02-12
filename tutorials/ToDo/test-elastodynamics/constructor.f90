program main
  use easifem
  use elastodynamics_class

  type( elastodynamics_ ) :: obj
  real( dfp ) :: dt 
  integer(i4b) :: nsd 
  
  dt = 0.1; nsd = 1
  call obj % initiate( nsd, dt )
end program main
