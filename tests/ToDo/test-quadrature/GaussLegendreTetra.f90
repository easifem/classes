program main
  use globaldata
  use io, only: displayarray
  use utility, only: int2str
  use GaussLegendreQPTetrahedron_Module

  implicit none

  real( dfp ), allocatable :: qp( :, : )
  integer( i4b ), parameter :: n = 6
  integer( i4b ) :: i, order(n)

  order = [1,2,3,4,5,6]
  do i = 1, n
    call getGaussLegendreQPTetrahedron( qp, Order( i ) )
    call DisplayArray( qp, int2str( Order(i) ) )
  end do

  do i = 1, n
    call getGaussLegendreQPTetrahedron( qp, Order( i ) )
    call DisplayArray( SUM( qp( 4, : ) ), "Sum of weights" )
  end do

end program main