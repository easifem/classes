program main
  use globaldata
  use io, only: displayarray
  use utility, only: int2str
  use GaussLegendreQPTriangle_Module

  implicit none

  real( dfp ), allocatable :: qp( :, : )
  integer( i4b ), parameter :: n = 10
  integer( i4b ) :: i, order(n)

  order = [1,2,3,4,5,6,7,8,9,11]
  do i = 1, n
    call getGaussLegendreQPTriangle( qp, Order( i ) )
    call DisplayArray( qp, int2str( Order(i) ) )
    call DisplayArray( SUM( qp( 3, : ) ), "Sum weights" )
  end do

  do i = 1, n
    call getGaussLegendreQPTriangle( qp, Order( i ) )
    call DisplayArray( SUM( qp( 3, : ) ), "Sum weights" )
  end do

  ! order = [1,3,4,6,7,12,13, 19, 28]
  ! do i = 1, n
  !   call getGaussLegendreQPTriangle( qp, [ Order(i) ] )
  !   call DisplayArray( qp, int2str( Order(i) ) )
  ! end do

end program main