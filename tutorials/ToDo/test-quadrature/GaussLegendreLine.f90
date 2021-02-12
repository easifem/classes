program main
  use globaldata
  use io, only: displayarray
  use utility, only: int2str
  use GaussLegendreQPLine_Module
  implicit none

  real( dfp ), allocatable :: qp( :, : )
  integer( i4b ) :: i, n

  n = 12
  do i = 1, n
    call getGaussLegendreQPLine( qp, i )
    call DisplayArray( qp, int2str( i ) )
  end do

  n = 12
  do i = 1, n
    call getGaussLegendreQPLine( qp, [i] )
    call DisplayArray( qp, int2str( i ) )
  end do

end program main