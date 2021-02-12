program main
  use basetype
  use basemethod
  implicit none

! block
!   type( quadraturepoint_ ) :: obj
!   real( dfp ) :: points( 3 ), w
!   integer( i4b ) :: n

!   write( *, * ) "enter order"
!   read( *, * ) n

!   obj = GaussLegendreQuadrature( TypeReferenceLine, n )
!   call display( obj, "GaussLegendreQuadrature")

!   CALL getQuadraturePoints( Obj, points, w, 1 )

!   write( *, * ) points, w

! end block

block
  type( quadraturepoint_ ) :: obj
  real( dfp ), allocatable :: points( :, : ), w( : )
  integer( i4b ) :: n

  write( *, * ) "enter order"
  read( *, * ) n

  obj = GaussLegendreQuadrature( TypeReferenceTriangle, n )
  call display( obj, "GaussLegendreQuadrature")

  call getQuadraturePoints( Obj, points, w )
  call display_array( points, "points " )

end block

  ! block
  !   type( quadraturepoint_ ) :: obj
  !   real( dfp ) :: points( 3 ), w
  !   integer( i4b ) :: n

  !   write( *, * ) "enter order"
  !   read( *, * ) n

  !   obj = GaussLegendreQuadrature( TypeReferenceQuadrangle, n )
  !   call display( obj, "GaussLegendreQuadrature")
  ! end block

end program main