!<---------------------------------------------------------------------------
!<--- Author: Vikas Sharma
!<--- this program shows how to obtain element shape data for
!<--- for facet element
!<---------------------------------------------------------------------------

program main
  use easifem
  implicit none

  class( element_ ), pointer :: cell, obj
  class( referenceelement_ ), pointer :: refelem
  type( quadraturepoint_ ) :: quad
  type( elemshapedata_ ) :: elemsd
  real( dfp ), allocatable :: xij( :, : )

  ! creating cell
  xiJ = reshape( [1., 0., 3.0, 0.0, 2., 1.], [2,3] )
  refelem => ReferenceTriangle_Pointer( NSD = 2 )
  call display( LocalNodeCoord( refelem ), 'Local node coord of cell' )

  cell => element_pointer( NPTRS = [1,2,3], Mat_Type = 1, RefElem = refelem )
  call display( cell, 'cell' )
  refelem => ReferenceLine_Pointer( NSD = 2 )
  obj => facetelement_pointer( nptrs = [2,3], Mat_type = 1, refelem =  refelem )
  call display( obj, 'facet' )
  call obj % setPointerToCell( Cell )
  ! obj % cell => cell
  call obj % setFacetLocalID( 2 )

  !<--- Generate quadrature points on facetelement
  quad = GaussLegendreQuadrature( refelem = obj % refelem, &
    & order = obj % refelem % order )
  call display( quad, 'quad')

  !<--- Generate shape data on facet element
  call obj % getElemShapeData( elemsd = elemsd, quad = quad, xiJ = xiJ, &
    & ContinuityType = TypeH1, InterpolType = TypeLagrangeInterpolation )

  call display( elemsd, 'elemsd' )

end program main
