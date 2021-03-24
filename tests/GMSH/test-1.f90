program main
use easifem

implicit none

type( gmsh_ ) :: gmsh
integer( i4b ) :: ierr, n, ii, p( 10 ), c( 10 ), ll( 10 ), s( 10 )
integer( i4b ), allocatable :: vec( : ), ent( :, : )

  ierr = gmsh % initialize( nsd = 2 )
  ierr = gmsh % model % add( "t1" )


  p(1) = gmsh % model % geo % addPoint( 1.d0, 0.d0, 0.d0, 1.d0, -1 )
  p(2) = gmsh % model % geo % addPoint( 2.d0, 0.d0, 0.d0, 1.d0, -1 )
  p(3) = gmsh % model % geo % addPoint( 2.d0, -1.d0, 0.d0, 1.d0, -1 )
  p(4) = gmsh % model % geo % addPoint( 3.d0, 0.d0, 0.d0, 1.d0, -1 )
  p(5) = gmsh % model % geo % addPoint( 2.d0, 1.d0, 0.d0, 1.d0, -1 )

  c(1) = gmsh % model % geo % addCircleArc(p(1), p(2), p(3), -1 )
  c(2) = gmsh % model % geo % addCircleArc(p(3), p(2), p(4), -1 )
  c(3) = gmsh % model % geo % addCircleArc(p(4), p(2), p(5), -1 )
  c(4) = gmsh % model % geo % addCircleArc(p(5), p(2), p(1), -1 )

  ll(1) = gmsh % model % geo % addCurveLoop( c(1:4), -1 )

  s(1) = gmsh % model % geo % addPlaneSurface( ll(1:1), -1 )

  ierr = gmsh % model  % addPhysicalGroup( dim = 1, tags = c(1:4), uid = 1 )
  ierr = gmsh % model % setPhysicalName( dim = 1, tag = 1, name = "boundary" )

  ierr = gmsh % model  % addPhysicalGroup( dim = 2, tags = s(1:1), uid = 1 )
  ierr = gmsh % model % setPhysicalName( dim = 2, tag = 1, name = "surface" )

  ! ent = gmsh % model % getPhysicalGroups( dim = 1 )
  ! call display( ent, "ent" )
  ! vec = gmsh % model % getEntitiesForPhysicalGroup( dim = 1, tag = 1 )
  ! call display( vec, 'getEntitiesForPhysicalGroup' )
  ! vec = gmsh % model % getPhysicalGroupsForEntity( dim = 1, tag = 1 )
  ! call display( vec, 'getPhysicalGroupsForEntity' )

  ierr = gmsh % model % geo % mesh % setSize( dim = 0, tags = [1,2,3,4,5], &
    & meshSize = 0.1_dfp )

  ierr = gmsh % model % mesh % generate( dim = 2 )

  ierr = gmsh % write([string("./"), string("model-1"), string( ".msh" )])
ierr = gmsh % finalize( )

! ierr = gmsh % initialize( nsd = 2 )
! ierr = gmsh % open( [string("./"), string("model-1"), string( ".msh" )] )
! ierr = gmsh % finalize( )

end program main