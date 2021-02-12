program main
use easifem

implicit none

type( gmsh_ ) :: gmsh
integer( i4b ) :: ierr, n, ii, p( 10 ), c( 10 ), ll( 10 ), s( 10 )

ierr = gmsh % initialize( )
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

ierr = gmsh % finalize( )

end program main