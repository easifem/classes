!> authors: Dr. Vikas Sharma
!
! This program will draw curves using spline

program main
use easifem

implicit none

type( gmsh_ ) :: gmsh
integer( i4b ) :: ierr, n, ii, p( 10 ), c( 10 )
real( dfp ):: x, y, z

ierr = gmsh % initialize( )
ierr = gmsh % model % add( "t1" )


p(1) = gmsh % model % geo % addPoint( 0.d0, 0.d0, 0.d0, 1.d0, -1 )
p(2) = gmsh % model % geo % addPoint( 1.d0, 0.d0, 0.d0, 1.d0, -1 )
p(3) = gmsh % model % geo % addPoint( 0.d0, 1.d0, 0.d0, 1.d0, -1 )

x = 2.0/3; y = cos( x*pi/2 )
p(4) = gmsh % model % geo % addPoint( x, y, 0.d0, 1.d0, -1 )
x = 1.0/3; y = cos( x*pi/2 )
p(5) = gmsh % model % geo % addPoint( x, y, 0.d0, 1.d0, -1 )

c(1) = gmsh % model % geo % addLine(p(1), p(2), -1 )
c(2) = gmsh % model % geo % addSpline(p([2,4,5,3]), -1 )
c(3) = gmsh % model % geo % addLine(p(3), p(1), -1 )
ierr = gmsh % finalize( )

end program main