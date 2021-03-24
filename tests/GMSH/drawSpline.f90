!> authors: Dr. Vikas Sharma
!
! This program will draw curves using spline

program main
use easifem

implicit none

type( gmsh_ ) :: gmsh
integer( i4b ) :: ierr, n, ii, p( 100 ), c( 100 )
real( dfp ):: x, y, z

ierr = gmsh % initialize( )
ierr = gmsh % model % add( "t1" )


p(1) = gmsh % model % geo % addPoint( 0.d0, 0.d0, 0.d0, 1.d0, -1 )
p(2) = gmsh % model % geo % addPoint( 1.d0, 0.d0, 0.d0, 1.d0, -1 )
c(1) = gmsh % model % geo % addLine(p(1), p(2), -1 )

n = 5
do ii = 1, n
  x = (n - ii)*1.0/n
  y = cos( x*pi/2 )
  p(2+ii ) = gmsh % model % geo % addPoint( x, y, 0.d0, 1.d0, -1 )
end do

c(2) = gmsh % model % geo % addSpline(p(2:7), -1 )
ierr = gmsh % finalize( )

end program main