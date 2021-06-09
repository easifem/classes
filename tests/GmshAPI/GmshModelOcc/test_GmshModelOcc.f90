! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be USEful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE test_m
USE GlobalData, ONLY: I4B, DFP, PI
USE Gmsh_Class
USE ISO_C_BINDING
IMPLICIT NONE

TYPE( gmsh_ ) :: gmsh
INTEGER( I4B ) :: ierr, tag, i, p1, p2, p3, p4, p5, p6, p7, p8, c1, c2, p9, &
  & c3, p10, c4, W1
REAL( DFP ), PARAMETER :: LENGTH=1.0, WIDTH=1.0, meshSize=0.1
REAL( DFP ) :: x, y, z, r, angle1, angle2, lc, a
CHARACTER( LEN = 120 ) :: name

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST1
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%occ%addCircle(0.0_DFP, 0.0_DFP, 0.0_DFP, 0.5_DFP, 1)
  ierr = gmsh%model%occ%addCurveLoop([1], 1)
  ierr = gmsh%model%occ%addCircle(0.1, 0.05, 1, 0.1, 2)
  ierr = gmsh%model%occ%addCurveLoop([2], 2)
  ierr = gmsh%model%occ%addCircle(-0.1, -0.1, 2, 0.3, 3)
  ierr = gmsh%model%occ%addCurveLoop([3], 3)
  ierr = gmsh%model%occ%addThruSections([1, 2, 3], 1)
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST1
  CALL GMSH_INIT
  ierr=gmsh%initialize()
  ierr = gmsh%model%occ%addTorus(0.0_DFP, 0.0_DFP, 0.0_DFP, 0.5_DFP, &
    & 0.2_DFP )
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST10
  CALL GMSH_INIT
  ierr=gmsh%initialize()
  ierr = gmsh%model%occ%addWedge(0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
    & 2.0_DFP, 3.0_DFP, ltx=0.5_DFP )
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST9
  CALL GMSH_INIT
  ierr=gmsh%initialize()
  ierr = gmsh%model%occ%addCone(0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, &
    & 0.0_DFP, 1.0_DFP, r1=1.0_DFP, r2=0.0_DFP)
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST8
  CALL GMSH_INIT
  ierr=gmsh%initialize()
  ierr = gmsh%model%occ%addCylinder(0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, &
    & 0.0_DFP, 1.0_DFP, r=0.5_DFP, angle=0.5*PI)
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ADDING SPHERE
SUBROUTINE TEST7
  CALL GMSH_INIT
  ierr=gmsh%initialize()
  ierr = gmsh%model%occ%addSphere(0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, angle1=0*PI, angle2=0.5*PI, angle3=PI)
  ierr = gmsh%model%occ%synchronize()
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! BSPLINE_FILLING.PY
SUBROUTINE TEST6
  CALL GMSH_INIT
  ierr=gmsh%initialize()

  lc = 0.02
  ! # Define the south B-Spline curve
  a = 0.25
  P1  = gmsh%model%occ%addPoint(0.00_DFP, 0.00_DFP, 0.00_DFP, lc)
  P2  = gmsh%model%occ%addPoint(0.33_DFP, 0.00_DFP + a, 0.00_DFP + a, lc)
  P3  = gmsh%model%occ%addPoint(0.66_DFP, 0.00_DFP - a, 0.00_DFP + a, lc)
  P4  = gmsh%model%occ%addPoint(1.00_DFP, 0.00_DFP, 0.00_DFP, lc)
  C1 = gmsh%model%occ%addBSpline([P1, P2, P3, P4], degree=3)

  ! # Define the north B-Spline curve
  P5  = gmsh%model%occ%addPoint(0.00_DFP, 1.00_DFP, 0.00_DFP, lc)
  P6  = gmsh%model%occ%addPoint(0.33_DFP, 1.00_DFP - a, 0.00_DFP - a, lc)
  P7  = gmsh%model%occ%addPoint(0.66_DFP, 1.00_DFP + a, 0.00_DFP - a, lc)
  P8  = gmsh%model%occ%addPoint(1.00_DFP, 1.00_DFP, 0.00_DFP, lc)
  C2 = gmsh%model%occ%addBSpline([P5, P6, P7, P8],degree=3)

  ! # Define the east B-Spline curve
  P9 = gmsh%model%occ%addPoint(0.00_DFP-a, 0.50_DFP, 0.00_DFP + a, lc)
  C3 = gmsh%model%occ%addBSpline([P1,P9, P5], degree=2)

  !# Define the west B-Spline curve
  P10 = gmsh%model%occ%addPoint(1.00_DFP+a, 0.50_DFP, 0.00_DFP - a, lc)
  C4 = gmsh%model%occ%addBSpline([P4, P10, P8], degree=3)

  !# Create a BSpline surface filling the 4 curves:
  W1 = gmsh%model%occ%addWire([C1, C3, C2, C4])

  !# gmsh.model.occ.addBSplineFilling(W1, type="Stretch")
  ierr = gmsh%model%occ%addBSplineFilling(W1)

  ! # Synchronize the CAD model
  ierr = gmsh%model%occ%synchronize()

  !# Show the model
  ierr = gmsh%fltk%run()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! SPLINE>CPP
SUBROUTINE TEST5
  CALL GMSH_INIT
  ierr = gmsh%initialize();
  ierr = gmsh%model%add("spline");
  DO i = 1, 10
    x=i+1; y=sin(i/9.0*2.0*PI); z=0.0
    ierr=gmsh%model%occ%addPoint(x, y, z, meshSize, i)
  END DO

  ierr=gmsh%model%occ%addSpline([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 1);
  ierr=gmsh%model%occ%addBSpline([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2, 3);
  ierr=gmsh%model%occ%addBezier([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 3);

  ierr=gmsh%model%occ%addPoint(0.2_DFP, -1.6_DFP, 0._DFP, meshSize, 101);
  ierr=gmsh%model%occ%addPoint(1.2_DFP, -1.6_DFP, 0._DFP, meshSize, 102);
  ierr=gmsh%model%occ%addPoint(1.2_DFP, -1.1_DFP, 0._DFP, meshSize, 103);
  ierr=gmsh%model%occ%addPoint(0.3_DFP, -1.1_DFP, 0._DFP, meshSize, 104);
  ierr=gmsh%model%occ%addPoint(0.7_DFP, -1._DFP, 0._DFP, meshSize, 105);

  !periodic bspline through the control points
  ierr=gmsh%model%occ%addSpline([103, 102, 101, 104, 105, 103], 100);

  !periodic bspline from given control points and default parameters - will
  !create a new vertex

  ierr=gmsh%model%occ%addBSpline([103, 102, 101, 104, 105, 103], 101, 3);

  ! general bspline with explicit degree, knots and multiplicities
  ierr = gmsh%model%occ%addPoint(0._DFP, -2._DFP, 0._DFP, meshSize, 201);
  ierr = gmsh%model%occ%addPoint(1._DFP, -2._DFP, 0._DFP, meshSize, 202);
  ierr = gmsh%model%occ%addPoint(1._DFP, -3._DFP, 0._DFP, meshSize, 203);
  ierr = gmsh%model%occ%addPoint(0._DFP, -3._DFP, 0._DFP, meshSize, 204);
  ierr = gmsh%model%occ%addBSpline([201, 202, 203, 204], 200, 2, &
    & knots=[0._DFP, 0.5_DFP, 1._DFP], multiplicities=[3, 1, 3]);

  ierr = gmsh%model%occ%synchronize();
  ierr = gmsh%fltk%run();
  ierr = gmsh%finalize();
  CALL GMSH_FINAL
END SUBROUTINE TEST5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! POINT, CIRCLE RECTANGLE ...
SUBROUTINE TEST4
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%setFileName('t1.geo')
  ierr = gmsh%Option%setNumber("Mesh.SaveAll", 1)
  x = 0.0; y = 0.0; z = 0.0; tag=1; r=LENGTH; angle1=0.0; angle2=2.0*PI
  ierr = gmsh%model%occ%addCircle(x,y,z,r,tag,angle1,angle2)
  ierr = gmsh%model%occ%Synchronize()
  ierr = gmsh%model%mesh%generate(dim=2)
  ierr = gmsh%write(fileName='t1.msh')
  ierr = gmsh%fltk%run()
  ierr = gmsh%fltk%wait(-1.0_DFP)
  ierr = gmsh%model%remove()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
end subroutine TEST4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! DISK
SUBROUTINE TEST3
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%setFileName('t1.geo')
  ierr = gmsh%Option%setNumber("Mesh.SaveAll", 1)
  x = 0.0; y = 0.0; z = 0.0; tag=1; r=LENGTH; angle1=0.0; angle2=2.0*PI
  ierr = gmsh%model%occ%addDisk(x,y,z,1.0_DFP, 1.0_DFP, 1)
  ierr = gmsh%model%occ%Synchronize()
  ierr = gmsh%model%mesh%generate(dim=2)
  ierr = gmsh%write(fileName='t1.msh')
  ierr = gmsh%model%remove()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!! POINT, CIRCLE RECTANGLE ...
SUBROUTINE TEST2
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%setFileName('t1.geo')
  ierr = gmsh%Option%setNumber("Mesh.SaveAll", 1)
  x = 0.0; y = 0.0; z = 0.0; tag=1; r=LENGTH; angle1=0.0; angle2=2.0*PI
  ierr = gmsh%model%occ%addCircle(x,y,z,r,tag,angle1,angle2)
  x = 0.0; y = 0.0; z = 0.0; tag=2
  ierr = gmsh%model%occ%addRectangle(x,y,z,LENGTH, WIDTH, tag, 0.0_DFP)
  ierr = gmsh%model%occ%Synchronize()
  ierr = gmsh%model%mesh%generate(dim=2)
  ierr = gmsh%write(fileName='t1.msh')
  ierr = gmsh%model%remove()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
end subroutine TEST2


end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
USE test_m
call TEST1
end program