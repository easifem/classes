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
USE GlobalData, ONLY: I4B, DFP
USE Gmsh_Class
USE ISO_C_BINDING
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! SQUARE MESH PROGRAM
SUBROUTINE TEST1
  TYPE( gmsh_ ) :: gmsh
  INTEGER( I4B ) :: ierr, tag
  REAL( DFP ), PARAMETER :: LENGTH=1.0, WIDTH=1.0, lc = 0.1
  REAL( DFP ) :: x, y, z, meshSize
  CHARACTER( LEN = 120 ), ALLOCATABLE :: names( : )
  CHARACTER( LEN = 120 ) :: name
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%setFileName('t1.geo')
  x = 0.0; y = 0.0; z = 0.0; meshSize = lc; tag=1
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = LENGTH; y = 0.0; z = 0.0; meshSize = lc; tag=2
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = LENGTH; y = WIDTH; z = 0.0; meshSize = lc; tag=3
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = 0.0; y = WIDTH; z = 0.0; meshSize = lc; tag=4
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  ierr = gmsh%model%geo%addLine(1,2,1)
  ierr = gmsh%model%geo%addLine(2,3,2)
  ierr = gmsh%model%geo%addLine(3,4,3)
  ierr = gmsh%model%geo%addLine(4,1,4)
  ierr = gmsh%model%geo%addCurveLoop([1,2,3,4], 1, reorient=1)
  ierr = gmsh%model%geo%addPlaneSurface([1], 1)
  ierr = gmsh%model%geo%Synchronize()
  ierr = gmsh%model%geo%AddPhysicalGroup(1, [1], 1)
  ierr = gmsh%model%SetPhysicalName(dim=1, tag=1, name="bottom" )
  ierr = gmsh%model%geo%AddPhysicalGroup(1, [2], 2)
  ierr = gmsh%model%SetPhysicalName(dim=1, tag=2, name="right" )
  ierr = gmsh%model%geo%AddPhysicalGroup(1, [3], 3)
  ierr = gmsh%model%SetPhysicalName(dim=1, tag=3, name="top" )
  ierr = gmsh%model%geo%AddPhysicalGroup(1, [4], 4)
  ierr = gmsh%model%SetPhysicalName(dim=1, tag=4, name="left" )
  ierr = gmsh%model%geo%AddPhysicalGroup(2, [1], 1)
  ierr = gmsh%model%SetPhysicalName(2, 1, "omega" )
  ierr = gmsh%model%mesh%generate(dim=2)
  ierr = gmsh%model%mesh%partition(4)
  ierr = gmsh%write(filename="square.msh")
  ierr = gmsh%model%remove()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
END SUBROUTINE TEST1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------



END MODULE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
USE test_m
call TEST1
end program