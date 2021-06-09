! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

module test_m
use easifemBase
use Gmsh_Class
use gmshInterface
use iso_c_binding
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
  type( gmsh_ ) :: gmsh
  integer( i4b ) :: ierr, names_n
  character( len = 120 ), allocatable :: names( : )
  CHARACTER( LEN = 120 ) :: name

  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%add("t2")
  names_n = 2
  ! ierr = gmsh%model%list(names, names_n)
  ierr = gmsh%model%getCurrent(name)
  call display( name, 'name: ')
  ierr = gmsh%model%setCurrent('t1')
  ierr = gmsh%model%getCurrent(name)
  call display( name, 'name: ')
  ierr = gmsh%model%setFileName('t1.geo')
  ierr = gmsh%model%getFileName(name)
  call display( name, 'name: ')
  ierr = gmsh%model%remove()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
end subroutine test1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE TEST2
  TYPE( gmsh_ ) :: gmsh
  INTEGER( I4B ) :: ierr, tag
  REAL( DFP ), PARAMETER :: LENGTH=1.0, WIDTH=1.0
  REAL( DFP ) :: x, y, z, meshSize
  CHARACTER( LEN = 120 ), ALLOCATABLE :: names( : )
  CHARACTER( LEN = 120 ) :: name

  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%model%add("t1")
  ierr = gmsh%model%setFileName('t1.geo')
  x = 0.0; y = 0.0; z = 0.0; meshSize = 0.01; tag=1
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = LENGTH; y = 0.0; z = 0.0; meshSize = 0.01; tag=2
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = LENGTH; y = WIDTH; z = 0.0; meshSize = 0.01; tag=3
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  x = 0.0; y = WIDTH; z = 0.0; meshSize = 0.01; tag=4
  ierr = gmsh%model%geo%addPoint(x,y,z,meshSize,tag)
  ierr = gmsh%model%geo%addLine(1,2,1)
  ierr = gmsh%model%geo%addLine(2,3,2)
  ierr = gmsh%model%geo%addLine(3,4,3)
  ierr = gmsh%model%geo%addLine(4,1,4)
  ierr = gmsh%model%geo%addCurveLoop([1,2,3,4], 4, 1, reorient=1)
  ierr = gmsh%model%geo%addPlaneSurface([1], 1, 1)
  ierr = gmsh%model%geo%Synchronize()

  BLOCK
    INTEGER( I4B ), ALLOCATABLE :: dimTags( :, : )
    INTEGER( I4B ) :: dimTags_n
    ierr = gmsh%model%GetEntities(dimTags, -1)
    CALL display(dimTags, "test-2: getEntities(dim=-1)=")
    ierr = gmsh%model%GetEntities(dimTags, 0)
    CALL display(dimTags, "test-2: getEntities(dim=0)=")
    ierr = gmsh%model%GetEntities(dimTags, 1)
    CALL display(dimTags, "test-2: getEntities(dim=1)=")
    ierr = gmsh%model%GetEntities(dimTags, 2)
    CALL display(dimTags, "test-2: getEntities(dim=2)=")
  END BLOCK

  ! ierr = gmsh%clear()
  ierr = gmsh%finalize()
  CALL GMSH_FINAL
end subroutine TEST2

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
USE TEST_M
! CALL TEST1
CALL TEST2
end program