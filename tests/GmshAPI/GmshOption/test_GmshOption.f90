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
  integer( i4b ) :: ierr
  real( dfp ) :: value
  CHARACTER( LEN = 120 ) :: val_str
  CALL GMSH_INIT
  ierr = gmsh%initialize()
  ierr = gmsh%Option%SetNumber("Mesh.SaveAll", 1)
  ierr = gmsh%Option%GetNumber("Mesh.SaveAll", value )
  call display( value, "value: ")
  ierr = gmsh%Option%SetString("General.DefaultFileName", "easifem.geo")
  ierr = gmsh%Option%GetString("General.DefaultFileName", val_str )
  call display( val_str, "value of string: ")
  ierr = gmsh%clear()
  CALL GMSH_FINAL
end subroutine test1

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
call test1
end program