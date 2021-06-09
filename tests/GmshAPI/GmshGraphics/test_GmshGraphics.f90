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
use easifemBase, ONLY:I4B, DFP
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
  CALL GMSH_INIT
  ierr =  gmsh%initialize()
  ierr = gmsh%open(fileName="t1.msh" )
  ierr = gmsh%graphics%draw()
  ierr = gmsh%clear()
  ierr = gmsh%finalize()
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