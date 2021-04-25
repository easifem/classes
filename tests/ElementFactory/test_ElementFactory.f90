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

module test_module
use easifemBase
use ElementFactory
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
class( element_ ), pointer :: elemobj, facetobj
class( ReferenceElement_ ), pointer :: refelem
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
! Reference Element
refelem => ReferenceTriangle_Pointer( NSD = 2 )
call FPL_INIT()
call param%initiate()
nptrs = [1,2,3]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
ierr = param%set(key="type", value="element_")
! Cell Element
elemobj => Factory( param=param, refelem=refelem )
call display( elemobj, 'test-1:elemobj:')
ierr = param%set(key="type", value="facetelement_")
facetobj => Factory( param=param, refelem=refelem )
call display( facetobj, 'test-1:facetobj:')
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_module
implicit none
call plan(23)
call test1
call done_testing()
end program main
