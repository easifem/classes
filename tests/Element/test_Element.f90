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

module test_Element
use easifemBase
use Element_Class
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
class( ReferenceElement_ ), pointer :: refelem
type( Element_ ) :: obj, anotherobj
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
refelem => ReferenceLine_Pointer( NSD = 1 )
call FPL_INIT()
call param%init()
nptrs = [1,2]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
call obj%initiate( param=param, refelem = refelem )
call obj%display( "test-1: obj = " )
call pass( 'elem_init_from_fpl()' )
call anotherobj%initiate(obj)
call anotherobj%display("test-1: anotherobj = ")
call pass( 'elem_init_from_elem' )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
class( ReferenceElement_ ), pointer :: refelem
class( Element_ ), pointer :: obj, anotherobj
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
refelem => ReferenceLine_Pointer( NSD = 1 )
call FPL_INIT()
call param%init()
nptrs = [1,2]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
allocate( Element_::obj )
call obj%initiate( param=param, refelem = refelem )
call obj%display( "test-2:" )
call pass('elem_init_from_fpl()')
allocate( Element_::anotherobj )
call anotherobj%initiate(obj)
call anotherobj%display("test-2:")
call pass( "elem_init_from_elem()" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
class( ReferenceElement_ ), pointer :: refelem
class( Element_ ), pointer :: obj, anotherobj
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
refelem => ReferenceLine_Pointer( NSD = 1 )
call FPL_INIT()
call param%init()
nptrs = [1,2]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
allocate( Element_::obj )
obj = Element( param=param, refelem = refelem )
call obj%display( "test-3:" )
call pass('elem_init_from_fpl()')
allocate( Element_::anotherobj )
anotherobj = Element(obj)
call anotherobj%display("test-3")
call pass( "elem_init_from_elem()" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
class( ReferenceElement_ ), pointer :: refelem
class( Element_ ), pointer :: obj, anotherobj
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
refelem => ReferenceLine_Pointer( NSD = 1 )
call FPL_INIT()
call param%init()
nptrs = [1,2]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
obj => Element_Pointer( param=param, refelem = refelem )
call obj%display( "test-4:" )
call pass('elem_init_from_fpl()')
allocate( Element_::anotherobj )
anotherobj => Element_Pointer(obj)
call anotherobj%display("test-4")
call pass( "elem_init_from_elem()" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
class( ReferenceElement_ ), pointer :: refelem
class( Element_ ), pointer :: obj
type( ParameterList_ ) :: param
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
refelem => ReferenceLine_Pointer( NSD = 1 )
call FPL_INIT()
call param%init()
nptrs = [1,2]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
obj => Element_Pointer( param=param, refelem = refelem )
call obj%setMaterialType(2)
call obj%setNptrs([3,4])
call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
call ok( obj%getMaterialType() == 2 , 'getNptrs')
call obj%display( "test-5:" )
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_Element
implicit none
call plan(23)
call test1
call test2
call test3
call test4
call test5
call done_testing()
end program main
