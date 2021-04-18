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
use FacetElement_Class
implicit none

class( element_ ), pointer :: cell, obj
class( ReferenceElement_ ), pointer :: refelem
type( ParameterList_ ) :: param
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test0
integer( I4B ), ALLOCATABLE :: nptrs(:)
integer( I4B ) :: ierr
! Reference Element
refelem => ReferenceTriangle_Pointer( NSD = 2 )
call FPL_INIT()
call param%initiate()
nptrs = [1,2,3]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
! Cell Element
cell => Element_Pointer( param=param, refelem=refelem )
! Facet element
nptrs = [2,3]
ierr = param%set(key='nptrs', value=nptrs)
ierr = param%set(key="mat_type", value=1)
refelem => ReferenceLine_Pointer( NSD = 2 )
obj => FacetElement_Pointer(param=param, refelem=refelem)
select type( obj )
type is (FacetElement_)
  call obj%setCellPointer(cell)
  call obj%setFacetLocalID(2)
end select
call param%DeallocateData()
call FPL_FINALIZE()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
call test0
call obj%display('test-1: facet elem:' )
call obj%DeallocateData()
call obj%display('test-1: facet elem after deallocatedata:' )
call cell%DeallocateData()
deallocate(cell, obj, refelem)
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
integer( i4b ), allocatable :: nptrs( : )
class( element_ ), pointer :: cellptr
call test0
select type( obj )
class is( facetelement_ )
nptrs = obj%getCellNptrs()
cellptr => obj%getCellPointer()
call display(nptrs, 'test-2:nptrs: ')
call cellptr%display('test-2:cellptr: ')
call obj%FreeCellPointer()
call obj%display('test2:freeCellPointer:')
call obj%setCellPointer(cellptr)
call obj%display('test2:setCellPointer:')
call display( obj%getFacetLocalID(), 'test2:getfacetlocalID():' )
call display( obj%getFacetLocalNptrs(), 'test2:getFacetLocalNptrs():' )
end select

deallocate(cell, obj, refelem)
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
call done_testing()
end program main
