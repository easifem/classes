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

module test
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function generateElem(nptrs, mat_type) result(obj)
  integer( I4B ), INTENT( IN ) :: nptrs(:)
  integer( i4b ), INTENT( IN ) :: mat_type
  type( Element_ ) ::  obj
  !
  class( ReferenceElement_ ), pointer :: refelem
  type( ParameterList_ ) :: param
  integer( I4B ) :: ierr

  refelem => ReferenceLine_Pointer( NSD = 1 )
  call FPL_INIT()
  call param%init()
  ierr = param%set(key='nptrs', value=nptrs)
  ierr = param%set(key="mat_type", value=mat_type)
  call obj%initiate( param=param, refelem = refelem )
  refelem => null()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type(ElementPointerVector_ ) :: obj, other
type(ElementPointer_) :: elem
type(ElementPointerIterator_) :: elemIter
integer( i4b ) :: i

call Display( colorize( 'TEST-1: TESTING PUSHBACK METHOD', color_bg = "WHITE", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
call obj%initiate()
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [1,2], 1 ); call obj%pushback( elem )
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [2,3], 1 ); call obj%pushback( elem )
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [3,4], 1 ); call obj%pushback( elem )
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [5,6], 1 ); call obj%pushback( elem )
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [6,7], 1 ); call obj%pushback( elem )
call obj%display("test1:")
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING SIZE, CAPACITY', color_bg = "CYAN", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
call display( obj%size(), "size:")
call display( obj%capacity(), "capacity:")
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING POPBACK', color_bg = "WHITE", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
nullify(elem%ptr)
call obj%popback(elem)
call display(elem%ptr, 'elem%ptr: ')
call display( obj%size(), "size:")
call display( obj%capacity(), "capacity:")
call obj%display("obj after popback:")
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING BEGIN', color_bg = "CYAN", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
elemIter = obj%begin()
call display(elemIter%value%ptr, 'obj%begin()')
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING ERASE', color_bg = "WHITE", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
call obj%erase(first=1, last=2 )
call display(obj%size(), 'size after erase:')
call obj%display('aftere erasing some elements: ')
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING SHRINKTOFIT', color_bg = "CYAN", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
call display(obj%size(), 'size before shrink:')
call display(obj%capacity(), 'capacity before shrink:')
call obj%shrinkToFit( )
call display(obj%size(), 'size after shrink:')
call display(obj%capacity(), 'capacity after shrink:')
call obj%display('obj after shrink:')
call equalline()
call blanklines(nol=3)

call Display( colorize( 'TEST-1: TESTING INSERT', color_bg = "WHITE", color_fg="BLACK", style="BOLD_ON" ) )
call equalline()
allocate( Element_::elem%ptr ); elem%ptr = generateElem( [1,2], 1 ); call obj%insert(pos=1, val=elem)
call obj%display('after inserting at 1:')
call obj%insert(pos=1, n=1, val=elem)
call obj%display('after inserting at 1:')
call obj%erase(pos=1)
call equalline()
call blanklines(nol=3)


call Display( colorize( 'TEST-1: TESTING ITERATOR', color_bg = "CYAN", color_fg="BLACK", style="BOLD_ON" ) )
elemIter = obj%begin()
do i = 1, obj%size()
  call display(elemIter%value%ptr, "elemIter%value%ptr::")
  call elemIter%Inc()
end do

call Display( colorize( 'TEST-1: TESTING elemIter+1', color_bg = "WHITE", color_fg="BLACK", style="BOLD_ON" ) )
elemIter = obj%begin()
elemIter = elemIter+1
call display(elemIter, 'elemIter+1')
elemIter = elemIter+4
call display(elemIter, 'elemIter+4')

! call note("test-1: testing deallocatedata()")
! call obj%DeallocateData()
! call display(elemIter%value%ptr, 'obj%begin()')
! call equalline()
! call blanklines(nol=3)

end subroutine test1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module test

program main
use test
call test1
end program main