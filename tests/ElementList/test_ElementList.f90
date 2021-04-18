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

module test_ElementList
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( ElementList_ ) :: obj
call note( "test-1: testing Initiate() and Delete()")
call obj%Initiate()
call obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( ElementList_ ) :: obj
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val
integer( i4b ) :: n

refelem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, refelem = refelem )

call note( "test-2: testing Initiate(n,val)")
n = 5
val = Elem
call obj%Initiate( n=n, val=val )
call display( obj, "obj: " )
call val%setNptrs([1,2])
call display( obj, "test-2: obj:(after) " )
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( ElementList_ ) :: obj1, obj2
integer( i4b ) :: n
type( ElementList_ ) :: obj
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val

refelem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, refelem = refelem )
n = 2; val = Elem

call note( "test-3: testing Initiate(obj1, obj2)")
call obj1%Initiate( n=n, val=val )
call obj2%Initiate( obj1 )
call display( obj1, 'test-3: obj1: ')
call display( obj2, 'test-3: obj2: ')
call obj1%Delete()
call obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type( ElementList_ ) :: obj1, obj2
integer( i4b ) :: n
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val

refelem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, refelem = refelem )

call note( "test-4: testing obj1=obj2")
n = 2; val = Elem
call obj1%Initiate( n=n, val=val )
obj2=obj1
call display( obj2, "obj2: " )
call obj1%Delete()
call obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type( ElementList_ ) :: obj
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val( 2 )

refelem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, refelem = refelem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, refelem = refelem )

call note( "test-5: testing obj%initiate(val)")
call obj%Initiate( val )
call Display( obj, "test-5 (before): ")
call val(2)%setNptrs([4,5])
call Display( obj, "test-5 (before): ")
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
type( ElementList_ ) :: obj
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val( 2 )

refelem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, refelem = refelem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, refelem = refelem )
call note( "test-6: testing obj=val")
obj=val
call display( obj, "test-6: obj: ")
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
type( ElementList_ ) :: obj
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val( 2 )
refelem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, refelem = refelem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, refelem = refelem )

call note( "test-7: testing obj%isEmpty(), obj%SIZE()")
call ok( obj%isEmpty(), "obj%isEmpty")
obj=val
call ok( .NOT. obj%isEmpty(), "obj%isEmpty" )
call ok( obj%SIZE() == SIZE(val), "obj%SIZE()" )
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
type( ElementList_ ) :: obj
integer( i4b ) ::  i
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val( 2 )
refelem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, refelem = refelem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, refelem = refelem )

call note( "test-8: testing obj=val")
call obj%initiate()
do i = 1, size( val )
  call obj%PushBack(val(i))
end do
do i = 1, size( val )
  call obj%PushFront(val(i))
end do
call display( obj, "test-9: obj: ")
call obj%delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
type( ElementList_ ) :: obj
type( ElementListIterator_ ) :: iter
integer( i4b ) :: i
CLASS( ReferenceElement_ ), POINTER :: refelem
TYPE( Element_ ) :: Elem, val( 2 )
refelem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, refelem = refelem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, refelem = refelem )

call note( "test-9: testing obj=val")
call obj%initiate()
call obj%pushback( val(1) )
call obj%pushback( val(2) )
call display( obj, "first two element" )
iter = obj%Begin()
call iter%inc(1)
call display( iter, "Second element: ")
call Elem%Initiate( nptrs = [3,4], Mat_Type=1, refelem = refelem )
call obj%insert( iter, Elem )
call display( obj, "After adding a new element: " )
call obj%delete()
end subroutine

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_ElementList
implicit none
call plan( 61 )
call test1
call test2
call test3
call test4
call test5
call test6
call test7
call test8
call test9
end program main