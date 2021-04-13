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

module test_ftlListElement
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( ftlListElement_ ) :: Obj
call note( "test-1: testing Initiate() and Delete()")
call Obj%Initiate()
call Obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( ftlListElement_ ) :: Obj
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val
integer( i4b ) :: n

RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-2: testing Initiate(n,val)")
n = 5
val = Elem
call Obj%Initiate( n=n, val=val )
call display( obj, "obj: " )
call val%setNptrs([1,2])
call display( obj, "test-2: obj:(after) " )
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( ftlListElement_ ) :: Obj1, Obj2
integer( i4b ) :: n
type( ftlListElement_ ) :: Obj
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val

RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, RefElem = RefElem )
n = 2; val = Elem

call note( "test-3: testing Initiate(Obj1, Obj2)")
call Obj1%Initiate( n=n, val=val )
call Obj2%Initiate( Obj1 )
call display( obj1, 'test-3: obj1: ')
call display( obj2, 'test-3: obj2: ')
call Obj1%Delete()
call Obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type( ftlListElement_ ) :: Obj1, Obj2
integer( i4b ) :: n
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val

RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL Elem%Initiate( nptrs = [1,2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-4: testing Obj1=Obj2")
n = 2; val = Elem
call Obj1%Initiate( n=n, val=val )
Obj2=Obj1
call display( Obj2, "Obj2: " )
call Obj1%Delete()
call Obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type( ftlListElement_ ) :: Obj
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val( 2 )

RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, RefElem = RefElem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-5: testing Obj%initiate(val)")
call Obj%Initiate( val )
call Display( obj, "test-5 (before): ")
call val(2)%setNptrs([4,5])
call Display( obj, "test-5 (before): ")
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
type( ftlListElement_ ) :: Obj
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val( 2 )

RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, RefElem = RefElem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, RefElem = RefElem )
call note( "test-6: testing Obj=val")
Obj=val
call display( obj, "test-6: obj: ")
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
type( ftlListElement_ ) :: Obj
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val( 2 )
RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, RefElem = RefElem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-7: testing Obj%isEmpty(), Obj%SIZE()")
call ok( Obj%isEmpty(), "Obj%isEmpty")
Obj=val
call ok( .NOT. Obj%isEmpty(), "Obj%isEmpty" )
call ok( Obj%SIZE() == SIZE(val), "Obj%SIZE()" )
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
type( ftlListElement_ ) :: Obj
integer( i4b ) ::  i
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val( 2 )
RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, RefElem = RefElem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-8: testing Obj=val")
call obj%initiate()
do i = 1, size( val )
  call Obj%PushBack(val(i))
end do
do i = 1, size( val )
  call Obj%PushFront(val(i))
end do
call display( obj, "test-9: obj: ")
call Obj%delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
type( ftlListElement_ ) :: Obj
type( ftlListElementIterator_ ) :: iter
integer( i4b ) :: i
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Elem, val( 2 )
RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL val(1)%Initiate( nptrs = [1,2], Mat_Type = 1, RefElem = RefElem )
CALL val(2)%Initiate( nptrs = [2,3], Mat_Type = 1, RefElem = RefElem )

call note( "test-9: testing Obj=val")
call obj%initiate()
call obj%pushback( val(1) )
call obj%pushback( val(2) )
call display( obj, "first two element" )
iter = Obj%Begin()
call iter%inc(1)
call display( iter, "Second element: ")
call Elem%Initiate( nptrs = [3,4], Mat_Type=1, RefElem = RefElem )
call obj%insert( iter, Elem )
call display( obj, "After adding a new element: " )
call Obj%delete()
end subroutine

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_ftlListElement
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