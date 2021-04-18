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

module test_IntList
use easifemBase
use easifemClasses
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( IntList_ ) :: obj
call note( "test-1: testing Initiate() and Delete()")
call obj%Initiate()
call obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( IntList_ ) :: obj
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-2: testing Initiate(n,val)")
n = 5; val = 1
call obj%Initiate( n=n, val=val )
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( IntList_ ) :: obj1, obj2
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-3: testing Initiate(obj1, obj2)")
n = 5; val = 1
call obj1%Initiate( n=n, val=val )
call obj2%Initiate( obj1 )
call obj1%Delete()
call obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type( IntList_ ) :: obj1, obj2
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-4: testing obj1=obj2")
n = 5; val = 1
call obj1%Initiate( n=n, val=val )
obj2=obj1
call obj1%Delete()
call obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type( IntList_ ) :: obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-5: testing obj%initiate(val)")
call obj%Initiate( val )
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
type( IntList_ ) :: obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-6: testing obj=val")
obj=val
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
type( IntList_ ) :: obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-7: testing obj%isEmpty(), obj%SIZE()")
call ok( obj%isEmpty(), "obj%isEmpty")
obj=val
call ok( .NOT. obj%isEmpty(), "obj%isEmpty")
call ok( obj%SIZE() == SIZE(val), "obj%SIZE()")
call obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
type( IntList_ ) :: obj
integer( i4b ) :: val( 5 ), i
call note( "test-8: testing obj=val")
val = [1,2,3,4,5]
call obj%initiate()
do i = 1, size( val )
  call obj%PushBack(val(i))
end do
do i = 1, size( val )
  call obj%PushFront(val(i))
end do
call obj%delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
type( IntList_ ) :: obj
type( IntListIterator_ ) :: iter
integer( i4b ) :: i
call note( "test-9: testing obj=val")
call obj%initiate()
call obj%pushback( 1 )
call obj%pushback( 2 )
call obj%pushback( 4 )
call display( obj, "[1,2,4]" )
iter = obj%Begin()
call iter%inc(2)
call display( iter, "Entry should be 4: ")
! call obj%insert( iter, [3] )
! call display( obj, "It should be [1,2,3,4]: " )
call obj%delete()
end subroutine

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_IntList
implicit none
call plan( 61 )
call test1; call test2; call test3; call test4
call test5; call test6; call test7; call test8
call test9
end program main