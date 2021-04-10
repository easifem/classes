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

module test_ftlListInt
use easifemBase
use easifemClasses
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( ftlListInt_ ) :: Obj
call note( "test-1: testing Initiate() and Delete()")
call Obj%Initiate()
call Obj%DeallocateData()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( ftlListInt_ ) :: Obj
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-2: testing Initiate(n,val)")
n = 5; val = 1
call Obj%Initiate( n=n, val=val )
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( ftlListInt_ ) :: Obj1, Obj2
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-3: testing Initiate(Obj1, Obj2)")
n = 5; val = 1
call Obj1%Initiate( n=n, val=val )
call Obj2%Initiate( Obj1 )
call Obj1%Delete()
call Obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type( ftlListInt_ ) :: Obj1, Obj2
integer( i4b ) :: n
integer( i4b ) :: val
call note( "test-4: testing Obj1=Obj2")
n = 5; val = 1
call Obj1%Initiate( n=n, val=val )
Obj2=Obj1
call Obj1%Delete()
call Obj2%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type( ftlListInt_ ) :: Obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-5: testing Obj%initiate(val)")
call Obj%Initiate( val )
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test6
type( ftlListInt_ ) :: Obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-6: testing Obj=val")
Obj=val
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test7
type( ftlListInt_ ) :: Obj
integer( i4b ) :: val( 4 ) = [1,2,3,4]
call note( "test-7: testing Obj%isEmpty(), Obj%SIZE()")
call ok( Obj%isEmpty(), "Obj%isEmpty")
Obj=val
call ok( .NOT. Obj%isEmpty(), "Obj%isEmpty")
call ok( Obj%SIZE() == SIZE(val), "Obj%SIZE()")
call Obj%Delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test8
type( ftlListInt_ ) :: Obj
integer( i4b ) :: val( 5 ), i
call note( "test-8: testing Obj=val")
val = [1,2,3,4,5]
call obj%initiate()
do i = 1, size( val )
  call Obj%PushBack(val(i))
end do
do i = 1, size( val )
  call Obj%PushFront(val(i))
end do
call Obj%delete()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test9
type( ftlListInt_ ) :: Obj
type( ftlListIntIterator_ ) :: iter
integer( i4b ) :: i

call note( "test-9: testing Obj=val")
call obj%initiate()
call obj%pushback( 1 )
call obj%pushback( 2 )
call obj%pushback( 4 )
call display( obj, "[1,2,4]" )
iter = Obj%End()
call display( iter, "End entry: ")
call iter%dec()
call display( iter, "Last entry: ")
call obj%insert( iter, 3 )
call display( obj, "[1,2,3,4]" )
! end do
! do i = 1, size( val )
!   call Obj%PushBack(val(i))
! end do
! do i = 1, size( val )
!   call Obj%PushFront(val(i))
! end do
! call Obj%delete()
end subroutine

end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_ftlListInt
implicit none
call plan( 61 )
call test1; call test2; call test3; call test4
call test5; call test6; call test7; call test8
call test9
end program main