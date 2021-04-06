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

module test_list
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
type( Intlist_ ) :: obj
integer( i4b ) :: data

call display( "test-1: testing add(), final(), resetIter(), nextIter()")
call obj%add(1)
call obj%add(2)
call obj%resetIter()
call obj%nextIter(data)
call ok( data == 1, "%nextIter")
call obj%Finalize()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( Intlist_ ) :: obj
INTEGER( I4B ) :: data
INTEGER( I4B ) :: ii, n

call note("test-2: testing size(), getHead(), getTail(), getNth()")
call obj%add(1)
call obj%add(2)
call obj%add(3)
call obj%add(4)
n = obj%size()
call ok( n == 4, 'obj%size()' )
do ii = 1, n
  call obj%getNth( ii, data )
  call ok( data .EQ. 1 * ii, "obj%getNth" )
end do
call obj%gethead(data)
call ok( data .EQ. 1, "obj%gethead()")
call obj%gettail(data)
call ok( data .EQ. 4, "obj%gettail()")
call obj%Finalize()
end

! !----------------------------------------------------------------------------
! !
! !----------------------------------------------------------------------------

subroutine test3
type( intlist_ ) :: obj
integer( i4b ) :: data
integer :: ii

call note( "test-3: push(), pop(), popHead(), popTail()")
call equalline(); call display( "push()"); call equalline()

call obj%add(1)
call obj%add(2)
call obj%add(3)
call obj%push(4)

call ok( obj%size() == 4, 'obj%size()' )

do ii = 1, obj%size()
  call obj%getNth( ii, data )
  call ok( data == 1 * ii, "obj%getNth" )
end do

call equalline(); call display( "popHead()"); call equalline()
call obj%popHead(data)
call ok( data == 1, "obj%gettail()")
do ii = 1, obj%size()
  call obj%getNth( ii, data )
  call ok( data == 1 * (ii+1), "obj%getNth" )
end do

call equalline(); call display( "popTail()"); call equalline()
call obj%popTail(data)
call ok( data == 4, "obj%gettail()")
do ii = 1, obj%size()
  call obj%getNth( ii, data )
  call ok( data == (ii+2), "obj%getNth" )
end do
call obj%Finalize()
end
end module


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_list
implicit none
call plan( 61 )
call test1
call test2
call test3
end program main