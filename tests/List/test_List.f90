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
type( list_ ) :: obj
class( * ), pointer :: data
call display( "test-1: testing add(), final(), resetIter(), nextIter()")
call obj%add(1.0_dfp)
call obj%add(2.0_dfp)
call obj%resetIter()
call obj%nextIter(data)
select type ( data )
  type is( REAL( DFP ) )
    call ok( data == 1.0_DFP, "%nextIter")
end select
call obj%Finalize()
call done_testing
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test2
type( list_ ) :: obj
class( * ), pointer :: data
integer( i4b ) :: ii, n

call note("test-2: testing size(), getHead(), getTail(), getNth()")

call obj%add(1.0_dfp)
call obj%add(2.0_dfp)
call obj%add(3.0_dfp)
call obj%add(4.0_dfp)

n = obj%size()
call ok( n == 4, 'obj%size()' )

do ii = 1, n
  call obj%getNth( ii, data )
  select type ( data )
    type is( REAL( DFP ) )
      call ok( data .APPROXEQA. 1.0_dfp * ii, "obj%getNth" )
  end select
end do

call obj%gethead(data)
select type( data ); type is( real( dfp )); call ok( data .APPROXEQA. 1.0_dfp, "obj%gethead()"); end select

call obj%gettail(data)
select type( data ); type is( real( dfp )); call ok( data .APPROXEQA. 4.0_dfp, "obj%gettail()"); end select

call obj%Finalize()
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
type( list_ ) :: obj
class( * ), pointer :: data
integer :: ii

call note( "test-3: push(), pop(), popHead(), popTail()")
call equalline(); call display( "push()"); call equalline()

call obj%add(1.0_dfp)
call obj%add(2.0_dfp)
call obj%add(3.0_dfp)
call obj%push(4.0_dfp)

call ok( obj%size() == 4, 'obj%size()' )

do ii = 1, obj%size()
  call obj%getNth( ii, data )
  select type ( data )
    type is( REAL( DFP ) )
      call ok( data .APPROXEQA. 1.0_dfp * ii, "obj%getNth" )
  end select
end do

call equalline(); call display( "popHead()"); call equalline()
call obj%popHead(data)
select type( data ); type is( real( dfp )); call ok( data .APPROXEQA. 1.0_dfp, "obj%gettail()"); end select
do ii = 1, obj%size()
  call obj%getNth( ii, data )
  select type ( data )
    type is( REAL( DFP ) )
      call ok( data .APPROXEQA. 1.0_dfp * (ii+1), "obj%getNth" )
  end select
end do

call equalline(); call display( "popTail()"); call equalline()
call obj%popTail(data)
select type( data ); type is( real( dfp )); call ok( data .APPROXEQA. 4.0_dfp, "obj%gettail()"); end select
do ii = 1, obj%size()
  call obj%getNth( ii, data )
  select type ( data )
    type is( REAL( DFP ) )
      call ok( data .APPROXEQA. 1.0_dfp * (ii+2), "obj%getNth" )
  end select
end do
call obj%Finalize()
end


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
type( list_ ) :: obj
class( * ), pointer :: data
integer( i4b ) :: ii

call equalline(); call display( "Intrinsic heterogeneous()"); call equalline()
call obj%add("hello world")
call obj%add(1_i4b)
call obj%add(1.0_dfp)
do ii = 1, obj%size()
  call obj%getNth( ii, data )
  select type ( data )
    type is( REAL( DFP ) )
      call display( data, 'real = ')
    type is ( INTEGER( I4B ) )
      call display( data, 'integer = ' )
    type is ( CHARACTER( LEN=* ))
      call display( data, 'string = ')
  end select
end do
call obj%Finalize()
end


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test5
type( list_ ) :: obj
class( * ), pointer :: data
type( realvector_ ) :: vec

call equalline(); call display( "data type heterogeneous()"); call equalline()
call initiate( vec, 10_i4b )
call obj%add( vec )
call obj%gethead( data )
select type( data ); type is (realvector_); call display( vec, "vec="); end select
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
call test4
call test5
end program main