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

module test_m
use easifemBase
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE test0
  TYPE( TxtFile_ ) :: obj
  TYPE( string ) :: line
  INTEGER( I4B ) :: ii

  CALL obj%initiate(file="./example.txt", status='OLD', &
    & action='READ')
  CALL obj%open()

  ii = 0
  DO
    CALL obj%readLine(line)
    IF( obj%isEOF() ) EXIT
    ii = ii + 1
    CALL display(line, "line " // str( ii, no_sign=.TRUE.) // "=" )
  END DO
  CALL obj%rewind()
  ii = 0
  DO
    CALL obj%readLine(line)
    IF( obj%isEOF() ) EXIT
    ii = ii + 1
    CALL display(line, "line " // str( ii, no_sign=.TRUE.) // "=" )
  END DO
  call obj%close()
END SUBROUTINE


end module test_m

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_m
implicit none
call test0
end program main