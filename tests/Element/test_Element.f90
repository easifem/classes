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
use easifemClasses
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1
CLASS( ReferenceElement_ ), POINTER :: RefElem
TYPE( Element_ ) :: Obj
RefElem => ReferenceLine_Pointer( NSD = 1 )
CALL Obj%Initiate( nptrs = [1,2,3], Mat_Type = 1, RefElem = RefElem )
CALL Obj%Display( "Obj = " )
end
end module

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_Element
implicit none
call test1
end program main
