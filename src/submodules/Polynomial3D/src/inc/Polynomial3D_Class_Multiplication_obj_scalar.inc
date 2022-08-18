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

!!
REAL( DFP ), ALLOCATABLE :: coeff( : )
INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
TYPE(String) :: varname(3)
!!
coeff = obj1%GetCoeff() * obj2
degree = obj1%GetDegree()
varname = obj1%GetVarname()
!!
CALL ans%Initiate( &
  & coeff=coeff, &
  & degree=degree, &
  & name1=varname(1)%chars(), &
  & name3=varname(3)%chars(), &
  & name2=varname(2)%chars() )
!!
IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
IF( ALLOCATED( degree ) ) DEALLOCATE( degree )