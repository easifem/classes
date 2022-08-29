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

REAL( DFP ) :: coeff( 2 )
INTEGER( I4B ) :: degree( 2, 3 )
TYPE( String ) :: varname( 3 )
!!
coeff( 1 ) = obj1
coeff( 2 ) = -1.0_DFP
!!
degree( 1, : ) = 0_I4B
degree( 2, : ) = obj2%GetDegree()
varname = obj2%GetVarname()
!!
CALL ans%Initiate( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname(1)%chars(), &
  & varname3=varname(3)%chars(), &
  & varname2=varname(2)%chars() )
!!