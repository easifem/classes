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

!! Monomial2D_Class_Add_scalar_obj.inc

REAL( DFP ) :: coeff( 2 )
INTEGER( I4B ) :: degree( 2, 2 )
TYPE( String ) :: varname( 2 )
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
  & name1=varname(1)%chars(), &
  & name2=varname(2)%chars() )
!!