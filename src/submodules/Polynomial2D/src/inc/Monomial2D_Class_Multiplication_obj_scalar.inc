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

!! Monomial2D_Class_Multiplication_obj_scalar.inc

REAL( DFP ) :: coeff( 1 )
INTEGER( I4B ) :: degree( 1, 2 )
TYPE( String ) :: varname( 2 )
!!
coeff( 1 ) = obj2
!!
degree( 1, : ) = obj1%GetDegree()
varname = obj1%GetVarname()
!!
CALL ans%Initiate( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname(1)%chars(), &
  & varname2=varname(2)%chars() )
!!