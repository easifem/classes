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

!! Monomial1D_Class_Add_obj_scalar.inc

REAL( DFP ) :: coeff
INTEGER( I4B ) :: degree
!!
IF( obj1%degree .EQ. 0_I4B ) THEN
  coeff = obj1%coeff - REAL( obj2, KIND=DFP )
  degree = obj1%degree
  ans=Monomial1D(coeff=coeff, degree=degree, varname=obj1%varname%chars())
END IF
!!