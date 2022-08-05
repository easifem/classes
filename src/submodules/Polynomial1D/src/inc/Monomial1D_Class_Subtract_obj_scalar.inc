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
REAL( DFP ) :: coeff( 2 )
INTEGER( I4B ) :: degree( 2 )
!!
coeff = [1.0_DFP, REAL(-obj2, KIND=DFP)]
degree = [obj1%getDegree(), 0_I4B]
!!
ans = Polynomial1D(coeff=coeff, degree=degree, &
  & varname=obj1%varname%chars())
!!

