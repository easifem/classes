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
INTEGER( I4B ), ALLOCATABLE :: degree( : )
!!
!!
!!
IF( ALLOCATED( obj2%x ) ) THEN
  !!
  coeff = obj2%getCoeff()
  coeff = coeff * REAL( obj1, KIND=DFP )
  degree = obj2%getDegree( )
  !!
ELSE
  !!
  degree = [0_I4B]
  coeff = [REAL( obj1, KIND=DFP )]
  !!
END IF
!!
ans = Polynomial1D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname=obj2%varname%chars())
!!
IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
IF( ALLOCATED( degree ) ) DEALLOCATE( degree )