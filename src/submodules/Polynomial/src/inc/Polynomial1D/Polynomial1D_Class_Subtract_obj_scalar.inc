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
IF( ALLOCATED( obj1%x ) ) THEN
  !!
  coeff = obj1%getCoeff()
  degree = obj1%getDegree( )
  !!
  IF( degree( 1 ) .EQ. 0_I4B ) THEN
    !!
    coeff( 1 ) = coeff( 1 ) - REAL(obj2, KIND=DFP)
    !!
  ELSE
    !!
    CALL APPEND( coeff, -REAL(obj2, KIND=DFP) )
    CALL APPEND( degree, 0_I4B )
    !!
  END IF
  !!
ELSE
  !!
  degree = [0_I4B]
  coeff = [-REAL( obj2, KIND=DFP )]
  !!
END IF
!!
ans = Polynomial1D( coeff=coeff, degree=degree, &
  & varname=obj1%varname%chars())
!!
IF( ALLOCATED( coeff ) ) DEALLOCATE( coeff )
IF( ALLOCATED( degree ) ) DEALLOCATE( degree )