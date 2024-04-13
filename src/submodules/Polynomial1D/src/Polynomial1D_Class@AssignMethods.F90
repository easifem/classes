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

SUBMODULE(Polynomial1D_Class) AssignMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjObj
  !!
  INTEGER( I4B ) :: ii, row
  !!
  !!
  IF( ALLOCATED( obj2%x ) ) THEN
    !!
    obj%degree=obj2%degree
    obj%coeff=obj2%coeff
    obj%varname = obj2%varname
    !!
    row = SIZE( obj2%x )
    !!
    IF( ALLOCATED( obj%x ) ) THEN
      IF( SIZE( obj%x ) .NE. row ) THEN
        DEALLOCATE( obj%x )
        ALLOCATE( obj%x( row ) )
      END IF
    ELSE
      ALLOCATE( obj%x( row ) )
    END IF
    !!
    DO ii = 1, row
      obj%x(ii) = obj2%x(ii)
    END DO
    !!
  END IF
  !!
END PROCEDURE func_AssignObjObj

!----------------------------------------------------------------------------
!                                                                 Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjMono
  obj = Polynomial1D(coeff=[1.0_DFP], degree=[obj2%getDegree()], &
    & varname=obj2%varname%chars())
END PROCEDURE func_AssignObjMono

!----------------------------------------------------------------------------
!                                                                 Assign
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjInt8
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjInt8
!!
MODULE PROCEDURE func_AssignObjInt16
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjInt16
!!
MODULE PROCEDURE func_AssignObjInt32
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjInt32
!!
MODULE PROCEDURE func_AssignObjInt64
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjInt64
!!
MODULE PROCEDURE func_AssignObjReal32
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjReal32
!!
MODULE PROCEDURE func_AssignObjReal64
  obj = Polynomial1D(coeff=[REAL(obj2, KIND=DFP)], degree=[0_I4B], &
    & varname="" )
END PROCEDURE func_AssignObjReal64

END SUBMODULE AssignMethods