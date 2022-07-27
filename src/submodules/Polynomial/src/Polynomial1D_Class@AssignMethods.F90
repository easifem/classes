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
  obj%degree=obj2%degree
  obj%varname=obj2%varname
  !!
  IF( ALLOCATED( obj2%x ) ) THEN
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

END SUBMODULE AssignMethods