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

SUBMODULE(Monomial1D_Class) AssignMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjObj
  obj%degree=obj2%degree
  obj%varname=obj2%varname
  obj%uid=obj2%uid
END PROCEDURE func_AssignObjObj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_AssignObjVecObjVec
  INTEGER( I4B ) :: n1, n2, ii
  !!
  n2 = SIZE( obj2 )
  !!
  IF( ALLOCATED( obj ) ) THEN
    n1 = SIZE( obj )
    IF( n1 .EQ. n2 ) THEN
      obj = obj2
    ELSE
      DEALLOCATE( obj )
      ALLOCATE( obj( n2 ) )
      obj = obj2
    END IF
  ELSE
    ALLOCATE( obj( n2 ) )
    obj = obj2
  END IF
END PROCEDURE func_AssignObjVecObjVec

END SUBMODULE AssignMethods