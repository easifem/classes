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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(Monomial3D_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  INTEGER( I4B ) :: n1, n2, n3
  TYPE( String ) :: varname( 3 )
  !!
  n1 = obj1%n1 + obj2%n1
  n2 = obj1%n2 + obj2%n2
  n3 = obj1%n3 + obj2%n3
  !!
  varname = obj1%GetVarname()
  !! fs
  CALL ans%Initiate( &
    & n1=n1, &
    & n2=n2, &
    & n3=n3, &
    & name1=varname(1)%chars(), &
    & name2=varname(2)%chars(), &
    & name3=varname(3)%chars() )
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE OperatorMethods