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

SUBMODULE(Monomial2D_Class) OperatorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS
!----------------------------------------------------------------------------
!                                                            Multiplication
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Multiplication_obj_obj
  INTEGER( I4B ) :: n1, n2
  !!
  n1 = obj1%x(1)%getDegree() + obj2%x(1)%getDegree()
  n2 = obj1%x(2)%getDegree() + obj2%x(2)%getDegree()
  ans=Monomial2D(n1=n1, n2=n2, name1=obj1%varname(1)%chars(), &
    & name2=obj1%varname(2)%chars() )
  !!
END PROCEDURE func_Multiplication_obj_obj

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE OperatorMethods