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

SUBMODULE(Monomial2D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
  CALL AbstractBasis2DDeallocate( obj )
  CALL obj%x(1)%Deallocate()
  CALL obj%x(2)%Deallocate()
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D1
  !!
  TYPE( String ) :: astr
  !!
  ans%x(1) = Monomial1D(degree=n1, varname=name1)
  ans%x(2) = Monomial1D(degree=n2, varname=name2)
  ans%varname(1) = TRIM(name1)
  ans%varname(2) = TRIM(name2)
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial2D1

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D2
  !!
  INTEGER( I4B ) :: n1, n2
  CHARACTER( LEN = 256 ) :: name1, name2
  !!
  n1 = f1%GetDegree()
  n2 = f2%GetDegree()
  name1 = f1%varname%chars()
  name2 = f2%varname%chars()
  !!
  ans = Monomial2D(n1=n1, n2=n2, name1=name1, name2=name2)
  !!
END PROCEDURE func_Monomial2D2

!----------------------------------------------------------------------------
!                                                         Monomial2D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer1
  TYPE( String ) :: astr
  !!
  ALLOCATE( Monomial2D_::ans )
  ans%x(1) = Monomial1D(degree=n1, varname=name1)
  ans%x(2) = Monomial1D(degree=n2, varname=name2)
  ans%varname(1) = TRIM(name1)
  ans%varname(2) = TRIM(name2)
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial2D_Pointer1

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer2
  !!
  INTEGER( I4B ) :: n1, n2
  CHARACTER( LEN = 256 ) :: name1, name2
  !!
  n1 = f1%GetDegree()
  n2 = f2%GetDegree()
  name1 = f1%varname%chars()
  name2 = f2%varname%chars()
  !!
  ans => Monomial2D_Pointer(n1=n1, n2=n2, name1=name1, name2=name2)
  !!
END PROCEDURE func_Monomial2D_Pointer2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods