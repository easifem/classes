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

SUBMODULE(Monomial1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
  CALL AbstractMonomialDeallocate( obj )
  obj%degree = 0_I4B
  obj%varname=""
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                               Monomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial1D1
  !!
  TYPE( String ) :: astr
  !!
  ans%coeff = coeff
  ans%degree = degree
  ans%varname = TRIM(varname)
  astr = ans%GetStringForUID()
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial1D1

!----------------------------------------------------------------------------
!                                                         Monomial1D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial1D_Pointer1
  TYPE( String ) :: astr
  !!
  ALLOCATE( Monomial1D_::ans )
  ans%coeff = coeff
  ans%degree = degree
  ans%varname = TRIM(varname)
  astr = ans%GetStringForUID()
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial1D_Pointer1

END SUBMODULE ConstructorMethods