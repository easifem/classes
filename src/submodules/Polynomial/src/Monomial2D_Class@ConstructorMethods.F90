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
  CALL AbstractMonomialDeallocate( obj )
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
  ans%coeff = coeff
  ans%x(1) = Monomial1D(coeff=1.0_DFP, degree=degree(1), varname=varname(1))
  ans%x(2) = Monomial1D(coeff=1.0_DFP, degree=degree(2), varname=varname(2))
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial2D1

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D2
  !!
  REAL( DFP ) :: coeff0
  INTEGER( I4B ) :: degree(2)
  CHARACTER( LEN = 256 ) :: varname(2)
  !!
  coeff0 = coeff * f1%coeff * f2%coeff
  degree(1) = f1%degree
  degree(2) = f2%degree
  varname(1) = f1%varname%chars()
  varname(2) = f2%varname%chars()
  !!
  ans = Monomial2D(coeff=coeff, degree=degree, varname=varname)
  !!
END PROCEDURE func_Monomial2D2

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer2
  !!
  REAL( DFP ) :: coeff0
  INTEGER( I4B ) :: degree(2)
  CHARACTER( LEN = 256 ) :: varname(2)
  !!
  coeff0 = coeff * f1%coeff * f2%coeff
  degree(1) = f1%degree
  degree(2) = f2%degree
  varname(1) = f1%varname%chars()
  varname(2) = f2%varname%chars()
  !!
  ans => Monomial2D_Pointer(coeff=coeff, degree=degree, varname=varname)
  !!
END PROCEDURE func_Monomial2D_Pointer2

!----------------------------------------------------------------------------
!                                                         Monomial2D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer1
  TYPE( String ) :: astr
  !!
  ALLOCATE( Monomial2D_::ans )
  ans%coeff = coeff
  ans%x(1) = Monomial1D(coeff=1.0_DFP, degree=degree(1), varname=varname(1))
  ans%x(2) = Monomial1D(coeff=1.0_DFP, degree=degree(2), varname=varname(2))
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial2D_Pointer1

END SUBMODULE ConstructorMethods