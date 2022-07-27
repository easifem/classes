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

SUBMODULE(Monomial3D_Class) ConstructorMethods
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
  CALL obj%x(3)%Deallocate()
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D1
  !!
  TYPE( String ) :: astr
  INTEGER( I4B ) :: ii
  !!
  ans%coeff = coeff
  !!
  DO ii = 1, MAX_COMPONENTS
    ans%x(ii) = Monomial1D( &
      & coeff=1.0_DFP, &
      & degree=degree(ii), &
      & varname=varname(ii))
  END DO
  !!
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial3D1

!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D2
  !!
  REAL( DFP ) :: coeff0
  INTEGER( I4B ) :: degree(MAX_COMPONENTS)
  CHARACTER( LEN = 256 ) :: varname(MAX_COMPONENTS)
  !!
  coeff0 = coeff * f1%coeff * f2%coeff * f3%coeff
  !!
  degree(1) = f1%degree
  degree(2) = f2%degree
  degree(3) = f3%degree
  !!
  varname(1) = f1%varname%chars()
  varname(2) = f2%varname%chars()
  varname(3) = f3%varname%chars()
  !!
  ans = Monomial3D(coeff=coeff, degree=degree, varname=varname)
  !!
END PROCEDURE func_Monomial3D2

!----------------------------------------------------------------------------
!                                                         Monomial3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D_Pointer1
  !!
  TYPE( String ) :: astr
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE( Monomial3D_::ans )
  ans%coeff = coeff
  !!
  DO ii = 1, MAX_COMPONENTS
    ans%x(ii) = Monomial1D( &
      & coeff=1.0_DFP, &
      & degree=degree(ii), &
      & varname=varname(ii))
  END DO
  !!
  astr = ans%GetStringForUID( )
  ans%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Monomial3D_Pointer1


!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D_Pointer2
  !!
  REAL( DFP ) :: coeff0
  INTEGER( I4B ) :: degree(MAX_COMPONENTS)
  CHARACTER( LEN = 256 ) :: varname(MAX_COMPONENTS)
  !!
  coeff0 = coeff * f1%coeff * f2%coeff * f3%coeff
  !!
  degree(1) = f1%degree
  degree(2) = f2%degree
  degree(3) = f3%degree
  !!
  varname(1) = f1%varname%chars()
  varname(2) = f2%varname%chars()
  varname(3) = f3%varname%chars()
  !!
  ans => Monomial3D_Pointer(coeff=coeff, degree=degree, varname=varname)
  !!
END PROCEDURE func_Monomial3D_Pointer2


END SUBMODULE ConstructorMethods