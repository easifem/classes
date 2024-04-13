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
  obj%n1 = -1
  obj%n2 = -1
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate1
  !!
  TYPE( String ) :: astr
  !!
  obj%n1 = n1
  obj%n2 = n2
  obj%varname(1) = TRIM(varname1)
  obj%varname(2) = TRIM(varname2)
  astr = obj%GetStringForUID( )
  obj%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Initiate1

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate2
  !!
  INTEGER( I4B ) :: n1, n2
  TYPE( String ) :: varname1, varname2
  !!
  n1 = f1%GetDegree()
  n2 = f2%GetDegree()
  varname1 = f1%GetVarname()
  varname2 = f2%GetVarname()
  !!
  CALL obj%Initiate(n1=n1, n2=n2, varname1=varname1%chars(), varname2=varname2%chars())
  !!
END PROCEDURE func_Initiate2

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D1
  CALL ans%Initiate( n1=n1, n2=n2, varname1=varname1, varname2=varname2)
END PROCEDURE func_Monomial2D1

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D2
  CALL ans%Initiate( f1=f1, f2=f2 )
END PROCEDURE func_Monomial2D2

!----------------------------------------------------------------------------
!                                                         Monomial2D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer1
  ALLOCATE( Monomial2D_::ans )
  CALL ans%Initiate( n1=n1, n2=n2, varname1=varname1, varname2=varname2)
END PROCEDURE func_Monomial2D_Pointer1

!----------------------------------------------------------------------------
!                                                               Monomial2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial2D_Pointer2
  ALLOCATE( Monomial2D_::ans )
  CALL ans%Initiate( f1=f1, f2=f2 )
END PROCEDURE func_Monomial2D_Pointer2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods