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
  CALL AbstractBasis1DDeallocate( obj )
  obj%degree = 0_I4B
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

MODULE PROCEDURE func_Initiate
  TYPE( String ) :: astr
  !!
  obj%degree = degree
  obj%varname = TRIM(varname)
  astr = obj%GetStringForUID()
  obj%uid = StringToUID(astr%chars())
END PROCEDURE func_Initiate

!----------------------------------------------------------------------------
!                                                               Monomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial1D1
  CALL ans%Initiate( degree=degree, varname=varname )
END PROCEDURE func_Monomial1D1

!----------------------------------------------------------------------------
!                                                         Monomial1D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial1D_Pointer1
  ALLOCATE( Monomial1D_::ans )
  CALL ans%Initiate( degree=degree, varname=varname )
END PROCEDURE func_Monomial1D_Pointer1

END SUBMODULE ConstructorMethods