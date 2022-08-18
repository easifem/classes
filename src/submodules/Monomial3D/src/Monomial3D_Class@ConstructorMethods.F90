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
  CALL AbstractBasis3DDeallocate( obj )
  obj%n1 = -1
  obj%n2 = -1
  obj%n3 = -1
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
  obj%n3 = n3
  obj%varname(1) = TRIM(name1)
  obj%varname(2) = TRIM(name2)
  obj%varname(3) = TRIM(name3)
  astr = obj%GetStringForUID( )
  obj%uid = StringToUID(astr%chars())
  !!
END PROCEDURE func_Initiate1

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate2
  !!
  INTEGER( I4B ) :: n1, n2, n3
  TYPE( String ) :: name1, name2, name3
  !!
  n1 = f1%GetDegree()
  n2 = f2%GetDegree()
  n3 = f3%GetDegree()
  name1 = f1%GetVarname()
  name2 = f2%GetVarname()
  name3 = f3%GetVarname()
  !!
  CALL obj%Initiate( &
    & n1=n1, &
    & n2=n2, &
    & n3=n3, &
    & name1=name1%chars(), &
    & name2=name2%chars(), &
    & name3=name3%chars() )
  !!
END PROCEDURE func_Initiate2

!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D1
  CALL ans%Initiate( &
    & n1=n1, &
    & n2=n2, &
    & n3=n3, &
    & name1=name1, &
    & name2=name2, &
    & name3=name3 )
END PROCEDURE func_Monomial3D1

!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D2
  CALL ans%Initiate( f1=f1, f2=f2, f3=f3 )
END PROCEDURE func_Monomial3D2

!----------------------------------------------------------------------------
!                                                         Monomial3D_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D_Pointer1
  ALLOCATE( Monomial3D_::ans )
  CALL ans%Initiate( &
    & n1=n1, &
    & n2=n2, &
    & n3=n3, &
    & name1=name1, &
    & name2=name2, &
    & name3=name3 )
END PROCEDURE func_Monomial3D_Pointer1

!----------------------------------------------------------------------------
!                                                               Monomial3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomial3D_Pointer2
  ALLOCATE( Monomial3D_::ans )
  CALL ans%Initiate( f1=f1, f2=f2, f3=f3 )
END PROCEDURE func_Monomial3D_Pointer2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods