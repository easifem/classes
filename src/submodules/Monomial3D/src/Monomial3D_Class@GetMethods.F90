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

SUBMODULE(Monomial3D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Eval
  ans = (x**obj%n1) * (y**obj%n2) * (z**obj%n3)
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                       Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  INTEGER( I4B ) :: n
  IF( dim .EQ. 1_I4B ) THEN
    n = MAX( 0_I4B, obj%n1-1_I4B )
    ans = obj%n1*(x**n) * (y**obj%n2) * (z**obj%n3)
  ELSEIF( dim .EQ. 2_I4B ) THEN
    n = MAX( 0_I4B, obj%n2-1_I4B )
    ans = obj%n2*(y**n) * (x**obj%n1) * (z**obj%n3)
  ELSE
    n = MAX( 0_I4B, obj%n3-1_I4B )
    ans = obj%n3*(z**n) * (x**obj%n1) * (y**obj%n2)
  END IF
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                       Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  INTEGER( I4B ) :: n1, n2, n3
  TYPE(String) :: varname(3)
  !!
  IF( dim .EQ. 1_I4B ) THEN
    n1 = MAX( 0_I4B, obj%n1-1_I4B )
    n2 = obj%n2
    n3 = obj%n3
  ELSEIF( dim .EQ. 2_I4B ) THEN
    n1 = obj%n1
    n2 = MAX( 0_I4B, obj%n2-1_I4B )
    n3 = obj%n3
  ELSE
    n1 = obj%n1
    n2 = obj%n2
    n3 = MAX( 0_I4B, obj%n3-1_I4B )
  END IF
  !!
  varname = obj%GetVarname()
  !!
  CALL ans%Initiate( &
    & n1=n1, &
    & n2=n2, &
    & n3=n3, &
    & varname1=varname(1)%chars(), &
    & varname2=varname(2)%chars(), &
    & varname3=varname(3)%chars() )
  !!
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  TYPE(String) :: varname(3)
  !!
  varname = obj%GetVarname()
  !!
  ans = &
    & varname(1)%chars() // "^" // TRIM(STR( obj%n1 )) // &
    & "*" // &
    & varname(2)%chars() // "^" // TRIM(STR( obj%n2 )) // &
    & "*" // &
    & varname(3)%chars() // "^" // TRIM(STR( obj%n3 ))
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                           GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  TYPE(String) :: varname(3)
  varname = obj%GetVarname()
  ans = ""
  IF( obj%n1 .NE. 0_I4B ) THEN
    ans = varname(1)%chars() // "^" // TRIM(STR( obj%n1 ))
  END IF
  IF( obj%n2 .NE. 0_I4B ) THEN
    ans = ans // " " // varname(2)%chars() // "^" // TRIM(STR( obj%n2 ))
  END IF
  IF( obj%n3 .NE. 0_I4B ) THEN
    ans = ans // " " // varname(3)%chars() // "^" // TRIM(STR( obj%n3 ))
  END IF
END PROCEDURE func_GetDisplayString

!----------------------------------------------------------------------------
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  ans = [obj%n1, obj%n2, obj%n3]
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                                 GetCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  ans = 1.0_DFP
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods