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

SUBMODULE(Monomial1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalScalar
  ans = x ** obj%degree
END PROCEDURE func_EvalScalar

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalVector
  ans = x ** obj%degree
END PROCEDURE func_EvalVector

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  ! Define internal values
  IF( obj%degree .LE. 0_I4B ) THEN
    ans = 0.0_DFP
  ELSE
    ans = obj%degree*( x**(obj%degree-1) )
  END IF
  !!
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  IF( obj%degree .LE. 0_I4B ) THEN
    ans = Monomial1D( &
      & degree=0_I4B, &
      & varname=obj%varname%chars())
  ELSE
    ans = Monomial1D( &
      & degree=obj%degree-1, &
      & varname=obj%varname%chars())
  END IF
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  ans = obj%varname%chars() // "^" // TRIM(STR( obj%degree ))
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  ans = obj%degree
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                          GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  !!
  ans = obj%varname%chars()// "^" // TRIM(STR( obj%degree ))
  !!
END PROCEDURE func_GetDisplayString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  ans = 1.0_DFP
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods