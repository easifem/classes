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
!                                                                       Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Eval
  ans = obj%x(1)%Eval(x) * obj%x(2)%Eval(y) * obj%x(3)%Eval(z)
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  ! Define internal values
  REAL( DFP ) :: a,b,c
  !!
  SELECT CASE( dim )
  CASE( 1 )
    a = obj%x(1)%EvalGradient( x )
    b = obj%x(2)%Eval(y)
    c = obj%x(3)%Eval(z)
  CASE( 2 )
    a = obj%x(1)%Eval( x )
    b = obj%x(2)%EvalGradient(y)
    c = obj%x(3)%Eval(z)
  CASE( 3 )
    a = obj%x(1)%Eval( x )
    b = obj%x(2)%Eval(y)
    c = obj%x(3)%EvalGradient(z)
  END SELECT
  !!
  ans = a*b*c
  !!
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  TYPE( Monomial1D_ ) :: f1,f2,f3
  !!
  SELECT CASE( dim )
  CASE( 1 )
    f1 = obj%x(1)%grad()
    f2 = obj%x(2)
    f3 = obj%x(3)
  CASE( 2 )
    f1 = obj%x(1)
    f2 = obj%x(2)%grad()
    f3 = obj%x(3)
  CASE( 3 )
    f1 = obj%x(1)
    f2 = obj%x(2)
    f3 = obj%x(3)%grad()
  END SELECT
  !!
  ans = Monomial3D(f1=f1, f2=f2, f3=f3 )
  !!
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  ans = obj%x(1)%GetStringForUID() // &
    & "*" // &
    & obj%x(2)%GetStringForUID() // &
    & "*" // &
    & obj%x(3)%GetStringForUID()
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                          GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  ans = obj%x(1)%GetDisplayString() // &
    & "*" // &
    & obj%x(2)%GetDisplayString() // &
    & "*" // &
    & obj%x(3)%GetDisplayString()
END PROCEDURE func_GetDisplayString

!----------------------------------------------------------------------------
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  ans = obj%x(1)%GetDegree() + obj%x(2)%GetDegree() + obj%x(3)%GetDegree()
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                                 GetCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  ans = 1.0_DFP
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods