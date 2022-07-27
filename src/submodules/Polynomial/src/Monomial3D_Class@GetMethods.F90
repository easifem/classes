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
  INTEGER( I4B ) :: ii
  !!
  ans = obj%coeff
  DO ii = 1, MAX_COMPONENTS
    ans = ans * (obj%x(ii) .Eval. [x( ii )] )
  END DO
  !!
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  ! Define internal values
  REAL( DFP ) :: f1, f2, f3, df1(1), df2(1), df3(1)
  !!
  f1 = obj%x(1) .Eval. [x(1)]
  f2 = obj%x(2) .Eval. [x(2)]
  f3 = obj%x(3) .Eval. [x(3)]
  !!
  df1 = obj%x(1) .Grad. [x(1)]
  df2 = obj%x(2) .Grad. [x(2)]
  df3 = obj%x(3) .Grad. [x(3)]
  !!
  ans = obj%coeff
  !!
  ans(1) = ans(1)*df1(1)*f2*f3
  ans(2) = ans(2)*f1*df2(1)*f3
  ans(3) = ans(3)*f1*f2*df3(1)
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
    f1 = .GRAD. obj%x(1)
    f2 = obj%x(2)
    f3 = obj%x(3)
  CASE( 2 )
    f1 = obj%x(1)
    f2 = .GRAD. obj%x(2)
    f3 = obj%x(3)
  CASE( 3 )
    f1 = obj%x(1)
    f2 = obj%x(2)
    f3 = .GRAD. obj%x(3)
  END SELECT
  !!
  ans = Monomial3D(coeff=obj%coeff, f1=f1, f2=f2, f3=f3 )
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
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  ans = obj%x(1)%GetDegree() + obj%x(2)%GetDegree() + obj%x(3)%GetDegree()
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                                 GetCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  ans = obj%coeff * obj%x(1)%GetCoeff() * obj%x(2)%GetCoeff() * &
    & obj%x(3)%GetCoeff()
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods