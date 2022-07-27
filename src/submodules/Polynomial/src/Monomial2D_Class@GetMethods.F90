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

SUBMODULE(Monomial2D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Eval
  REAL( DFP ) :: x1( 1 ), x2( 1 )
  !!
  x1( 1 ) = x( 1 )
  x2( 1 ) = x( 2 )
  ans = obj%coeff * (obj%x(1) .Eval. x1) * (obj%x(2) .Eval. x2 )
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  ! Define internal values
  REAL( DFP ) :: x1( 1 ), x2( 1 )
  !!
  x1( 1 ) = x( 1 )
  x2( 1 ) = x( 2 )
  !!
  ans(1:1) = obj%coeff * (obj%x(1) .Grad. x1) * (obj%x(2) .Eval. x2 )
  ans(2:2) = obj%coeff * (obj%x(1) .Eval. x1) * (obj%x(2) .Grad. x2 )
  !!
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  TYPE( Monomial1D_ ) :: f1,f2
  !!
  IF( dim .EQ. 1 ) THEN
    f1 = .GRAD. obj%x(1)
    f2 = obj%x(2)
  ELSE
    f1 = obj%x(1)
    f2 = .GRAD. obj%x(2)
  END IF
  !!
  ans = Monomial2D(coeff=obj%coeff, f1=f1, f2=f2)
  !!
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  ans = obj%x(1)%GetStringForUID()// "*" // obj%x(2)%GetStringForUID()
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                                 GetDegree
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDegree
  ans = obj%x(1)%GetDegree() + obj%x(2)%GetDegree()
END PROCEDURE func_GetDegree

!----------------------------------------------------------------------------
!                                                                 GetCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetCoeff
  ans = obj%coeff * obj%x(1)%GetCoeff() * obj%x(2)%GetCoeff()
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods