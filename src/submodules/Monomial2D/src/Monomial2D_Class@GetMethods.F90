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
  ans = obj%x(1)%Eval(x) * obj%x(2)%Eval(y)
END PROCEDURE func_Eval

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvalGradient
  IF( dim .EQ. 1_I4B ) THEN
    ans = obj%x(1)%EvalGradient(x) * obj%x(2)%Eval(y)
  ELSE
    ans = obj%x(1)%Eval(x) * obj%x(2)%EvalGradient(y)
  END IF
END PROCEDURE func_EvalGradient

!----------------------------------------------------------------------------
!                                                                      Grad
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Grad
  TYPE( Monomial1D_ ) :: f1,f2
  !!
  IF( dim .EQ. 1 ) THEN
    f1 = obj%x(1)%Grad()
    f2 = obj%x(2)
  ELSE
    f1 = obj%x(1)
    f2 = obj%x(2)%Grad()
  END IF
  !!
  ans = Monomial2D(f1=f1, f2=f2)
  !!
END PROCEDURE func_Grad

!----------------------------------------------------------------------------
!                                                            GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetStringForUID
  ans = obj%x(1)%GetStringForUID()// "*" // obj%x(2)%GetStringForUID()
END PROCEDURE func_GetStringForUID

!----------------------------------------------------------------------------
!                                                           GetDisplayString
!----------------------------------------------------------------------------

MODULE PROCEDURE func_GetDisplayString
  ans = TRIM(obj%x(1)%GetDisplayString())// &
    & "*"// &
    & TRIM(obj%x(2)%GetDisplayString())
END PROCEDURE func_GetDisplayString

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
  ans = 1.0_DFP
END PROCEDURE func_GetCoeff

END SUBMODULE GetMethods