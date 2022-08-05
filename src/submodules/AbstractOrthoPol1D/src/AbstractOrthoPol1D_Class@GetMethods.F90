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

SUBMODULE(AbstractOrthopol1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetOrder
  ans = obj%n
END PROCEDURE Orthopol_GetOrder

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalScalar
  REAL( DFP ) :: Jn_1, Jn_2
  !!
  SELECT CASE( obj%n )
  CASE( -1_I4B )
    ans = obj%Pm1( x=x )
    !!
  CASE( 0_I4B )
    ans = obj%P0( x=x )
    !!
  CASE DEFAULT
    !!
    IF( ASSOCIATED( obj%Jn_1 ) ) THEN
      Jn_1 = obj%Jn_1%Eval( x )
    ELSE
      Jn_1 = 0.0_DFP
    END IF
    !!
    IF( ASSOCIATED( obj%Jn_2 ) ) THEN
      Jn_2 = obj%Jn_2%Eval( x )
    ELSE
      Jn_2 = 0.0_DFP
    END IF
    !!
    ans = (x-obj%an_1)*obj%sn_1*Jn_1 - obj%bn_1*obj%sn_2*Jn_2
    !!
  END SELECT
  !!
END PROCEDURE Orthopol_EvalScalar

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_P0
  ans = 1.0_DFP
END PROCEDURE Orthopol_P0

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Pm1
  ans = 0.0_DFP
END PROCEDURE Orthopol_Pm1

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_dP0
  ans = 0.0_DFP
END PROCEDURE Orthopol_dP0

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_dPm1
  ans = 0.0_DFP
END PROCEDURE Orthopol_dPm1

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_EvalGradientScalar
  REAL( DFP ) :: GJn_1, GJn_2, Jn_1
  !!
  SELECT CASE( obj%n )
  CASE( -1_I4B )
    ans = obj%dPm1(x=x)
  CASE( 0_I4B )
    ans = obj%dP0(x=x)
  CASE DEFAULT
    !!
    IF( ASSOCIATED( obj%Jn_1 ) ) THEN
      GJn_1 = obj%Jn_1%EvalGradient( x )
      Jn_1 = obj%Jn_1%Eval( x )
    ELSE
      GJn_1 = 0.0_DFP
      Jn_1 = 0.0_DFP
    END IF
    !!
    IF( ASSOCIATED( obj%Jn_2 ) ) THEN
      GJn_2 = obj%Jn_2%EvalGradient( x )
    ELSE
      GJn_2 = 0.0_DFP
    END IF
    !!
    ans = (x-obj%an_1) * obj%sn_1 * GJn_1 &
      & - obj%bn_1 * obj%sn_2 * GJn_2 &
      & + obj%sn_1 * Jn_1
    !!
  END SELECT
  !!
END PROCEDURE Orthopol_EvalGradientScalar

!----------------------------------------------------------------------------
!                                                         GetStringToDisplay
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetStringToDisplay
  INTEGER( I4B ) :: n_1, n_2
  !!
  IF( obj%n .EQ. 0 ) THEN
    ans = "P_0( " // obj%varname%chars() // " ) = 1"
  ELSEIF( obj%n .EQ. -1 ) THEN
    ans = ""
  ELSEIF( obj%n .EQ. 1 ) THEN
    ans = "P_1( " // obj%varname%chars() // " ) = " // &
    & "( x-"// TRIM(STR( n=obj%an_1, fm="(f0.3)")) // " )* " // &
    & TRIM(STR( n=obj%sn_1, fm="(f0.3)")) // "* P_0"
  ELSE
    !!
    IF( ASSOCIATED( obj%Jn_1 ) ) THEN
      n_1 = obj%Jn_1%n
    ELSE
      n_1 = 0
    END IF
    !!
    IF( ASSOCIATED( obj%Jn_2 ) ) THEN
      n_2 = obj%Jn_2%n
    ELSE
      n_2 = 0
    END IF
    !!
    ans = "P_" // TRIM( STR( obj%n ) ) // "( " //  &
      & obj%varname%chars() // " ) = " //&
      &  "( x-"// TRIM(STR( n=obj%an_1, fm="(f0.3)")) //&
      & " )* " // TRIM(STR( n=obj%sn_1, fm="(f0.3)")) //&
      &  "* P_" // &
      & TRIM( STR( n_1 ) ) // &
      & "( " //  obj%varname%chars() // " ) - " // &
      & TRIM( STR( n=obj%bn_1, fm="(f0.3)") ) // &
      & "* " // TRIM(STR( n=obj%sn_2, fm="(f0.3)")) //&
      & "* P_" // TRIM( STR( n_2 ) ) // &
      & "( " //  obj%varname%chars() // " )"
  END IF
END PROCEDURE Orthopol_GetStringToDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods