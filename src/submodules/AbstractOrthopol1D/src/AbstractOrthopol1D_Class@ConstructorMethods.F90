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

SUBMODULE(AbstractOrthopol1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate
  CALL obj%Deallocate2()
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    CALL obj%Jn_1%Deallocate2()
  END IF
  CALL obj%Deallocate1()
END PROCEDURE Orthopol_Deallocate

!----------------------------------------------------------------------------
!                                                               Deallocate1
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate1
  !!
  CALL AbstractBasis1DDeallocate( obj )
  obj%n = 0
  obj%an_1 = 0.0_DFP
  obj%bn_1 = 0.0_DFP
  obj%sn_1 = 1.0_DFP
  obj%sn_2 = 1.0_DFP
  !!
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    CALL obj%Jn_1%Deallocate1()
    DEALLOCATE(obj%Jn_1)
    obj%Jn_1 => NULL()
  END IF
  !!
END PROCEDURE Orthopol_Deallocate1

!----------------------------------------------------------------------------
!                                                               Deallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Deallocate2
  !!
  IF( ASSOCIATED( obj%Jn_2 ) ) THEN
    !!
    CALL obj%Jn_2%Deallocate2()
    NULLIFY( obj%Jn_2 )
    !!
  END IF
  !!
END PROCEDURE Orthopol_Deallocate2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Initiate
  !!
  CALL obj%Deallocate()
  CALL obj%Initiate1( &
    & n=n, &
    & varname=varname, &
    & coeff=coeff, &
    & scale=scale )
  CALL obj%Initiate2( j1 = obj )
  CALL obj%Initiate2( j1 = obj%Jn_1 )
  !!
END PROCEDURE Orthopol_Initiate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Initiate1
  !!
  IF( n .LE. 0_I4B ) THEN
    !!
    CALL obj%SetParam(  &
      & n=n, &
      & an_1=0.0_DFP, &
      & bn_1=0.0_DFP, &
      & sn_1=1.0_DFP, &
      & sn_2=1.0_DFP, &
      & varname=varname )
    !!
    obj%Jn_1 => NULL()
    obj%Jn_2 => NULL()
    !!
  ELSE
    !!
    CALL obj%SetParam( &
      & n=n, &
      & an_1=coeff( n-1, 1), &
      & bn_1=coeff( n-1, 2), &
      & sn_1=scale( n-1, 1 ), &
      & sn_2=scale( n-1, 2 ), &
      & varname=varname )
    !!
    ALLOCATE( obj%Jn_1, SOURCE=obj )
    !!
    CALL obj%Jn_1%Initiate1( &
      & varname=varname, &
      & n=n-1, &
      & coeff = coeff, &
      & scale = scale)
    !!
  END IF
  !!
END PROCEDURE Orthopol_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Initiate2
  IF( j1%n .LE. 1_I4B ) THEN
    RETURN
  ELSE
    j1%Jn_2 => j1%Jn_1%Jn_1
    CALL obj%Initiate2( j1 = j1%Jn_2 )
  END IF
END PROCEDURE Orthopol_Initiate2

!----------------------------------------------------------------------------
!                                                              isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_isInitiated
  ans = .FALSE.
  IF( ASSOCIATED( obj%Jn_1 ) ) ans = .TRUE.
  IF( ASSOCIATED( obj%Jn_2 ) ) ans = .TRUE.
END PROCEDURE Orthopol_isInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods