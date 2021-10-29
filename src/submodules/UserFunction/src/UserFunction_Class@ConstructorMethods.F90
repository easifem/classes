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

SUBMODULE(UserFunction_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  UserFunctionGetReturnType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetReturnType
  SELECT CASE( TRIM(name) )
  CASE( "Scalar")
    ans = Scalar
  CASE( "Vector")
    ans = Vector
  CASE( "Matrix")
    ans = Matrix
  END SELECT
END PROCEDURE UserFunctionGetReturnType

!----------------------------------------------------------------------------
!                                                  UserFunctionGetArgType
!----------------------------------------------------------------------------

MODULE PROCEDURE UserFunctionGetArgType
  SELECT CASE( TRIM(name) )
  CASE( "Constant")
    ans = Constant
  CASE( "Space")
    ans = Space
  CASE( "SpaceTime")
    ans = SpaceTime
  CASE( "SolutionDependent")
    ans = SolutionDependent
  END SELECT
END PROCEDURE UserFunctionGetArgType

!----------------------------------------------------------------------------
!                                                      SetUserFunctionParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetUserFunctionParam
  INTEGER( I4B ) :: ierr
  !> main
  ierr = param%set( key="UserFunction/returnType", &
    & value=returnType )
  ierr = param%set( key="UserFunction/argType", &
    & value=argType )
END PROCEDURE SetUserFunctionParam

!----------------------------------------------------------------------------
!                                                        CheckEssentiaParam
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_CheckEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName="auf_CheckEssentialParam"
  INTEGER( I4B ) :: ii
  INTEGER( I4B ), PARAMETER :: maxEssentialParam = 2
  TYPE( String ) :: essentialParam( maxEssentialParam )
  !> main
  essentialParam(1) = "UserFunction/argType"
  essentialParam(2) = "UserFunction/returnType"
  DO ii = 1, maxEssentialParam
    IF( .NOT. param%isPresent(key=TRIM(essentialParam(ii)%chars()))) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & TRIM(essentialParam(ii)%chars()) // ' should be present in param')
    END IF
  END DO
END PROCEDURE auf_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_DeallocateData
  !> main
  obj%returnType = 0
  obj%argType = 0
  obj%isUserFunctionSet = .FALSE.
END PROCEDURE auf_DeallocateData

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Final
  CALL obj%DeallocateData()
END PROCEDURE auf_Final

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_Initiate
  obj%returnType = returnType
  obj%argType = argType
END PROCEDURE auf_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods