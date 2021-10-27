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

SUBMODULE(UserFunction_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_getScalarValue
  IF( obj%isUserFunctionSet .AND. ASSOCIATED( obj%userFunction ) ) THEN
    CALL obj%userFunction%Get( args=args, val=val )
  ELSE
    val = obj%scalarValue
  END IF
END PROCEDURE auf_getScalarValue

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_getVectorValue
  IF( obj%isUserFunctionSet .AND. ASSOCIATED( obj%userFunction ) ) THEN
    CALL obj%userFunction%Get( args=args, val=val )
  ELSE
    val = obj%vectorValue
  END IF
END PROCEDURE auf_getVectorValue

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_getMatrixValue
  IF( obj%isUserFunctionSet .AND. ASSOCIATED( obj%userFunction ) ) THEN
    CALL obj%userFunction%Get( args=args, val=val )
  ELSE
    val = obj%matrixValue
  END IF
END PROCEDURE auf_getMatrixValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_getArgType
  ans = obj%argType
END PROCEDURE auf_getArgType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE auf_getReturnType
  ans = obj%returnType
END PROCEDURE auf_getReturnType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods