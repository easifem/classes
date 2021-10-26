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

MODULE UserFunction_Class
USE GlobalData
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE

TYPE( ExceptionHandler_ ) :: e
CHARACTER( LEN = * ), PARAMETER :: modName = "UserFunction_Class"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: UserFunction_
  INTEGER( I4B ) :: returnType=0
  INTEGER( I4B ) :: argType=0
  LOGICAL :: isUserFunctionSet = .FALSE.
  CLASS( UserFunction_ ), POINTER :: userFunction => NULL()
  CONTAINS
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => &
    & auf_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => auf_DeallocateData
  FINAL :: auf_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => auf_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Set => auf_Set1
  PROCEDURE, PUBLIC, PASS( obj ) :: getScalarValue => auf_getScalarValue
  PROCEDURE, PUBLIC, PASS( obj ) :: getVectorValue => auf_getVectorValue
  PROCEDURE, PUBLIC, PASS( obj ) :: getMatrixValue => auf_getMatrixValue
  GENERIC, PUBLIC :: get => getScalarValue, getVectorValue, getMatrixValue
  PROCEDURE, PUBLIC, PASS( obj ) :: getArgType => auf_getArgType
  PROCEDURE, PUBLIC, PASS( obj ) :: getReturnType => auf_getReturnType
END TYPE UserFunction_

PUBLIC :: UserFunction_

!----------------------------------------------------------------------------
!                                                CheckEssentialParam@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Check the essential parameter

INTERFACE
MODULE SUBROUTINE auf_CheckEssentialParam( obj, param )
  CLASS( UserFunction_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE auf_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                setUserFunctionParam@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets user funciton parameter

INTERFACE
MODULE SUBROUTINE setUserFunctionParam( param, returnType, argType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  INTEGER( I4B ), INTENT( IN ) :: returnType
  INTEGER( I4B ), INTENT( IN )  :: argType
END SUBROUTINE setUserFunctionParam
END INTERFACE

PUBLIC :: setUserFunctionParam

!----------------------------------------------------------------------------
!                                                    DeallocateData@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in [[Userfunction_]]

INTERFACE
MODULE SUBROUTINE auf_DeallocateData(obj)
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
END SUBROUTINE auf_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Final@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in [[Userfunction_]]

INTERFACE
MODULE SUBROUTINE auf_Final(obj)
  TYPE( UserFunction_ ), INTENT( INOUT ) :: obj
END SUBROUTINE auf_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Initiate the user function

INTERFACE
MODULE SUBROUTINE auf_Initiate( obj,argType, returnType, param )
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: argType
  INTEGER( I4B ), INTENT( IN ) :: returnType
  TYPE( ParameterList_ ), OPTIONAL, INTENT( IN ) :: param
END SUBROUTINE auf_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Set@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets the user function

INTERFACE
MODULE SUBROUTINE auf_Set1( obj, anotherObj )
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
  CLASS( UserFunction_ ), TARGET, INTENT( IN ) :: anotherobj
END SUBROUTINE auf_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Get@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the scalar value

INTERFACE
MODULE RECURSIVE SUBROUTINE auf_getScalarValue(obj, val, args)
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( OUT ) :: val
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: args( : )
END SUBROUTINE auf_getScalarValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Get@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the vector value

INTERFACE
MODULE RECURSIVE SUBROUTINE auf_getVectorValue(obj, val, args)
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( OUT ) :: val( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: args( : )
END SUBROUTINE auf_getVectorValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Get@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
MODULE RECURSIVE SUBROUTINE auf_getMatrixValue(obj, val, args)
  CLASS( UserFunction_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( OUT ) :: val( :, : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: args( : )
END SUBROUTINE auf_getMatrixValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getArgType@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the argument type

INTERFACE
MODULE PURE FUNCTION auf_getArgType(obj) RESULT( ans )
  CLASS( UserFunction_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION auf_getArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getReturnType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the return type

INTERFACE
MODULE PURE FUNCTION auf_getReturnType(obj) RESULT( ans )
  CLASS( UserFunction_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION auf_getReturnType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UserFunction_Class