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
USE HDF5File_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
!!
CHARACTER(LEN=*), PARAMETER :: modName = "UserFunction_Class"
CHARACTER(LEN=*), PARAMETER :: NAME_RETURN_TYPE(3) =  &
  & [ &
  & "Scalar", &
  & "Vector", &
  & "Matrix"  &
  & ]
CHARACTER(LEN=*), PARAMETER :: NAME_ARG_TYPE(5) =  &
  & [ &
  & "Constant         ", &
  & "Space            ",  &
  & "Time             ", &
  & "SpaceTime        ",  &
  & "SolutionDependent"  &
  & ]

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: UserFunction_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  LOGICAL(LGT) :: isUserFunctionSet = .FALSE.
  INTEGER(I4B) :: returnType = 0
  INTEGER(I4B) :: argType = 0
  REAL(DFP) :: scalarValue = 0.0
  REAL(DFP), ALLOCATABLE :: vectorValue(:)
  REAL(DFP), ALLOCATABLE :: matrixValue(:, :)
  CLASS(UserFunction_), POINTER :: userFunction => NULL()
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & auf_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => auf_Deallocate
  FINAL :: auf_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => auf_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Set1 => auf_Set1
  PROCEDURE, PUBLIC, PASS(obj) :: Set2 => auf_Set2
  GENERIC, PUBLIC :: Set => Set1, Set2
  PROCEDURE, PUBLIC, PASS(obj) :: getScalarValue => auf_getScalarValue
  PROCEDURE, PUBLIC, PASS(obj) :: getVectorValue => auf_getVectorValue
  PROCEDURE, PUBLIC, PASS(obj) :: getMatrixValue => auf_getMatrixValue
  GENERIC, PUBLIC :: get => getScalarValue, getVectorValue, getMatrixValue
  PROCEDURE, PUBLIC, PASS(obj) :: getArgType => auf_getArgType
  PROCEDURE, PUBLIC, PASS(obj) :: getReturnType => auf_getReturnType
  PROCEDURE, PUBLIC, PASS(obj) :: Display => auf_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Import => auf_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => auf_Export
END TYPE UserFunction_

PUBLIC :: UserFunction_

!----------------------------------------------------------------------------
!                                   getReturnTypeFromName@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Oct 2021
! summary: Returns the Integer corresponding to name

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetReturnType(name) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION UserFunctionGetReturnType
END INTERFACE

PUBLIC :: UserFunctionGetReturnType

!----------------------------------------------------------------------------
!                                      etArgTypeFromName@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Oct 2021
! summary: Returns the integer for arg type

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetArgType(name) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION UserFunctionGetArgType
END INTERFACE

PUBLIC :: UserFunctionGetArgType

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Check the essential parameter

INTERFACE
  MODULE SUBROUTINE auf_CheckEssentialParam(obj, param)
    CLASS(UserFunction_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE auf_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                    setUserFunctionParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets user funciton parameter

INTERFACE
  MODULE SUBROUTINE setUserFunctionParam(param, returnType, argType)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    INTEGER(I4B), INTENT(IN) :: returnType
    INTEGER(I4B), INTENT(IN) :: argType
  END SUBROUTINE setUserFunctionParam
END INTERFACE

PUBLIC :: setUserFunctionParam

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in [[Userfunction_]]

INTERFACE
  MODULE SUBROUTINE auf_Deallocate(obj)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
  END SUBROUTINE auf_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in [[Userfunction_]]

INTERFACE
  MODULE SUBROUTINE auf_Final(obj)
    TYPE(UserFunction_), INTENT(INOUT) :: obj
  END SUBROUTINE auf_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Initiate the user function

INTERFACE
  MODULE SUBROUTINE auf_Initiate(obj, argType, returnType, param)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: argType
    INTEGER(I4B), INTENT(IN) :: returnType
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
  END SUBROUTINE auf_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets the user function

INTERFACE
  MODULE SUBROUTINE auf_Set1(obj, anotherObj)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), TARGET, INTENT(IN) :: anotherobj
  END SUBROUTINE auf_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets the user function

INTERFACE
  MODULE SUBROUTINE auf_Set2(obj, scalarValue, vectorValue, matrixValue)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarValue
    REAL(DFP), OPTIONAL, INTENT(IN) :: vectorValue(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: matrixValue(:, :)
  END SUBROUTINE auf_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the scalar value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_getScalarValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_getScalarValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the vector value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_getVectorValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_getVectorValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_getMatrixValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_getMatrixValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getArgType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the argument type

INTERFACE
  MODULE PURE FUNCTION auf_getArgType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION auf_getArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getReturnType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the return type

INTERFACE
  MODULE PURE FUNCTION auf_getReturnType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION auf_getReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE auf_Display(obj, msg, unitNo)
    CLASS(UserFunction_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE auf_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE auf_Import(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE auf_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE auf_Export(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE auf_Export
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UserFunction_Class
