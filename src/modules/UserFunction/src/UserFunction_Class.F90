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
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class"
CHARACTER(*), PARAMETER :: myprefix = "UserFunction"
CHARACTER(*), PARAMETER :: NAME_RETURN_TYPE(3) =  &
  & [ &
  & "Scalar", &
  & "Vector", &
  & "Matrix"  &
  & ]
CHARACTER(*), PARAMETER :: NAME_ARG_TYPE(5) =  &
  & [ &
  & "Constant         ", &
  & "Space            ",  &
  & "Time             ", &
  & "SpaceTime        ",  &
  & "SolutionDependent"  &
  & ]
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_SCALAR = 1
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_VECTOR = 3
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_MATRIX = 6
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_CONSTANT = 0
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_SPACE = 3
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_TIME = 1
INTEGER(I4B), PARAMETER :: DEFAULT_NUM_ARG_SPACETIME = 4

PUBLIC :: UserFunction_
PUBLIC :: UserFunctionGetReturnType
PUBLIC :: UserFunctionGetArgType
PUBLIC :: SetUserFunctionParam
PUBLIC :: UserFunctionImportFromToml
PUBLIC :: UserFunctionImportParamFromToml
PUBLIC :: UserFunctionPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary:  User defined function

TYPE :: UserFunction_
  PRIVATE
  TYPE(String) :: name
  !! name of the function
  LOGICAL(LGT) :: isInitiated = .FALSE.
  LOGICAL(LGT) :: isUserFunctionSet = .FALSE.
  LOGICAL(LGT) :: isLuaScript = .FALSE.
  TYPE(String) :: luaScript
  TYPE(String) :: luaFunctionName
  INTEGER(I4B) :: returnType = 0
  INTEGER(I4B) :: argType = 0
  INTEGER(I4B) :: numArgs = 0
  !! Number of arguments
  !! number of args is 1 for scalar argType scalar
  INTEGER(I4B) :: numReturns = 0
  !! Number of return types
  !! number of return type is 1 for scalar return
  INTEGER(I4B) :: returnShape(2) = 0
  !! Shape of return
  !! Only used when returnType is matrix
  REAL(DFP) :: scalarValue = 0.0
  !! Scalar constant value
  REAL(DFP), ALLOCATABLE :: vectorValue(:)
  !! Vector constant value
  REAL(DFP), ALLOCATABLE :: matrixValue(:, :)
  !! Matrix constant value
  PROCEDURE(iface_ScalarFunction), POINTER, NOPASS :: scalarFunction =>  &
    & NULL()
  !! Scalar function pointer
  PROCEDURE(iface_VectorFunction), POINTER, NOPASS :: vectorFunction =>  &
    & NULL()
  !! vector function pointer
  PROCEDURE(iface_MatrixFunction), POINTER, NOPASS :: matrixFunction =>  &
    & NULL()
  !! matrix function pointer
CONTAINS

  PRIVATE
  
  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & auf_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => auf_Deallocate
  FINAL :: auf_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => auf_Initiate

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Set => auf_Set1

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetScalarValue => auf_GetScalarValue
  PROCEDURE, PUBLIC, PASS(obj) :: GetVectorValue => auf_GetVectorValue
  PROCEDURE, PUBLIC, PASS(obj) :: GetMatrixValue => auf_GetMatrixValue
  GENERIC, PUBLIC :: Get => GetScalarValue, GetVectorValue, GetMatrixValue
  PROCEDURE, PUBLIC, PASS(obj) :: GetArgType => auf_GetArgType
  PROCEDURE, PUBLIC, PASS(obj) :: GetReturnType => auf_GetReturnType
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => auf_GetName
  !! Get name of the function

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => auf_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => auf_Import
  !! Import from HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Export => auf_Export
  !! Export to HDF5File
  PROCEDURE, PASS(obj) :: ImportFromToml1 => auf_ImportFromToml1
  !! Import from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => auf_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    & ImportFromToml2
  !! Import abstract kernel from toml
  PROCEDURE, PUBLIC, PASS(obj) :: ImportParamFromToml =>  &
    & auf_ImportParamFromToml
  !! Import param from toml
END TYPE UserFunction_

!----------------------------------------------------------------------------
!                                                       UserFunctionPointer_
!----------------------------------------------------------------------------

TYPE :: UserFunctionPointer_
  CLASS(UserFunction_), POINTER :: ptr => NULL()
END TYPE UserFunctionPointer_

!----------------------------------------------------------------------------
!                                   GetReturnTypeFromName@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Oct 2021
! summary: Returns the Integer number for given return type name (String)

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetReturnType(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION UserFunctionGetReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                      etArgTypeFromName@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Oct 2021
! summary: Returns the integer for arg type

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetArgType(name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION UserFunctionGetArgType
END INTERFACE

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
  MODULE SUBROUTINE SetUserFunctionParam(param, name, returnType, argType,  &
    & numArgs, numReturns, luaScript, luaFunctionName, returnShape)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! parameter to be constructed
    CHARACTER(*), INTENT(IN) :: name
    !! name of the function
    INTEGER(I4B), INTENT(IN) :: returnType
    !! Scalar, Vector, Matrix
    INTEGER(I4B), INTENT(IN) :: argType
    !! Constant, Space, Time, SpaceTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numArgs
    !! number of argument
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numReturns
    !! number of returns
    CHARACTER(*), OPTIONAL, INTENT(IN) :: luaScript
    !! lua script
    CHARACTER(*), OPTIONAL, INTENT(IN) :: luaFunctionName
    !! lua function name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: returnShape(2)
    !! Shape of return type
    !! Only used when returnType is Matrix
  END SUBROUTINE SetUserFunctionParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in UserFunction.

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
! summary: Deallocate the data in UserFunction.

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
  MODULE SUBROUTINE auf_Initiate(obj, param)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE auf_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the scalar value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_GetScalarValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_GetScalarValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the vector value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_GetVectorValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_GetVectorValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE RECURSIVE SUBROUTINE auf_GetMatrixValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE auf_GetMatrixValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetArgType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the argument type

INTERFACE
  MODULE PURE FUNCTION auf_GetArgType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION auf_GetArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetReturnType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the return type

INTERFACE
  MODULE PURE FUNCTION auf_GetReturnType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION auf_GetReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-23
! summary:  Get name of the function

INTERFACE
  MODULE PURE FUNCTION auf_GetName(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION auf_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary:  Display the content of UserFunction

INTERFACE
  MODULE SUBROUTINE auf_Display(obj, msg, unitNo)
    CLASS(UserFunction_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE auf_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary: Import data from HDF5File

INTERFACE
  MODULE SUBROUTINE auf_Import(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE auf_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                              ImportParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param by reading the toml table

INTERFACE UserFunctionImportParamFromToml
  MODULE SUBROUTINE auf_ImportParamFromToml(obj, param, table)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(INOUT) :: param
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE auf_ImportParamFromToml
END INTERFACE UserFunctionImportParamFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE UserFunctionImportFromToml
  MODULE SUBROUTINE auf_ImportFromToml1(obj, table)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE auf_ImportFromToml1
END INTERFACE UserFunctionImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE UserFunctionImportFromToml
  MODULE SUBROUTINE auf_ImportFromToml2(obj, tomlName, afile,  &
    & filename, printToml)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE auf_ImportFromToml2
END INTERFACE UserFunctionImportFromToml

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary:  Export data to HDF5File

INTERFACE
  MODULE SUBROUTINE auf_Export(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE auf_Export
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Sets the user function

SUBROUTINE auf_Set1(obj, scalarValue, vectorValue, matrixValue,  &
  & luaScript, luaFunctionName, scalarFunction, vectorFunction,  &
  & matrixFunction)
  USE BaseMethod, ONLY: Reallocate, tostring
  CLASS(UserFunction_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: scalarValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: vectorValue(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: matrixValue(:, :)
  CHARACTER(*), OPTIONAL, INTENT(IN) :: luaScript
  CHARACTER(*), OPTIONAL, INTENT(IN) :: luaFunctionName
  PROCEDURE(iface_ScalarFunction), POINTER, OPTIONAL, INTENT(IN) ::  &
    & scalarFunction
  PROCEDURE(iface_VectorFunction), POINTER, OPTIONAL, INTENT(IN) ::  &
    & vectorFunction
  PROCEDURE(iface_MatrixFunction), POINTER, OPTIONAL, INTENT(IN) ::  &
    & matrixFunction

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "auf_Set()"
  LOGICAL(LGT) :: isNotOK
  INTEGER(I4B) :: tsize, myshape(2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] Set()')
#endif

  isNotOK = .NOT. obj%isInitiated
  IF (isNotOK) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj is not initiated.')
    RETURN
  END IF

  IF (PRESENT(scalarValue)) THEN
    isNotOK = (obj%argType .NE. Constant) .OR. (obj%returnType .NE. Scalar)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%argType is NOT Constant '//  &
      & ' or UserFunction_::obj%returnType is not Scalar')
      RETURN
    END IF
    obj%scalarValue = scalarValue
  END IF

  IF (PRESENT(vectorValue)) THEN
    isNotOK = (obj%argType .NE. Constant) .OR. (obj%returnType .NE. Vector)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%argType '//  &
      & CHAR_LF//' is NOT Constant '//  &
      & CHAR_LF//'or UserFunction_::obj%returnType is not Vector.')
      RETURN
    END IF
    tsize = SIZE(vectorValue)
    isNotOK = tsize .NE. obj%numReturns
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%numReturns '//  &
      & CHAR_LF//tostring(obj%numReturns)//'is NOT equal to '//  &
      & CHAR_LF//' the size of vectorValue ('//tostring(tsize)//').')
      RETURN
    END IF
    CALL Reallocate(obj%vectorValue, SIZE(vectorValue))
    obj%vectorValue = vectorValue
  END IF

  IF (PRESENT(matrixValue)) THEN
    isNotOK = (obj%argType .NE. Constant) .OR. (obj%returnType .NE. Matrix)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%argType '//  &
      & CHAR_LF//'is NOT Constant '//  &
      & CHAR_LF//'or UserFunction_::obj%returnType is not Matrix')
      RETURN
    END IF
    myshape = SHAPE(matrixValue)

    isNotOK = ALL(myshape .NE. obj%returnShape)
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: UserFunction_::obj%returnType is '//  &
        & 'Matrix, but shape of matrixValue is not same as obj%returnShape')
      RETURN
    END IF

    CALL Reallocate(obj%matrixValue, myshape(1), myshape(2))
    obj%matrixValue = matrixValue
  END IF

  IF (PRESENT(luaScript)) THEN
    obj%isLuaScript = .TRUE.
    obj%luaScript = luaScript
    IF (PRESENT(luaFunctionName)) THEN
      obj%luaFunctionName = luaFunctionName
    ELSE
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: both luaScript and luaFunctionName '//  &
        & 'should be present.')
      RETURN
    END IF
  END IF

  IF (PRESENT(scalarFunction)) THEN
    isNotOK = obj%returnType .NE. Scalar
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%returnType is not Scalar')
      RETURN
    END IF
    obj%isUserFunctionSet = .TRUE.
    obj%scalarFunction => scalarFunction
  END IF

  IF (PRESENT(vectorFunction)) THEN
    isNotOK = obj%returnType .NE. Vector
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%returnType is not Vector')
      RETURN
    END IF
    obj%isUserFunctionSet = .TRUE.
    obj%vectorFunction => vectorFunction
  END IF

  IF (PRESENT(matrixFunction)) THEN
    isNotOK = obj%returnType .NE. Matrix
    IF (isNotOK) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: UserFunction_::obj%returnType is not Matrix')
      RETURN
    END IF
    obj%isUserFunctionSet = .TRUE.
    obj%matrixFunction => matrixFunction
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] Set()')
#endif
END SUBROUTINE auf_Set1

END MODULE UserFunction_Class
