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
USE GlobalData, ONLY: DFP, LGT, I4B
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: InterfaceScalarSubroutine
USE BaseType, ONLY: InterfaceVectorSubroutine
USE BaseType, ONLY: InterfaceMatrixSubroutine
USE String_Class, ONLY: String
USE HDF5File_Class, ONLY: HDF5File_
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
USE BaseType, ONLY: funcopt => TypeUserFunctionOpt
USE BaseType, ONLY: math => TypeMathOpt
USE EquationParser_Class, ONLY: EquationParser_
USE EquationParser_Class, ONLY: EquationParserPointer_

IMPLICIT NONE
PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "UserFunction_Class"
#endif

CHARACTER(*), PARAMETER :: NAME_RETURN_TYPE(3) = &
                           ["Scalar", "Vector", "Matrix"]

CHARACTER(*), PARAMETER :: NAME_ARG_TYPE(5) = &
                           ["Constant         ", &
                            "Space            ", &
                            "Time             ", &
                            "SpaceTime        ", &
                            "SolutionDependent"]

PUBLIC :: UserFunction_
PUBLIC :: UserFunctionGetReturnType
PUBLIC :: UserFunctionGetArgType
PUBLIC :: UserFunctionImportFromToml
PUBLIC :: UserFunctionPointer_
PUBLIC :: UserFunctionDeallocate
PUBLIC :: UserFunctionDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-20
! summary: User defined function

TYPE :: UserFunction_
  PRIVATE
  CHARACTER(funcopt%maxlen) :: name = ""
  !! name of the function
  LOGICAL(LGT) :: isInit = math%no
  !! True if the user function is initiated
  LOGICAL(LGT) :: isExternalFunc = math%no
  !! True if user function is set
  LOGICAL(LGT) :: isLuaScript = math%no
  !! True if lua script is used
  LOGICAL(LGT) :: isEquationParser = math%no
  !! are we using equation parser
  CHARACTER(funcopt%maxlen) :: luaScript = ""
  !! lua script file name
  CHARACTER(funcopt%maxlen) :: luaFunctionName = ""
  !! lua function name
  INTEGER(I4B) :: engineID = math%zero_i
  !! ID for the engine, used internally.
  INTEGER(I4B) :: returnType = math%zero_i
  !! scalar, vector, matrix
  INTEGER(I4B) :: argType = math%zero_i
  !! constant, space. time, spacetime
  INTEGER(I4B) :: numArgs = math%zero_i
  !! Number of arguments
  !! number of args is 1 for scalar argType scalar
  !! number of args is required for lua script
  INTEGER(I4B) :: numReturns = math%zero_i
  !! Number of return types
  !! number of return type is 1 for scalar return
  !! This variable is needed for lua script
  INTEGER(I4B) :: returnShape(2) = math%zero_i
  !! Shape of return
  !! Only used when returnType is matrix
  REAL(DFP) :: scalarValue = math%zero
  !! Scalar constant value
  REAL(DFP) :: vectorValue(funcopt%vectorFuncNumReturns) = math%zero
  !! Vector constant value
  REAL(DFP) :: matrixValue(funcopt%matrixFuncNumReturns, &
                           funcopt%matrixFuncNumReturns) = math%zero
  !! Matrix constant value
  TYPE(EquationParser_) :: scalarEqParser
  !! Equation parser for scalar function

  TYPE(EquationParserPointer_) :: vectorEqParser(funcopt%vectorFuncNumReturns)
  !! Equation parser for vector function

  TYPE(EquationParserPointer_) :: matrixEqParser( &
                                  funcopt%matrixFuncNumReturns, &
                                  funcopt%matrixFuncNumReturns)
  !! equation parser for matrix function

  PROCEDURE(InterfaceScalarSubroutine), POINTER, NOPASS :: scalarFunction => &
    NULL()
  !! Scalar function pointer
  PROCEDURE(InterfaceVectorSubroutine), POINTER, NOPASS :: vectorFunction => &
    NULL()
  !! vector function pointer
  PROCEDURE(InterfaceMatrixSubroutine), POINTER, NOPASS :: matrixFunction => &
    NULL()
  !! matrix function pointer

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data
  FINAL :: obj_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the user function from the arguments

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetScalarFunctionPointer => &
    obj_SetScalarFunctionPointer
  !! Set the scalar function pointer
  PROCEDURE, PUBLIC, PASS(obj) :: SetScalarConstantVal => &
    obj_SetScalarConstantVal
  !! Set the constant value for a scalar user function
  PROCEDURE, PUBLIC, PASS(obj) :: SetScalarEquationParser => &
    obj_SetScalarEquationParser
  !! Set the scalar equation parser
  PROCEDURE, PUBLIC, PASS(obj) :: SetVectorFunctionPointer => &
    obj_SetVectorFunctionPointer
  !! Set the Vector function pointer
  PROCEDURE, PUBLIC, PASS(obj) :: SetVectorConstantVal => &
    obj_SetVectorConstantVal
  !! Set the constant value for a vector user function
  PROCEDURE, PUBLIC, PASS(obj) :: SetVectorEquationParser => &
    obj_SetVectorEquationParser
  !! Set the Vector equation parser
  PROCEDURE, PUBLIC, PASS(obj) :: SetMatrixFunctionPointer => &
    obj_SetMatrixFunctionPointer
  !! Set the Matrix function pointer
  PROCEDURE, PUBLIC, PASS(obj) :: SetMatrixConstantVal => &
    obj_SetMatrixConstantVal
  !! Set the constant value for a Matrix user function
  PROCEDURE, PUBLIC, PASS(obj) :: SetMatrixEquationParser => &
    obj_SetMatrixEquationParser
  !! Set the Matrix equation parser
  PROCEDURE, PUBLIC, PASS(obj) :: SetLuaScript => obj_SetLuaScript
  !! Set the lua script in user function
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  !! Set name of the function

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetArgType => obj_GetArgType
  PROCEDURE, PUBLIC, PASS(obj) :: GetReturnType => obj_GetReturnType
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Get name of the function
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumReturns => obj_GetNumReturns
  !! Get the number of returns
  PROCEDURE, PUBLIC, PASS(obj) :: GetReturnShape => obj_GetReturnShape
  !! Get the shape of return matrix
  !! Use only when return type if matrix.
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns isInit
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumArgs => obj_GetNumArgs
  !! Get the number of Args

  ! GET:
  ! @GetScalarValueMethods
  PROCEDURE, PASS(obj) :: GetScalarValue => obj_GetScalarValue
  GENERIC, PUBLIC :: Get => GetScalarValue
  GENERIC, PUBLIC :: Get_ => GetScalarValue

  ! GET:
  ! @GetVectorValueMethods
  PROCEDURE, PASS(obj) :: GetVectorValue => obj_GetVectorValue
  PROCEDURE, PASS(obj) :: GetVectorValue_ => obj_GetVectorValue_
  GENERIC, PUBLIC :: Get => GetVectorValue
  GENERIC, PUBLIC :: Get_ => GetVectorValue_

  ! GET:
  ! @GetMatrixValueMethods
  PROCEDURE, PASS(obj) :: GetMatrixValue => obj_GetMatrixValue
  PROCEDURE, PASS(obj) :: GetMatrixValue_ => obj_GetMatrixValue_
  GENERIC, PUBLIC :: Get => GetMatrixValue
  GENERIC, PUBLIC :: Get_ => GetMatrixValue_

  ! GET:
  ! @GetFEVariableMethods
  PROCEDURE, PASS(obj) :: GetFEVariable => obj_GetFEVariable
  PROCEDURE, PASS(obj) :: GetFEVariable_ => obj_GetFEVariable_
  GENERIC, PUBLIC :: Get => GetFEVariable
  GENERIC, PUBLIC :: Get_ => GetFEVariable_

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content

  ! IO:
  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to HDF5File

  ! IO:
  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import abstract kernel from toml
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
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in UserFunction.

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE UserFunctionDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Deallocate a vector of UserFunction_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    CLASS(UserFunction_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE UserFunctionDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary:  Deallocate vector of UserFunctionPointer_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(UserFunctionPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE UserFunctionDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Deallocate the data in UserFunction.

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(UserFunction_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Initiate the user function

INTERFACE
  MODULE SUBROUTINE obj_Initiate( &
    obj, name, returnType, argType, numArgs, numReturns, returnShape)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! User function object
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: returnShape(2)
    !! Shape of return type
    !! Only used when returnType is Matrix
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Get@GetScalarValueMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the scalar value

INTERFACE
  MODULE SUBROUTINE obj_GetScalarValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE obj_GetScalarValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the vector value

INTERFACE
  MODULE SUBROUTINE obj_GetVectorValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE obj_GetVectorValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the vector value no allocation

INTERFACE
  MODULE SUBROUTINE obj_GetVectorValue_(obj, val, tsize, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! returned value
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of return values
    !! data written in val
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE obj_GetVectorValue_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE SUBROUTINE obj_GetMatrixValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE obj_GetMatrixValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE SUBROUTINE obj_GetMatrixValue_(obj, val, nrow, ncol, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), OPTIONAL, INTENT(IN) :: args(:)
  END SUBROUTINE obj_GetMatrixValue_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE SUBROUTINE obj_GetFEVariable(obj, fevar, xij, times)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE obj_GetFEVariable
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the Matrix value

INTERFACE
  MODULE SUBROUTINE obj_GetFEVariable_(obj, fevar, xij, times)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE obj_GetFEVariable_
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetArgType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the argument type

INTERFACE
  MODULE PURE FUNCTION obj_GetArgType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetReturnType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Oct 2021
! summary: Returns the return type

INTERFACE
  MODULE PURE FUNCTION obj_GetReturnType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-23
! summary:  Get name of the function

INTERFACE
  MODULE PURE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNumReturn@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetNumReturns(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNumReturns
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNumArgs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary:  Get the number of arguments

INTERFACE
  MODULE PURE FUNCTION obj_GetNumArgs(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNumArgs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetReturnShape@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetReturnShape(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetReturnShape
END INTERFACE

!----------------------------------------------------------------------------
!                                                     IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary:  Returns isInit

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary:  Display the content of UserFunction

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(UserFunction_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE UserFunctionDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine displays the content of the instance

INTERFACE
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(UserFunction_), INTENT(INOUT) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE

INTERFACE UserFunctionDisplay
  MODULE PROCEDURE obj_Display_Vector
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2021
! summary: This routine displays the content of the instance

INTERFACE
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(UserFunctionPointer_), INTENT(INOUT) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE

INTERFACE UserFunctionDisplay
  MODULE PROCEDURE obj_Display_Ptr_Vector
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary: Import data from HDF5File

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE UserFunctionImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE UserFunctionImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-20
! summary:  Export data to HDF5File

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-14
! summary:  Set name of the function

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetLuaScript@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the lua script in user function

INTERFACE
  MODULE SUBROUTINE obj_SetLuaScript(obj, luaScript, luaFunctionName)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: luaScript
    CHARACTER(*), INTENT(IN) :: luaFunctionName
  END SUBROUTINE obj_SetLuaScript
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetScalarConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a scalar user function

INTERFACE
  MODULE SUBROUTINE obj_SetScalarConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val
  END SUBROUTINE obj_SetScalarConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetScalarFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the value in user function

INTERFACE
  MODULE SUBROUTINE obj_SetScalarFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    PROCEDURE(InterfaceScalarSubroutine), POINTER, INTENT(INOUT) :: func
  END SUBROUTINE obj_SetScalarFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetScalarEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a scalar user function

INTERFACE
  MODULE SUBROUTINE obj_SetScalarEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: funcStr
    !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Array with variable names
  END SUBROUTINE obj_SetScalarEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetVectorConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a vector user function

INTERFACE
  MODULE SUBROUTINE obj_SetVectorConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
  END SUBROUTINE obj_SetVectorConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetVectorFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the value in user function

INTERFACE
  MODULE SUBROUTINE obj_SetVectorFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    PROCEDURE(InterfaceVectorSubroutine), POINTER, INTENT(IN) :: func
  END SUBROUTINE obj_SetVectorFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetVectorEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a Vector user function

INTERFACE
  MODULE SUBROUTINE obj_SetVectorEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: funcStr(:)
    !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Array with variable names
  END SUBROUTINE obj_SetVectorEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetMatrixConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a Matrix user function

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
  END SUBROUTINE obj_SetMatrixConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetMatrixFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the value in user function

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    PROCEDURE(InterfaceMatrixSubroutine), POINTER, INTENT(IN) :: func
  END SUBROUTINE obj_SetMatrixFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMatrixEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a Matrix user function

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: funcStr(:, :)
    !! Function string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Array with variable names
  END SUBROUTINE obj_SetMatrixEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UserFunction_Class
