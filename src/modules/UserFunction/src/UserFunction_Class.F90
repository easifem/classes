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

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-12
! update:
!   - 2026-10-24: Document the current interfaces and runnable examples.
! summary: Scalar, vector and matrix user-defined functions
!
! Provides [[UserFunction_]] for boundary conditions, material properties
! and other prescribed data. Configure the return rank and argument kind
! with Initiate, then select a constant, Fortran callback, Lua script or
! equation-parser backend with the corresponding setter.
!
! Get evaluates one argument tuple; Get_ uses caller-provided numerical
! storage. The rank-specific GetScalarValue_, GetVectorValue_ and
! GetMatrixValue_ generics also accept batches with one tuple per column.
! FEVariable_ evaluation supports Constant, Space, Time and SpaceTime.
! SolutionDependent is a recognized argument code but is not dispatched
! by the FEVariable_ evaluators. Lua evaluation requires USE_LUA support.
!
! See also: [[obj_Initiate]], [[obj_ImportFromToml1]]

MODULE UserFunction_Class
USE GlobalData, ONLY: DFP, LGT, I4B
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: InterfaceScalarSubroutine
USE BaseType, ONLY: InterfaceVectorSubroutine
USE BaseType, ONLY: InterfaceMatrixSubroutine
USE BaseType, ONLY: funcopt => TypeUserFunctionOpt
USE BaseType, ONLY: math => TypeMathOpt
USE String_Class, ONLY: String
USE HDF5File_Class, ONLY: HDF5File_
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE tomlf, ONLY: toml_table
USE EquationParser_Class, ONLY: EquationParser_
USE EquationParser_Class, ONLY: EquationParserPointer_
USE ISO_C_BINDING, ONLY: C_PTR
IMPLICIT NONE

PRIVATE

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
! update:
!   - 2026-10-24: Describe storage, backend selection and evaluation.
! summary: Store and evaluate a user-defined function
!
! Stores a fixed-length name, initialization status, return and argument
! codes, input and output counts, and a two-entry matrix return shape.
! Constant values use fixed-capacity scalar, vector and matrix storage;
! active vector and matrix extents are numReturns and returnShape.
!
! engineID selects constant, external callback, Lua or equation-parser
! evaluation. Callback pointers refer to caller-supplied procedures.
! scalarEqParser stores one expression; vectorEqParser and matrixEqParser
! hold pointers to component expressions. Lua uses a script path and
! function name. The backend flags record configuration; engineID, not
! those flags, controls evaluation.
!
! Initiate resets previous data and stores metadata, but does not select
! an evaluation backend. Call a matching setter before evaluating. Matrix
! functions require returnShape and a matching total numReturns. Vector
! and matrix dimensions must fit TypeUserFunctionOpt storage capacities.
! Finalization calls DEALLOCATE. See the cleanup interfaces for pointer
! ownership limitations.
!
! See also: [[obj_Initiate]], [[obj_Deallocate]]
!

TYPE :: UserFunction_
  PRIVATE
  CHARACTER(funcopt%maxlen) :: name = ""
  !! name of the function
  LOGICAL(LGT) :: isInit = math%no
  !! True if the user function is initiated
  LOGICAL(LGT) :: isExternalFunc = math%no
  !! True after selecting an external Fortran callback
  LOGICAL(LGT) :: isLuaScript = math%no
  !! True if lua script is used
  LOGICAL(LGT) :: isEquationParser = math%no
  !! Equation-parser status flag; setters select the backend via engineID
  CHARACTER(funcopt%maxlen) :: luaScript = ""
  !! lua script file name
  CHARACTER(funcopt%maxlen) :: luaFunctionName = ""
  !! lua function name
  INTEGER(I4B) :: engineID = math%zero_i
  !! ID for the engine, used internally.
  INTEGER(I4B) :: returnType = math%zero_i
  !! scalar, vector, matrix
  INTEGER(I4B) :: argType = math%zero_i
  !! Constant, Space, Time, SpaceTime or SolutionDependent code
  INTEGER(I4B) :: numArgs = math%zero_i
  !! Number of real inputs in each argument tuple
  INTEGER(I4B) :: numReturns = math%zero_i
  !! Number of returned real components; one for a scalar
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
  TYPE(EquationParserPointer_) :: &
    vectorEqParser(funcopt%vectorFuncNumReturns)
  !! Equation parser for vector function
  TYPE(EquationParserPointer_) :: &
    matrixEqParser(funcopt%matrixFuncNumReturns, funcopt%matrixFuncNumReturns)
  !! equation parser for matrix function
  TYPE(C_PTR) :: luaState
  !! lua state, it is used when engineID is lua
  PROCEDURE(InterfaceScalarSubroutine), POINTER, NOPASS :: &
    scalarFunction => NULL()
  !! Scalar function pointer
  PROCEDURE(InterfaceVectorSubroutine), POINTER, NOPASS :: &
    vectorFunction => NULL()
  !! vector function pointer
  PROCEDURE(InterfaceMatrixSubroutine), POINTER, NOPASS :: &
    matrixFunction => NULL()
  !! matrix function pointer

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data
  FINAL :: obj_Final
  !! Invoke object cleanup on finalization
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the user function from the arguments

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

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetArgType => obj_GetArgType
  !! Get the argument-kind code
  PROCEDURE, PUBLIC, PASS(obj) :: GetReturnType => obj_GetReturnType
  !! Get the scalar, vector or matrix return code
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Get name of the function
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumReturns => obj_GetNumReturns
  !! Get the number of returns
  PROCEDURE, PUBLIC, PASS(obj) :: GetReturnShape => obj_GetReturnShape
  !! Get the stored matrix shape (rows, columns)
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns isInit
  PROCEDURE, PUBLIC, PASS(obj) :: GetNumArgs => obj_GetNumArgs
  !! Get the number of Args

  ! @GetScalarValueMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetScalarValue1_ => obj_GetScalarValue_1
  !! Evaluate one scalar value
  PROCEDURE, PRIVATE, PASS(obj) :: GetScalarValue2_ => obj_GetScalarValue_2
  !! Evaluate scalar values for argument columns
  GENERIC, PUBLIC :: GetScalarValue_ => GetScalarValue1_, GetScalarValue2_
  !! Evaluate one scalar or a batch in caller storage
  GENERIC, PUBLIC :: Get => GetScalarValue1_
  !! Evaluate a scalar through the general Get interface
  GENERIC, PUBLIC :: Get_ => GetScalarValue1_
  !! Evaluate a scalar in caller storage

  ! @GetVectorValueMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetVectorValue => obj_GetVectorValue
  !! Allocate or resize storage and evaluate a vector
  GENERIC, PUBLIC :: Get => GetVectorValue
  !! Evaluate a vector with allocatable output
  PROCEDURE, PRIVATE, PASS(obj) :: GetVectorValue1_ => obj_GetVectorValue_1
  !! Evaluate one vector in caller storage
  PROCEDURE, PRIVATE, PASS(obj) :: GetVectorValue2_ => obj_GetVectorValue_2
  !! Evaluate one vector per argument column
  GENERIC, PUBLIC :: GetVectorValue_ => GetVectorValue1_, GetVectorValue2_
  !! Evaluate one vector or a batch in caller storage
  GENERIC, PUBLIC :: Get_ => GetVectorValue1_
  !! Evaluate a vector without resizing its output array

  ! @GetMatrixValueMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetMatrixValue => obj_GetMatrixValue
  !! Allocate or resize storage and evaluate a matrix
  GENERIC, PUBLIC :: Get => GetMatrixValue
  !! Evaluate a matrix with allocatable output
  PROCEDURE, PRIVATE, PASS(obj) :: GetMatrixValue1_ => obj_GetMatrixValue_1
  !! Evaluate one matrix in caller storage
  PROCEDURE, PRIVATE, PASS(obj) :: GetMatrixValue2_ => obj_GetMatrixValue_2
  !! Evaluate one matrix per argument column
  GENERIC, PUBLIC :: GetMatrixValue_ => GetMatrixValue1_, GetMatrixValue2_
  !! Evaluate one matrix or a batch in caller storage
  GENERIC, PUBLIC :: Get_ => GetMatrixValue1_
  !! Evaluate a matrix without resizing its output array

  ! @GetFEVariableMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetFEVariable => obj_GetFEVariable
  !! Construct a nodal FEVariable_ from evaluated values
  PROCEDURE, PRIVATE, PASS(obj) :: GetFEVariable_ => obj_GetFEVariable_
  !! Set an FEVariable_ from evaluated values
  GENERIC, PUBLIC :: Get => GetFEVariable
  !! Evaluate into a newly constructed FEVariable_
  GENERIC, PUBLIC :: Get_ => GetFEVariable_
  !! Evaluate through FEVariable_ Set without adding contributions

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content

  ! @HDFMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from HDF5File
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to HDF5File

  ! @TomlMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import from toml
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import from toml
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import user-function configuration from a table or file
END TYPE UserFunction_

!----------------------------------------------------------------------------
!                                                       UserFunctionPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Hold a polymorphic pointer to a user function
!
! ptr is initially disassociated. Arrays of this wrapper can refer to
! independently stored UserFunction_ targets without copying them.
! UserFunctionDeallocate on an allocatable wrapper array cleans associated
! targets and nullifies the pointers, but does not deallocate the targets.
! The caller remains responsible for target lifetime and storage.
!
! See also: [[UserFunction_]], [[obj_Deallocate_Ptr_Vector]]
!
TYPE :: UserFunctionPointer_
  CLASS(UserFunction_), POINTER :: ptr => NULL()
  !! Referenced user function; target storage is managed by the caller
END TYPE UserFunctionPointer_

!----------------------------------------------------------------------------
!                               UserFunctionGetReturnType@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Convert a return-type name to its integer code
!
! Inspects the first character, ignoring case: S, V and M select Scalar,
! Vector and Matrix from TypeFEVariableOpt. Other initials return -1.
! The input must contain at least one character; full names are not
! validated.
!
! Arguments:
!   name : CHARACTER(*) - nonempty return-type name
!
! Returns:
!   ans : INTEGER(I4B) - return-type code, or -1 for an unknown initial
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunctionGetReturnType
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! CALL OK(UserFunctionGetReturnType("Scalar") == opt%Scalar, "Scalar")
! CALL OK(UserFunctionGetReturnType("unknown") == -1, "Unknown")
! END PROGRAM main
!```
!
! See also: [[UserFunctionGetArgType]], [[obj_GetReturnType]]

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetReturnType(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! Nonempty return-type name; only its first character is inspected
    INTEGER(I4B) :: ans
    !! TypeFEVariableOpt return code, or -1 for an unrecognized initial
  END FUNCTION UserFunctionGetReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                  UserFunctionGetArgType@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Convert an argument-kind name to its integer code
!
! Reads the first two characters without case sensitivity. CO selects
! Constant, TI selects Time, and OT or SO selects SolutionDependent.
! SP selects Space when LEN_TRIM(name) <= 5, otherwise SpaceTime.
! Other prefixes return -1; full names are not validated. Supply at
! least two characters.
!
! Arguments:
!   name : CHARACTER(*) - argument-kind name with LEN(name) >= 2
!
! Returns:
!   ans : INTEGER(I4B) - TypeFEVariableOpt argument code, or -1
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunctionGetArgType
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! CALL OK(UserFunctionGetArgType("Space") == opt%Space, "Space")
! CALL OK(UserFunctionGetArgType("SpaceTime") == opt%SpaceTime, &
!   "SpaceTime")
! CALL OK(UserFunctionGetArgType("unknown") == -1, "Unknown")
! END PROGRAM main
!```
!
! See also: [[UserFunctionGetReturnType]], [[obj_GetArgType]]

INTERFACE
  MODULE PURE FUNCTION UserFunctionGetArgType(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! Argument-kind name with at least two characters
    INTEGER(I4B) :: ans
    !! TypeFEVariableOpt argument code, or -1 for an unknown prefix
  END FUNCTION UserFunctionGetArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Clear user-function data and callback associations
!
! Clears initialization and external/Lua flags, names, counts, shapes
! and constant values. Nullifies callback pointers and calls DEALLOCATE
! on the scalar parser and each associated component parser. Component
! parser targets are not deallocated or nullified here. engineID and
! isEquationParser are not reset. Select a backend again before reuse.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to clear
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE UserFunction_Class, ONLY: UserFunctionDeallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL UserFunctionDeallocate(obj)
! CALL OK(.NOT. obj%IsInitiated(), "Object cleared")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_Deallocate_Vector]]

INTERFACE UserFunctionDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! User function whose metadata, values and parser data are cleared
  END SUBROUTINE obj_Deallocate
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Clean and deallocate an allocatable user-function array
!
! Returns immediately if obj is unallocated. Otherwise calls DEALLOCATE
! on each element and releases the array storage.
!
! Arguments:
!   obj : CLASS(UserFunction_)(:) - allocatable array of SIZE(obj) objects
!
! Examples
!
!```fortran
! PROGRAM main
! USE UserFunction_Class, ONLY: UserFunction_
! USE UserFunction_Class, ONLY: UserFunctionDeallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! CLASS(UserFunction_), ALLOCATABLE :: obj(:)
! ALLOCATE(obj(2))
! CALL UserFunctionDeallocate(obj)
! CALL OK(.NOT. ALLOCATED(obj), "Array released")
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_Deallocate_Ptr_Vector]]

INTERFACE UserFunctionDeallocate
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    CLASS(UserFunction_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of UserFunction to be deallocated.
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-27
! summary: Clean pointed-to functions and release a pointer-wrapper array
!
! For an allocated array, calls each associated target's DEALLOCATE and
! nullifies that element's ptr, then deallocates the wrapper array. Does
! not deallocate target storage. An unallocated obj is left unchanged.
!
! Arguments:
!   obj : TYPE(UserFunctionPointer_)(:) - allocatable wrapper array
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE UserFunction_Class, ONLY: UserFunctionPointer_
! USE UserFunction_Class, ONLY: UserFunctionDeallocate
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_), TARGET :: target
! TYPE(UserFunctionPointer_), ALLOCATABLE :: obj(:)
! CALL target%Initiate("f", opt%Scalar, opt%Constant)
! ALLOCATE(obj(1))
! obj(1)%ptr => target
! CALL UserFunctionDeallocate(obj)
! CALL OK(.NOT. ALLOCATED(obj), "Wrappers released")
! CALL OK(.NOT. target%IsInitiated(), "Target cleared, not destroyed")
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[UserFunctionPointer_]]

INTERFACE UserFunctionDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(UserFunctionPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Wrapper array; associated targets are cleaned, not deallocated
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE UserFunctionDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Finalize a user-function object
!
! Calls obj%DEALLOCATE when Fortran finalizes the object. This is an
! automatic cleanup hook, not a public type-bound call.
!
! Arguments:
!   obj : TYPE(UserFunction_) - object being finalized
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(UserFunction_), INTENT(INOUT) :: obj
    !! Object being finalized
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Reset a user function and configure its evaluation metadata
!
! Calls DEALLOCATE, marks the object initiated, and stores the name,
! returnType and argType. Omitted counts use TypeUserFunctionOpt defaults
! for the chosen codes. Defaults are not inferred from returnShape.
! Set a constant, callback, expression or Lua backend before evaluation.
!
! A scalar requires numReturns=1. For a matrix, supply returnShape and
! numReturns equal to its product. Keep vector lengths and each matrix
! extent within the corresponding fixed storage capacity. For
! SolutionDependent supply numArgs explicitly; there is no default count.
! Most consistency checks are enabled only in DEBUG_VER builds.
!
! Arguments:
!   obj : CLASS(UserFunction_) - object to reset and initialize
!   name : CHARACTER(*) - function name, stored in fixed-length storage
!   returnType : INTEGER(I4B) - Scalar, Vector or Matrix option code
!   argType : INTEGER(I4B) - argument-kind option code
!   numArgs : INTEGER(I4B), optional - real inputs per evaluation
!   numReturns : INTEGER(I4B), optional - returned real components
!   returnShape : INTEGER(I4B)(2), optional - matrix rows and columns
!
! Formula:
!   For a matrix with shape (r,c),
!   $$ n_{\mathrm{returns}} = r c. $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: scalar, vector, matrix
! REAL(DFP) :: value
! CALL scalar%Initiate("s", opt%Scalar, opt%Constant)
! CALL scalar%SetScalarConstantVal(2.0_DFP)
! CALL scalar%Get(value)
! CALL OK(value == 2.0_DFP, "Scalar constant")
! CALL vector%Initiate("v", opt%Vector, opt%Space, &
!   numArgs=1, numReturns=2)
! CALL OK(vector%GetNumReturns() == 2, "Vector size")
! CALL matrix%Initiate("m", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL OK(ALL(matrix%GetReturnShape() == [2, 2]), "Matrix shape")
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_SetScalarConstantVal]],
! [[obj_SetScalarFunctionPointer]], [[obj_SetScalarEquationParser]],
! [[obj_SetLuaScript]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, name, returnType, argType, numArgs, &
                                 numReturns, returnShape)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! User function object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the function
    INTEGER(I4B), INTENT(IN) :: returnType
    !! TypeFEVariableOpt Scalar, Vector or Matrix return code
    INTEGER(I4B), INTENT(IN) :: argType
    !! Constant, Space, Time, SpaceTime or SolutionDependent code
    INTEGER(I4B), INTENT(IN), OPTIONAL :: numArgs
    !! Real inputs per tuple; defaults according to argType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: numReturns
    !! Returned components; defaults according to returnType
    INTEGER(I4B), INTENT(IN), OPTIONAL :: returnShape(2)
    !! Matrix rows and columns; required for Matrix returnType
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Get@GetScalarValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Evaluate one scalar function value
!
! Dispatches to the configured constant, external callback, Lua or
! equation-parser backend. Supply args in callback or parser variable
! order; nonconstant backends require the configured inputs. This
! specific is available through Get, Get_ and GetScalarValue_.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured scalar function
!   val : REAL(DFP) - evaluated scalar, overwritten on success
!   args : REAL(DFP)(:), optional - tuple with SIZE(args) >= numArgs
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL obj%SetScalarConstantVal(2.0_DFP)
! CALL obj%Get(val=val)
! CALL OK(val == 2.0_DFP, "Scalar evaluation")
! END PROGRAM main
!```
!
! See also: [[obj_GetScalarValue_2]], [[obj_SetScalarEquationParser]]
INTERFACE
  MODULE SUBROUTINE obj_GetScalarValue_1(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured scalar function
    REAL(DFP), INTENT(INOUT) :: val
    !! Evaluated scalar value
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
    !! Input tuple in backend variable order; at least numArgs entries
  END SUBROUTINE obj_GetScalarValue_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Get@GetScalarValueMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Evaluate a scalar function for a batch of argument tuples
!
! Uses one input tuple per column of args and writes val(1:tsize),
! where tsize=SIZE(args,2). The caller supplies sufficient output storage
! and at least numArgs input rows. Non-Lua backends loop over the
! single-value evaluator. Lua batches reuse one state and loaded function
! for the batch; Lua support is required for that backend.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured scalar function
!   val : REAL(DFP)(:) - output buffer, SIZE(val) >= SIZE(args,2)
!   tsize : INTEGER(I4B) - number of values written
!   args : REAL(DFP)(:,:) - input tuples in columns
!
! Formula:
!   $$ v_j = f(\mathrm{args}(:,j)). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2), args(1, 2)
! INTEGER(I4B) :: tsize
! CALL obj%Initiate("f", opt%Scalar, opt%Space, numArgs=1)
! CALL obj%SetScalarEquationParser("2*x", ["x"])
! args(1, :) = [1.0_DFP, 2.0_DFP]
! CALL obj%GetScalarValue_(val, tsize, args)
! CALL OK(tsize == 2, "Batch size")
! CALL OK(ALL(val == [2.0_DFP, 4.0_DFP]), "Scalar batch")
! END PROGRAM main
!```
!
! See also: [[obj_GetScalarValue_1]]
INTERFACE
  MODULE SUBROUTINE obj_GetScalarValue_2(obj, val, tsize, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured scalar function
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! Output buffer with at least SIZE(args,2) entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of values written; SIZE(args,2)
    REAL(DFP), INTENT(IN) :: args(:, :)
    !! Input tuples in columns, with at least numArgs rows
  END SUBROUTINE obj_GetScalarValue_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate a vector with allocatable output storage
!
! Allocates or resizes val to numReturns and delegates evaluation to
! [[obj_GetVectorValue_1]]. Available through Get or GetVectorValue.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured vector function
!   val : REAL(DFP)(:), allocatable - resized output vector
!   args : REAL(DFP)(:), optional - at least numArgs ordered inputs
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP), ALLOCATABLE :: val(:)
! CALL obj%Initiate("f", opt%Vector, opt%Constant, numReturns=2)
! CALL obj%SetVectorConstantVal([1.0_DFP, 2.0_DFP])
! CALL obj%Get(val=val)
! CALL OK(SIZE(val) == 2, "Allocated vector size")
! CALL OK(ALL(val == [1.0_DFP, 2.0_DFP]), "Vector value")
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue_1]], [[obj_GetVectorValue_2]]

INTERFACE
  MODULE SUBROUTINE obj_GetVectorValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured vector function
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:)
    !! Allocated or resized to numReturns before evaluation
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
    !! Ordered input tuple with at least numArgs entries
  END SUBROUTINE obj_GetVectorValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate one vector in caller-provided storage
!
! Dispatches to the selected backend without resizing val. The configured
! component count must fit SIZE(val). tsize is initially numReturns and
! may be set by an external callback. args follows callback or parser
! variable order; omit it only when the backend requires no inputs.
! Available through Get_ and GetVectorValue_.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured vector function
!   val : REAL(DFP)(:) - output buffer, SIZE(val) >= numReturns
!   tsize : INTEGER(I4B) - number of valid output components
!   args : REAL(DFP)(:), optional - at least numArgs ordered inputs
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2)
! INTEGER(I4B) :: tsize
! CALL obj%Initiate("f", opt%Vector, opt%Constant, numReturns=2)
! CALL obj%SetVectorConstantVal([1.0_DFP, 2.0_DFP])
! CALL obj%Get_(val=val, tsize=tsize)
! CALL OK(tsize == 2, "Vector size")
! CALL OK(ALL(val == [1.0_DFP, 2.0_DFP]), "Vector value")
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue]], [[obj_GetVectorValue_2]]
INTERFACE
  MODULE SUBROUTINE obj_GetVectorValue_1(obj, val, tsize, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured vector function
    REAL(DFP), INTENT(INOUT) :: val(:)
    !! Output buffer with at least numReturns entries
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Number of valid components written to val
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
    !! Ordered input tuple with at least numArgs entries
  END SUBROUTINE obj_GetVectorValue_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate one vector per argument column
!
! Calls the single-vector evaluator for each column of args, storing
! results in the corresponding column of val. No output allocation is
! performed. ncol=SIZE(args,2); nrow is the last evaluation's component
! count and is undefined for an empty batch. Use a nonempty batch when
! reading nrow. Call through GetVectorValue_, not the general Get_.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured vector function
!   val : REAL(DFP)(:,:) - buffer of at least (numReturns,SIZE(args,2))
!   nrow : INTEGER(I4B) - component count from the last evaluation
!   ncol : INTEGER(I4B) - number of evaluated argument columns
!   args : REAL(DFP)(:,:) - tuples, SIZE(args,1) >= numArgs
!
! Formula:
!   $$ \mathrm{val}(:,j) = f(\mathrm{args}(:,j)). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2, 3), args(0, 3)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Initiate("f", opt%Vector, opt%Constant, numReturns=2)
! CALL obj%SetVectorConstantVal([1.0_DFP, 2.0_DFP])
! CALL obj%GetVectorValue_(val, nrow, ncol, args)
! CALL OK(nrow == 2 .AND. ncol == 3, "Batch dimensions")
! CALL OK(ALL(val(1, :) == 1.0_DFP), "First component")
! CALL OK(ALL(val(2, :) == 2.0_DFP), "Second component")
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue_1]]
INTERFACE
  MODULE SUBROUTINE obj_GetVectorValue_2(obj, val, nrow, ncol, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured vector function
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! Output columns; capacity at least (numReturns,SIZE(args,2))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: last component count; ncol: SIZE(args,2)
    REAL(DFP), INTENT(IN) :: args(:, :)
    !! Input tuples in columns, with at least numArgs rows
  END SUBROUTINE obj_GetVectorValue_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate a matrix with allocatable output storage
!
! Allocates or resizes val to returnShape, then delegates to
! [[obj_GetMatrixValue_1]]. Available through Get or GetMatrixValue.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured matrix function
!   val : REAL(DFP)(:,:), allocatable - output resized to returnShape
!   args : REAL(DFP)(:), optional - at least numArgs ordered inputs
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP), ALLOCATABLE :: val(:, :)
! CALL obj%Initiate("f", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL obj%SetMatrixConstantVal(RESHAPE([1.0_DFP, 2.0_DFP, &
!   3.0_DFP, 4.0_DFP], [2, 2]))
! CALL obj%Get(val=val)
! CALL OK(ALL(SHAPE(val) == [2, 2]), "Allocated shape")
! CALL OK(val(2, 1) == 2.0_DFP, "Column-major values")
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue_1]], [[obj_GetMatrixValue_2]]

INTERFACE
  MODULE SUBROUTINE obj_GetMatrixValue(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured matrix function
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val(:, :)
    !! Allocated or resized to returnShape before evaluation
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
    !! Ordered input tuple with at least numArgs entries
  END SUBROUTINE obj_GetMatrixValue
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Evaluate one matrix in caller-provided storage
!
! Dispatches to the selected backend without resizing val. Each output
! extent must accommodate returnShape. Constant and parser backends
! report returnShape; callbacks supply nrow and ncol. Lua returns a flat
! sequence that is reshaped in Fortran column-major order. Lua support
! is required for Lua evaluation. Available through Get_ and GetMatrixValue_.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured matrix function
!   val : REAL(DFP)(:,:) - matrix buffer with sufficient row/column capacity
!   nrow : INTEGER(I4B) - number of valid output rows
!   ncol : INTEGER(I4B) - number of valid output columns
!   args : REAL(DFP)(:), optional - at least numArgs ordered inputs
!
! Formula:
!   Lua component k maps to row i and column j for r output rows:
!   $$ k = i + (j-1)r. $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2, 2), want(2, 2)
! INTEGER(I4B) :: nrow, ncol
! want = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
! CALL obj%Initiate("f", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL obj%SetMatrixConstantVal(want)
! CALL obj%Get_(val=val, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 2 .AND. ncol == 2, "Matrix dimensions")
! CALL OK(ALL(val == want), "Matrix value")
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue]], [[obj_GetMatrixValue_2]]
INTERFACE
  MODULE SUBROUTINE obj_GetMatrixValue_1(obj, val, nrow, ncol, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured matrix function
    REAL(DFP), INTENT(INOUT) :: val(:, :)
    !! Matrix buffer; extents must accommodate returnShape
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! Valid row and column counts written by the backend
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
    !! Ordered input tuple with at least numArgs entries
  END SUBROUTINE obj_GetMatrixValue_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Evaluate a matrix for each argument column
!
! Loops over args columns, writing a matrix to each val(:,:,j) plane.
! No output allocation is performed. dim3=SIZE(args,2); dim1 and dim2
! are the final evaluation's row and column counts. The latter two are
! undefined for an empty batch. Call through GetMatrixValue_.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured matrix function
!   val : REAL(DFP)(:,:,:) - capacity (rows,columns,SIZE(args,2)) or larger
!   dim1 : INTEGER(I4B) - last evaluation's row count
!   dim2 : INTEGER(I4B) - last evaluation's column count
!   dim3 : INTEGER(I4B) - number of argument tuples
!   args : REAL(DFP)(:,:) - tuples, SIZE(args,1) >= numArgs
!
! Formula:
!   $$ \mathrm{val}(:,:,j) = f(\mathrm{args}(:,j)). $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2, 2, 3), args(0, 3), want(2, 2)
! INTEGER(I4B) :: dim1, dim2, dim3
! want = 2.0_DFP
! CALL obj%Initiate("f", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL obj%SetMatrixConstantVal(want)
! CALL obj%GetMatrixValue_(val, dim1, dim2, dim3, args)
! CALL OK(ALL([dim1, dim2, dim3] == [2, 2, 3]), "Batch dimensions")
! CALL OK(ALL(val == 2.0_DFP), "Matrix batch")
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue_1]]
INTERFACE
  MODULE SUBROUTINE obj_GetMatrixValue_2(obj, val, dim1, dim2, dim3, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured matrix function
    REAL(DFP), INTENT(INOUT) :: val(:, :, :)
    !! Output matrices in successive third-dimension planes
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! Last matrix rows, last matrix columns, and SIZE(args,2)
    REAL(DFP), INTENT(IN) :: args(:, :)
    !! Input tuples in columns, with at least numArgs rows
  END SUBROUTINE obj_GetMatrixValue_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate a function and construct a nodal FEVariable_
!
! Dispatches by argument kind and return rank, evaluates values and
! assigns a NodalVariable to fevar. Constant needs no sampling arrays;
! Space needs xij; Time needs times; SpaceTime needs both and evaluates
! all point/time combinations. xij columns are spatial points. Scalar
! SpaceTime results have point then time axes; vector/matrix component
! axes precede these sampling axes. SolutionDependent is not supported.
!
! For scalar/vector SpaceTime, coordinates precede time in the argument
! tuple. The current matrix SpaceTime branch copies numArgs coordinate
! rows over that tuple; do not rely on it to preserve a trailing time
! argument. Use the rank-specific batch evaluator for explicit tuples.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured function
!   fevar : TYPE(FEVariable_) - replaced by the constructed nodal variable
!   xij : REAL(DFP)(:,:), optional - SIZE(xij,2) spatial points
!   times : REAL(DFP)(:), optional - SIZE(times) sampling times
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE BaseType, ONLY: FEVariable_
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! TYPE(FEVariable_) :: fevar
! REAL(DFP) :: xij(1, 2)
! CALL obj%Initiate("f", opt%Scalar, opt%Space, numArgs=1)
! CALL obj%SetScalarEquationParser("2*x", ["x"])
! xij(1, :) = [1.0_DFP, 2.0_DFP]
! CALL obj%Get(fevar=fevar, xij=xij)
! CALL OK(ALL(fevar%val(1:2) == [2.0_DFP, 4.0_DFP]), "Nodal values")
! END PROGRAM main
!```
!
! See also: [[obj_GetFEVariable_]], [[obj_GetScalarValue_2]]

INTERFACE
  MODULE SUBROUTINE obj_GetFEVariable(obj, fevar, xij, times)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured function with a supported argument kind and return rank
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    !! Output nodal variable, constructed from evaluated values
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Spatial coordinates in columns; required for Space and SpaceTime
    REAL(DFP), INTENT(IN), OPTIONAL :: times(:)
    !! Sampling times; required for Time and SpaceTime
  END SUBROUTINE obj_GetFEVariable
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Evaluate values and set an FEVariable_ without accumulation
!
! Uses the same rank/argument-kind dispatch as [[obj_GetFEVariable]],
! but passes evaluated values to FEVariable_Method Set with scale=1
! and addContribution=false instead of assigning a new NodalVariable.
! Supply a compatible destination. Internal temporary arrays may still
! be allocated; this is not an allocation-free evaluation guarantee.
!
! Space and Time require their respective sampling arrays; SpaceTime
! requires both and samples all combinations. Scalar SpaceTime uses
! a fixed four-entry argument buffer. Matrix SpaceTime has the coordinate
! overwrite limitation described in [[obj_GetFEVariable]].
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured function
!   fevar : TYPE(FEVariable_) - destination for overwritten values
!   xij : REAL(DFP)(:,:), optional - coordinates at SIZE(xij,2) points
!   times : REAL(DFP)(:), optional - SIZE(times) sampling times
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE BaseType, ONLY: FEVariable_
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! TYPE(FEVariable_) :: fevar
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL obj%SetScalarConstantVal(2.0_DFP)
! CALL obj%Get(fevar=fevar)
! CALL obj%SetScalarConstantVal(3.0_DFP)
! CALL obj%Get_(fevar=fevar)
! CALL OK(fevar%val(1) == 3.0_DFP, "Overwrite, not accumulation")
! END PROGRAM main
!```
!
! See also: [[obj_GetFEVariable]]

INTERFACE
  MODULE SUBROUTINE obj_GetFEVariable_(obj, fevar, xij, times)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Configured function with a supported argument kind and return rank
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    !! Compatible destination, set with no added contribution
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Spatial coordinates in columns; required for Space and SpaceTime
    REAL(DFP), INTENT(IN), OPTIONAL :: times(:)
    !! Sampling times; required for Time and SpaceTime
  END SUBROUTINE obj_GetFEVariable_
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetArgType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Get the stored argument-kind code
!
! Returns argType without validation or evaluation.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : INTEGER(I4B) - stored TypeFEVariableOpt argument-kind code
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Scalar, opt%Time)
! CALL OK(obj%GetArgType() == opt%Time, "Time argument kind")
! END PROGRAM main
!```
!
! See also: [[UserFunctionGetArgType]], [[obj_Initiate]]

INTERFACE
  MODULE PURE FUNCTION obj_GetArgType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    INTEGER(I4B) :: ans
    !! Stored argument-kind code
  END FUNCTION obj_GetArgType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetReturnType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-26
! summary: Get the stored return-type code
!
! Returns returnType without validation or evaluation.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : INTEGER(I4B) - stored Scalar, Vector or Matrix option code
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL OK(obj%GetReturnType() == opt%Scalar, "Scalar return type")
! END PROGRAM main
!```
!
! See also: [[UserFunctionGetReturnType]], [[obj_GetNumReturns]]

INTERFACE
  MODULE PURE FUNCTION obj_GetReturnType(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    INTEGER(I4B) :: ans
    !! Stored return-type code
  END FUNCTION obj_GetReturnType
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-23
! summary: Get the function name without trailing blanks
!
! Copies TRIM(obj%name) into a deferred-length character result.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : CHARACTER(:), allocatable - trimmed function name
!
! Examples
!
!```fortran
! PROGRAM main
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%SetName("load")
! CALL OK(obj%GetName() == "load", "Trimmed name")
! END PROGRAM main
!```
!
! See also: [[obj_SetName]], [[obj_Initiate]]

INTERFACE
  MODULE PURE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    CHARACTER(:), ALLOCATABLE :: ans
    !! Function name with trailing blanks removed
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNumReturns@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Get the configured number of returned real components
!
! Returns numReturns, not the return rank or a freshly evaluated size.
! A scalar normally returns one component; a matrix uses the product
! of its two returnShape entries.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : INTEGER(I4B) - stored component count
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Vector, opt%Constant, numReturns=2)
! CALL OK(obj%GetNumReturns() == 2, "Two components")
! END PROGRAM main
!```
!
! See also: [[obj_GetReturnType]], [[obj_GetReturnShape]]

INTERFACE
  MODULE PURE FUNCTION obj_GetNumReturns(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    INTEGER(I4B) :: ans
    !! Stored number of returned real components
  END FUNCTION obj_GetNumReturns
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNumArgs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary: Get the configured number of real inputs per evaluation
!
! Returns numArgs without inspecting a backend or evaluating the function.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : INTEGER(I4B) - stored input count per argument tuple
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Scalar, opt%Space, numArgs=2)
! CALL OK(obj%GetNumArgs() == 2, "Two coordinates")
! END PROGRAM main
!```
!
! See also: [[obj_GetArgType]], [[obj_Initiate]]

INTERFACE
  MODULE PURE FUNCTION obj_GetNumArgs(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    INTEGER(I4B) :: ans
    !! Stored number of real inputs per evaluation
  END FUNCTION obj_GetNumArgs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetReturnShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-13
! summary: Get the stored matrix return shape
!
! Copies returnShape in row/column order. The shape is meaningful for
! matrix functions; it is not inferred for scalar or vector functions.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : INTEGER(I4B)(2) - stored matrix row and column counts
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%Initiate("f", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL OK(ALL(obj%GetReturnShape() == [2, 2]), "Rows and columns")
! END PROGRAM main
!```
!
! See also: [[obj_GetNumReturns]], [[obj_Initiate]]

INTERFACE
  MODULE PURE FUNCTION obj_GetReturnShape(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    INTEGER(I4B) :: ans(2)
    !! Matrix row and column counts, copied from returnShape
  END FUNCTION obj_GetReturnShape
END INTERFACE

!----------------------------------------------------------------------------
!                                                     IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary: Query the initialization flag
!
! Returns isInit only. A true result does not guarantee that a backend
! has been configured or that all evaluation metadata is valid.
!
! Arguments:
!   obj : CLASS(UserFunction_) - user function to query
!
! Returns:
!   ans : LOGICAL(LGT) - stored initialization flag
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL OK(.NOT. obj%IsInitiated(), "Initially uninitialized")
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL OK(obj%IsInitiated(), "Initialized metadata")
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Cleared")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! User function to query
    LOGICAL(LGT) :: ans
    !! Stored initialization flag; not a backend-readiness check
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-20
! summary: Display function metadata and backend configuration
!
! Writes msg and the initialization flag. For an initiated object also
! writes name, counts, return/argument kinds and backend-specific data.
! An omitted unitNo uses the Display_Method default output unit.
!
! Arguments:
!   obj : CLASS(UserFunction_) - object to display
!   msg : CHARACTER(*) - heading printed before the object
!   unitNo : INTEGER(I4B), optional - output unit
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! INTEGER(I4B) :: unitNo, ios
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL obj%SetScalarConstantVal(2.0_DFP)
! OPEN(NEWUNIT=unitNo, STATUS="SCRATCH", FORM="FORMATTED")
! CALL obj%Display("Function:", unitNo=unitNo)
! REWIND(unitNo)
! READ(unitNo, *, IOSTAT=ios)
! CALL OK(ios == 0, "Display produced output")
! CLOSE(unitNo)
! END PROGRAM main
!```
!
! See also: [[obj_Display_Vector]], [[obj_Display_Ptr_Vector]]

INTERFACE UserFunctionDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! Object whose metadata and backend configuration are displayed
    CHARACTER(*), INTENT(IN) :: msg
    !! Heading printed before the object data
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! Output unit; omitted to use the Display_Method default
  END SUBROUTINE obj_Display
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-27
! summary: Display every element of a user-function array
!
! Applies the scalar Display method to each element with an indexed
! heading derived from msg. Forwards the optional output unit.
!
! Arguments:
!   obj : TYPE(UserFunction_)(:) - SIZE(obj) objects to display
!   msg : CHARACTER(*) - base heading
!   unitNo : INTEGER(I4B), optional - output unit
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE UserFunction_Class, ONLY: UserFunction_
! USE UserFunction_Class, ONLY: UserFunctionDisplay
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj(2)
! INTEGER(I4B) :: unitNo, ios
! OPEN(NEWUNIT=unitNo, STATUS="SCRATCH", FORM="FORMATTED")
! CALL UserFunctionDisplay(obj, "Functions", unitNo)
! REWIND(unitNo)
! READ(unitNo, *, IOSTAT=ios)
! CALL OK(ios == 0, "Array display produced output")
! CLOSE(unitNo)
! END PROGRAM main
!```
!
! See also: [[obj_Display]], [[obj_Display_Ptr_Vector]]

INTERFACE UserFunctionDisplay
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(UserFunction_), INTENT(INOUT) :: obj(:)
    !! Array of user functions to display
    CHARACTER(*), INTENT(IN) :: msg
    !! Base heading for indexed element headings
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! Output unit; omitted to use the Display_Method default
  END SUBROUTINE obj_Display_Vector
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-08-27
! summary: Display associated targets in a pointer-wrapper array
!
! Prints msg and the association status of each wrapper, then displays
! each associated target. Uses indexed headings derived from msg and
! forwards unitNo. Disassociated entries have no target data to print.
!
! Arguments:
!   obj : TYPE(UserFunctionPointer_)(:) - SIZE(obj) pointer wrappers
!   msg : CHARACTER(*) - base heading
!   unitNo : INTEGER(I4B), optional - output unit
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE UserFunction_Class, ONLY: UserFunction_
! USE UserFunction_Class, ONLY: UserFunctionPointer_
! USE UserFunction_Class, ONLY: UserFunctionDisplay
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_), TARGET :: target
! TYPE(UserFunctionPointer_) :: obj(2)
! INTEGER(I4B) :: unitNo, ios
! obj(1)%ptr => target
! OPEN(NEWUNIT=unitNo, STATUS="SCRATCH", FORM="FORMATTED")
! CALL UserFunctionDisplay(obj, "Functions", unitNo)
! REWIND(unitNo)
! READ(unitNo, *, IOSTAT=ios)
! CALL OK(ios == 0, "Pointer display produced output")
! CLOSE(unitNo)
! END PROGRAM main
!```
!
! See also: [[obj_Display]], [[UserFunctionPointer_]]

INTERFACE UserFunctionDisplay
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(UserFunctionPointer_), INTENT(INOUT) :: obj(:)
    !! Pointer wrappers; disassociated entries are skipped
    CHARACTER(*), INTENT(IN) :: msg
    !! Base heading for indexed element headings
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! Output unit; omitted to use the Display_Method default
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE UserFunctionDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-20
! summary: Read stored constant-function fields from HDF5
!
! Reads name, returnType and argType from group, optionally reads
! isExternalFunc, and reads the rank-specific constant value dataset
! when present. The file must already be open for reading. Only constant
! nonexternal functions are supported; debug builds check these limits.
!
! This is not a complete restore: it does not call Initiate or restore
! isInit, engineID, numArgs, numReturns or returnShape. Prepare matching
! metadata and a constant backend before importing an evaluable function.
!
! Arguments:
!   obj : CLASS(UserFunction_) - destination with matching metadata
!   hdf5 : TYPE(HDF5File_) - open readable file
!   group : CHARACTER(*) - parent path; empty string selects the root
!
! Examples
!
! The program creates userfunction_import.h5 in the working directory.
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: source, restored
! TYPE(HDF5File_) :: file
! REAL(DFP) :: val
! CALL source%Initiate("f", opt%Scalar, opt%Constant)
! CALL source%SetScalarConstantVal(2.0_DFP)
! CALL file%Initiate(filename="userfunction_import.h5", MODE="NEW")
! CALL file%Open()
! CALL source%Export(file, "")
! CALL file%DEALLOCATE()
! CALL restored%Initiate("f", opt%Scalar, opt%Constant)
! CALL restored%SetScalarConstantVal(0.0_DFP)
! CALL file%Initiate(filename="userfunction_import.h5", MODE="READ")
! CALL file%Open()
! CALL restored%Import(file, "")
! CALL file%DEALLOCATE()
! CALL restored%Get(val)
! CALL OK(val == 2.0_DFP, "Imported scalar constant")
! END PROGRAM main
!```
!
! See also: [[obj_Export]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Destination; evaluation metadata is not restored by this routine
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file already open for reading
    CHARACTER(*), INTENT(IN) :: group
    !! Parent dataset path; empty string selects root datasets
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! update:
!   - 2026-10-24: Document table schema and backend selection.
! summary: Configure a user function from a TOML table
!
! Clears obj, marks it initiated and reads name (default UserFunction),
! required returnType and argType strings, optional counts, and the
! matrix returnShape when applicable. Omitted counts use the same
! TypeUserFunctionOpt defaults as Initiate; matrix counts should be
! supplied explicitly to match the shape product.
!
! luaScript plus luaFunctionName selects Lua. A rank-appropriate value
! selects a constant backend. equationParser=true with vars and
! expression selects component expressions. Readers run in that order,
! so later setters can replace an earlier engine selection. Supply one
! backend definition, not conflicting alternatives. Vector expressions
! use an array; matrix expressions use a matrix matching returnShape.
!
! Arguments:
!   obj : CLASS(UserFunction_) - function to replace
!   table : TYPE(toml_table) - function configuration, not a parent table
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE tomlf, ONLY: toml_table
! USE tomlf, ONLY: set_value
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(toml_table) :: table
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val
! CALL set_value(table, "name", "f")
! CALL set_value(table, "returnType", "Scalar")
! CALL set_value(table, "argType", "Constant")
! CALL set_value(table, "value", 2.0_DFP)
! CALL obj%ImportFromToml(table=table)
! CALL obj%Get(val)
! CALL OK(obj%IsInitiated(), "Imported metadata")
! CALL OK(val == 2.0_DFP, "Imported constant backend")
! END PROGRAM main
!```
!
! See also: [[obj_ImportFromToml2]], [[obj_Initiate]]

INTERFACE UserFunctionImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Function whose prior configuration is cleared
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Function configuration table containing metadata and backend keys
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE UserFunctionImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-08
! summary: Load a named TOML table and import its function configuration
!
! Parses a file through TomlUtility GetValue, obtains the top-level table
! named tomlName and delegates to [[obj_ImportFromToml1]]. Supply either
! filename or a readable TxtFile_ through afile. The temporary parsed
! table is released after import. printToml is currently unused.
!
! Arguments:
!   obj : CLASS(UserFunction_) - function to replace
!   tomlName : CHARACTER(*) - key of the function table in the TOML root
!   afile : TYPE(TxtFile_), optional - input file wrapper
!   filename : CHARACTER(*), optional - TOML file path
!   printToml : LOGICAL(LGT), optional - unused compatibility argument
!
! Examples
!
! The program creates and then deletes its own TOML input file.
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! INTEGER(I4B) :: unitNo
! REAL(DFP) :: val
! OPEN(NEWUNIT=unitNo, FILE="userfunction_doc.toml", STATUS="REPLACE")
! WRITE(unitNo, '(A)') '[load]', 'name="f"', &
!   'returnType="Scalar"', 'argType="Constant"', 'value=2.0'
! CLOSE(unitNo)
! CALL obj%ImportFromToml(tomlName="load", &
!   filename="userfunction_doc.toml")
! CALL obj%Get(val)
! CALL OK(val == 2.0_DFP, "File import")
! OPEN(NEWUNIT=unitNo, FILE="userfunction_doc.toml", STATUS="OLD")
! CLOSE(unitNo, STATUS="DELETE")
! END PROGRAM main
!```
!
! See also: [[obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Function to replace with the imported configuration
    CHARACTER(*), INTENT(IN) :: tomlName
    !! Key of the function table in the parsed root table
    TYPE(TxtFile_), INTENT(INOUT), OPTIONAL :: afile
    !! Readable input file wrapper, alternative to filename
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! TOML input path, alternative to afile
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: printToml
    !! Currently unused; does not control printing
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-20
! summary: Write constant-function fields to HDF5
!
! Requires an open writable file. Writes name, isExternalFunc, returnType
! and argType under group, followed by scalarValue, vectorValue or
! matrixValue for the active return rank. Only constant, nonexternal
! functions are supported. Callbacks, scripts, expressions, engineID,
! initialization status, counts and returnShape are not serialized.
! Consequently this is not a complete evaluation-state checkpoint.
!
! Arguments:
!   obj : CLASS(UserFunction_) - configured constant function
!   hdf5 : TYPE(HDF5File_) - open writable file
!   group : CHARACTER(*) - parent dataset path; empty string for root
!
! Examples
!
! The program creates userfunction_export.h5 in the working directory.
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! TYPE(HDF5File_) :: file
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL obj%SetScalarConstantVal(2.0_DFP)
! CALL file%Initiate(filename="userfunction_export.h5", MODE="NEW")
! CALL file%Open()
! CALL obj%Export(file, "")
! CALL OK(file%PathExists("/scalarValue"), "Constant dataset written")
! CALL file%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(UserFunction_), INTENT(IN) :: obj
    !! Configured constant function to serialize
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file already open for writing
    CHARACTER(*), INTENT(IN) :: group
    !! Parent dataset path; empty string selects root datasets
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-14
! summary: Set the stored function name
!
! Assigns name to fixed-length storage of TypeUserFunctionOpt%maxlen
! characters. Longer names are truncated and shorter names padded. Does
! not initialize the object or change its evaluation backend.
!
! Arguments:
!   obj : CLASS(UserFunction_) - function to rename
!   name : CHARACTER(*) - replacement name
!
! Examples
!
!```fortran
! PROGRAM main
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! CALL obj%SetName("load")
! CALL OK(obj%GetName() == "load", "Renamed function")
! END PROGRAM main
!```
!
! See also: [[obj_GetName]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Function to rename
    CHARACTER(*), INTENT(IN) :: name
    !! Replacement name, truncated or padded to fixed-length storage
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetLuaScript@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select a Lua script and function for evaluation
!
! Stores the script path and function name, sets isLuaScript and selects
! the Lua engine. Initiate must be called first with matching numArgs,
! numReturns and, for matrices, returnShape. Does not load or validate
! the script here; evaluation requires a USE_LUA-enabled library.
! Inputs are positional real arguments and outputs are separate numeric
! return values. Matrix returns follow Fortran column-major order.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated function
!   luaScript : CHARACTER(*) - script path, stored in fixed-length storage
!   luaFunctionName : CHARACTER(*) - global Lua function name
!
! Examples
!
! Requires Lua support. Creates and then deletes its Lua script.
!
!```fortran
! PROGRAM main
!   USE GlobalData, ONLY: DFP, I4B, LGT
!   USE UserFunction_Class, ONLY: UserFunction_
!   USE BaseType, ONLY: varopt => TypeFEVariableOpt
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
!   USE Test_Method, ONLY: OK, ISNEAR
!   IMPLICIT NONE
!   TYPE(UserFunction_) :: obj
!   CHARACTER(*), PARAMETER :: testname = "Scalar func: lua script test", &
!     luaScript = "./scalarfuncs.lua", &
!     luaFunctionName = "Func1"
!   INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 1
!   REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
!   REAL(DFP) :: want, got
!   CALL e%setQuietMode(math%yes)
!   CALL obj%Initiate(NAME="func", &
!                     returnType=varopt%scalar, &
!                     argType=varopt%space, &
!                     numArgs=numArgs, &
!                     numReturns=numReturns)
!   CALL obj%SetLuaScript(luaScript=luaScript, &
!                         luaFunctionName=luaFunctionName)
!   CALL obj%Get_(val=got, args=args)
!   want = val
!   CALL OK(got.APPROXEQ.want, testname)
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetMatrixValue_1]]

INTERFACE
  MODULE SUBROUTINE obj_SetLuaScript(obj, luaScript, luaFunctionName)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated function with configured input/output counts
    CHARACTER(*), INTENT(IN) :: luaScript
    !! Path to the script loaded during evaluation
    CHARACTER(*), INTENT(IN) :: luaFunctionName
    !! Name of the global Lua function to invoke
  END SUBROUTINE obj_SetLuaScript
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetScalarConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a scalar user function
!
! Stores val and selects the constant engine. obj must be initiated
! with scalar returnType; use Constant argType for a no-input function.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated scalar function
!   val : REAL(DFP) - constant scalar value
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val
! CALL obj%Initiate("f", opt%Scalar, opt%Constant)
! CALL obj%SetScalarConstantVal(2.0_DFP)
! CALL obj%Get(val)
! CALL OK(val == 2.0_DFP, "Scalar constant")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetScalarValue_1]]

INTERFACE
  MODULE SUBROUTINE obj_SetScalarConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated scalar function
    REAL(DFP), INTENT(IN) :: val
    !! Constant value stored by the scalar backend
  END SUBROUTINE obj_SetScalarConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetScalarFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select an external scalar Fortran callback
!
! Associates scalarFunction with func, sets isExternalFunc and selects
! the external engine. Initiate obj with scalar returnType first. func
! must be associated with a procedure matching InterfaceScalarSubroutine
! and remain valid while obj uses it; no procedure or closure is copied.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated scalar function
!   func : PROCEDURE(InterfaceScalarSubroutine), pointer - callback
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: func
! REAL(DFP) :: val
! CALL obj%Initiate("f", opt%Scalar, opt%Space, numArgs=1)
! func => evaluate
! CALL obj%SetScalarFunctionPointer(func)
! CALL obj%Get(val=val, args=[2.0_DFP])
! CALL OK(val == 4.0_DFP, "Scalar callback")
! CONTAINS
! SUBROUTINE evaluate(args, nargs, ans)
! REAL(DFP), INTENT(IN) :: args(:)
! INTEGER(I4B), INTENT(IN) :: nargs
! REAL(DFP), INTENT(INOUT) :: ans
! ans = 2.0_DFP * SUM(args(1:nargs))
! END SUBROUTINE evaluate
! END PROGRAM main
!```
!
! See also: [[obj_GetScalarValue_1]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetScalarFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated scalar function
    PROCEDURE(InterfaceScalarSubroutine), POINTER, INTENT(INOUT) :: func
    !! Associated callback; its target must outlive use by obj
  END SUBROUTINE obj_SetScalarFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetScalarEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select a parsed scalar expression for evaluation
!
! Initiates scalarEqParser with funcStr and var, then selects the parser
! engine. Initiate obj as a scalar function first and set numArgs to
! SIZE(var). During evaluation args(i) supplies the value of var(i).
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated scalar function
!   funcStr : CHARACTER(*) - scalar mathematical expression
!   var : CHARACTER(*)(:) - SIZE(var) ordered variable names
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val
! CALL obj%Initiate("f", opt%Scalar, opt%Space, numArgs=1)
! CALL obj%SetScalarEquationParser("2*x", ["x"])
! CALL obj%Get(val=val, args=[2.0_DFP])
! CALL OK(val == 4.0_DFP, "Scalar expression")
! END PROGRAM main
!```
!
! See also: [[obj_GetScalarValue_1]], [[obj_SetVectorEquationParser]]

INTERFACE
  MODULE SUBROUTINE obj_SetScalarEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated scalar function with numArgs matching SIZE(var)
    CHARACTER(LEN=*), INTENT(IN) :: funcStr
    !! Scalar mathematical expression to parse
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Ordered names corresponding to entries in evaluation args
  END SUBROUTINE obj_SetScalarEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetVectorConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Set the constant value for a vector user function
!
! Copies the first numReturns entries of val to internal vector storage
! and selects the constant engine. Initiate obj with vector returnType
! first. numReturns must fit the fixed vector capacity.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated vector function
!   val : REAL(DFP)(:) - constants, SIZE(val) >= numReturns
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2)
! INTEGER(I4B) :: tsize
! CALL obj%Initiate("f", opt%Vector, opt%Constant, numReturns=2)
! CALL obj%SetVectorConstantVal([1.0_DFP, 2.0_DFP])
! CALL obj%Get_(val=val, tsize=tsize)
! CALL OK(tsize == 2, "Two constants")
! CALL OK(ALL(val == [1.0_DFP, 2.0_DFP]), "Vector constants")
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue_1]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetVectorConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated vector function
    REAL(DFP), INTENT(IN) :: val(:)
    !! Constants; the first numReturns entries are copied
  END SUBROUTINE obj_SetVectorConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetVectorFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select an external vector Fortran callback
!
! Associates vectorFunction with func, sets isExternalFunc and selects
! the external engine. Initiate obj with vector returnType first. The
! callback must match InterfaceVectorSubroutine and remain valid while
! referenced. It writes ans and sets tsize to the valid component count;
! it must not exceed the caller's output capacity.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated vector function
!   func : PROCEDURE(InterfaceVectorSubroutine), pointer - callback
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceVectorSubroutine
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! PROCEDURE(InterfaceVectorSubroutine), POINTER :: func
! REAL(DFP) :: val(2)
! INTEGER(I4B) :: tsize
! CALL obj%Initiate("f", opt%Vector, opt%Space, &
!   numArgs=1, numReturns=2)
! func => evaluate
! CALL obj%SetVectorFunctionPointer(func)
! CALL obj%Get_(val=val, tsize=tsize, args=[2.0_DFP])
! CALL OK(tsize == 2, "Callback size")
! CALL OK(ALL(val == [2.0_DFP, 4.0_DFP]), "Vector callback")
! CONTAINS
! SUBROUTINE evaluate(args, nargs, ans, tsize)
! REAL(DFP), INTENT(IN) :: args(:)
! INTEGER(I4B), INTENT(IN) :: nargs
! REAL(DFP), INTENT(INOUT) :: ans(:)
! INTEGER(I4B), INTENT(OUT) :: tsize
! tsize = 2
! ans(1:2) = [SUM(args(1:nargs)), 2.0_DFP * SUM(args(1:nargs))]
! END SUBROUTINE evaluate
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue_1]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetVectorFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated vector function
    PROCEDURE(InterfaceVectorSubroutine), POINTER, INTENT(IN) :: func
    !! Associated callback; its target must outlive use by obj
  END SUBROUTINE obj_SetVectorFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetVectorEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select one parsed expression per vector component
!
! Creates an EquationParser_ target for each component expression and
! selects the parser engine. Existing associated parser targets have
! their data cleared before replacement. Initiate obj with vector return
! type, numReturns=SIZE(funcStr) and numArgs=SIZE(var) first. All component
! expressions share var ordering. Keep numReturns within vector capacity.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated vector function
!   funcStr : TYPE(String)(:) - SIZE(funcStr) component expressions
!   var : CHARACTER(*)(:) - SIZE(var) ordered variable names
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE String_Class, ONLY: String
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! TYPE(String) :: expressions(2)
! REAL(DFP) :: val(2)
! INTEGER(I4B) :: tsize
! expressions(1) = "x"
! expressions(2) = "2*x"
! CALL obj%Initiate("f", opt%Vector, opt%Space, &
!   numArgs=1, numReturns=2)
! CALL obj%SetVectorEquationParser(expressions, ["x"])
! CALL obj%Get_(val=val, tsize=tsize, args=[2.0_DFP])
! CALL OK(tsize == 2, "Expression count")
! CALL OK(ALL(val == [2.0_DFP, 4.0_DFP]), "Vector expressions")
! END PROGRAM main
!```
!
! See also: [[obj_GetVectorValue_1]], [[obj_SetScalarEquationParser]]

INTERFACE
  MODULE SUBROUTINE obj_SetVectorEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated vector function with matching input/output counts
    TYPE(String), INTENT(IN) :: funcStr(:)
    !! One expression per output component; SIZE(funcStr)=numReturns
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Ordered names corresponding to entries in evaluation args
  END SUBROUTINE obj_SetVectorEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!                                             SetMatrixConstantVal@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Store a constant matrix and select constant evaluation
!
! Copies the leading returnShape block of val into matrixValue and
! selects the constant engine. Initiate obj with matrix returnType and
! matching shape and component count first. Each shape extent must fit
! the fixed matrix storage capacity.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated matrix function
!   val : REAL(DFP)(:,:) - constant matrix; each extent >= returnShape
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! REAL(DFP) :: val(2, 2), want(2, 2)
! INTEGER(I4B) :: nrow, ncol
! want = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
! CALL obj%Initiate("f", opt%Matrix, opt%Constant, &
!   numReturns=4, returnShape=[2, 2])
! CALL obj%SetMatrixConstantVal(want)
! CALL obj%Get_(val=val, nrow=nrow, ncol=ncol)
! CALL OK(nrow == 2 .AND. ncol == 2, "Constant matrix shape")
! CALL OK(ALL(val == want), "Matrix constants")
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue_1]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixConstantVal(obj, val)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated matrix function with a configured returnShape
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! Constant matrix; its leading returnShape block is copied
  END SUBROUTINE obj_SetMatrixConstantVal
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetMatrixFunctionPointer@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select an external matrix Fortran callback
!
! Associates matrixFunction with func, sets isExternalFunc and selects
! the external engine. Initiate obj with matrix returnType first. func
! must match InterfaceMatrixSubroutine and remain valid while referenced.
! It writes ans and reports nrow and ncol within the output capacity;
! keep these consistent with the configured returnShape.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated matrix function
!   func : PROCEDURE(InterfaceMatrixSubroutine), pointer - callback
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceMatrixSubroutine
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! PROCEDURE(InterfaceMatrixSubroutine), POINTER :: func
! REAL(DFP) :: val(2, 2)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Initiate("f", opt%Matrix, opt%Space, numArgs=1, &
!   numReturns=4, returnShape=[2, 2])
! func => evaluate
! CALL obj%SetMatrixFunctionPointer(func)
! CALL obj%Get_(val=val, nrow=nrow, ncol=ncol, args=[2.0_DFP])
! CALL OK(nrow == 2 .AND. ncol == 2, "Callback matrix shape")
! CALL OK(ALL(val == 2.0_DFP), "Matrix callback")
! CONTAINS
! SUBROUTINE evaluate(args, nargs, ans, nrow, ncol)
! REAL(DFP), INTENT(IN) :: args(:)
! INTEGER(I4B), INTENT(IN) :: nargs
! REAL(DFP), INTENT(INOUT) :: ans(:, :)
! INTEGER(I4B), INTENT(OUT) :: nrow, ncol
! nrow = 2
! ncol = 2
! ans(1:nrow, 1:ncol) = SUM(args(1:nargs))
! END SUBROUTINE evaluate
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue_1]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixFunctionPointer(obj, func)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated matrix function
    PROCEDURE(InterfaceMatrixSubroutine), POINTER, INTENT(IN) :: func
    !! Associated callback; its target must outlive use by obj
  END SUBROUTINE obj_SetMatrixFunctionPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMatrixEquationParser@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Select a parsed expression for each matrix entry
!
! Creates a parser target for every entry in the returnShape block and
! selects the parser engine. Existing associated parser targets have
! their data cleared before replacement. Initiate obj with matrix return
! type, returnShape=SHAPE(funcStr), numReturns=SIZE(funcStr) and
! numArgs=SIZE(var) first. All entries share the same variable ordering.
! Each matrix extent must fit the fixed parser-array capacity.
!
! Arguments:
!   obj : CLASS(UserFunction_) - initiated matrix function
!   funcStr : TYPE(String)(:,:) - expressions with shape returnShape
!   var : CHARACTER(*)(:) - SIZE(var) ordered variable names
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: opt => TypeFEVariableOpt
! USE String_Class, ONLY: String
! USE UserFunction_Class, ONLY: UserFunction_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(UserFunction_) :: obj
! TYPE(String) :: expressions(2, 2)
! REAL(DFP) :: val(2, 2)
! INTEGER(I4B) :: nrow, ncol
! expressions(1, 1) = "x"
! expressions(2, 1) = "2*x"
! expressions(1, 2) = "3*x"
! expressions(2, 2) = "4*x"
! CALL obj%Initiate("f", opt%Matrix, opt%Space, numArgs=1, &
!   numReturns=4, returnShape=[2, 2])
! CALL obj%SetMatrixEquationParser(expressions, ["x"])
! CALL obj%Get_(val=val, nrow=nrow, ncol=ncol, args=[2.0_DFP])
! CALL OK(nrow == 2 .AND. ncol == 2, "Expression matrix shape")
! CALL OK(ALL(val == RESHAPE([2.0_DFP, 4.0_DFP, &
!   6.0_DFP, 8.0_DFP], [2, 2])), "Matrix expressions")
! END PROGRAM main
!```
!
! See also: [[obj_GetMatrixValue_1]], [[obj_SetVectorEquationParser]]

INTERFACE
  MODULE SUBROUTINE obj_SetMatrixEquationParser(obj, funcStr, var)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    !! Initiated matrix function with matching shape and input count
    TYPE(String), INTENT(IN) :: funcStr(:, :)
    !! One expression per matrix entry; SHAPE(funcStr)=returnShape
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: var
    !! Ordered names corresponding to entries in evaluation args
  END SUBROUTINE obj_SetMatrixEquationParser
END INTERFACE

!----------------------------------------------------------------------------
!                                    CheckError_Scalar_Lua@ScalarLuaMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: private method to check error while getting value from scalar lua
!
! This method is private. It is used in [[GetScalarValue1_]] and
! [[GetScalarValue2_]] method when engineID is Lua.
! These methods are defined in ScalarLuaMethods submodule.

INTERFACE
  MODULE SUBROUTINE CheckError_Scalar_Lua(obj, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
  END SUBROUTINE CheckError_Scalar_Lua
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetScalarValue_Lua1@ScalarLuaMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get value from scalar lua user function
!
! This is a private method. It is called from [[GetScalarValue1_]]
! and [[GetScalarValue2_]].
INTERFACE
  MODULE SUBROUTINE GetScalarValue_Lua1(obj, val, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:)
  END SUBROUTINE GetScalarValue_Lua1
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetScalarValue_Lua2@ScalarLuaMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get value from scalar lua user function
!
! This is a private method. It is called from [[GetScalarValue1_]]
! and [[GetScalarValue2_]].
INTERFACE
  MODULE SUBROUTINE GetScalarValue_Lua2(obj, val, tsize, args)
    CLASS(UserFunction_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: val(:)
    INTEGER(I4B), INTENT(IN) :: tsize
    !! note tsize is intent(IN)
    REAL(DFP), INTENT(IN), OPTIONAL :: args(:, :)
  END SUBROUTINE GetScalarValue_Lua2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UserFunction_Class
